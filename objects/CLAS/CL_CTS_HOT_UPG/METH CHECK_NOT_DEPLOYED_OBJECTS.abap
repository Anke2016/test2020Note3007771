  METHOD check_not_deployed_objects.
    DATA lv_trobj_name TYPE trobj_name.

    SELECT abap_hana_package_id, abap_hana_object_name_suffix, hana_package_id, hana_object_name, hana_object_suffix
                                 FROM cts_hot_object
                                 WHERE abap_status = 'A'
                                   AND hot_status NOT IN ( 'A', 'N' )
                                 ORDER BY hana_package_id ASCENDING, hana_object_name ASCENDING, hana_object_suffix ASCENDING
                                 INTO TABLE @DATA(lt_not_deployed_objects). "#EC CI_NOFIRST

    LOOP AT mt_packages_without_prework REFERENCE INTO DATA(lr_package).
      DELETE lt_not_deployed_objects WHERE abap_hana_package_id = lr_package->abap_hana_package_id.
    ENDLOOP.

    IF lt_not_deployed_objects IS INITIAL.
      rv_success = abap_true.
      mr_logger->message( iv_msg_nr = COND #( WHEN mt_packages_without_prework IS INITIAL
                                              THEN '407'   "  Alle HANA-Repository-Objekte sind deployt
                                              ELSE '413' ) "  Alle Objekte, für deren Pakete die Vorarbeit erledigt ist, sind deployt
                          iv_level = 3 ).
    ELSE.
      rv_success = abap_false.
      mr_logger->error( '408' ). "  Folgende HANA-Repository-Objekte sind nicht deployt:
      LOOP AT lt_not_deployed_objects REFERENCE INTO DATA(lr_not_dep_obj).
        lv_trobj_name = lr_not_dep_obj->abap_hana_package_id.
        lv_trobj_name+40 = lr_not_dep_obj->abap_hana_object_name_suffix.

        mr_logger->long_text( iv_msg_id = 'SCTS_HDI' "Use SCTS_HDI 100 here to keep spaces between abap_hana_package_id and abap_hana_object_name_suffix which is not working in SCTS_HOT
                              iv_msg_nr = '100' "    &1&2&3
                              iv_level = 2
                              iv_severity = 'E'
                              iv_text = |{ lv_trobj_name } --> |
                                     && |{ lr_not_dep_obj->hana_object_name }.{ lr_not_dep_obj->hana_object_suffix } ({ lr_not_dep_obj->hana_package_id }) [{ mr_helper->get_akh_for_hota( lr_not_dep_obj->abap_hana_package_id ) }]| ).
      ENDLOOP.
    ENDIF.

    mr_logger->flush( ).
  ENDMETHOD.