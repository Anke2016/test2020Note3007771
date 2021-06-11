  METHOD check_object_versions.
    DATA: lv_trobj_name TYPE trobj_name,
          lv_sql_cond   TYPE string.

    rv_success = abap_true.

    SELECT DISTINCT abap_hana_package_id, hana_package_id FROM cts_hot_object
                                                          WHERE abap_status = 'A'
                                                          ORDER BY hana_package_id
                                                          INTO TABLE @DATA(lt_packages_with_objects). "#EC CI_NOFIRST

    LOOP AT mt_packages_without_prework REFERENCE INTO DATA(lr_package).
      DELETE lt_packages_with_objects WHERE abap_hana_package_id = lr_package->abap_hana_package_id.
    ENDLOOP.

    TRY. "if there is any error in HANA communication, stop immediately

        lv_sql_cond = COND #( WHEN iv_check_consistency = abap_true
                              THEN |HOT_STATUS IN ( 'A', 'N' )|
                              ELSE |HOT_STATUS NOT IN ( 'D', 'Z' )| ).

        LOOP AT lt_packages_with_objects REFERENCE INTO DATA(lr_package_with_object).
          SELECT abap_hana_package_id, abap_hana_object_name_suffix, hana_package_id, hana_object_name, hana_object_suffix, hana_object_version, hana_content_bdata, hana_content_cdata
                                                       FROM cts_hot_object
                                                       WHERE abap_hana_package_id = @lr_package_with_object->abap_hana_package_id
                                                         AND abap_status = 'A'
                                                         AND (lv_sql_cond)
                                                       ORDER BY hana_object_name ASCENDING, hana_object_suffix ASCENDING
                                                       INTO TABLE @DATA(lt_different_objects). "#EC CI_DYNWHERE
          LOOP AT lt_different_objects REFERENCE INTO DATA(lr_different_object).
            DATA(lv_hana_version) = mr_helper->read_hana_object_version( iv_package_id = lr_different_object->hana_package_id
                                                                         iv_object_name = lr_different_object->hana_object_name
                                                                         iv_object_suffix = lr_different_object->hana_object_suffix ).

            "if version is different, check whether content is the same in HTA and HANA and only in case of different content report an error
            IF lv_hana_version <> lr_different_object->hana_object_version.
              mr_helper->read_hana_object_data(
                EXPORTING
                  iv_package_id = lr_different_object->hana_package_id
                  iv_object_name = lr_different_object->hana_object_name
                  iv_object_suffix = lr_different_object->hana_object_suffix
                IMPORTING
                  ev_cdata = DATA(lv_cdata)
                  ev_bdata = DATA(lv_bdata) ).

              IF lr_different_object->hana_content_cdata <> lv_cdata OR lr_different_object->hana_content_bdata <> lv_bdata.
                IF rv_success = abap_true.
                  rv_success = abap_false.

                  IF iv_check_consistency = abap_false.
                    RETURN.
                  ENDIF.
                  mr_logger->error( '411' ). "  Folgende HANA-Repository-Objekte sind unterschiedlich in HTA und HANA:
                ENDIF.

                lv_trobj_name = lr_different_object->abap_hana_package_id.
                lv_trobj_name+40 = lr_different_object->abap_hana_object_name_suffix.

                mr_logger->long_text( iv_msg_id = 'SCTS_HDI' "Use SCTS_HDI 100 here to keep spaces between abap_hana_package_id and abap_hana_object_name_suffix which is not working in SCTS_HOT
                                      iv_msg_nr = '100' "    &1&2&3
                                      iv_level = 2
                                      iv_severity = 'E'
                                      iv_text = |{ lv_trobj_name } --> |
                            && |{ lr_different_object->hana_object_name }.{ lr_different_object->hana_object_suffix } ({ lr_different_object->hana_package_id }) [{ mr_helper->get_akh_for_hota( lr_different_object->abap_hana_package_id ) }]| ).
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      CATCH cx_hana_object_transport INTO DATA(lr_exc).
        mr_logger->abnormal_termination_exception( lr_exc ).
        rv_success = abap_false.
    ENDTRY.

    IF rv_success = abap_true AND iv_check_consistency = abap_true.
      mr_logger->message( iv_msg_nr = COND #( WHEN mt_packages_without_prework IS INITIAL AND iv_all_objects_deployed = abap_true
                                              THEN '410'   "  Alle HANA-Repository-Objekte im HTA haben gleichen Inhalt wie in HANA
                                              ELSE '414' ) "  Alle noch nicht als Fehler protokol. Obj. sind gleich in HTA und HANA
                          iv_level = 3 ).
    ENDIF.

    mr_logger->flush( ).
  ENDMETHOD.