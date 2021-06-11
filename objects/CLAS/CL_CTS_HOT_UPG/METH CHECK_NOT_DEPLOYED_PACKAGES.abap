  METHOD check_not_deployed_packages.
    SELECT abap_hana_package_id, hana_package_id FROM cts_hot_package
                                                 WHERE abap_status = 'A'
                                                   AND hot_status NOT IN ( 'A', 'N' )
                                                 ORDER BY hana_package_id ASCENDING
                                                 INTO TABLE @DATA(lt_not_deployed_packages).

    LOOP AT mt_packages_without_prework REFERENCE INTO DATA(lr_package).
      DELETE lt_not_deployed_packages WHERE abap_hana_package_id = lr_package->abap_hana_package_id.
    ENDLOOP.

    IF lt_not_deployed_packages IS INITIAL.
      rv_success = abap_true.
      mr_logger->message( iv_msg_nr = COND #( WHEN mt_packages_without_prework IS INITIAL
                                              THEN '405'   "  Alle HANA-Repository-Pakete sind deployt
                                              ELSE '412' ) "  Alle Pakete sind deployt (Ausnahme: Pakete mit fehlender Vorarbeit)
                          iv_level = 3 ).
    ELSE.
      rv_success = abap_false.
      mr_logger->error( '406' ). "  Folgende HANA-Repository-Pakete sind nicht deployt:
      LOOP AT lt_not_deployed_packages REFERENCE INTO DATA(lr_not_dep_pkg).
        mr_logger->long_text( iv_level = 2
                              iv_severity = 'E'
                              iv_text = |{ lr_not_dep_pkg->abap_hana_package_id } --> { lr_not_dep_pkg->hana_package_id } [{ mr_helper->get_akh_for_hota( lr_not_dep_pkg->abap_hana_package_id ) }]| ).
      ENDLOOP.
    ENDIF.

    mr_logger->flush( ).
  ENDMETHOD.