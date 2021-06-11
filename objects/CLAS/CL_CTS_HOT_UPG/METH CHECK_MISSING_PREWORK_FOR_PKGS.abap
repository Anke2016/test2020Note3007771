  METHOD check_missing_prework_for_pkgs.
    SELECT pa~abap_hana_package_id, pa~hana_package_id FROM cts_hot_package AS pa LEFT OUTER JOIN cts_hot_prework AS pr
                                                                                    ON pr~abap_hana_package_id = pa~abap_hana_package_id
                                                       WHERE pa~abap_status = 'A'
                                                         AND pa~hot_activation_mode = 'P'
                                                         AND ( pr~prework_done IS NULL OR pr~prework_done <> 'X' )
                                                       INTO TABLE @mt_packages_without_prework.
    IF mt_packages_without_prework IS INITIAL.
      rv_success = abap_true.
      mr_logger->message( iv_msg_nr = '403'
                          iv_level = 3 ). "Keine HANA-Repository-Pakete mit fehlender Vorarbeit gefunden
    ELSE.
      rv_success = abap_false.
      mr_logger->error( '404' ). "  Vorarbeit ist nicht erledigt für folgende Pakete:
      SORT mt_packages_without_prework BY hana_package_id ASCENDING.
      LOOP AT mt_packages_without_prework REFERENCE INTO DATA(lr_package).
        mr_logger->long_text( iv_level = 2
                              iv_severity = 'E'
                              iv_text = |{ lr_package->abap_hana_package_id } --> { lr_package->hana_package_id } [{ mr_helper->get_akh_for_hota( lr_package->abap_hana_package_id ) }]| ).
      ENDLOOP.
    ENDIF.

    mr_logger->flush( ).
  ENDMETHOD.