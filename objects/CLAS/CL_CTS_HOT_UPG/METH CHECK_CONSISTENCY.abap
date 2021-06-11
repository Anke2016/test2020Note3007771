  METHOD check_consistency.
    rv_success = abap_true.
    IF sy-dbsys <> 'HDB'.
      RETURN.
    ENDIF.

    mr_logger->message( iv_msg_nr = '401'
                        iv_level  = 2 ). "Beginn Konsistenzprüfung der HANA-Repository-Pakete und -Objekte

    DATA(lv_success_prework)        = me->check_missing_prework_for_pkgs( ).
    DATA(lv_success_deployed_pkgs)  = me->check_not_deployed_packages( ).
    DATA(lv_success_deployed_objs)  = me->check_not_deployed_objects( ).
    DATA(lv_success_object_version) = me->check_object_versions( lv_success_deployed_objs ).

    IF lv_success_prework = abap_false
        OR lv_success_deployed_pkgs  = abap_false
        OR lv_success_deployed_objs  = abap_false
        OR lv_success_object_version = abap_false.
      rv_success = abap_false.
      mr_logger->error( '409' ). "  Inkonsistenzen zwischen HTA und HANA gefunden. Siehe Hinweis 2776773
    ENDIF.

    mr_logger->message( iv_msg_nr = '402'
                        iv_level  = 2
                        iv_var1   = COND #( WHEN rv_success = abap_true THEN 'Success'(002) ELSE 'Error'(001) ) ). "Ende Konsistenzprüfung der HANA-Repository-Pakete und -Objekte: &1

    mr_logger->flush( ).
  ENDMETHOD.