  METHOD if_cts_hot_db_access~exist_broken_object_or_package.
    DATA: ls_hot_object  TYPE cts_hot_object,
          ls_hot_package TYPE cts_hot_package.

    r_result = abap_false.

    SELECT SINGLE abap_hana_package_id abap_hana_object_name_suffix abap_status FROM cts_hot_object INTO ls_hot_object
                        WHERE ( hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error OR hot_status = if_cts_hot_db_access=>co_hot_status_delete_error )
                              AND abap_status = 'A'.    "#EC CI_NOFIRST

    IF ls_hot_object IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      "check if broken packages exist and in case trigger deploy_all_failed via job
      SELECT SINGLE abap_hana_package_id abap_status FROM cts_hot_package INTO ls_hot_package
                        WHERE ( hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error OR hot_status = if_cts_hot_db_access=>co_hot_status_delete_error )
                              AND abap_status = 'A'.
      IF ls_hot_package IS NOT INITIAL.
        r_result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.