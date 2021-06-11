  METHOD if_cts_hot_ext_call_internal~is_blue_green_update_running.
    TRY.
        IF NEW cl_soi_deploy_parameters( )->is_deployment_running( ) = abap_true.
          rv_result = abap_true.
        ENDIF.
      CATCH cx_soi INTO DATA(lr_exc).
        RAISE EXCEPTION TYPE cx_cts_hta EXPORTING previous = lr_exc.
    ENDTRY.
  ENDMETHOD.