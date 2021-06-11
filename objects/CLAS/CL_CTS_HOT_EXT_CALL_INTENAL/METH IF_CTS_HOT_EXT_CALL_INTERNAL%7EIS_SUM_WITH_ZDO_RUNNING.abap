  METHOD if_cts_hot_ext_call_internal~is_sum_with_zdo_running.
    TRY.
        CALL METHOD ('CL_UPG_UPGPARAM_HANDLER')=>('USE_ZDO_HTA_HDI')
          RECEIVING
            p_bool = rv_result.
      CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
        "system not in upgrade
    ENDTRY.
  ENDMETHOD.