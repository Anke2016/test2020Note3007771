  METHOD get_deploy_state_aggregated.
    DATA: lr_component             TYPE REF TO if_cts_hta_component,
          lv_deployed_found        TYPE abap_bool,
          lv_partly_deployed_found TYPE abap_bool,
          lv_not_deployed_found    TYPE abap_bool.

    IF i_components IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_components INTO lr_component.
      CASE lr_component->get_deploy_state( ).
        WHEN ce_cts_hta_deploy_state=>deployed.
          lv_deployed_found = abap_true.
        WHEN ce_cts_hta_deploy_state=>not_deployed.
          lv_not_deployed_found = abap_true.
        WHEN ce_cts_hta_deploy_state=>partly_deployed.
          r_result = ce_cts_hta_deploy_state=>partly_deployed.
          RETURN.
        WHEN OTHERS.
          "should not happen (only in case ce_cts_hta_deploy_state is extended with new values...)
      ENDCASE.

      IF lv_deployed_found = abap_true AND lv_not_deployed_found = abap_true AND i_partly_deployed_supported = abap_false.
        r_result = ce_cts_hta_deploy_state=>partly_deployed.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF lv_deployed_found = abap_true AND lv_not_deployed_found = abap_true. "this if with both abap_true is needed again because of the case i_partly_deployed_supported = abap_true...
      r_result = ce_cts_hta_deploy_state=>partly_deployed.
    ELSEIF lv_deployed_found = abap_true AND lv_not_deployed_found = abap_false.
      r_result = ce_cts_hta_deploy_state=>deployed.
    ELSEIF lv_deployed_found = abap_false AND lv_not_deployed_found = abap_true.
      r_result = ce_cts_hta_deploy_state=>not_deployed.
    ENDIF.
  ENDMETHOD.