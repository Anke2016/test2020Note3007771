  METHOD if_cts_hot_ext_call_internal~is_switch_switched_off.
    IF cl_abap_switch=>get_overall_switch_state( p_switch_id = i_switch_id ) =  cl_abap_switch=>c_off. "ignore client
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.