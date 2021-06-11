  METHOD if_cts_hot_ext_call_internal~get_switch_id_for_devclass.
    r_result = cl_switch=>sw_devclass( p_package = i_devclass ).
  ENDMETHOD.