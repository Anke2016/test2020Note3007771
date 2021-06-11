  METHOD if_cts_hot_ext_call_internal~is_view_layer_system.
    rv_result = cl_system_view_layer=>is_view_layer_active( ).
  ENDMETHOD.