  METHOD if_cts_hta_api_factory~create_component_list.
    r_result = cl_cts_hta_component_list=>create_instance( ).
  ENDMETHOD.