  METHOD if_cts_hta_api_factory~create_package_by_trobj_name.
    r_result = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = i_transport_object_name ).
  ENDMETHOD.