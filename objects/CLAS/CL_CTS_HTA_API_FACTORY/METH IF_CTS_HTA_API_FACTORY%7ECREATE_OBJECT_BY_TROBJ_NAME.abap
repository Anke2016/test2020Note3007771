  METHOD if_cts_hta_api_factory~create_object_by_trobj_name.
    "1. get hta_package for passed trobjname
    DATA(lr_hta_package) = me->if_cts_hta_api_factory~create_package_by_trobj_name( i_transport_object_name = i_transport_object_name ).
    "2. get hta_object for passed trobjname
    r_result = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = i_transport_object_name
                                                                 i_hta_package = lr_hta_package ).
  ENDMETHOD.