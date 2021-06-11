  METHOD if_cts_hta_api_factory~create_object_by_hana_name.
    DATA(lr_hta_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = i_hana_package_name ).
    r_result = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package        = lr_hta_package
                                                                 i_hana_object_name   = i_hana_object_name
                                                                 i_hana_object_suffix = i_hana_object_suffix ).
  ENDMETHOD.