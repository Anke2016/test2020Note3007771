  METHOD if_cts_hta_api_factory~create_package_by_hana_name.
    r_result = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = i_hana_package_name ).
  ENDMETHOD.