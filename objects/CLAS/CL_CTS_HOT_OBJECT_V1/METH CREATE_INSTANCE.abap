  METHOD create_instance.

    DATA(lo_package) = cl_cts_hot_package=>create_instance( iv_hana_package_id = iv_hana_package_id ).
    r_result = create_instance2( io_cts_hot_package = lo_package iv_hana_object_name = iv_hana_object_name iv_hana_object_suffix = iv_hana_object_suffix ).

  ENDMETHOD.