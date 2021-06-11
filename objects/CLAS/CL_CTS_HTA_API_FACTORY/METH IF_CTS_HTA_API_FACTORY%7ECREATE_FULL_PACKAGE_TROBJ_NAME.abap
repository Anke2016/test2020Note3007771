  METHOD if_cts_hta_api_factory~create_full_package_trobj_name.
    DATA: lr_package TYPE REF TO if_cts_hta_package,
          lt_objects TYPE if_cts_hta_types=>ty_cts_hta_objects.

    lr_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = i_transport_object_name ).
    lt_objects = cl_cts_hta_object=>create_instances_from_package( i_cts_hta_package = lr_package
                                                                   i_search_in_hta_only = abap_false ).

    r_result = cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = lr_package
                                                                      i_cts_hta_objects = lt_objects ).
  ENDMETHOD.