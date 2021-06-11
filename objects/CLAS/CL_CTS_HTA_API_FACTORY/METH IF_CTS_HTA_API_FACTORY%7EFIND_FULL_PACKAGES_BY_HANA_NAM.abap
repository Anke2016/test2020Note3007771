  METHOD if_cts_hta_api_factory~find_full_packages_by_hana_nam.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages,
          lr_package  TYPE REF TO if_cts_hta_package,
          lt_objects  TYPE if_cts_hta_types=>ty_cts_hta_objects.

    r_result = cl_cts_hta_component_list=>create_instance( ).

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( i_hana_package_name = i_hana_package_name
                                                                      i_search_in_hta_only = i_search_in_hta_only ).
    LOOP AT lt_packages INTO lr_package.
      lt_objects = cl_cts_hta_object=>create_instances_from_package( i_cts_hta_package = lr_package
                                                                     i_search_in_hta_only = abap_false ).

      r_result->add_component( cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = lr_package
                                                                                      i_cts_hta_objects = lt_objects ) ).
    ENDLOOP.
  ENDMETHOD.