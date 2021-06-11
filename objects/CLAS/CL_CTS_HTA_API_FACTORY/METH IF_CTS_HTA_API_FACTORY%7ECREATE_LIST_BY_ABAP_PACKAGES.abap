  METHOD if_cts_hta_api_factory~create_list_by_abap_packages.
    DATA: lt_hta_packages    TYPE if_cts_hta_types=>ty_cts_hta_packages,
          lr_hta_package     TYPE REF TO if_cts_hta_package,
          lt_hta_objects     TYPE if_cts_hta_types=>ty_cts_hta_objects,
          lr_hta_object      TYPE REF TO if_cts_hta_object,
          lv_is_full_package TYPE abap_bool.

    "create component_list
    r_result = cl_cts_hta_component_list=>create_instance( ).

    "create all packages by devclass taking i_deployable_only into account
    lt_hta_packages = cl_cts_hta_package=>create_instances_from_devclass( i_devclasses = i_devclasses
                                                                          i_deployable_only = i_deployable_only ).

    "for all packages get their objects and if all objects, create full packages and add full_packages to result or add package and objects as LIMU to result
    LOOP AT lt_hta_packages INTO lr_hta_package.
      lt_hta_objects = cl_cts_hta_object=>create_instances_from_package( EXPORTING i_cts_hta_package = lr_hta_package
                                                                                   i_deployable_only = i_deployable_only
                                                                         IMPORTING e_all_objects_of_package = lv_is_full_package ).

      IF lv_is_full_package = abap_true.
        r_result->add_component( cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = lr_hta_package
                                                                                        i_cts_hta_objects = lt_hta_objects ) ).
      ELSE.
        r_result->add_component( lr_hta_package ).
        LOOP AT lt_hta_objects INTO lr_hta_object.
          r_result->add_component( lr_hta_object ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.