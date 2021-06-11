  METHOD get_objects_of_pkg_from_hana.

    DATA(lr_hot_package) = cl_cts_hot_package=>create_instance( i_hana_package ).
    DATA(lt_found_objects) = find_active_objects_in_hana( i_hana_package_id = i_hana_package i_object_status = '-1' ).
    LOOP AT lt_found_objects INTO DATA(lr_object).
      APPEND cl_cts_hot_object_v1=>create_instance2(
                io_cts_hot_package = lr_hot_package
                iv_hana_object_name = lr_object->name
                iv_hana_object_suffix = lr_object->suffix
             ) TO r_object_list.
    ENDLOOP.

  ENDMETHOD.