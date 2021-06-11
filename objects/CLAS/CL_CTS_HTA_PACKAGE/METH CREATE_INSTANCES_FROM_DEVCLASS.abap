  METHOD create_instances_from_devclass.
    DATA: lt_obj_names TYPE ty_sobj_names,
          lt_packages  TYPE ty_hta_package_names,
          lv_package   TYPE cts_hot_package_id.

    lt_obj_names = g_tadir_access->read_hota_objnames_for_devclas( i_devclasses ).
    lt_packages = g_db_access->read_packages( i_sobj_names = lt_obj_names i_deployable_only = i_deployable_only ).
    LOOP AT lt_packages INTO lv_package.
      APPEND create_instance_from_obj_name( i_transport_object_name = CONV #( lv_package ) ) TO r_result.
    ENDLOOP.
  ENDMETHOD.