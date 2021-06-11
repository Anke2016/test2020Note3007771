  METHOD create_instances_from_package.
    DATA: lt_objects      TYPE ty_hta_object_names,
          ls_object       TYPE ty_object_key,
          lv_trobj_bname  TYPE trobj_name,
          hta_api_factory TYPE REF TO if_cts_hta_api_factory.

    e_all_objects_of_package = abap_true.

    " search objects in HTA
    lt_objects = g_db_access->read_objects_for_package( i_cts_hta_package->transport_object_name(40) ).

    LOOP AT lt_objects INTO ls_object.
      IF i_deployable_only = abap_false OR
         ( i_deployable_only = abap_true AND ( ls_object-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
                                               OR ls_object-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                                               OR ls_object-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
                                               OR ls_object-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error ) ).
        lv_trobj_bname = ls_object-abap_hana_package_id.
        lv_trobj_bname+40 = ls_object-abap_hana_object_name_suffix.

        APPEND create_instance_from_obj_name( i_transport_object_name = lv_trobj_bname
                                              i_hta_package = i_cts_hta_package
                                            ) TO r_result.
      ELSE.
        e_all_objects_of_package = abap_false.
      ENDIF.
    ENDLOOP.

    " search objects in HANA only if use case is not deployment and if HANA should be looked up
    IF i_deployable_only = abap_false AND i_search_in_hta_only = abap_false.
      TRY.
          create_hot_hana_connector( ).

          DATA(lt_hana_objects) = gr_hot_hana_connector->get_objects_of_pkg_from_hana( i_cts_hta_package->hana_package_name ).

          LOOP AT lt_hana_objects INTO DATA(lr_hana_object).
            IF NOT line_exists( r_result[ table_line->transport_object_name = lr_hana_object->transport_object_name ]  ).
              APPEND NEW cl_cts_hta_object( i_hta_package = i_cts_hta_package i_hot_object = lr_hana_object ) TO r_result.
            ENDIF.
          ENDLOOP.
        CATCH cx_cts_hta_no_hana_database.
          "continue and return only data found in HTA.
      ENDTRY.
    ENDIF.
  ENDMETHOD.