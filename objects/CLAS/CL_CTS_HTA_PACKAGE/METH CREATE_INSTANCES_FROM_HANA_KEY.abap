  METHOD create_instances_from_hana_key.
    DATA: lr_hot_package           TYPE REF TO cl_cts_hot_package,
          lv_hana_package_name     TYPE if_cts_hta_types=>ty_hana_package_name,
          lt_package_names_in_hta  TYPE ty_hta_package_names,
          lv_hta_package_name      TYPE cts_hot_package_id,
          lt_package_names_in_hana TYPE if_cts_hot_hana_conn_internal=>ty_hana_package_names.

    lv_hana_package_name = i_hana_package_name.
    REPLACE ALL OCCURRENCES OF '*' IN lv_hana_package_name WITH '%'.

    "1. search packages in HTA
    lt_package_names_in_hta = g_db_access->find_hta_package_names( lv_hana_package_name ).
    LOOP AT lt_package_names_in_hta INTO lv_hta_package_name.
      lr_hot_package = cl_cts_hot_package=>create_instance_from_objname( iv_objname = lv_hta_package_name ).

      IF lr_hot_package->hana_package_id IS INITIAL. "should not occur as we read the data before in find_hta_package_names...
        RAISE EXCEPTION TYPE cx_cts_hta_not_found
          EXPORTING
            textid             = cx_cts_hta_not_found=>package_not_found_in_hta
            message_variable_1 = CONV #( lv_hta_package_name ).
      ENDIF.

      APPEND NEW cl_cts_hta_package( i_hot_package = lr_hot_package ) TO r_result.
    ENDLOOP.

    "2. search packages in HANA but only consider packages not yet part of the list
    IF i_search_in_hta_only = abap_false.
      TRY.
          create_hot_hana_connector( ).
          lt_package_names_in_hana = gr_hot_hana_connector->list_hana_packages( i_hana_package_name ).

          LOOP AT lt_package_names_in_hana INTO lv_hana_package_name.
            lr_hot_package = cl_cts_hot_package=>create_instance( iv_hana_package_id = lv_hana_package_name ).
            IF NOT line_exists( r_result[ table_line->transport_object_name = lr_hot_package->abap_hana_package_id ]  ).
              APPEND NEW cl_cts_hta_package( i_hot_package = lr_hot_package ) TO r_result.
            ENDIF.
          ENDLOOP.
        CATCH cx_cts_hta_no_hana_database.
          "continue and return only data found in HTA.
      ENDTRY.
    ENDIF.

  ENDMETHOD.