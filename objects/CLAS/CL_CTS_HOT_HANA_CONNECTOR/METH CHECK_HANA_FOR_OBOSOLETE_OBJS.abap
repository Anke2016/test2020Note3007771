  METHOD check_hana_for_obosolete_objs.
    DATA ls_cts_hot_object TYPE cts_hot_object.

    LOOP AT i_packages INTO DATA(lr_package).
      IF lr_package->hana_package_id IS INITIAL.
        "ignore check for packages we do not know... because HANA would return ALL objects then.
        CONTINUE.
      ENDIF.
      DATA(lt_objects) = get_objects_of_pkg_from_hana( lr_package->hana_package_id ).
      LOOP AT lt_objects INTO DATA(lr_object).
        IF NOT me->m_cts_hot_db_access->exists_object( i_abap_hana_package_id = lr_object->abap_hana_package_id
                                                       i_abap_hana_object_name_suffix = lr_object->abap_hana_object_name_suffix ).
          CLEAR ls_cts_hot_object.
          ls_cts_hot_object-abap_hana_package_id = lr_object->abap_hana_package_id.
          ls_cts_hot_object-abap_hana_object_name_suffix = lr_object->abap_hana_object_name_suffix.
          ls_cts_hot_object-abap_status = 'A'.
          ls_cts_hot_object-hana_package_id = lr_object->hana_package_id.
          ls_cts_hot_object-hana_object_name = lr_object->hana_object_name.
          ls_cts_hot_object-hana_object_suffix = lr_object->hana_object_suffix.
          ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted.
          me->m_cts_hot_db_access->modify_cts_hot_object( ls_cts_hot_object ).

          APPEND lr_object TO r_object_list.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.