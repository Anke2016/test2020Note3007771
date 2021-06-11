  METHOD read_object_data_from_hana.
    TRY.
        DATA(lr_active_metadata) = me->read_object_metadata_from_hana( i_cts_hot_object = i_cts_hot_object ).
      CATCH cx_nhi_hana_repository INTO DATA(lr_exc).
        DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( lr_exc->get_text( ) ).
        RAISE EXCEPTION TYPE cx_hana_object_transport
          EXPORTING
            textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
            msgv1    = ls_split_text-chunk1
            msgv2    = ls_split_text-chunk2
            msgv3    = ls_split_text-chunk3
            msgv4    = ls_split_text-chunk4
            previous = lr_exc.
    ENDTRY.

    IF lr_active_metadata IS BOUND.
      r_object_data-hana_package_id = i_cts_hot_object->hana_package_id.
      r_object_data-hana_object_name = i_cts_hot_object->hana_object_name.
      r_object_data-hana_object_suffix = i_cts_hot_object->hana_object_suffix.
      r_object_data-hana_object_version = lr_active_metadata->version_id.
      r_object_data-hana_read_system = g_hana_sid.
      r_object_data-hana_source_build_version = g_hana_build_version.
      r_object_data-hana_activated_by = lr_active_metadata->activated_by.
      r_object_data-hana_activated_at = conv_hana_actvted_at_to_timest( lr_active_metadata->activated_at ).
    ELSE.
      CLEAR r_object_data.
    ENDIF.
  ENDMETHOD.