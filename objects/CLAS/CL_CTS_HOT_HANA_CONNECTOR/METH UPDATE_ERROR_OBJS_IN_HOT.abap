  METHOD update_error_objs_in_hot.
    DATA: lv_exec_commit_work       TYPE abap_bool,
          lv_new_status             TYPE cts_hot_object_status,
          ls_hot_obj_status_version TYPE ty_hot_obj_status_version.

    LOOP AT i_error_objects INTO DATA(lr_error_object).
      ls_hot_obj_status_version = VALUE #( i_object_status_versions[ object = lr_error_object abap_status = i_abap_status ] OPTIONAL ).

      IF ls_hot_obj_status_version IS INITIAL.
        CONTINUE. "should not happen because all objects of deployment should be in the table, but to be on safe side
      ENDIF.

      CASE ls_hot_obj_status_version-hot_status.
        WHEN if_cts_hot_db_access=>co_hot_status_to_be_deleted.
          lv_new_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        WHEN if_cts_hot_db_access=>co_hot_status_inactive.
          lv_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error.
        WHEN if_cts_hot_db_access=>co_hot_status_deploy_error OR if_cts_hot_db_access=>co_hot_status_delete_error.
          CONTINUE. "status in DB is already error.
        WHEN OTHERS.
          lv_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error.
      ENDCASE.

      m_cts_hot_db_access->update_object_after_failed_dep(
          i_old_object = VALUE cts_hot_object( abap_hana_package_id = lr_error_object->abap_hana_package_id
                                               abap_hana_object_name_suffix = lr_error_object->abap_hana_object_name_suffix
                                               abap_status = ls_hot_obj_status_version-abap_status
                                               hot_status = ls_hot_obj_status_version-hot_status
                                               hana_source_object_version = ls_hot_obj_status_version-hana_source_object_version )
          i_new_status = lv_new_status
      ).

      lv_exec_commit_work = abap_true. "now at least 1 entry to be updated.
    ENDLOOP.

    IF lv_exec_commit_work = abap_true.
      m_cts_hot_db_access->commit_work( ).
    ENDIF.
  ENDMETHOD.