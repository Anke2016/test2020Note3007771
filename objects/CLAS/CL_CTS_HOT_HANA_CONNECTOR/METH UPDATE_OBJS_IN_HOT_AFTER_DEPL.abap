  METHOD update_objs_in_hot_after_depl.
    DATA: lv_deployed_at              TYPE timestampl,
          lv_hot_status               TYPE cts_hot_object_status,
          lv_hot_status_a             TYPE cts_hot_object_status,
          ls_hot_obj_status_version   TYPE ty_hot_obj_status_version,
          ls_hot_obj_status_version_a TYPE ty_hot_obj_status_version,
          lv_exec_commit_work         TYPE abap_bool,
          lv_rc_hot_status            TYPE syst_subrc.

    lv_deployed_at = me->m_timestamp_provider->get_timestamp( ).

    "##TODO: Performance?
    LOOP AT i_successfull_deploy_objects INTO DATA(lo_successfull_object).
      ls_hot_obj_status_version = VALUE #( i_object_status_versions[ object = lo_successfull_object abap_status = i_abap_status ] OPTIONAL ).

      IF ls_hot_obj_status_version IS INITIAL.
        CONTINUE. "should not happen because all objects of deployment should be in the table, but to be on safe side
      ENDIF.

*      DATA(ls_cts_hot_object) = m_cts_hot_db_access->read_cts_hot_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
*                                                                          i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
*                                                                          i_abap_status = i_abap_status ).
*      IF ls_cts_hot_object IS INITIAL.
*        CONTINUE.
*      ENDIF.

      lv_hot_status = m_cts_hot_db_access->read_hot_status_for_object( EXPORTING
                                                                         i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                                         i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                                         i_abap_status = i_abap_status
                                                                       IMPORTING
                                                                         e_return_code = lv_rc_hot_status ).

      IF lv_rc_hot_status <> 0.
        CONTINUE. "object does not exist in DB anymore, nothing to update
      ENDIF.

      IF i_abap_status = 'I'. "SNote/CWB case
        "independent whether we update data in HTA later or not, we need to update SMODI in SNote/CWB case
        IF lv_hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
          OR lv_hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
          me->delete_smodi_entries( i_obj_type = 'HOTO' i_obj_name = lo_successfull_object->transport_object_name ).
        ELSE.
          me->update_smodi_entries( i_obj_type = 'HOTO' i_obj_name = lo_successfull_object->transport_object_name ).
        ENDIF.
        lv_exec_commit_work = abap_true.

        "for SNote/CWB case we need to compare the object with ABAP_STATUS = 'A' from before the deployment with current entry in DB for ABAP_STATUS = 'A'
        "TP might have triggered parallel import and main import was already done so that 'A' version is different than before SNote/CWB.
        ls_hot_obj_status_version_a = VALUE #( i_object_status_versions[ object = lo_successfull_object abap_status = 'A' ] OPTIONAL ).
        lv_hot_status_a = m_cts_hot_db_access->read_hot_status_for_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                                           i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix ).
        IF lv_hot_status_a <> ls_hot_obj_status_version_a-hot_status. "if parallel import happened, only delete 'I' entry but do not change 'A' entry that was deployed in parallel
          m_cts_hot_db_access->delete_cts_hot_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                      i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                      i_abap_status                  = 'I' ).
          CONTINUE. "do not update 'A' DB entry if the status is not as before the deployment
        ENDIF.
      ENDIF.

      IF lv_hot_status <> ls_hot_obj_status_version-hot_status.
        CONTINUE. "do not update any DB entry if the status is not as before the deployment
      ENDIF.

      lv_exec_commit_work = abap_true. "now at least 1 entry to be updated because one of the following statements will change something in DB.

      DATA(lo_active_metadata) = read_object_metadata_from_hana( lo_successfull_object ).

      IF lo_active_metadata IS NOT BOUND "object does not exist in HANA anymore
          AND ( lv_hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                OR lv_hot_status = if_cts_hot_db_access=>co_hot_status_delete_error ).
        m_cts_hot_db_access->delete_cts_hot_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                    i_abap_status = i_abap_status ).
        "if CWB/SNote case, also delete active version but only if not changed in parallel
        IF i_abap_status = 'I' AND ls_hot_obj_status_version_a IS NOT INITIAL AND ls_hot_obj_status_version_a-hot_status = lv_hot_status_a.
          m_cts_hot_db_access->delete_cts_hot_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                      i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                      i_abap_status = 'A' ).
        ENDIF.

        "finally delete SMODI entries in all delete cases.
        me->delete_smodi_entries( i_obj_type = 'HOTO' i_obj_name = lo_successfull_object->transport_object_name ).
      ELSE.
        IF i_abap_status = 'A'.
          m_cts_hot_db_access->update_object_after_succes_dep(
            i_old_object = VALUE cts_hot_object( abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                 abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                 abap_status = ls_hot_obj_status_version-abap_status
                                                 hot_status = ls_hot_obj_status_version-hot_status
                                                 hana_source_object_version = ls_hot_obj_status_version-hana_source_object_version )
            i_new_deployed_at = lv_deployed_at
            i_new_deployed_by = sy-uname
            i_new_hana_activated_at = conv_hana_actvted_at_to_timest( lo_active_metadata->activated_at )
            i_new_hana_activated_by = lo_active_metadata->activated_by
            i_new_hana_object_version = CONV #( lo_active_metadata->version_id )
          ).
        ELSEIF i_abap_status = 'I'.
          DATA(ls_cts_hot_object_i) = m_cts_hot_db_access->read_cts_hot_object( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                                                i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                                                i_abap_status = 'I' ).

          DATA(ls_cts_hot_object_a) = m_cts_hot_db_access->read_cts_hot_object_wo_bcdata( i_abap_hana_package_id = lo_successfull_object->abap_hana_package_id
                                                                                          i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                                                          i_abap_status = 'A' ).

          m_cts_hot_db_access->delete_cts_hot_object( i_abap_hana_package_id         = lo_successfull_object->abap_hana_package_id
                                                      i_abap_hana_object_name_suffix = lo_successfull_object->abap_hana_object_name_suffix
                                                      i_abap_status                  = 'I' ).

          "move object with abap_status = 'I' to abap_status = 'A' only if current abap_status = 'A' version was not changed in parallel, e.g. by TP
          IF ls_cts_hot_object_a-hot_status = ls_hot_obj_status_version_a-hot_status
            AND ls_cts_hot_object_a-hana_source_object_version = ls_hot_obj_status_version_a-hana_source_object_version.
            ls_cts_hot_object_i-abap_status = 'A'.
            ls_cts_hot_object_i-hot_status = if_cts_hot_db_access=>co_hot_status_active.
            ls_cts_hot_object_i-abap_deployed_at = lv_deployed_at.
            ls_cts_hot_object_i-abap_deployed_by = sy-uname.
            ls_cts_hot_object_i-hana_activated_at = conv_hana_actvted_at_to_timest( lo_active_metadata->activated_at ).
            ls_cts_hot_object_i-hana_activated_by = lo_active_metadata->activated_by.
            ls_cts_hot_object_i-hana_object_version = CONV #( lo_active_metadata->version_id ).
            m_cts_hot_db_access->modify_cts_hot_object( ls_cts_hot_object_i ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_exec_commit_work = abap_true.
      m_cts_hot_db_access->commit_work( ).
    ENDIF.
  ENDMETHOD.