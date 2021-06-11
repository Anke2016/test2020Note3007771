  METHOD deploy_objects_to_hana.
    DATA: ls_cts_hot_object              TYPE cts_hot_object,
          lt_nhi_objs_for_act            TYPE cl_nhi_object_id=>ty_objlist,
          lo_inactive_session            TYPE REF TO cl_nhi_inactive_session,
          lo_active_version              TYPE REF TO cl_nhi_active_version,
          lo_metadata_for_write_request  TYPE REF TO cl_nhi_metadata,
          "! All objects to be activated as mapping of hana names to nhi class to hot class.
          lt_map_hana_2_hot_2_nhi_obj_in TYPE ty_map_hana_2_hot_2_nhi_objs,
          ls_map_hana_2_hot_2_nhi_obj    TYPE ty_map_hana_2_hot_2_nhi_obj,
          ls_log_message                 TYPE ty_log_message,
          "! flag to remember whether import to inactive or deletion from inactive was successful and object should be added to activation list
          lv_import_ok                   TYPE abap_bool,
          "! flag to remember whether object exists as active object or not
          lv_active_object_exists        TYPE abap_bool,
          "! flag to remember whether object exists as inactive object or not
          lv_inactive_object_exists      TYPE abap_bool,
          lt_object_texts                TYPE cl_nhi_text=>ty_texts,
          lt_object_content_texts        TYPE cl_nhi_text=>ty_texts,
          "! Objects for which import/revert failed and must be set to hot status 'E'/'Z'
          lt_failed_import_objects       TYPE if_cts_hot_hana_conn_internal=>ty_cts_hot_objects,
          "! Stores hot_status and hana_source_object_version per object for ABAP_STATUS = 'A'.
          lt_object_status_versions      TYPE ty_hot_obj_status_versions.

    CLEAR: e_deploy_result, e_failed_objects, e_skipped_objects, e_skipped_objects_n, e_successfull_objects.

    lo_inactive_session = cl_nhi_inactive_session=>create_inactive_session( owner = me->m_nhi_api_user workspace = '' ).
    lo_active_version = cl_nhi_active_version=>create_active_version( ).

    LOOP AT i_objects INTO DATA(lo_cts_hot_object).
      ls_cts_hot_object = me->m_cts_hot_db_access->read_cts_hot_object(
                i_abap_hana_package_id = lo_cts_hot_object->abap_hana_package_id
                i_abap_hana_object_name_suffix = lo_cts_hot_object->abap_hana_object_name_suffix
                i_abap_status = i_abap_status
            ).
      "skip not existing objects
      IF ls_cts_hot_object IS INITIAL.
        INSERT lo_cts_hot_object INTO TABLE e_skipped_objects.
        CONTINUE.
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid = cx_hana_object_transport=>object_not_found_in_hot
*            msgv1  = lo_cts_hot_object->abap_hana_package_id && '.' && lo_cts_hot_object->abap_hana_object_name_suffix.
      ENDIF.

      "ignore checks on HOT STATUS in CWB/NA case (ABAP_STATUS = 'I')
      IF ls_cts_hot_object-abap_status = 'A'.
        "skip already active objects
        IF ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_active.
          INSERT lo_cts_hot_object INTO TABLE e_skipped_objects.
          CONTINUE.
        ENDIF.

        " Deployment can only handle hot status of following if statement. If not such a status, we log as error.
        IF ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_inactive
              AND ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_deploy_error
              AND ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_to_be_deleted
              AND ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_delete_error.

          IF ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_new.
            INSERT lo_cts_hot_object INTO TABLE e_skipped_objects_n.
          ELSE.
            append_log_message( EXPORTING i_hot_object = lo_cts_hot_object i_severity = '3' i_error_code = '0' i_is_hana_message = abap_false
                                          i_message = |Object can not be deployed because of its status { ls_cts_hot_object-hot_status } |
                                CHANGING  c_logs = e_deploy_result-import_result-log_messages ).
            INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
            INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
          ENDIF.
          CONTINUE.
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid = cx_hana_object_transport=>wrong_hot_status_for_obj_depl
*            msgv1  = ls_cts_hot_object-hana_package_id && '.' && ls_cts_hot_object-hana_object_name && '.' && ls_cts_hot_object-hana_object_suffix
*            msgv2  = CONV #( ls_cts_hot_object-hot_status ).
        ENDIF.
      ELSEIF i_abap_status = 'I'.
        "read active version and preserve hot_status and source_object_version
        DATA(ls_cts_hot_object_a) = me->m_cts_hot_db_access->read_cts_hot_object_wo_bcdata(
                  i_abap_hana_package_id = lo_cts_hot_object->abap_hana_package_id
                  i_abap_hana_object_name_suffix = lo_cts_hot_object->abap_hana_object_name_suffix
              ).
        IF ls_cts_hot_object_a IS NOT INITIAL.
          INSERT VALUE ty_hot_obj_status_version( object = lo_cts_hot_object
                                                  abap_status = ls_cts_hot_object_a-abap_status
                                                  hot_status = ls_cts_hot_object_a-hot_status
                                                  hana_source_object_version = ls_cts_hot_object_a-hana_source_object_version )
                 INTO TABLE lt_object_status_versions.
          CLEAR ls_cts_hot_object_a. "free memory
        ENDIF.
      ENDIF.

      "preserve hot_status and source_object_version (for abap_status 'I' or 'A')
      INSERT VALUE ty_hot_obj_status_version( object = lo_cts_hot_object
                                              abap_status = ls_cts_hot_object-abap_status
                                              hot_status = ls_cts_hot_object-hot_status
                                              hana_source_object_version = ls_cts_hot_object-hana_source_object_version )
             INTO TABLE lt_object_status_versions.

      "read metadata needed for write/delete request.
      lo_metadata_for_write_request = read_object_metadata_from_hana( lo_cts_hot_object ).
      IF lo_metadata_for_write_request IS NOT BOUND.
        lo_metadata_for_write_request = cl_nhi_metadata_active_ver=>create_metadata(
                  version_id = '0'
                  activated_at = ''
                  activated_by = ''
                  edit = abap_true ).
        lv_active_object_exists = abap_false.
      ELSE.
        lv_active_object_exists = abap_true.
      ENDIF.

      "prepare text in master language
*      DATA(lt_obj_texts) = me->m_cts_hot_db_access->read_cts_hot_object_texts_mstr( i_cts_hot_object = lo_cts_hot_object ).
*      LOOP AT lt_obj_texts INTO DATA(ls_object_text).
*        IF ls_object_text-text_type = if_cts_hot_db_access=>co_cts_hot_otexts_type_text.
*          APPEND cl_nhi_text=>create_text(
*               text_id    = CONV #( ls_object_text-hana_text_id )
*               text_type  = CONV #( ls_object_text-hana_text_type )
*               max_length = CONV #( ls_object_text-hana_text_max_length )
*               content    = CONV #( ls_object_text-hana_text_content ) )
*            TO lt_object_texts.
*        ELSEIF ls_object_text-text_type = if_cts_hot_db_access=>co_cts_hot_otexts_type_content.
*          APPEND cl_nhi_text=>create_text(
*               text_id    = CONV #( ls_object_text-hana_text_id )
*               text_type  = CONV #( ls_object_text-hana_text_type )
*               max_length = CONV #( ls_object_text-hana_text_max_length )
*               content    = CONV #( ls_object_text-hana_text_content ) )
*            TO lt_object_content_texts.
*        ELSE.
*          "##TODO ERROR unknown text type
*        ENDIF.
*      ENDLOOP.

      DATA(lo_nhi_object_id) = cl_nhi_object_id=>create_object_id( tenant = ''
                                                            package = ls_cts_hot_object-hana_package_id
                                                            name = ls_cts_hot_object-hana_object_name
                                                            suffix = ls_cts_hot_object-hana_object_suffix
                                                            version = lo_active_version
                                                            metadata = lo_metadata_for_write_request ).

      "write inactive content or delete object in HANA
      lv_import_ok = abap_false. "flag whether import or inactive deletion was successful or not to know whether activation should be triggered or not
      IF ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
          OR ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
          OR  ( ls_cts_hot_object-abap_status = 'I'
                  AND ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_to_be_deleted
                  AND ls_cts_hot_object-hot_status <> if_cts_hot_db_access=>co_hot_status_delete_error ).
        DATA(lo_write_obj_req) = me->m_nhi_object_api->create_write_object_req(
            determinereferences = abap_true
            "xreflist currently not working because of 2 problems.
            " 1. CL_NHI_ABSTRACT_API->create_object_json_object does not work if version and metadata is not specified
            " 2. all_refs contain objects with invalid types. Maybe because ABAP does not want to Support these types? In my test scenario these types were CATALOG tables
            xreflist            = VALUE #( )
            "xreflist            = all_refs
            texts               = lt_object_texts "enable again if texts are handled in abap language tables
            content_texts       = lt_object_content_texts "enable again if texts are handled in abap language tables
            object              = lo_nhi_object_id
            session             = lo_inactive_session
            metadata            = lo_metadata_for_write_request
            cdata               = ls_cts_hot_object-hana_content_cdata
            bdata               = ls_cts_hot_object-hana_content_bdata
        ).

        "Write object to HANA inactive.
        "First we try to write object to inactive. If it works, we exit the loop.
        "If it fails we revert the object and try again (2nd loop).
        DO 2 TIMES.
          DATA(lo_object_write_resp) = me->m_nhi_object_api->write( lo_write_obj_req ).

          raise_exc_if_resp_is_not_bound( i_response = lo_object_write_resp i_what = cl_nhi_write_object_req=>co_what i_action = cl_nhi_write_object_req=>co_action ).

          IF lo_object_write_resp->error_code = '0'.
            append_log_message( EXPORTING i_severity = '1' i_error_code = lo_object_write_resp->error_code i_message = lo_object_write_resp->error_msg i_hot_object = lo_cts_hot_object
                                CHANGING c_logs = e_deploy_result-import_result-log_messages ).
            INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-successfully_imported_objects.
            lv_import_ok = abap_true.
            EXIT. "exit do loop as write was successfull
          ELSEIF lo_object_write_resp->error_code = '40124'. "object already exists inactive
            DATA(lo_revert_obj_req) = me->m_nhi_object_api->create_revert_objects_req( objlist = VALUE cl_nhi_object_id=>ty_objlist( ( lo_nhi_object_id ) ) session = lo_inactive_session ).
            DATA(lo_revert_obj_response) = me->m_nhi_object_api->revert( request = lo_revert_obj_req ).

            raise_exc_if_resp_is_not_bound( i_response = lo_revert_obj_response i_what = cl_nhi_revert_objects_req=>co_what i_action = cl_nhi_revert_objects_req=>co_action ).

            IF lo_revert_obj_response->error_code <> '0'.
              append_log_message( EXPORTING i_severity = '3' i_error_code = lo_revert_obj_response->error_code i_message = lo_revert_obj_response->error_msg i_hot_object = lo_cts_hot_object
                                  CHANGING c_logs = e_deploy_result-import_result-log_messages ).

              IF NOT line_exists( e_failed_objects[ table_line = lo_cts_hot_object ] ).
                INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
                INSERT lo_cts_hot_object INTO TABLE lt_failed_import_objects.
                INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
              ENDIF.
            ENDIF.
          ELSEIF lo_object_write_resp->error_code <> '0'.
            append_log_message( EXPORTING i_severity = '3' i_error_code = lo_object_write_resp->error_code i_message = lo_object_write_resp->error_msg i_hot_object = lo_cts_hot_object
                                CHANGING c_logs = e_deploy_result-import_result-log_messages ).

            IF NOT line_exists( e_failed_objects[ table_line = lo_cts_hot_object ] ).
              INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
              INSERT lo_cts_hot_object INTO TABLE lt_failed_import_objects.
              INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
            ENDIF.
            EXIT.
          ENDIF.
        ENDDO.
      ELSEIF ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
            OR ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        CLEAR lv_inactive_object_exists.
        IF lv_active_object_exists = abap_false.
          "Object does not exist as active anymore in HANA (maybe deleted already earlier but not updated in HOT or was never active but exists as inactive).
          "check inactive existence and in case delete(revert it. Otherwise mark as skipped but delete in HOT
          DATA(lr_resp_object_exists_inactive) =  me->m_nhi_object_api->exists( me->m_nhi_object_api->create_object_exists_req(
                                  object  = lo_nhi_object_id
                                  session = lo_inactive_session
                              ) ).

          raise_exc_if_resp_is_not_bound( i_response = lr_resp_object_exists_inactive i_what = cl_nhi_object_exists_req=>co_what i_action = cl_nhi_object_exists_req=>co_action ).

          IF lr_resp_object_exists_inactive->exists = abap_true.
            lv_inactive_object_exists = abap_true.
          ELSE.
            INSERT lo_cts_hot_object INTO TABLE e_skipped_objects.
            update_objs_in_hot_after_depl( i_successfull_deploy_objects = VALUE ty_cts_hot_objects( ( lo_cts_hot_object ) )
                                           i_abap_status = i_abap_status
                                           i_object_status_versions = lt_object_status_versions ).
          ENDIF.
        ENDIF.

        "if object exists as active or inactive, trigger deletion.
        IF lv_active_object_exists = abap_true OR lv_inactive_object_exists = abap_true.
          DATA(lv_delete_obj_req) = me->m_nhi_object_api->create_delete_object_req(
                                action   = cl_nhi_delete_object_req=>co_action
                                what     = cl_nhi_delete_object_req=>co_what
                                session  = lo_inactive_session
                                object   = lo_nhi_object_id
                            ).

          DATA(lv_resp_object_delete) = me->m_nhi_object_api->delete( request = lv_delete_obj_req ).

          raise_exc_if_resp_is_not_bound( i_response = lv_resp_object_delete i_what = cl_nhi_delete_object_req=>co_what i_action = cl_nhi_delete_object_req=>co_action ).

          IF lv_resp_object_delete->error_code = '0'.
            IF lv_active_object_exists = abap_true.
              INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-successfully_deleted_objects.
              append_log_message( EXPORTING i_severity = '1' i_error_code = lv_resp_object_delete->error_code i_message = lv_resp_object_delete->error_msg i_hot_object = lo_cts_hot_object
                                  CHANGING c_logs = e_deploy_result-import_result-log_messages ).
              lv_import_ok = abap_true.
            ELSEIF lv_inactive_object_exists = abap_true.
              "object existed only as inactive object, so only a revert was needed. (do not set lv_import_ok because activation must not be triggered!)
              IF lv_resp_object_delete->isreverted = '1'.
                INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-successfully_reverted_objects.
                INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
                INSERT lo_cts_hot_object INTO TABLE e_successfull_objects.
              ELSE.
                "Usually should not occur because we checked inactive/active existence before deletion...
                append_log_message( EXPORTING i_severity = '3' i_error_code = lv_resp_object_delete->error_code
                                              i_hot_object = lo_cts_hot_object
                                              i_message = |Unexpected result during object deletion ({ lv_resp_object_delete->isreverted }).|
                                              i_is_hana_message = abap_false
                              CHANGING  c_logs = e_deploy_result-import_result-log_messages ).
                IF NOT line_exists( e_failed_objects[ table_line = lo_cts_hot_object ] ).
                  INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
                  INSERT lo_cts_hot_object INTO TABLE lt_failed_import_objects.
                  INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            append_log_message( EXPORTING i_severity = '3' i_error_code = lv_resp_object_delete->error_code i_message = lv_resp_object_delete->error_msg i_hot_object = lo_cts_hot_object
                                CHANGING c_logs = e_deploy_result-import_result-log_messages ).

            IF NOT line_exists( e_failed_objects[ table_line = lo_cts_hot_object ] ).
              INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
              INSERT lo_cts_hot_object INTO TABLE lt_failed_import_objects.
              INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        append_log_message( EXPORTING i_hot_object = lo_cts_hot_object i_severity = '3' i_error_code = '0' i_is_hana_message = abap_false
                                      i_message = |Object can not be deployed because of its status { ls_cts_hot_object-hot_status } |
                            CHANGING  c_logs = e_deploy_result-import_result-log_messages ).

        INSERT lo_cts_hot_object INTO TABLE e_failed_objects.
        INSERT lo_cts_hot_object INTO TABLE e_deploy_result-import_result-objects_with_last_action.

*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid = cx_hana_object_transport=>wrong_hot_status_for_obj_depl
*            msgv1  = ls_cts_hot_object-hana_package_id && '.' && ls_cts_hot_object-hana_object_name && '.' && ls_cts_hot_object-hana_object_suffix
*            msgv2  = CONV #( ls_cts_hot_object-hot_status ).
      ENDIF.

      IF lv_import_ok = abap_true.
        ls_map_hana_2_hot_2_nhi_obj-package_id = lo_nhi_object_id->package.
        ls_map_hana_2_hot_2_nhi_obj-object_name = lo_nhi_object_id->name.
        ls_map_hana_2_hot_2_nhi_obj-object_suffix = lo_nhi_object_id->suffix.
        ls_map_hana_2_hot_2_nhi_obj-hot_object = lo_cts_hot_object.
        ls_map_hana_2_hot_2_nhi_obj-nhi_object = lo_nhi_object_id.
        INSERT ls_map_hana_2_hot_2_nhi_obj INTO TABLE lt_map_hana_2_hot_2_nhi_obj_in.
      ENDIF.
    ENDLOOP.

    "update reverted objects in HTA
    me->update_objs_in_hot_after_depl( i_successfull_deploy_objects = e_successfull_objects
                                       i_abap_status = i_abap_status
                                       i_object_status_versions = lt_object_status_versions ).

    " update objects with import or revert (delete inactive) errors
    me->update_error_objs_in_hot( i_error_objects = lt_failed_import_objects
                                  i_abap_status = i_abap_status
                                  i_object_status_versions = lt_object_status_versions ).

    me->activate_objects( EXPORTING i_objects_to_be_activated    = lt_map_hana_2_hot_2_nhi_obj_in
                                    i_abap_status                = i_abap_status
                                    i_object_status_versions     = lt_object_status_versions
                                    i_activation_mode            = i_activation_mode
                                    i_max_nr_activation_attempts = i_max_nr_activation_attempts
                                    i_activate_with_hints        = i_activate_with_hints
                          CHANGING  c_deploy_result      = e_deploy_result
                                    c_failed_objects     = e_failed_objects
                                    c_successful_objects = e_successfull_objects ).
  ENDMETHOD.