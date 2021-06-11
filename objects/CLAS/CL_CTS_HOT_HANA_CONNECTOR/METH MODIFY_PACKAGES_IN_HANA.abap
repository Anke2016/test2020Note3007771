  METHOD modify_packages_in_hana.
    DATA: ls_cts_hot_package         TYPE cts_hot_package,
          ls_cts_hot_package_a       TYPE cts_hot_package,
          lo_cts_hot_package         TYPE REF TO cl_cts_hot_package,
          lo_package_exists_response TYPE REF TO cl_nhi_exists_package_res,
          lo_create_package_response TYPE REF TO cl_nhi_create_package_res,
          lo_update_package_response TYPE REF TO cl_nhi_update_package_res,
          lo_delete_package_response TYPE REF TO cl_nhi_delete_package_res,
          lv_package_created         TYPE abap_bool.

    CLEAR: e_created_packages, e_updated_packages, e_deleted_packages, e_skipped_packages, e_skipped_packages_n, e_failed_packages.

    LOOP AT i_itab_packages INTO lo_cts_hot_package.
      ls_cts_hot_package = me->m_cts_hot_db_access->read_cts_hot_package( i_abap_hana_package_id = lo_cts_hot_package->abap_hana_package_id i_abap_status = i_abap_status ).

      IF ls_cts_hot_package IS INITIAL.                     "Can occur in API case??!!??
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid = cx_hana_object_transport=>package_not_found_in_hot
*            msgv1  = CONV #( lo_cts_hot_package->abap_hana_package_id ).
        append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '2' i_error_code = '0' i_is_hana_message = abap_false
                                              i_message = |Package not found in HANA Object Transport Repository.|
                                    CHANGING  c_logs = e_skipped_packages ).
        CONTINUE.
      ENDIF.

      "also read ABAP_STATUS='A' version if ABAP_STATUS='I' version is to be handled.
      IF i_abap_status = 'I'.
        ls_cts_hot_package_a = m_cts_hot_db_access->read_cts_hot_package( i_abap_hana_package_id = lo_cts_hot_package->abap_hana_package_id ).
      ENDIF.

      "ignore checks on HOT STATUS in CWB/NA case (ABAP_STATUS = 'I')
      IF ls_cts_hot_package-abap_status = 'A'.
        IF ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_active.
          append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '2' i_error_code = '0' i_is_hana_message = abap_false
                                                i_message = '' "no message so that it is not shown in log
                                      CHANGING  c_logs = e_skipped_packages ).
          CONTINUE.
        ENDIF.

        IF ls_cts_hot_package-hot_status <> if_cts_hot_db_access=>co_hot_status_inactive
              AND ls_cts_hot_package-hot_status <> if_cts_hot_db_access=>co_hot_status_deploy_error
              AND ls_cts_hot_package-hot_status <> if_cts_hot_db_access=>co_hot_status_to_be_deleted
              AND ls_cts_hot_package-hot_status <> if_cts_hot_db_access=>co_hot_status_delete_error.
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid = cx_hana_object_transport=>wrong_hot_status_for_pack_depl
*            msgv1  = CONV #( ls_cts_hot_package-hana_package_id )
*            msgv2  = CONV #( ls_cts_hot_package-hot_status ).
          IF ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_new.
            append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '2' i_error_code = '0' i_is_hana_message = abap_false
                                                  i_message = '' "no message so that it is not shown in log
                                        CHANGING  c_logs = e_skipped_packages_n ).
          ELSE.
            append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '3' i_error_code = '0' i_is_hana_message = abap_false
                                                  i_message = |Package can not be deployed because of status { ls_cts_hot_package-hot_status } |
                                        CHANGING  c_logs = e_failed_packages ).
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

      "* DELETE PACKAE
      IF ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
         OR ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        lo_delete_package_response = me->m_nhi_package_api->delete( request = me->m_nhi_package_api->create_delete_package_req(
                                        tenant                  = ''
                                        package                 = ls_cts_hot_package-hana_package_id
                                        ignore_inactive_objects = abap_true
                                    )
        ).

        raise_exc_if_resp_is_not_bound( i_response = lo_delete_package_response i_what = cl_nhi_delete_package_req=>co_what i_action = cl_nhi_delete_package_req=>co_action ).

        IF lo_delete_package_response->error_code = '0'.
          append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '1' i_error_code = lo_delete_package_response->error_code
                                                i_message = lo_delete_package_response->error_msg
                                      CHANGING c_logs = e_deleted_packages ).
          update_pkg_in_hot_after_modify( i_cts_hot_package = ls_cts_hot_package
                                          i_cts_hot_package_a = ls_cts_hot_package_a ).
        ELSE.
          append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '3' i_error_code = lo_delete_package_response->error_code
                                                i_message = lo_delete_package_response->error_msg
                                      CHANGING  c_logs = e_failed_packages ).
          update_error_pkg_in_hot( ls_cts_hot_package ).
        ENDIF.

        CONTINUE.
      ENDIF.

      " CREATE/UPDATE Package with existence check before
      DATA(l_exists_package_request) = me->m_nhi_package_api->create_exists_package_req(
          tenant      = ''
          package     = ls_cts_hot_package-hana_package_id
*          ignore_case = ABAP_FALSE "if this is uncommented some other drawbacks exist!
        ).
      lo_package_exists_response = me->m_nhi_package_api->exists( l_exists_package_request ).

      raise_exc_if_resp_is_not_bound( i_response = lo_package_exists_response i_what = cl_nhi_exists_package_req=>co_what i_action = cl_nhi_exists_package_req=>co_action ).

      IF lo_package_exists_response->error_code IS NOT INITIAL.
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid          = cx_hana_object_transport=>read_package_error
*            msgv1           = CONV #( ls_cts_hot_package-hana_package_id )
*            hana_error_code = lo_package_exists_response->error_code
*            hana_error_msg  = lo_package_exists_response->error_msg.
        append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '3' i_error_code = lo_package_exists_response->error_code
                                              i_message = lo_package_exists_response->error_msg
                                    CHANGING  c_logs = e_failed_packages ).
        update_error_pkg_in_hot( ls_cts_hot_package ).
        CONTINUE.
      ENDIF.

      "if du does not exist in HANA, package can not reference it, so do not use it while creating / updating package
*      IF ls_cts_hot_package-hana_pack_du_vendor IS NOT INITIAL OR ls_cts_hot_package-hana_pack_delivery_unit IS NOT INITIAL.
*        IF abap_false = is_du_existing_in_hana( i_du_vendor = ls_cts_hot_package-hana_pack_du_vendor i_du_name = ls_cts_hot_package-hana_pack_delivery_unit ).
*          CLEAR ls_cts_hot_package-hana_pack_du_vendor.
*          CLEAR ls_cts_hot_package-hana_pack_delivery_unit.
*        ENDIF.
*      ENDIF.

      IF lo_package_exists_response->exists = abap_false.
        "create package in HANA if not existing
        lo_create_package_response = me->m_nhi_package_api->create( request = me->m_nhi_package_api->create_create_package_req(
                                        tenant                  = ''
                                        package                 = ls_cts_hot_package-hana_package_id
                                        description             = ls_cts_hot_package-hana_pack_description
                                        responsible             = ls_cts_hot_package-hana_pack_responsible
                                        orig_lang               = ls_cts_hot_package-hana_pack_orig_lang
                                        structural              = conv_is_structural_2_abap_bool( ls_cts_hot_package-hana_pack_is_structural )
                                        delivery_unit           = '' "HTA must not write DU name to HANA due to migration safeguarding for deleted objects/packages (with this undeploy of migrated HTC will delete old packages/objects)
                                        du_vendor               = '' "HTA must not write DU vendor to HANA due to migration safeguarding for deleted objects/packages (with this undeploy of migrated HTC will delete old packages/objects)
                                        text_collection         = ls_cts_hot_package-hana_pack_text_collection
                                        text_status             = ls_cts_hot_package-hana_pack_text_status
                                        text_terminology_domain = ls_cts_hot_package-hana_pack_text_term_domain
                                        hints_for_translation   = ls_cts_hot_package-hana_pack_hints_for_transl
                                        texts                   = VALUE #( )
                                    )
        ).

        raise_exc_if_resp_is_not_bound( i_response = lo_create_package_response i_what = cl_nhi_create_package_req=>co_what i_action = cl_nhi_create_package_req=>co_action ).

        IF lo_create_package_response->error_code = '0'.
          lv_package_created = abap_true.
          "adding to e_created_packaes done later, only if update was also successfull
        ELSE.
*          RAISE EXCEPTION TYPE cx_hana_object_transport
*            EXPORTING
*              textid          = cx_hana_object_transport=>create_package_error
*              msgv1           = CONV #( ls_cts_hot_package-hana_package_id )
*              hana_error_code = lo_create_package_response->error_code
*              hana_error_msg  = lo_create_package_response->error_msg.
          append_log_message_package( EXPORTING i_severity = '3' i_error_code = lo_create_package_response->error_code i_hot_package = lo_cts_hot_package
                                                i_message = lo_create_package_response->error_msg
                                      CHANGING  c_logs = e_failed_packages ).
          update_error_pkg_in_hot( ls_cts_hot_package ).
          CONTINUE.
        ENDIF.
      ELSE.
        lv_package_created = abap_false.
      ENDIF.

      "update package in hANA always because create does not allow setting src_system and src_tenant
      lo_update_package_response = me->m_nhi_package_api->update( request = me->m_nhi_package_api->create_update_package_req(
                                          tenant                  = ''
                                          package                 = ls_cts_hot_package-hana_package_id
                                          src_system              = ls_cts_hot_package-hana_pack_src_system
                                          src_tenant              = ls_cts_hot_package-hana_pack_src_tenant
                                          description             = ls_cts_hot_package-hana_pack_description
                                          responsible             = ls_cts_hot_package-hana_pack_responsible
                                          orig_lang               = ls_cts_hot_package-hana_pack_orig_lang
                                          structural              = conv_is_structural_2_abap_bool( ls_cts_hot_package-hana_pack_is_structural )
                                          delivery_unit           = '' "HTA must not write DU name to HANA due to migration safeguarding for deleted objects/packages (with this undeploy of migrated HTC will delete old packages/objects)
                                          du_vendor               = '' "HTA must not write DU vendor to HANA due to migration safeguarding for deleted objects/packages (with this undeploy of migrated HTC will delete old packages/objects)
                                          text_collection         = ls_cts_hot_package-hana_pack_text_collection
                                          text_status             = ls_cts_hot_package-hana_pack_text_status
                                          text_terminology_domain = ls_cts_hot_package-hana_pack_text_term_domain
                                          hints_for_translation   = ls_cts_hot_package-hana_pack_hints_for_transl
                                          texts                   = VALUE #( )
                                      )
          ).

      raise_exc_if_resp_is_not_bound( i_response = lo_update_package_response i_what = cl_nhi_update_package_req=>co_what i_action = cl_nhi_update_package_req=>co_action ).

      IF lo_update_package_response->error_code = '0'.
        "report success only if update was requested, if created was requested, no logging
        IF lv_package_created = abap_false.
          append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '1' i_error_code = lo_update_package_response->error_code
                                                i_message = lo_update_package_response->error_msg
                                      CHANGING c_logs = e_updated_packages ).
        ELSE.
          append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '1' i_error_code = lo_create_package_response->error_code
                                                i_message = lo_create_package_response->error_msg
                                      CHANGING  c_logs = e_created_packages ).
        ENDIF.
        update_pkg_in_hot_after_modify( i_cts_hot_package = ls_cts_hot_package
                                        i_cts_hot_package_a = ls_cts_hot_package_a ).
      ELSE.
*        RAISE EXCEPTION TYPE cx_hana_object_transport
*          EXPORTING
*            textid          = cx_hana_object_transport=>create_package_error
*            msgv1           = CONV #( ls_cts_hot_package-hana_package_id )
*            hana_error_code = l_update_package_response->error_code
*            hana_error_msg  = l_update_package_response->error_msg.
        append_log_message_package( EXPORTING i_hot_package = lo_cts_hot_package i_severity = '3' i_error_code = lo_update_package_response->error_code
                                              i_message = lo_update_package_response->error_msg
                                    CHANGING  c_logs = e_failed_packages ).
        update_error_pkg_in_hot( ls_cts_hot_package ).
        CONTINUE.
      ENDIF.
    ENDLOOP.

    IF e_created_packages IS NOT INITIAL OR e_updated_packages IS NOT INITIAL OR e_deleted_packages IS NOT INITIAL OR e_failed_packages IS NOT INITIAL.
      m_cts_hot_db_access->commit_work( ).
    ENDIF.
  ENDMETHOD.