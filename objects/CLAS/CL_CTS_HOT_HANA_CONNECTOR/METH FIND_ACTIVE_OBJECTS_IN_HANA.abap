  METHOD find_active_objects_in_hana.

    TRY.
        DATA(lr_find_request) = me->m_nhi_object_api->create_find_objects_req(
                                tenant            = ''
                                package           = i_hana_package_id
                                object_status     = i_object_status
                                session           = cl_nhi_active_session=>create_active_session( ) ).
        DATA(lr_find_response) = me->m_nhi_object_api->find( lr_find_request ).

        raise_exc_if_resp_is_not_bound( i_response = lr_find_response i_what = cl_nhi_find_objects_req=>co_what i_action = cl_nhi_find_objects_req=>co_action ).

        IF lr_find_response->error_code IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid          = cx_hana_object_transport=>find_objects_of_package_error
              msgv1           = CONV #( i_hana_package_id )
              msgv2           = CONV #( i_object_status )
              hana_error_code = lr_find_response->error_code
              hana_error_msg  = lr_find_response->error_msg.
        ENDIF.

        r_found_objects = lr_find_response->objects.
      CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
        DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( nhi_exc->get_text( ) ).
        RAISE EXCEPTION TYPE cx_hana_object_transport
          EXPORTING
            textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
            msgv1    = ls_split_text-chunk1
            msgv2    = ls_split_text-chunk2
            msgv3    = ls_split_text-chunk3
            msgv4    = ls_split_text-chunk4
            previous = nhi_exc.
    ENDTRY.

  ENDMETHOD.