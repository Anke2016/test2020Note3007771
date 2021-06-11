  METHOD read_object_metadata_from_hana.
    "create necessary input for NHI API.
    DATA(lo_nhi_object_id) = cl_nhi_object_id=>create_object_id( tenant = ''
                                          package = i_cts_hot_object->hana_package_id
                                          name = i_cts_hot_object->hana_object_name
                                          suffix = i_cts_hot_object->hana_object_suffix
                                          version = cl_nhi_active_version=>create_active_version( )
                                          metadata = cl_nhi_metadata_active_ver=>create_metadata(
                                            version_id = ''
                                            activated_at = ''
                                            activated_by = ''
                                            edit = '' )
                                          ).

    DATA(lo_object_metadata_read_requst) = me->m_nhi_object_api->create_read_obj_metadata_req(
                              object        = lo_nhi_object_id
                              session       = cl_nhi_active_session=>create_active_session(  ) ).

    DATA(lo_object_metadata_read_resp) = me->m_nhi_object_api->read_metadata( lo_object_metadata_read_requst ).

    raise_exc_if_resp_is_not_bound( i_response = lo_object_metadata_read_resp i_what = cl_nhi_read_obj_metadata_req=>co_what i_action = cl_nhi_read_obj_metadata_req=>co_action ).

    IF lo_object_metadata_read_resp->error_code IS INITIAL.
      r_metadata = CAST cl_nhi_metadata_active_ver( lo_object_metadata_read_resp->metadata ).
    ELSEIF lo_object_metadata_read_resp->error_code = '40112'. "object does not exist in target hana
      CLEAR r_metadata.
    ELSE.
      "##TODO proper error handling - throw exception.
      RAISE EXCEPTION TYPE cx_hana_object_transport
        EXPORTING
          textid          = cx_hana_object_transport=>read_object_metadata_error
          msgv1           = i_cts_hot_object->hana_package_id && '.' && i_cts_hot_object->hana_object_name && '.' && i_cts_hot_object->hana_object_suffix
          hana_error_code = lo_object_metadata_read_resp->error_code
          hana_error_msg  = lo_object_metadata_read_resp->error_msg.
    ENDIF.
  ENDMETHOD.