  METHOD read_package_data_from_hana.
    TRY.
        DATA(lr_read_package_request) = me->m_nhi_package_api->create_read_package_req(
                                    tenant        = ''
                                    package       = i_hana_package_id
                                    lang          = ''
                                    lang_fallback = ''
        ).
        DATA(lr_read_package_response) = me->m_nhi_package_api->read( lr_read_package_request ).
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

    raise_exc_if_resp_is_not_bound( i_response = lr_read_package_response i_what = cl_nhi_read_package_req=>co_what i_action = cl_nhi_read_package_req=>co_action ).

    IF lr_read_package_response->error_code IS INITIAL.
      IF ( lr_read_package_response->transportable = abap_true ).
        r_package_data-hana_package_id = lr_read_package_response->package.
        r_package_data-hana_read_system = g_hana_sid.
        r_package_data-hana_pack_delivery_unit = lr_read_package_response->delivery_unit.
        r_package_data-hana_pack_du_vendor = lr_read_package_response->du_vendor.
        r_package_data-hana_pack_description = lr_read_package_response->description.
        r_package_data-hana_pack_hints_for_transl = lr_read_package_response->hints_for_translation.
        r_package_data-hana_pack_is_structural = conv_abap_bool_2_is_structural( lr_read_package_response->structural ).
        r_package_data-hana_pack_orig_lang = lr_read_package_response->orig_lang.
        r_package_data-hana_pack_responsible = lr_read_package_response->responsible.
        r_package_data-hana_pack_src_system = lr_read_package_response->src_system.
        r_package_data-hana_pack_src_tenant = lr_read_package_response->src_tenant.
        r_package_data-hana_pack_text_collection = lr_read_package_response->text_collection.
        r_package_data-hana_pack_text_status = lr_read_package_response->text_status.
        r_package_data-hana_pack_text_term_domain = lr_read_package_response->text_terminology_domain.
      ELSE.
        CLEAR r_package_data. "we do not support transport of not transportable packages
      ENDIF.
    ELSEIF lr_read_package_response->error_code = '40132'. "40132 = package not existing
      CLEAR r_package_data. "return initial if package does not exist
    ELSE.
      ls_split_text = cl_cts_hot_utility=>split_text_50_chars( i_hana_package_id ).
      RAISE EXCEPTION TYPE cx_hana_object_transport
        EXPORTING
          textid          = cx_hana_object_transport=>read_package_error
          msgv1           = ls_split_text-chunk1
          msgv2           = ls_split_text-chunk2
          hana_error_code = lr_read_package_response->error_code
          hana_error_msg  = lr_read_package_response->error_msg.
    ENDIF.
  ENDMETHOD.