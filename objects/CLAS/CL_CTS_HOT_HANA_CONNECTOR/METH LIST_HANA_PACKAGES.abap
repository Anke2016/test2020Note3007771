  METHOD list_hana_packages.
    TRY.
        DATA(lo_list_package_res) = me->m_nhi_package_api->list( me->m_nhi_package_api->create_list_packages_req(
                        tenant                  = ''
                        package                 = |{ i_hana_package_name }| "finds all packages with current package in name
                        src_system              = ''
                        src_tenant              = ''
                        description             = ''
                        responsible             = ''
                        orig_lang               = ''
                        structural              = ''
                        delivery_unit           = ''
                        transportable           = '' "usually abap_true but can not be used as filter... with regards to repo rest documentation
                        content                 = ''
                        du_vendor               = ''
                        text_collection         = ''
                        text_status             = ''
                        text_terminology_domain = ''
                        hints_for_translation   = ''
                    ) ).
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

    IF lo_list_package_res->error_code = '0'.
      LOOP AT lo_list_package_res->packages INTO DATA(lo_list_package).
        IF lo_list_package->transportable = abap_true.
          APPEND lo_list_package->package TO r_result.
        ENDIF.
      ENDLOOP.
    ELSE.
      RAISE EXCEPTION TYPE cx_hana_object_transport
        EXPORTING
          textid          = cx_hana_object_transport=>list_packages_error
          msgv1           = CONV #( i_hana_package_name )
          hana_error_code = lo_list_package_res->error_code
          hana_error_msg  = lo_list_package_res->error_msg.
    ENDIF.
  ENDMETHOD.