  METHOD is_du_existing_in_hana.

    r_exists = abap_false.
    TRY.
        DATA(lo_exists_du_response) = m_nhi_delivery_unit_api->exists( request = m_nhi_delivery_unit_api->create_exists_du_req(
                                   name    = i_du_name
                                   vendor  = i_du_vendor
                              ) ).

        IF lo_exists_du_response IS BOUND AND lo_exists_du_response->error_code = '0'.
          r_exists = lo_exists_du_response->exists.
        ENDIF.
      CATCH cx_nhi_hana_repository.    "ignore and assume not existing
        r_exists = abap_false.
    ENDTRY.

  ENDMETHOD.