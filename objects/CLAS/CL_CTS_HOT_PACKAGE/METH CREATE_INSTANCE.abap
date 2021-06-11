  METHOD create_instance.
    DATA: lv_hana_package_id      TYPE string,
          lv_abap_hana_package_id TYPE string.

    lv_hana_package_id = iv_hana_package_id.

    CONDENSE lv_hana_package_id. "do not allow spaces in beginning and end
    IF strlen( lv_hana_package_id ) <= 40.
      lv_abap_hana_package_id = to_upper( lv_hana_package_id ).
    ELSE.
      DATA(lv_hana_package_id_upper) = to_upper( lv_hana_package_id ).
      TRY.
          DATA(lv_hash_string) = cl_cts_hot_utility=>string_to_hash_as_base32( lv_hana_package_id_upper ).
        CATCH cx_abap_message_digest INTO DATA(lo_exc).
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid   = cx_hana_object_transport=>create_package_hash_error
              msgv1    = CONV #( iv_hana_package_id )
              previous = lo_exc.
      ENDTRY.
      DATA(lv_nr_of_non_hash_chars) = 39 - strlen( lv_hash_string ). "39 because of 40 - seperator
      lv_abap_hana_package_id = lv_hana_package_id_upper(lv_nr_of_non_hash_chars) && co_hash_seperator && lv_hash_string.
    ENDIF.

    CREATE OBJECT r_result
      EXPORTING
        iv_hana_package_id      = lv_hana_package_id
        iv_abap_hana_package_id = lv_abap_hana_package_id.

  ENDMETHOD.