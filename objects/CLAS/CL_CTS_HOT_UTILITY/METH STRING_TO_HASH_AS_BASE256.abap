  METHOD string_to_hash_as_base256.

    DATA lv_webide_file_hash TYPE string.

    TRY.
        cl_abap_message_digest=>calculate_hash_for_raw(
                      EXPORTING
                        if_algorithm           = 'SHA256'
                        if_data                = iv_file_content
                      IMPORTING
                        ef_hashstring         = lv_webide_file_hash
                    ).
        rv_web_ide_sha256 = to_lower( lv_webide_file_hash ).
      CATCH cx_abap_message_digest INTO DATA(lr_exc).
        RAISE EXCEPTION TYPE cx_cts_hta_hdi EXPORTING previous = lr_exc.
    ENDTRY.
  ENDMETHOD.