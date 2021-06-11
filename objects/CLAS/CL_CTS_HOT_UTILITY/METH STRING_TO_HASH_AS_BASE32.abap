  METHOD string_to_hash_as_base32.

    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm           = 'SHA1'    " Hash-Algorithmus
        if_data                = i_string  " Daten
      IMPORTING
        ef_hashxstring         = DATA(lv_hashsxtring)    " Hash-Wert binär als XString
    ).

    e_hash_base32_string = hex_to_base32( lv_hashsxtring ).

  ENDMETHOD.