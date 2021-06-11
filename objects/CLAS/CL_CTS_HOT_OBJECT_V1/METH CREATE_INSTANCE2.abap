  METHOD create_instance2.

    data: lv_hana_object_name type string,
          lv_hana_object_suffix type string,
          lv_abap_hana_object_name_suffx type string.

    lv_hana_object_name = iv_hana_object_name.
    lv_hana_object_suffix = iv_hana_object_suffix.

    CONDENSE lv_hana_object_name. "do not allow spaces in beginning and end
    CONDENSE lv_hana_object_suffix. "do not allow spaces in beginning and end

    "Check that 70 chars are not exceeded for object_name + delimitr + object_suffix
    "and in case hash the object_name and objecT_suffix to 70 chars with the following format:
    "object_name(length n) + Separator + Hash + Separator + suffix (n = 68-len(suffix)) '68 becuse of 70 - 2 hash_seperators
    "But oly take up to 20 chars for suffix, otherwise also cut off suffix.
    DATA(lv_object_name_length)  = strlen( lv_hana_object_name ).
    DATA(lv_suffix_length) = strlen( lv_hana_object_suffix ).

    IF lv_object_name_length + lv_suffix_length < 70. "< 70because delimitr is also 1 char to count to support 70 chars without hashing
      lv_abap_hana_object_name_suffx = to_upper( lv_hana_object_name && co_object_name_suffix_delimitr && lv_hana_object_suffix ).
    ELSE.
      DATA(lv_obj_name_suffix_upper) = to_upper( lv_hana_object_name && co_object_name_suffix_delimitr && lv_hana_object_suffix ).
      TRY.
          DATA(lv_hash_string) = cl_cts_hot_utility=>string_to_hash_as_base32( lv_obj_name_suffix_upper ).
        CATCH cx_abap_message_digest INTO DATA(lo_exc).
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid   = cx_hana_object_transport=>create_object_name_hash_error
              msgv1    = CONV #( lv_hana_object_name )
              msgv2    = CONV #( lv_hana_object_suffix )
              previous = lo_exc.
      ENDTRY.

      DATA(lv_final_suffix) = substring( val = lv_obj_name_suffix_upper off = ( lv_object_name_length + 1 ) len = lv_suffix_length ).
      DATA(lv_nr_of_non_hash_chars) = 68 - 32 - lv_suffix_length. "68 because of 2 seperators, 32 for base32 atring length

      IF lv_suffix_length > 20.
        lv_final_suffix = substring( val = lv_final_suffix off = ( strlen( lv_final_suffix ) - 20 ) len = 20 ).
        lv_nr_of_non_hash_chars = 16.
      ENDIF.

      lv_abap_hana_object_name_suffx = lv_obj_name_suffix_upper(lv_nr_of_non_hash_chars)
                                                && cl_cts_hot_package=>co_hash_seperator
                                                && lv_hash_string
                                                && cl_cts_hot_package=>co_hash_seperator
                                                && lv_final_suffix.
    ENDIF.

    CREATE OBJECT r_result
      EXPORTING
        iv_abap_hana_package_id        = io_cts_hot_package->abap_hana_package_id
        iv_abap_hana_object_name_suffx = conv #( lv_abap_hana_object_name_suffx )
        iv_hana_package_id             = io_cts_hot_package->hana_package_id
        iv_hana_object_name            = lv_hana_object_name
        iv_hana_object_suffix          = lv_hana_object_suffix.

  ENDMETHOD.