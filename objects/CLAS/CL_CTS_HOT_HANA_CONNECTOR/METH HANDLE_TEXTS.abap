  METHOD handle_texts.
    DATA: lo_text_with_language LIKE LINE OF texts,
          ls_otext_h            TYPE cts_hot_otexts_h,
          ls_otext_l            TYPE cts_hot_otexts_l,
          ls_otext_s            TYPE cts_hot_otexts_s,
          lv_text_ref           TYPE string.

    LOOP AT texts INTO lo_text_with_language.
      CLEAR: ls_otext_h, ls_otext_l, ls_otext_s.

      " Skip texts that contain only spaces. Do not use condense because this would also reduce several spaces to 1 space if there are spaces in a row that might be intended!
      IF numofchar( lo_text_with_language->content ) = 0.
        CONTINUE.
      ENDIF.

      cl_lxe_hot=>is_short_text(
                          EXPORTING i_max_length_raw = CONV int4( lo_text_with_language->max_length )
                                    i_string = lo_text_with_language->content
                          IMPORTING e_max_length = DATA(lv_max_length)
                                    e_is_short_text = DATA(lv_is_short_text) ).

      ls_otext_h-abap_object_reference = object_reference.
      ls_otext_h-text_type = text_type.
      ls_otext_h-hana_text_id = lo_text_with_language->text_id.
      ls_otext_h-hana_text_type = lo_text_with_language->text_type.
      ls_otext_h-hana_text_max_length = lv_max_length.

      lv_text_ref = |{ ls_otext_h-text_type }_{ ls_otext_h-hana_text_id }|.
      IF strlen( lv_text_ref ) > 32.
        TRY.
            lv_text_ref = cl_cts_hot_utility=>string_to_hash_as_base32( lv_text_ref ).
          CATCH cx_abap_message_digest INTO DATA(lo_exc).
            RAISE EXCEPTION TYPE cx_cts_hta_hashing
              EXPORTING
                textid             = cx_cts_hta_hashing=>create_text_ref_hash_error
                previous           = lo_exc
                message_variable_1 = CONV #( lv_text_ref ).
        ENDTRY.
      ENDIF.
      ls_otext_h-abap_text_reference = lv_text_ref.

      IF lv_is_short_text = abap_false.
        ls_otext_h-abap_is_long_text = 'X'.
        ls_otext_l-abap_object_reference = ls_otext_h-abap_object_reference.
        ls_otext_l-abap_text_reference = ls_otext_h-abap_text_reference.
        ls_otext_l-language = lang.
        ls_otext_l-hana_text_content = lo_text_with_language->content.
        ls_otext_l-hot_status = if_cts_hot_db_access=>co_hot_status_new.

        MODIFY cts_hot_otexts_l FROM ls_otext_l.
      ELSE.
        ls_otext_s-abap_object_reference = ls_otext_h-abap_object_reference.
        ls_otext_s-abap_text_reference = ls_otext_h-abap_text_reference.
        ls_otext_s-language = lang.
        ls_otext_s-hana_text_content = lo_text_with_language->content.
        ls_otext_s-hot_status = if_cts_hot_db_access=>co_hot_status_new.

        MODIFY cts_hot_otexts_s FROM ls_otext_s.
      ENDIF.

      MODIFY cts_hot_otexts_h FROM ls_otext_h.
    ENDLOOP.
  ENDMETHOD.