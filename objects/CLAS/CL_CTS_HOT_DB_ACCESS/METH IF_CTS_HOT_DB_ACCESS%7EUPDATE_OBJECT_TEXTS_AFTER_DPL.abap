  METHOD if_cts_hot_db_access~update_object_texts_after_dpl.
    DATA lv_deployed_at TYPE timestampl.

    GET TIME STAMP FIELD lv_deployed_at.
    UPDATE cts_hot_otexts_s
      SET hot_status = @if_cts_hot_db_access=>co_hot_status_active,
          abap_deployed_at = @lv_deployed_at,
          abap_deployed_by = @sy-uname
      WHERE abap_object_reference = @i_abap_object_reference
        AND abap_text_reference IN ( SELECT abap_text_reference FROM cts_hot_otexts_h WHERE abap_object_reference = @i_abap_object_reference AND text_type = @i_text_type )
        AND language = @i_language.

    UPDATE cts_hot_otexts_l
      SET hot_status = @if_cts_hot_db_access=>co_hot_status_active,
          abap_deployed_at = @lv_deployed_at,
          abap_deployed_by = @sy-uname
      WHERE abap_object_reference = @i_abap_object_reference
        AND abap_text_reference IN ( SELECT abap_text_reference FROM cts_hot_otexts_h WHERE abap_object_reference = @i_abap_object_reference AND text_type = @i_text_type )
        AND language = @i_language.
  ENDMETHOD.