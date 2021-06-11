  METHOD if_cts_hot_db_access~read_deployable_texts_for_obj.
    IF i_languages IS INITIAL.
      "select short texts
      SELECT
         cts_hot_otexts_h~abap_object_reference,
         cts_hot_otexts_h~abap_text_reference,
         cts_hot_otexts_h~text_type,
         cts_hot_otexts_h~hana_text_id,
         cts_hot_otexts_h~hana_text_type,
         cts_hot_otexts_h~hana_text_max_length,
         cts_hot_otexts_s~language,
         cts_hot_otexts_s~hana_text_content
       FROM
        cts_hot_object
       JOIN cts_hot_otexts_h ON cts_hot_object~abap_object_reference = cts_hot_otexts_h~abap_object_reference
       JOIN cts_hot_otexts_s ON cts_hot_otexts_h~abap_object_reference = cts_hot_otexts_s~abap_object_reference
                            AND cts_hot_otexts_h~abap_text_reference = cts_hot_otexts_s~abap_text_reference
       APPENDING CORRESPONDING FIELDS OF TABLE @r_object_texts
       WHERE cts_hot_object~abap_hana_package_id = @i_abap_hana_package_id
         AND cts_hot_object~abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
         AND cts_hot_otexts_s~hot_status = @if_cts_hot_db_access=>co_hot_status_inactive.

      "select long texts
      SELECT
         cts_hot_otexts_h~abap_object_reference,
         cts_hot_otexts_h~abap_text_reference,
         cts_hot_otexts_h~text_type,
         cts_hot_otexts_h~hana_text_id,
         cts_hot_otexts_h~hana_text_type,
         cts_hot_otexts_h~hana_text_max_length,
         cts_hot_otexts_l~language,
         cts_hot_otexts_l~hana_text_content
       FROM
        cts_hot_object
       JOIN cts_hot_otexts_h ON cts_hot_object~abap_object_reference = cts_hot_otexts_h~abap_object_reference
       JOIN cts_hot_otexts_l ON cts_hot_otexts_h~abap_object_reference = cts_hot_otexts_l~abap_object_reference
                            AND cts_hot_otexts_h~abap_text_reference = cts_hot_otexts_l~abap_text_reference
       APPENDING CORRESPONDING FIELDS OF TABLE @r_object_texts
       WHERE cts_hot_object~abap_hana_package_id = @i_abap_hana_package_id
         AND cts_hot_object~abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
         AND cts_hot_otexts_l~hot_status = @if_cts_hot_db_access=>co_hot_status_inactive.
    ELSE.
      LOOP AT i_languages INTO DATA(lv_lang).
        "select short texts
        SELECT
            cts_hot_otexts_h~abap_object_reference,
            cts_hot_otexts_h~abap_text_reference,
            cts_hot_otexts_h~text_type,
            cts_hot_otexts_h~hana_text_id,
            cts_hot_otexts_h~hana_text_type,
            cts_hot_otexts_h~hana_text_max_length,
            cts_hot_otexts_s~language,
            cts_hot_otexts_s~hana_text_content
          FROM
            cts_hot_object
          JOIN cts_hot_otexts_h ON cts_hot_object~abap_object_reference = cts_hot_otexts_h~abap_object_reference
          JOIN cts_hot_otexts_s ON cts_hot_otexts_h~abap_object_reference = cts_hot_otexts_s~abap_object_reference
                               AND cts_hot_otexts_h~abap_text_reference = cts_hot_otexts_s~abap_text_reference
          APPENDING CORRESPONDING FIELDS OF TABLE @r_object_texts
          WHERE cts_hot_object~abap_hana_package_id = @i_abap_hana_package_id
            AND cts_hot_object~abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
            AND cts_hot_otexts_s~language = @lv_lang
            AND cts_hot_otexts_s~hot_status = @if_cts_hot_db_access=>co_hot_status_inactive.

        "select long texts
        SELECT
            cts_hot_otexts_h~abap_object_reference,
            cts_hot_otexts_h~abap_text_reference,
            cts_hot_otexts_h~text_type,
            cts_hot_otexts_h~hana_text_id,
            cts_hot_otexts_h~hana_text_type,
            cts_hot_otexts_h~hana_text_max_length,
            cts_hot_otexts_l~language,
            cts_hot_otexts_l~hana_text_content
          FROM
            cts_hot_object
          JOIN cts_hot_otexts_h ON cts_hot_object~abap_object_reference = cts_hot_otexts_h~abap_object_reference
          JOIN cts_hot_otexts_l ON cts_hot_otexts_h~abap_object_reference = cts_hot_otexts_l~abap_object_reference
                              AND cts_hot_otexts_h~abap_text_reference = cts_hot_otexts_l~abap_text_reference
          APPENDING CORRESPONDING FIELDS OF TABLE @r_object_texts
          WHERE cts_hot_object~abap_hana_package_id = @i_abap_hana_package_id
            AND cts_hot_object~abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
            AND cts_hot_otexts_l~language = @lv_lang
            AND cts_hot_otexts_l~hot_status = @if_cts_hot_db_access=>co_hot_status_inactive.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.