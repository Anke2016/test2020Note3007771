  METHOD if_cts_hot_db_access~read_default_translation_relev.
    r_result = if_cts_hot_db_access~co_hot_relevant_for_transl. "default translation_relevance if not overwritten in CTS_HOT_PARAMS

    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_relevant_for_transl) WHERE name = 'TRANSLATION_RELEVANCE_DEFAULT'.
    CONDENSE lv_relevant_for_transl.
    IF lv_relevant_for_transl = 'N'.
      r_result = if_cts_hot_db_access~co_hot_not_relevant_for_transl.
    ENDIF.
  ENDMETHOD.