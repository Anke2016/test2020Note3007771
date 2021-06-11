  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: lv_message_variable_4 TYPE symsgv.

    "show as many chars of hana package name in exception text as possible...
    DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( hana_package ).

    IF textid <> unsupported_master_lang.
      lv_message_variable_4 = ls_split_text-chunk4.
    ELSE.
      lv_message_variable_4 = hana_language.
    ENDIF.

    CALL METHOD super->constructor
      EXPORTING
        textid             = textid
        previous           = previous
        message_variable_1 = ls_split_text-chunk1
        message_variable_2 = ls_split_text-chunk2
        message_variable_3 = ls_split_text-chunk3
        message_variable_4 = lv_message_variable_4.
  ENDMETHOD.