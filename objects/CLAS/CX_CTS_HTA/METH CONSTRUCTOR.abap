  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous           = previous
        message_variable_1 = message_variable_1
        message_variable_2 = message_variable_2
        message_variable_3 = message_variable_3
        message_variable_4 = message_variable_4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->cts_hta_component = cts_hta_component.
  ENDMETHOD.