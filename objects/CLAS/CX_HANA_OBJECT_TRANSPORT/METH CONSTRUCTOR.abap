  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA(lv_msgv1) = msgv1.
    DATA(lv_msgv2) = msgv2.
    DATA(lv_msgv3) = msgv3.
    DATA(lv_msgv4) = msgv4.

    "convert to cx_cts_hta... using only message_variable_1 to message_variable_4
    IF hana_error_code IS NOT INITIAL.
      lv_msgv3 = hana_error_code.
    ENDIF.
    IF hana_error_msg IS NOT INITIAL.
      lv_msgv4 = hana_error_msg.
    ENDIF.

    CALL METHOD super->constructor
      EXPORTING
        previous           = previous
        message_variable_1 = lv_msgv1
        message_variable_2 = lv_msgv2
        message_variable_3 = lv_msgv3
        message_variable_4 = lv_msgv4.

    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    me->hana_error_code = hana_error_code.
    me->hana_error_msg = hana_error_msg.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.