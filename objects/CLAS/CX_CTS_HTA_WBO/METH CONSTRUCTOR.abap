  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: ls_textid TYPE scx_t100key.

    ls_textid-msgid = sy-msgid.
    ls_textid-msgno = sy-msgno.

    ls_textid-attr1 = 'MESSAGE_VARIABLE_1'.
    ls_textid-attr2 = 'MESSAGE_VARIABLE_2'.
    ls_textid-attr3 = 'MESSAGE_VARIABLE_3'.
    ls_textid-attr4 = 'MESSAGE_VARIABLE_4'.

    CALL METHOD super->constructor
      EXPORTING
        textid             = ls_textid
        message_variable_1 = sy-msgv1
        message_variable_2 = sy-msgv2
        message_variable_3 = sy-msgv3
        message_variable_4 = sy-msgv4
        cts_hta_component  = cts_hta_component.
  ENDMETHOD.