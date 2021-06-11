  METHOD raise_message_exception.
    DATA: lr_exc    TYPE REF TO cx_cts_hta,
          ls_textid TYPE scx_t100key.

    ls_textid-attr1 = 'MESSAGE_VARIABLE_1'.
    ls_textid-attr2 = 'MESSAGE_VARIABLE_2'.
    ls_textid-attr3 = 'MESSAGE_VARIABLE_3'.
    ls_textid-attr4 = 'MESSAGE_VARIABLE_4'.

    ls_textid-msgid = sy-msgid.
    ls_textid-msgno = sy-msgno.

    CREATE OBJECT lr_exc
      EXPORTING
        textid             = ls_textid
*       previous           =
        message_variable_1 = sy-msgv1
        message_variable_2 = sy-msgv2
        message_variable_3 = sy-msgv3
        message_variable_4 = sy-msgv4.
    RAISE EXCEPTION lr_exc.
  ENDMETHOD.