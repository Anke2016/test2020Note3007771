  METHOD fill_object_in_container.

    DATA lt_text TYPE ty_lt_text.
*    DATA lr_text_editor TYPE REF TO cl_gui_textedit.
    DATA lr_container TYPE REF TO cl_gui_container.

    "Get container instance
    lr_container = it_container[ 1 ].

    lr_container->set_visible(
      EXPORTING
        visible           = abap_true    " Visible
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HDI'
        NUMBER '100'
        EXPORTING
          message_variable_1 = TEXT-t01.
    ENDIF.

    "Convert string to readable text table
    cl_cts_hot_utility=>convert_string_to_text_table(
      EXPORTING
        iv_buffer      = iv_content_as_string
      IMPORTING
        et_texttab     = lt_text
    ).

    me->set_text_editor_properties(
      it_text        = lt_text
      ir_text_editor = NEW cl_gui_textedit( lr_container )
    ).

  ENDMETHOD.