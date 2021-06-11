  METHOD set_text_editor_properties.

    ir_text_editor->set_visible(
                EXPORTING
                  visible           =  abap_true   " Visible
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

    ir_text_editor->set_readonly_mode(
                EXPORTING
                  readonly_mode          = '1'    " read-only mode; eq 0: OFF ; ne 0: ON
                EXCEPTIONS
                  error_cntl_call_method = 1
                  invalid_parameter      = 2
                  OTHERS                 = 3
              ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HDI'
        NUMBER '100'
        EXPORTING
          message_variable_1 = TEXT-t02.
    ENDIF.

    ir_text_editor->set_text_as_stream(
                EXPORTING
                  text            = it_text " text as stream with carrige retruns and linefeeds
                EXCEPTIONS
                  error_dp        = 1
                  error_dp_create = 2
                  OTHERS          = 3
              ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HDI'
        NUMBER '100'
        EXPORTING
          message_variable_1 = TEXT-t03.
    ENDIF.

  ENDMETHOD.