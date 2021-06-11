  METHOD convert_string_to_text_table.

    FIELD-SYMBOLS <ls_table_line> TYPE c.

    DATA:
      lv_position    TYPE i,
      lv_text_length TYPE i.

    CLEAR et_texttab.
    ev_buffer_size = strlen( iv_buffer ).

    WHILE lv_position < ev_buffer_size.
      APPEND INITIAL LINE TO et_texttab ASSIGNING <ls_table_line> CASTING .

      " Get the length of the table line ...
      IF sy-index = 1. " ... only for the first iteration
        DESCRIBE FIELD <ls_table_line> LENGTH lv_text_length IN CHARACTER MODE.
      ENDIF.

      <ls_table_line> = iv_buffer+lv_position.
      ADD lv_text_length TO lv_position.
    ENDWHILE.

  ENDMETHOD.