*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_log_helper_4_scts_hot DEFINITION.
  PUBLIC SECTION.
    "chunk size 32 but only 31 are used. 1 is reserved for adding a space in the beginning
    "if previous variable ended with space because space ending is cut off later in logging
    "and leads to words being merged without space
    CONSTANTS msg_para_chunk_size TYPE i VALUE 32.

    TYPES:
      BEGIN OF ty_split_message,
        var1 TYPE c LENGTH msg_para_chunk_size,
        var2 TYPE c LENGTH msg_para_chunk_size,
        var3 TYPE c LENGTH msg_para_chunk_size,
        var4 TYPE c LENGTH msg_para_chunk_size,
      END OF ty_split_message,
      tt_split_message TYPE STANDARD TABLE OF ty_split_message WITH DEFAULT KEY.

    CLASS-METHODS:
      "! Splits a message in maximum 4 pieces each of max length msg_para_chunk_size.
      split_message
        IMPORTING
          message                 TYPE string
        RETURNING
          VALUE(rt_split_message) TYPE tt_split_message. "copied from cl_nhi_dup_utility

  PRIVATE SECTION.
    CLASS-METHODS:
      "! Internal helper method to shift trailing spaces to next variable because trailing spaces of variables are deleted when being logged
      fix_trailing_spaces CHANGING ch_line TYPE ty_split_message.
ENDCLASS.

CLASS lcl_log_helper_4_scts_hot IMPLEMENTATION.
  METHOD split_message.
    DATA:
      lv_message    LIKE message,
      lt_messages   TYPE STANDARD TABLE OF string,
      lv_var_length TYPE i,
      lv_var        TYPE i,
      lv_line       TYPE ty_split_message.

    SPLIT message AT cl_abap_char_utilities=>newline INTO TABLE lt_messages IN CHARACTER MODE. "HANA might return lines with line break...

    LOOP AT lt_messages INTO lv_message.
      CLEAR lv_line.

      DATA(lv_current_length) = strlen( lv_message ).
      WHILE lv_current_length > 0.

        IF lv_current_length < msg_para_chunk_size - 1.
          lv_var_length = lv_current_length.
        ELSE.
          lv_var_length = msg_para_chunk_size - 1. " -1 because we must reserve 1 space for shifting (see description of msg_para_chunk_size)
        ENDIF.

        lv_var = sy-index MOD 4.
        CASE lv_var.
          WHEN 1.
            lv_line-var1 = substring( val = lv_message len = lv_var_length ).
          WHEN 2.
            lv_line-var2 = substring( val = lv_message len = lv_var_length ).
          WHEN 3.
            lv_line-var3 = substring( val = lv_message len = lv_var_length ).
          WHEN 0.
            lv_line-var4 = substring( val = lv_message len = lv_var_length ).
            fix_trailing_spaces( CHANGING ch_line = lv_line ).
            APPEND lv_line TO rt_split_message.
            CLEAR lv_line.
        ENDCASE.
        lv_message = substring( val = lv_message off = lv_var_length ).
        IF lv_var = 0. "as of second line add 2 spaces in front to better see in the log that it belongs to first row line (first row is where sy-index = 1)
          lv_message = |  { lv_message }|.
        ENDIF.
        lv_current_length = strlen( lv_message ).
      ENDWHILE.

      IF lv_line IS NOT INITIAL.
        fix_trailing_spaces( CHANGING ch_line = lv_line ).
        APPEND lv_line TO rt_split_message.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fix_trailing_spaces.

    DATA: space_check_pos_var1 TYPE i,
          space_check_pos_var2 TYPE i,
          space_check_pos_var3 TYPE i.

    space_check_pos_var1 = msg_para_chunk_size - 2. "Position in line-var1 to check for space ending and shifting of next variable to prevent merge of 2 words.
    space_check_pos_var2 = msg_para_chunk_size - 2. "Position in line-var2 to check for space ending and shifting of next variable to prevent merge of 2 words.
    space_check_pos_var3 = msg_para_chunk_size - 2. "Position in line-var3 to check for space ending and shifting of next variable to prevent merge of 2 words.

    IF ch_line IS NOT INITIAL.
      IF ch_line-var1+space_check_pos_var1(1) = space AND strlen( ch_line-var2 ) > 0.
        SHIFT ch_line-var2 RIGHT.
        space_check_pos_var2 = space_check_pos_var2 + 1. "increase because we shifted var-2 to the right.
      ENDIF.
      IF ch_line-var2+space_check_pos_var2(1) = space AND strlen( ch_line-var3 ) > 0.
        SHIFT ch_line-var3 RIGHT.
        space_check_pos_var3 = space_check_pos_var3 + 1. "increase because we shifted var-3 to the right.
      ENDIF.
      IF ch_line-var3+space_check_pos_var3(1) = space AND strlen( ch_line-var4 ) > 0.
        SHIFT ch_line-var4 RIGHT.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_log_helper DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: co_chunk_size_50 TYPE i VALUE 50,
               co_chunk_size_31 TYPE i VALUE 31.

    TYPES:
      " Use 3 times length of 50 chars, even in total 131 chars are only allowed for ABAP GUI logger.
      " However, if spaces are moved, e.g. from the end of var1 to beginning of var2 to keep them visible in logging,
      " then var3 can contain 32 chars instead of 31
      BEGIN OF ty_s_split_message,
        var1 TYPE c LENGTH co_chunk_size_50,
        var2 TYPE c LENGTH co_chunk_size_50,
        var3 TYPE c LENGTH co_chunk_size_50,
      END OF ty_s_split_message,
      ty_t_split_message TYPE STANDARD TABLE OF ty_s_split_message WITH EMPTY KEY.

    CLASS-METHODS:
      "! Splits a message in maximum 3 pieces with max length 50, 50 and 32
      split_message
        IMPORTING
          iv_message               TYPE string
        RETURNING
          VALUE(rt_split_messages) TYPE ty_t_split_message.
ENDCLASS.

CLASS lcl_log_helper IMPLEMENTATION.
  METHOD split_message.
    DATA:
      lv_message         LIKE iv_message,
      lt_messages        TYPE STANDARD TABLE OF string,
      lv_var_length      TYPE i,
      lv_var             TYPE i,
      lv_position        TYPE i,
      lv_current_length  TYPE i,
      lv_next_chunk_size TYPE i,
      ls_line            TYPE ty_s_split_message,
      lv_indent          TYPE string,
      lv_append_line     TYPE abap_bool,
      lv_space_counter   TYPE i,
      lv_moved_spaces    TYPE i.

    lv_message = iv_message.

    "find out how many spaces/indent are there in the beginning of the text to keep them if text spans more than 1 logline
    lv_position = 0.
    lv_current_length = strlen( lv_message ).
    WHILE lv_position < lv_current_length AND lv_message+lv_position(1) = ` `.
      lv_indent = | { lv_indent }|.
      lv_position = lv_position + 1.
    ENDWHILE.

    "remove trailing spaces
    lv_position = lv_current_length - 1.
    WHILE lv_position > 0 AND lv_message+lv_position(1) = ` `. "not lv_position>=0 to keep at least one space if there are only spaces in iv_message
      lv_space_counter = lv_space_counter + 1.
      lv_position = lv_position - 1.
    ENDWHILE.
    IF lv_space_counter > 0.
      lv_message = substring( val = lv_message len = lv_current_length - lv_space_counter ).
    ENDIF.

    SPLIT lv_message AT cl_abap_char_utilities=>newline INTO TABLE lt_messages IN CHARACTER MODE. "HANA might return lines with line break...

    LOOP AT lt_messages INTO lv_message.
      CLEAR ls_line.

      lv_current_length = strlen( lv_message ).
      lv_next_chunk_size = co_chunk_size_50.

      WHILE lv_current_length > 0.
        lv_var = sy-index MOD 3. "3 because of 3 vars (50,50,31)

        IF lv_current_length <= lv_next_chunk_size.
          lv_var_length = lv_current_length.
        ELSE.
          lv_var_length = lv_next_chunk_size.
        ENDIF.

        CASE lv_var.
          WHEN 1.
            CLEAR lv_moved_spaces. "every new line starts with 0 moved spaces within the line
            ls_line-var1 = substring( val = lv_message len = lv_var_length ).
            lv_next_chunk_size = co_chunk_size_50.
            lv_append_line = abap_true.
          WHEN 2.
            ls_line-var2 = substring( val = lv_message len = lv_var_length ).
            lv_next_chunk_size = co_chunk_size_31.
*            IF lv_next_chunk_size > co_chunk_size_50.
*              lv_next_chunk_size = co_chunk_size_50.
*            ENDIF.
            lv_append_line = abap_true.
          WHEN 0.
            ls_line-var3 = substring( val = lv_message len = lv_var_length ).
            lv_next_chunk_size = co_chunk_size_50.
            APPEND ls_line TO rt_split_messages.
            CLEAR ls_line.
            CLEAR lv_append_line.
        ENDCASE.

        "spaces in the end of current chunk need to be moved to beginning of next chunk
        "because they are removed automatically when writing to log but only if there are more chunks to come.
        "But only within 1 logline. Spaces at the end of a line are not taken over to next line
        IF strlen( lv_message ) <> lv_var_length AND lv_var <> 0.
          lv_position = lv_var_length - 1. "position of last char of current chunk
          WHILE lv_position > 0 AND lv_message+lv_position(1) = ` `.
            lv_var_length = lv_var_length - 1.
            lv_position = lv_position - 1.
            lv_moved_spaces = lv_moved_spaces + 1. "count moved spaces because space is removed from line-var and thus last var can contain 1 more char
          ENDWHILE.
        ENDIF.

        lv_message = substring( val = lv_message off = lv_var_length ).
        IF lv_var = 0 AND lv_message IS NOT INITIAL. "as of second line add 2 spaces in front to better see in the log that it belongs to first row line (first row is where sy-index = 1)
          lv_message = |{ lv_indent }  { lv_message }|.
        ENDIF.
        lv_current_length = strlen( lv_message ).

        IF lv_var = 2.
          "increase length of var3 if spaces were moved from end of var1 to beginning of var2 or from
          "end of var2 to beginning of var3 because spaces in the end of a var will be removed by the system
          lv_next_chunk_size = lv_next_chunk_size + lv_moved_spaces.
        ENDIF.
      ENDWHILE.

      IF lv_append_line = abap_true.
        APPEND ls_line TO rt_split_messages.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.