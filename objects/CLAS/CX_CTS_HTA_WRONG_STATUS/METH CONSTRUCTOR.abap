  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( name_of_obj_or_package ).
    CALL METHOD super->constructor
      EXPORTING
        textid             = textid
        previous           = previous
        message_variable_1 = ls_split_text-chunk1
        message_variable_2 = ls_split_text-chunk2
        message_variable_3 = ls_split_text-chunk3
        message_variable_4 = CONV #( hot_status )
        cts_hta_component  = cts_hta_component.
  ENDMETHOD.