  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA(ls_split_text_conf) = cl_cts_hot_utility=>split_text_50_chars( name_of_obj_or_package_conf ).
    DATA(ls_split_text_hta) = cl_cts_hot_utility=>split_text_50_chars( name_of_obj_or_package_hta ).
    CALL METHOD super->constructor
      EXPORTING
        textid             = textid
        previous           = previous
        message_variable_1 = ls_split_text_conf-chunk1
        message_variable_2 = ls_split_text_conf-chunk2
        message_variable_3 = ls_split_text_hta-chunk1
        message_variable_4 = ls_split_text_hta-chunk2
        cts_hta_component  = cts_hta_component.
  ENDMETHOD.