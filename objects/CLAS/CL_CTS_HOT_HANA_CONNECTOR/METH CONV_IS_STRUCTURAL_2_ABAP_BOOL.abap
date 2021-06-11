  METHOD conv_is_structural_2_abap_bool.

    IF i_in = 0.
      r_abap_bool = abap_false.
    ELSEIF i_in = 1.
      r_abap_bool = abap_true.
    ELSE.
      "##TODO different class to be used? Or Assertion?
      RAISE EXCEPTION TYPE cx_cts_hot_invalid_input EXPORTING value = '' && i_in.
    ENDIF.

  ENDMETHOD.