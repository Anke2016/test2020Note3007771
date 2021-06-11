  METHOD conv_abap_bool_2_is_structural.

    IF i_abap_bool = abap_false.
      r_b = 0.
    ELSEIF i_abap_bool = abap_true.
      r_b = 1.
    ELSE.
      "##TODO different class to be used? Or Assertion?
      RAISE EXCEPTION TYPE cx_cts_hot_invalid_input EXPORTING value = CONV #( i_abap_bool ).
    ENDIF.

  ENDMETHOD.