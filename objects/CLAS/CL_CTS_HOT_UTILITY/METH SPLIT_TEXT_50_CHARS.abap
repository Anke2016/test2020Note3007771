  METHOD split_text_50_chars.
    DATA lv_length TYPE i.

    lv_length = strlen( i_text ).

    IF lv_length <= 50.
      r_result-chunk1 = i_text.
    ELSEIF lv_length <= 100.
      r_result-chunk1 = substring( val = i_text len = 50 ).
      r_result-chunk2 = substring( val = i_text off = 50 len = lv_length - 50 ).
    ELSEIF lv_length <= 150.
      r_result-chunk1 = substring( val = i_text len = 50 ).
      r_result-chunk2 = substring( val = i_text off = 50 len = lv_length - 50 ).
      r_result-chunk3 = substring( val = i_text off = 100 len = lv_length - 100 ).
    ELSE.
      r_result-chunk1 = substring( val = i_text len = 50 ).
      r_result-chunk2 = substring( val = i_text off = 50 len = lv_length - 50 ).
      r_result-chunk3 = substring( val = i_text off = 100 len = lv_length - 100 ).
      r_result-chunk4 = substring( val = i_text off = 150 len = lv_length - 150 ).
    ENDIF.
  ENDMETHOD.