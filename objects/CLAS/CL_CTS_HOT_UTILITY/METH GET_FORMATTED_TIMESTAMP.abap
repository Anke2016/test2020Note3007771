  METHOD get_formatted_timestamp.
    DATA(lv_timestamp) = iv_timestamp.

    IF lv_timestamp IS INITIAL.
      GET TIME STAMP FIELD lv_timestamp.
    ENDIF.

    DATA(lv_tmp) = |{ lv_timestamp }|.
    IF strlen( lv_tmp ) >= 14.
      rv_result = |{ lv_tmp(4) }-{ lv_tmp+4(2) }-{ lv_tmp+6(2) } { lv_tmp+8(2) }:{ lv_tmp+10(2) }:{ lv_tmp+12(2) }|.
    ENDIF.
  ENDMETHOD.