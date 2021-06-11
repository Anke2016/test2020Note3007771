  METHOD hex_to_base32.

    DATA: lv_hex        TYPE xstring,
          lv_int        TYPE i,
          lv_do_cnt     TYPE i VALUE 0,
          lv_var_length TYPE i VALUE 5,
          lv_tmp_base32 TYPE string,
          lv_char       TYPE c,
          lv_mod_32     TYPE i.

    "split hex string (40 chars) into 8 chunks of 5 chars (3 Byte)
    DO 8 TIMES.
      lv_do_cnt = sy-index.

      CLEAR lv_tmp_base32.
      CLEAR lv_char.

      lv_hex = '0' && substring( val = CONV string( i_hex_string ) off = ( lv_do_cnt - 1 ) * lv_var_length len = lv_var_length ).
      lv_int = lv_hex. "convert hex (3 bytes) to int

      WHILE lv_int > 0.
        lv_mod_32 = lv_int MOD 32.
        CASE lv_mod_32.
          WHEN 10. lv_char = 'A'.
          WHEN 11. lv_char = 'B'.
          WHEN 12. lv_char = 'C'.
          WHEN 13. lv_char = 'D'.
          WHEN 14. lv_char = 'E'.
          WHEN 15. lv_char = 'F'.
          WHEN 16. lv_char = 'G'.
          WHEN 17. lv_char = 'H'.
          WHEN 18. lv_char = 'I'.
          WHEN 19. lv_char = 'J'.
          WHEN 20. lv_char = 'K'.
          WHEN 21. lv_char = 'L'.
          WHEN 22. lv_char = 'M'.
          WHEN 23. lv_char = 'N'.
          WHEN 24. lv_char = 'O'.
          WHEN 25. lv_char = 'P'.
          WHEN 26. lv_char = 'Q'.
          WHEN 27. lv_char = 'R'.
          WHEN 28. lv_char = 'S'.
          WHEN 29. lv_char = 'T'.
          WHEN 30. lv_char = 'U'.
          WHEN 31. lv_char = 'V'.
          WHEN OTHERS. lv_char = lv_mod_32. "0 to 9
        ENDCASE.

        lv_tmp_base32 = lv_char && lv_tmp_base32.
        lv_int = lv_int DIV 32.
      ENDWHILE.

      "fill up with leading zeros
      IF strlen( lv_tmp_base32 ) = 4.
        e_base32 = e_base32 && lv_tmp_base32.
      ELSEIF strlen( lv_tmp_base32 ) = 3.
        e_base32 = e_base32 && '0' && lv_tmp_base32.
      ELSEIF strlen( lv_tmp_base32 ) = 2.
        e_base32 = e_base32 && '00' && lv_tmp_base32.
      ELSEIF strlen( lv_tmp_base32 ) = 1.
        e_base32 = e_base32 && '000' && lv_tmp_base32.
      ELSEIF strlen( lv_tmp_base32 ) = 0.
        e_base32 = e_base32 && '0000'.
      ENDIF.
    ENDDO.

  ENDMETHOD.