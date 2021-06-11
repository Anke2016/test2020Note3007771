  METHOD hex_to_base41.

    DATA: lv_hex        TYPE xstring,
          lv_int        TYPE int8,
          lv_do_cnt     TYPE i VALUE 0,
          lv_var_length TYPE i VALUE 8,
          lv_tmp_base41 TYPE string,
          lv_char       TYPE c,
          lv_mod_41     TYPE i.

    "split hex string (40 chars) into 5 chunks of 8 chars
    DO 5 TIMES.
      lv_do_cnt = sy-index.

      CLEAR lv_tmp_base41.
      CLEAR lv_char.

      "'00' &&
      lv_hex = substring( val = CONV string( i_hex_string ) off = ( lv_do_cnt - 1 ) * lv_var_length len = lv_var_length ). "00 in beginning because hey is always 6 chars
      lv_int = lv_hex. "convert hex to int

      WHILE lv_int > 0.
        lv_mod_41 = lv_int MOD 41.
        CASE lv_mod_41.
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
          WHEN 32. lv_char = 'W'.
          WHEN 33. lv_char = 'X'.
          WHEN 34. lv_char = 'Y'.
          WHEN 35. lv_char = 'Z'.
          WHEN 36. lv_char = '-'.
          WHEN 37. lv_char = '_'.
          WHEN 38. lv_char = '.'.
          WHEN 39. lv_char = '='.
          WHEN 40. lv_char = ';'.
          WHEN OTHERS. lv_char = lv_mod_41. "0 to 9
        ENDCASE.

        lv_tmp_base41 = lv_char && lv_tmp_base41.
        lv_int = lv_int DIV 41.
      ENDWHILE.

      "fill up with leading zeros
      IF strlen( lv_tmp_base41 ) = 6.
        e_base41 = e_base41 && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 5.
        e_base41 = e_base41 && '0' && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 4.
        e_base41 = e_base41 && '00' && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 3.
        e_base41 = e_base41 && '000' && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 2.
        e_base41 = e_base41 && '0000' && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 1.
        e_base41 = e_base41 && '00000' && lv_tmp_base41.
      ELSEIF strlen( lv_tmp_base41 ) = 0.
        e_base41 = e_base41 && '000000' && lv_tmp_base41.
      ENDIF.
    ENDDO.

  ENDMETHOD.