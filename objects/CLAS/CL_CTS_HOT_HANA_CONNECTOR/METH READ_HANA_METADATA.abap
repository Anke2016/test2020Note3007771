  METHOD read_hana_metadata.
    "method idea copied from cl_nhi_dup_utility->lif_db_access~get_repository_api_version
    CONSTANTS: co_select_read_metadata TYPE string VALUE 'select key, value from M_HOST_INFORMATION where key = ''sid'' or key = ''build_version'' or key = ''timezone_offset''' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_hana_data,
        key   TYPE c LENGTH 32,
        value TYPE c LENGTH 256,
      END OF ty_hana_data.

    DATA:
      lt_hana_data         TYPE STANDARD TABLE OF ty_hana_data,
      ls_hana_data         TYPE ty_hana_data,
      lo_values            TYPE REF TO data,
      lv_post_timezone     TYPE c,
      lv_timediff_in_hours TYPE i.

    TRY.
        DATA(lo_database_connection) = cl_sql_connection=>get_connection( cl_sql_connection=>c_default_connection ).

        DATA(lo_sql_statement) = NEW cl_sql_statement( con_ref = lo_database_connection ).

        "read all metadata
        DATA(lo_sql_result) = lo_sql_statement->execute_query( statement = co_select_read_metadata ).

        GET REFERENCE OF lt_hana_data INTO lo_values.

        lo_sql_result->set_param_table( lo_values ).
        lo_sql_result->next_package( ).
        lo_sql_result->close( ).

        LOOP AT lt_hana_data INTO ls_hana_data.
          IF ls_hana_data-key = 'sid'.
            g_hana_sid = ls_hana_data-value.
          ELSEIF ls_hana_data-key = 'build_version'.
            g_hana_build_version = ls_hana_data-value.
          ELSEIF ls_hana_data-key = 'timezone_offset'.
            g_hana_timezone_offset = ls_hana_data-value.

            "convert to timezone string as in table TTZZ (e.g. UTC+4, UTC+63, UTC-10, ...)
            IF g_hana_timezone_offset MOD 3600 <> 0.
              lv_post_timezone = '3'.
            ENDIF.

            lv_timediff_in_hours = g_hana_timezone_offset / 3600.
            IF lv_timediff_in_hours < 0.
              g_hana_timezone_string = |UTC{ lv_timediff_in_hours }{ lv_post_timezone } |.
            ELSEIF lv_timediff_in_hours > 0.
              g_hana_timezone_string = |UTC+{ lv_timediff_in_hours }{ lv_post_timezone } |.
            ELSE.
              IF g_hana_timezone_offset > 0.
                g_hana_timezone_string = |UTC+0{ lv_post_timezone }|.
              ELSEIF g_hana_timezone_offset < 0.
                g_hana_timezone_string = |UTC-0{ lv_post_timezone }|.
              ELSE.
                g_hana_timezone_string = |UTC|.
              ENDIF.
            ENDIF.

*          "could end with CONVT_NO_NUMBER in case HANA does not return a number anymore
*          "CATCH SYSTEM-EXCEPTIONS CONVT_NO_NUMBER = 5.
*          " DO.
*          "   ......
*          "   ......
*          " ENDDO.
*          "ENDCATCH.
          ENDIF.
        ENDLOOP.

        IF g_hana_timezone_offset IS INITIAL.
*          "'#TODO set AS ABAP timezone offset
        ENDIF.

      CATCH cx_sql_exception INTO DATA(lx_repository_exception).
        "ignore exception and set g_hana_sid to space
        g_hana_sid = ''.
        "'#TODO set AS ABAP timezone offset
        g_hana_timezone_offset = 0.
        g_hana_build_version = 'unknown'.
    ENDTRY.

  ENDMETHOD.