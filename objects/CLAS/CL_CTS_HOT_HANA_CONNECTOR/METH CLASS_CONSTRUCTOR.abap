  METHOD class_constructor.
    "check if we are on HANA
    TRY.
        cl_nhi_api=>create_instance( ).

        read_hana_metadata( ).
      CATCH cx_nhi_not_supported.
        "not on HANA, set defaults
        g_hana_sid = ''.
        "'#TODO set AS ABAP timezone offset
        g_hana_timezone_offset = 0.
        g_hana_build_version = 'unknown'.
    ENDTRY.
  ENDMETHOD.