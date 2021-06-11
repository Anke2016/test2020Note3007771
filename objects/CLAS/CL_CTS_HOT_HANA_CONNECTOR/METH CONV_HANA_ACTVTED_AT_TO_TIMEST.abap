  METHOD conv_hana_actvted_at_to_timest.
    DATA: activated_at    TYPE string,
          day             TYPE d,
          time            TYPE t,
          timest          TYPE timestampl,
          lv_sys_timezone TYPE timezone.

    activated_at = i_activated_at.

    "remove all unwanted chars in activated_at string (2014-04-14 14:26.13.1000000)
    REPLACE ALL OCCURRENCES OF REGEX '[\-\:\.\s]' IN activated_at WITH ''.

    day = activated_at(8).
    time = activated_at+8(6).

    "g_hana_timezone_string is calculated in read_hana_metadata. HANA always returns local times
    CONVERT DATE day TIME time INTO TIME STAMP r_timest TIME ZONE g_hana_timezone_string.
    IF r_timest IS INITIAL.
      "g_hana_timezone_string might be unkown for AS ABAP --> use ABAP timezone
      CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
        IMPORTING
          timezone            = lv_sys_timezone " Zeitzone
        EXCEPTIONS
          customizing_missing = 1
          OTHERS              = 2.

      IF sy-subrc <> 0.
        lv_sys_timezone = sy-zonlo.
      ENDIF.

      CONVERT DATE day TIME time INTO TIME STAMP r_timest TIME ZONE lv_sys_timezone.
    ENDIF.

    IF r_timest IS NOT INITIAL. "we saw customer case where it still was initial and then noly seconds were added which leads to error in SCTS_HTA UI
      r_timest = r_timest + ( activated_at+14(3) / 1000 ). "adds the ms to the timestamp
    ENDIF.
  ENDMETHOD.