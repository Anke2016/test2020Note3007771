  METHOD if_cts_hot_ext_call_internal~check_transport_tools_for_sync.
    DATA: lt_tp_params       TYPE STANDARD TABLE OF tpparams,
          lv_r3trans_hota    TYPE trtppvalue,
          lv_r3trans_version TYPE trtppvalue.

    IF g_transport_tool_check_done = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TRINT_TP_INTERFACE'
      EXPORTING
        iv_tp_command                = 'GETCAPABILITIES'     " tp Kommando (SHOWBUFFER,IMPORT,...)
        iv_system_name               = CONV sysname( sy-sysid )    " System Name
      TABLES
        tt_params                    = lt_tp_params    " Inhalt der tp Parameterdatei (nur bei SHOWPARAM)
      EXCEPTIONS
        unsupported_tp_command       = 1
        invalid_tp_command           = 2
        missing_parameter            = 3
        invalid_parameter            = 4
        get_tpparam_failed           = 5
        update_tp_destination_failed = 6
        get_tms_info_failed          = 7
        permission_denied            = 8
        tp_call_failed               = 9
        insert_tpstat_failed         = 10
        insert_tplog_failed          = 11
        OTHERS                       = 12.

    IF sy-subrc <> 0.
      cx_cts_hta=>raise_message_exception( ). "creates cx_cts_hta using sy-field values
    ENDIF.

    lv_r3trans_hota = VALUE #( lt_tp_params[ name = 'R3TRANS_HANA_OBJECT_TRANSPORT' ]-value DEFAULT lv_r3trans_hota ).
    lv_r3trans_version = VALUE #( lt_tp_params[ name = 'R3TRANS_VERSION2' ]-value DEFAULT lv_r3trans_version ).

    g_transport_tool_check_done = abap_true.

    " R3trans must have at least R3TRANS_HANA_OBJECT_TRANSPORT = 5 or R3TRANS_VERSION2 >= 20150902.
    " R3TRANS_VERSION2 >= 20150902 is checked because increase of R3TRANS_HANA_OBJECT_TRANSPORT from 4 to 5
    " was not done for important fixes
    IF lv_r3trans_hota IS NOT INITIAL
        AND lv_r3trans_version IS NOT INITIAL
        AND ( ( CONV i( lv_r3trans_hota ) >= 5 ) OR lv_r3trans_version >= '20150902' ).
      "R3trans is OK
    ELSE.
      "R3trans is too old
      IF i_suppress_dialog = 'X'.
        RAISE EXCEPTION TYPE cx_cts_hta
          EXPORTING
            textid = VALUE #( msgid = 'SCTS_HOT' msgno = '021' ). "021=Transportwerkzeuge müssen aktualisiert werden
      ELSE.
        CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
          EXPORTING
            titel = 'Warnung'(001)     " Titel
            msgid = 'SCTS_HOT'    " Nachrichten, Nachrichtenklasse
            msgty = 'W'    " Nachrichten, Nachrichtentyp
            msgno = 021.    " Nachrichten, Nachrichtennummer
      ENDIF.
    ENDIF.
  ENDMETHOD.