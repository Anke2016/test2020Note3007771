  METHOD adjust_admin_data.
    FIELD-SYMBOLS: <head>      TYPE ANY TABLE,
                   <head_line> TYPE cts_hot_package.
    DATA: lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp. "  UTC time

    ASSIGN ir_tlogo->ar_content->('CTS_HOT_PACKAGE') TO <head>.

    LOOP AT <head> ASSIGNING <head_line>.

      IF sy-tcode = 'SNOTE' .
        <head_line>-abap_sync_system = |SAP-{ sy-tcode }|.
      ELSEIF sy-tcode = 'SCWB' .
        <head_line>-abap_sync_system = |SAPCWB-{ <head_line>-abap_sync_system }|.
      ELSEIF sy-tcode = 'UDO'.
        <head_line>-abap_sync_system = |SAP{ sy-tcode }-{ <head_line>-abap_sync_system }|.
      ELSE.
*       can occur?
        CLEAR <head_line>-abap_sync_system.
        CLEAR <head_line>-hana_read_system.
      ENDIF.

      <head_line>-abap_synced_at = lv_timestamp.
      <head_line>-abap_synced_by = iv_name.

      CLEAR <head_line>-abap_deployed_at.
      CLEAR <head_line>-abap_deployed_by.

      CLEAR <head_line>-abap_import_timestamp.

    ENDLOOP.
  ENDMETHOD.