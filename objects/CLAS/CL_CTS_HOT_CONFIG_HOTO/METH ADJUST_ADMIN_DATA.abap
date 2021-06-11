  METHOD adjust_admin_data.

    FIELD-SYMBOLS: <head>          TYPE ANY TABLE,
                   <head_line_hdi> TYPE cts_hdi_object,
                   <head_line_hot> TYPE cts_hot_object.
    DATA: lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp. "  UTC time

    IF ir_tlogo->av_objname CA '/'.
      DATA(lv_hdi_object) = 'X'.
    ENDIF.

    IF lv_hdi_object = 'X'.
      ASSIGN ir_tlogo->ar_content->('CTS_HDI_OBJECT') TO <head>.
      LOOP AT <head> ASSIGNING <head_line_hdi>.

        IF sy-tcode = 'SNOTE' .
          <head_line_hdi>-abap_sync_system = |SAP-{ sy-tcode }|.
        ELSEIF sy-tcode = 'SCWB' .
          <head_line_hdi>-abap_sync_system = |SAPCWB-{ me->get_source_system_id( <head_line_hdi>-abap_sync_system ) }|.
        ELSEIF sy-tcode = 'UDO'.
          <head_line_hdi>-abap_sync_system = |SAP{ sy-tcode }-{ me->get_source_system_id( <head_line_hdi>-abap_sync_system ) }|.
        ELSE.
*       can occur?
          CLEAR <head_line_hdi>-abap_sync_system.
          CLEAR <head_line_hdi>-hana_read_system.
        ENDIF.

*** NEW
        <head_line_hdi>-abap_changed_at = lv_timestamp. " TODO ==> CTS_HOT_OBJECT auch?
        <head_line_hdi>-abap_changed_by = iv_name.

***        <head_line_hdi>-abap_synced_at = lv_timestamp. ==> CTS_HOT_OBJECT auch?
***        <head_line_hdi>-abap_synced_by = iv_name.

        CLEAR <head_line_hdi>-abap_deployed_at.
        CLEAR <head_line_hdi>-abap_deployed_by.
        CLEAR <head_line_hdi>-hdi_deployed_at.
        CLEAR <head_line_hdi>-abap_import_timestamp.

      ENDLOOP.
    ELSE.
      ASSIGN ir_tlogo->ar_content->('CTS_HOT_OBJECT') TO <head>.
      LOOP AT <head> ASSIGNING <head_line_hot>.

        IF sy-tcode = 'SNOTE' .
          <head_line_hot>-abap_sync_system = |SAP-{ sy-tcode }|.
        ELSEIF sy-tcode = 'SCWB' .
          <head_line_hot>-abap_sync_system = |SAPCWB-{ <head_line_hot>-abap_sync_system }|.
        ELSEIF sy-tcode = 'UDO'.
          <head_line_hot>-abap_sync_system = |SAP{ sy-tcode }-{ <head_line_hot>-abap_sync_system }|.
        ELSE.
*       can occur?
          CLEAR <head_line_hot>-abap_sync_system.
          CLEAR <head_line_hot>-hana_read_system.
        ENDIF.

        <head_line_hot>-abap_synced_at = lv_timestamp.
        <head_line_hot>-abap_synced_by = iv_name.

        CLEAR <head_line_hot>-abap_deployed_at.
        CLEAR <head_line_hot>-abap_deployed_by.
        CLEAR <head_line_hot>-abap_import_timestamp.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.