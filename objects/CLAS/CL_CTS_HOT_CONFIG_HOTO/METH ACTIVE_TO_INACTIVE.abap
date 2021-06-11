  METHOD active_to_inactive.
    FIELD-SYMBOLS: <table>    TYPE ANY TABLE,
                   <line_hdi> TYPE cts_hdi_object,
                   <line_hot> TYPE cts_hot_object.

    rr_tlogo_db_inactive = cl_svrs_tlogo_db_view=>create( iv_objtype = ir_tlogo_db_active->av_objtype
                                                          iv_objname = ir_tlogo_db_active->av_objname
                                                          iv_versno  = ir_tlogo_db_active->av_versno
                                                          ir_content = ir_tlogo_db_active->ar_content
                                                          iv_create_content_copy = abap_true ).

    IF ir_tlogo_db_active->av_objname CA '/'.
      DATA(lv_hdi_object) = 'X'.
    ENDIF.

    IF lv_hdi_object = 'X'.
      ASSIGN rr_tlogo_db_inactive->ar_content->('CTS_HDI_OBJECT') TO <table>.
      LOOP AT <table> ASSIGNING <line_hdi>.
        <line_hdi>-abap_status = 'I'.
        IF <line_hdi>-hot_status <> 'D'. "if entry was to be deleted in source system, also delete here, else mark as "to be imported"
          <line_hdi>-hot_status = 'I'.
        ENDIF.
      ENDLOOP.
    ELSE.
      ASSIGN rr_tlogo_db_inactive->ar_content->('CTS_HOT_OBJECT') TO <table>.
      LOOP AT <table> ASSIGNING <line_hot>.
        <line_hot>-abap_status = 'I'.
        IF <line_hot>-hot_status <> 'D'. "if entry was to be deleted in source system, also delete here, else mark as "to be imported"
          <line_hot>-hot_status = 'I'.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.