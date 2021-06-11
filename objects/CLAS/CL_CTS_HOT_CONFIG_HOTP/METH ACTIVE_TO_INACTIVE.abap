  METHOD active_to_inactive.
    FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                   <line>  TYPE cts_hot_package.

    rr_tlogo_db_inactive = cl_svrs_tlogo_db_view=>create( iv_objtype = ir_tlogo_db_active->av_objtype
                                                          iv_objname = ir_tlogo_db_active->av_objname
                                                          iv_versno  = ir_tlogo_db_active->av_versno
                                                          ir_content = ir_tlogo_db_active->ar_content
                                                          iv_create_content_copy = abap_true ).

    ASSIGN rr_tlogo_db_inactive->ar_content->('CTS_HOT_PACKAGE') TO <table>.

    LOOP AT <table> ASSIGNING <line>.
      <line>-abap_status = 'I'.
      IF <line>-hot_status <> 'D'. "if entry was to be deleted in source system, also delete here, else mark as "to be imported"
        <line>-hot_status = 'I'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.