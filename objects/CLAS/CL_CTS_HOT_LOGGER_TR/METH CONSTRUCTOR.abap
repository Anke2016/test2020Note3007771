  METHOD constructor.
    DATA: lv_system_name TYPE sy-sysid.

    super->constructor( ).

    " Abort in case of exceptions
    IF gv_sysname IS INITIAL.
      CALL FUNCTION 'TR_SYS_PARAMS'
        IMPORTING
          systemname = lv_system_name.

      gv_sysname = lv_system_name.
    ENDIF.
  ENDMETHOD.