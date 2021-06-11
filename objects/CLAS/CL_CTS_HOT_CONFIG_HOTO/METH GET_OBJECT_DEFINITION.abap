  METHOD get_object_definition.
    DATA: ls_objsl TYPE objsl.
    CLEAR rs_obj_definition.

*   get object definition for active/inactive version
    IF iv_state = cl_svrs_tlogo_controller=>co_active OR iv_state = cl_svrs_tlogo_controller=>co_inactive.

*     Get header information
      rs_obj_definition-objh-objectname = 'HOTO'. " LIMU HOTO
      rs_obj_definition-objh-objecttype = 'L'.    " 'L' - log. tr. object
      rs_obj_definition-objh-langdep    = ' '.    " table not language dependant <=> 'X'.
      rs_obj_definition-objh-objcateg   = 'SYST'. " 'SYST sytem/workbench object
      rs_obj_definition-objh-phaselevel = 0.      " '0' ==> "Import Phase level zur Steuerung der Import Reihenfolge"?TODO
      rs_obj_definition-objh-checkid    = 'L'.    " 'L' => lockable, check of origin and transportable
      rs_obj_definition-objh-objnamelen = 99.     " If objnamelen = 99, CL_SVRS_TLOGO_READER->GET_WHERE_CLAUSE will internally work with
      rs_obj_definition-objh-mccategory = 110.    "    local variable lv_objname_len = is_obj_def-objh-mccategory = 110
      rs_obj_definition-objh-objtransp  = 2.      " 2 = automativ transport
      rs_obj_definition-objh-objcharset = 1.      " 1 => allowed character set

*     Get object information
      ls_objsl-objectname = 'HOTO'.            " LIMU HOTO
      ls_objsl-objecttype = 'L'.               " 'L' - log. tr. object
      ls_objsl-trwcount   = 0.                 " 0 transportwesen count
      ls_objsl-tpgmid     = 'R3TR'.  " TODO: 'R3TR'.
      ls_objsl-tobject    = 'TABU'.
      ls_objsl-tobj_name  = 'CTS_HOT_OBJECT'.

      IF iv_state = cl_svrs_tlogo_controller=>co_active.
        ls_objsl-tobjkey    = '/&A'.
      ELSE.                                 " cl_svrs_tlogo_controller=>co_inactive
        ls_objsl-tobjkey    = '/&I'.
      ENDIF.

      ls_objsl-masknlen   = 14.           "Maskenlänge für Namensfeld ('CTS_HOT_PACKAGE')
      ls_objsl-maskklen   = 2.            "Maskenlänge für Keyfeld: 1 ('/&') + 1 ('A'/'I') = 2
      ls_objsl-prim_table = 'X'.
      APPEND ls_objsl TO rs_obj_definition-objsl.

      ls_objsl-tobj_name  = 'CTS_HDI_OBJECT'.
      APPEND ls_objsl TO rs_obj_definition-objsl.
    ELSE.
*   other state not supported, throw exception
      RAISE EXCEPTION TYPE cx_svrs_illegal_state
        EXPORTING
          textid  = cx_svrs_illegal_state=>cx_svrs_illegal_state
          state   = iv_state
          objtype = me->av_objtype.
    ENDIF.

  ENDMETHOD.