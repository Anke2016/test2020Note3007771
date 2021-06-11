  METHOD if_cts_hot_ext_call_internal~rs_corr_check.
    DATA:
      lv_text            TYPE string,
      lv_global_lock     TYPE c,
      lv_suppress_dialog TYPE c VALUE space.

    CLEAR e_tadir.

    IF i_pgmid = 'R3TR' OR i_object_type = 'HOTA'. "HOTA required for LANG HOTA
      lv_global_lock = 'X'.
    ENDIF.

    IF i_suppress_dialog = 'X'.
      lv_suppress_dialog = 'D'. "dark mode...
    ENDIF.

    CALL FUNCTION 'RS_CORR_CHECK'
      EXPORTING
        global_lock         = lv_global_lock    " SPACE: Kleine Sperre (LIMU); 'X': Gr. Sp. (R3TR)
*       mode                = SPACE    " INSERT falls Objekt neu
        object              = i_object_name    " Name des Objekts
        object_class        = i_object_type    " Obj.-Klasse (ABAP,SCUA,SCRP,DICT,FUNC,.)
        suppress_dialog     = lv_suppress_dialog    " alle Meldungen des Korrektursystems unterdrücken
*       genflag             = SPACE    " 'X'-> generiertes Objekt
*       devclass_gen        = SPACE    " Entwicklungsklasse(berücksichtigt nur bei generierten Objekt
      IMPORTING
*       devclass            =
*       error_info          =     " obsolet: Fehlercode Korrektursystem
*       master_language     =
*       korrnum             =     " Aufgabennummer
        ordernum            = r_result    " Auftragsnummer
*       transport_key       =
        tadire              = e_tadir
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_wbo EXPORTING cts_hta_component = i_cts_hta_component.
    ENDIF.
  ENDMETHOD.