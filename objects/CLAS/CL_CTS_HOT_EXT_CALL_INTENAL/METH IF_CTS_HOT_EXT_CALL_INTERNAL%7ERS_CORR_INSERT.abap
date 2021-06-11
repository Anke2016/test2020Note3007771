  METHOD if_cts_hot_ext_call_internal~rs_corr_insert.

    DATA:
      lv_text            TYPE string,
      lv_global_lock     TYPE c VALUE space,
      lv_suppress_dialog TYPE c VALUE space.

    IF i_pgmid = 'R3TR' OR i_object_type = 'HOTA'. "HOTA required for LANG HOTA
      lv_global_lock = 'X'.
    ENDIF.

    IF i_suppress_dialog = 'X'.
      lv_suppress_dialog = 'X'. "compare to rs_corr_check, here no dark mode supported...
    ENDIF.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = i_object_name    " Name des Objekts
        object_class        = i_object_type    " Obj.-Klasse (ABAP,SCUA,SCRP,DICT,FUNC,.)
        mode                = i_mode    " I(nsert), falls Objekt neu
        global_lock         = lv_global_lock    " SPACE: Kleine Sperre (LIMU); 'X': Gr. Sp. (R3TR)
        devclass            = i_devclass    " Entwicklungsklasse
        korrnum             = i_trkorr    " Korrektur- oder Auftragsnummer
*       use_korrnum_immediatedly = lv_suppress_dialog    " 'X'->Kein Dialog für die in KORRNUM mitgegebene Auftragsnumm
*       author              = SPACE
        master_language     = i_masterlang
        genflag             = i_genflag    " 'X'->generiertes Objekt
*       program             = SPACE    " Hauptcl_cprogramm (nur bei Klasse ABAP)
*       object_class_supports_ma = 'X'    " 'X'->Objektklasse unterstützt den Modifikationsassistenten
*       extend              = SPACE
        suppress_dialog     = lv_suppress_dialog    " alle Meldungen des Korrektursystems unterdrücken
*       mod_langu           = SPACE    " Modifikationssprache (nur für CLM_MODIFY_PROT_ONLY)
*       activation_call     = SPACE    " 'X'->Ruf aus der Aktivierung (--> CLM_MODIFY_PROT_ONLY unter
      IMPORTING
*       devclass            =     " Entwicklungsklasse
*       korrnum             =     " Korrektur- oder Auftragsnummer
        ordernum            = r_result    " Auftragsnummer
*       new_corr_entry      =     " 'X'->Objekt wurde neu in die Korrektur aufgenommen
*       author              =
*       transport_key       =     " Transportschlüssel
*       new_extend          =     " 'X'->Objektbearbeitung im Modifikationsmodus
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_wbo EXPORTING cts_hta_component = i_cts_hta_component.
    ENDIF.
  ENDMETHOD.