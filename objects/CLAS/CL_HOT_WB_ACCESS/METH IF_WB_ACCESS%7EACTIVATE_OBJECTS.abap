  METHOD if_wb_access~activate_objects.

    DATA: lt_packages               TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lt_successful_packages    TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lr_successfull_package    TYPE REF TO cl_cts_hot_package,
          lt_objects                TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lt_successful_objects     TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lr_successful_object      TYPE REF TO cl_cts_hot_object_v1,
          lt_log                    TYPE if_cts_hta_types=>ty_deploy_messages,  "if_cts_hot_logger=>ty_t_sprot_u,
          ls_object                 TYPE LINE OF sewor_objtab,
          lv_logname_file           TYPE trfile,
          lv_msgtype                TYPE sy-msgty,
          ls_symsg                  TYPE symsg,
          lv_log_dir_name           TYPE eps2path,
          lv_file_name              TYPE stpa-file,
          lv_severity_text          TYPE syst_msgv,
          lv_max_severity           TYPE sprot_u-severity,
          lv_max_severity_hdi       TYPE sprot_u-severity,
          lt_hdi_objects            TYPE cl_cts_hdi_object=>ty_t_hdi_objects,
          lr_hdi_object             TYPE REF TO cl_cts_hdi_object,
          lt_successful_hdi_objects TYPE cl_cts_hdi_object=>ty_t_hdi_objects,
          lr_successful_hdi_object  TYPE REF TO cl_cts_hdi_object,
          lt_log_2                  TYPE if_cts_hta_types=>ty_deploy_messages. "if_cts_hot_logger=>ty_t_sprot_u.

* TODO: HOTA kann vorkommen?
    LOOP AT p_objects INTO ls_object WHERE object = 'HOTP'.
      APPEND cl_cts_hot_package=>create_instance_from_objname( iv_objname = ls_object-obj_name(40) iv_abap_status = 'I' ) TO lt_packages.
    ENDLOOP.

    DATA(lr_db_access) = NEW cl_cts_hdi_object_db_access( ).
    LOOP AT p_objects INTO ls_object WHERE object = 'HOTO'.
      IF ls_object-obj_name CA '/'.
        TRY.
            lr_hdi_object = cl_cts_hdi_object=>create_from_trobj_name(
                iv_trobj_name           = ls_object-obj_name
                iv_abap_status          = if_cts_hdi_abap_types=>co_abap_status_inactive
                ir_hdi_object_db_access = lr_db_access
            ) .
          CATCH  cx_cts_hta_hdi_not_found .
            ASSERT lr_hdi_object->ms_cts_hdi_object IS NOT INITIAL. "If this fails, SNOTE/CWB did not create inactive entries in HTA
        ENDTRY.
        ASSERT lr_hdi_object->ms_cts_hdi_object IS NOT INITIAL. "If this fails, SNOTE/CWB did not create inactive entries in HTA
        INSERT lr_hdi_object INTO TABLE lt_hdi_objects.
      ELSE.
        APPEND cl_cts_hot_object_v1=>create_instance_from_objname( iv_objname = ls_object-obj_name(110) iv_abap_status = 'I' ) TO lt_objects.
      ENDIF.
    ENDLOOP.

    "HDI Deployment
    me->activate_hdi_objects(
      EXPORTING
        it_objects = lt_hdi_objects
      IMPORTING
        et_successful_objects = lt_successful_hdi_objects
        et_log = lt_log
        ev_max_severity = lv_max_severity_hdi
    ).

    IF lt_packages IS NOT INITIAL OR lt_objects IS NOT INITIAL.
      "HANA Repository Deployment
      PERFORM deploy IN PROGRAM rddhanadeployment USING lt_packages lt_objects 'I' CHANGING lt_successful_packages lt_successful_objects lt_log_2 lv_max_severity.

      APPEND LINES OF lt_log_2 TO lt_log.
    ENDIF.


* todo: Rainer fragen: Parameter von RS_DELETE_FROM_WORKING_AREA
    LOOP AT lt_successful_hdi_objects INTO lr_successful_hdi_object.
      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          "actualize_working_area = 'X'
          immediate = 'X'
          object    = 'HOTO'
          obj_name  = lr_successful_hdi_object->mv_transport_object_name.
    ENDLOOP.

* todo: Rainer fragen: Parameter von RS_DELETE_FROM_WORKING_AREA
    LOOP AT lt_successful_packages INTO lr_successfull_package.
      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          "actualize_working_area = 'X'
          immediate = 'X'
          object    = 'HOTP'
          obj_name  = CONV trobj_name( lr_successfull_package->abap_hana_package_id ).
    ENDLOOP.
* todo: Rainer fragen: Parameter von RS_DELETE_FROM_WORKING_AREA
    LOOP AT lt_successful_objects INTO lr_successful_object.
      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          "actualize_working_area = 'X'
          immediate = 'X'
          object    = 'HOTO'
          obj_name  = CONV trobj_name( lr_successful_object->transport_object_name ).
    ENDLOOP.

* always write activation protocol into file system (<TRANS_DIR_LOG>/HTA_WB_ACTIVATION_yyyymmdd.<SID>)
*   first read parameter DIR_TRANS_LOG
    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir       = '$TR_LOG' " stands for DIR_TRANS_LOG
      IMPORTING
        ev_long_dir_name = lv_log_dir_name.

    CONCATENATE 'HTA_WB_ACTIVATION_' sy-datum '.' sy-sysid INTO lv_file_name.

    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_file_name
                         ID 'PATH'     FIELD lv_log_dir_name
                         ID 'RESULT'   FIELD lv_logname_file.  " complete filename inclusive log directory

    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'FILE'          " Protokolltyp: 'FILE', 'DB', 'MEMORY'
        iv_logname_file   = lv_logname_file " Name des Protokolls (File)
        iv_condense       = ' '
      TABLES
        it_msgs           = lt_log          " Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    IF ( line_exists( lt_log[ severity = 'A' ] ) OR line_exists( lt_log[ severity = 'E' ] ) ).
    IF ( lv_max_severity = 'A' OR lv_max_severity = 'E'
         OR lv_max_severity_hdi = 'A' OR lv_max_severity_hdi = 'E' ).
      lv_msgtype       = 'E'.
      lv_severity_text = 'Fehler'(001).
*    ELSEIF ( line_exists( lt_log[ severity = 'W' ] ) ).
    ELSEIF lv_max_severity = 'W' OR lv_max_severity_hdi = 'W'.
      lv_msgtype        = 'W'.
      lv_severity_text = 'Warnungen'(002).
    ENDIF.

    IF p_no_dialog IS INITIAL  AND ( lv_msgtype = 'E' OR lv_msgtype = 'W' ).
*   in dialog

      MESSAGE ID 'SCTS_HOT' TYPE 'I' NUMBER '588'
              WITH lv_severity_text lv_log_dir_name lv_file_name DISPLAY LIKE lv_msgtype.  "TODO: wieder reinnehmen

      " delete memory log if still existing
      CALL FUNCTION 'TR_DELETE_LOG'
        EXPORTING
          iv_log_type       = 'MEMORY'
          iv_logname_memory = CONV trfilename( lv_file_name ) " Name des Protokolls (Memory)
        EXCEPTIONS
          invalid_input     = 1
          file_access_error = 2
          db_access_error   = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* show log
      "first write to memory: TODO: write to memory necessary? display directly possible?
      CALL FUNCTION 'TR_WRITE_LOG'
        EXPORTING
          iv_log_type       = 'MEMORY'
          iv_logname_memory = CONV trfilename( lv_file_name )
        TABLES
          it_msgs           = lt_log          " Tabelle mit den Meldungen
        EXCEPTIONS
          invalid_input     = 1
          file_access_error = 2
          db_access_error   = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      " then display log from memory
      DATA lv_titlebar TYPE syst_title.
      lv_titlebar = 'SAP HANA Repository Deployment (Ergebnisse)'(003).
      CALL FUNCTION 'TR_READ_AND_DISPLAY_LOG'
        EXPORTING
          iv_log_type            = 'MEMORY'
          iv_logname_memory      = CONV trfilename( lv_file_name )
          iv_titlebar            = lv_titlebar                       " Titelzeile
          iv_heading             = 'Deployment Log'(004)             " Überschrift
*         iv_top_line            = '0'                               " Positionierung
          iv_display_level       = '2'                               " Anzeigestufe (1 bis 9)
          iv_with_long_text_icon = 'X'                               " Mit Ikone 'Langtext'
*         iv_with_refresh_icon   = 'X'                               " Mit Ikone 'Auffrischen'
*         iv_with_level          = ' '                               " Mit techn. Anzeige der Meldungsstufe/Fehlerart
*         iv_language            = SYST-LANGU                        " Anzeige-Sprache
        EXCEPTIONS
          invalid_input          = 1
          access_error           = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      " finally delete log in memory
      CALL FUNCTION 'TR_DELETE_LOG'
        EXPORTING
          iv_log_type       = 'MEMORY'
          iv_logname_memory = CONV trfilename( lv_file_name ) " Name des Protokolls (Memory)
        EXCEPTIONS
          invalid_input     = 1
          file_access_error = 2
          db_access_error   = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

**** return error/warning
***      IF p_checklist IS NOT BOUND.
***        CREATE OBJECT p_checklist.
***      ENDIF.
***
***      ls_symsg-msgid = 'SCTS_HOT'.
***      ls_symsg-msgno =  '588'.
***      ls_symsg-msgty =  lv_msgtype. "I'. " todo wieder zurück 'lv_msgtype.
***      ls_symsg-msgv1 =  lv_severity_text.
***      ls_symsg-msgv2 =  lv_log_dir_name.
***      ls_symsg-msgv3 =  lv_file_name.
***
***      p_checklist->add_t100_error_message(
***         EXPORTING
***            p_message = ls_symsg
***            p_object_text = CONV string( 'Ergebnis der HTA-WB-Aktivierung'(005) )
***         EXCEPTIONS
***           OTHERS               = 1  ).
***      IF sy-subrc <> 0.
***        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
***             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
***      ENDIF.

    ENDIF. "if p_no_dialog IS INITIAL  AND ( lv_msgtype = 'E' OR lv_msgtype = 'W' ).


*===================================================


*      CALL FUNCTION 'TR_READ_AND_DISPLAY_LOG'
*        EXPORTING
*          iv_log_type            = 'FILE'                            " Protokolltyp: 'FILE', 'DB', 'MEMORY'
*          iv_logname_file        = lv_logname_file                   " Name des Protokolls (File)
*          iv_titlebar            = 'SAP HANA Repository Deployment'  " Titelzeile
*          iv_heading             = 'Deploymentlog'                   " Überschrift
**         iv_top_line            = '0'                               " Positionierung
*          iv_display_level       = '2'                               " Anzeigestufe (1 bis 9)
*          iv_with_long_text_icon = 'X'                               " Mit Ikone 'Langtext'
**         iv_with_refresh_icon   = 'X'                               " Mit Ikone 'Auffrischen'
**         iv_with_level          = ' '                               " Mit techn. Anzeige der Meldungsstufe/Fehlerart
**         iv_language            = SYST-LANGU                        " Anzeige-Sprache
*        EXCEPTIONS
*          invalid_input          = 1
*          access_error           = 2
*          OTHERS                 = 3.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.



*    CALL 'C_SAPGPARAM'  ID 'NAME'  FIELD 'DIR_TRANS'
*                        ID 'VALUE' FIELD lv_dir_trans.
*    IF lv_dir_trans = space.
** error handling "todo
*    ENDIF.
*
*
*    call EPS_GET_DIRECTORY_PATH
*    importing
*    EPS_SUBDIR = '$TR_LOG'
*
*    CONCATENATE lv_dir_trans '/log/HTA_WB_ACTIVATION_' sy-datum '.' sy-sysid INTO lv_logname_file.
*
*
*      lv_file_name           TYPE stpa-file.
*
** always write activation protocoll into file system
**   read profile parameter DIR_TRANS
**CALL 'C_SAPGPARAM'  ID 'NAME'  FIELD 'DIR_TRANS'
**                    ID 'VALUE' FIELD lv_dir_trans.
**IF lv_dir_trans = space.
***   error handling "todo
**ENDIF.



*      error_list->add_t100_error_message(
*        EXPORTING
**          p_detail_request     =     " Workbench Manager: Request
**          p_correction_request =     " Correction Request for Automatic Correction
**          p_tool_request_edit  =     " Navigation Request for Edit
*          p_tool_request_show  = show_request    " Navigation Request for Display
*          p_message            =  msg   " Structure of Message Variables
**          p_object_text        =     " Object Description
**          p_line               =     " Line With Errors in Object
**          p_free_arrangement   = SPACE    " Display messages one by one (no automatic grouping)
**          p_message_text_add   =     " Additional info
*      ).


*      "delete log if still existing
*      CALL FUNCTION 'TR_DELETE_LOG'
*        EXPORTING
*          iv_log_type       = 'MEMORY'    " Protokolltyp: 'FILE', 'DB', 'MEMORY'
**         iv_logname_file   =             " Name des Protokolls (File)
**         iv_logname_db     =             " Name des Protokolls (Datenbank)
*          iv_logname_memory = 'ACT_PROT'  " Name des Protokolls (Memory)
*        EXCEPTIONS
*          invalid_input     = 1
*          file_access_error = 2
*          db_access_error   = 3
*          OTHERS            = 4.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**      ENDIF.

    "save log
*      CALL FUNCTION 'TR_WRITE_LOG'
*        EXPORTING
*          iv_log_type       = 'MEMORY'    " Protokolltyp: 'FILE', 'DB', 'MEMORY'
**         iv_logname_file   =     " Name des Protokolls (File)
**         iv_logname_db     =     " Name des Protokolls (Datenbank)
*          iv_logname_memory = 'ACT_PROT'    " Name des Protokolls (Memory)
**         iv_append_mode    = ' '    " Memory-Protokoll: Anfügen der Meldungen
**         iv_condense       = 'X'    " Zeilen werden zusammengeschoben
*        TABLES
*          it_msgs           = lt_log    " Tabelle mit den Meldungen
*        EXCEPTIONS
*          invalid_input     = 1
*          file_access_error = 2
*          db_access_error   = 3
*          OTHERS            = 4.




*    IF lt_packages IS NOT INITIAL.
*      TRY.
*          lr_hot_hana_connector->modify_packages_in_hana(
*            EXPORTING
*              i_itab_packages          = lt_packages
*              i_abap_status            = 'I'
*            IMPORTING
*              e_created_packages       = lt_created_packages
*              e_updated_packages       = lt_updated_packages
*              e_deleted_packages       = lt_deleted_packages
*              e_skipped_packages       = lt_skipped_packages
*              e_failed_packages        = lt_failed_packages
*          ).
*        CATCH cx_hana_object_transport INTO DATA(hot_exc).
*          DATA(t100_key) = hot_exc->if_t100_message~t100key.
*          MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
*        CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
*          t100_key = nhi_exc->if_t100_message~t100key.
*          MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4.
*      ENDTRY.
*
*      APPEND LINES OF lt_created_packages TO lt_ok_packages.
*      APPEND LINES OF lt_updated_packages TO lt_ok_packages.
*      APPEND LINES OF lt_deleted_packages TO lt_ok_packages.
**    APPEND LINES OF lt_skipped_packages TO lt_ok_packages. "todo?
*      DATA: lv_objname TYPE trobj_name.
*
*      LOOP AT lt_ok_packages INTO ls_ok_package.
*        lv_objname = ls_ok_package-cts_hot_package->abap_hana_package_id.
*        CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
*          EXPORTING
*            "actualize_working_area = 'X'
*            immediate = 'X'
*            object    = 'HOTP'
*            obj_name  = lv_objname.
*      ENDLOOP.
*
*      MESSAGE |created/updated/deleted/skipped/failed packages: { lines( lt_created_packages ) }/{ lines( lt_updated_packages ) }/{ lines( lt_deleted_packages ) }/{ lines( lt_skipped_packages ) }/{ lines( lt_failed_packages ) }| TYPE 'I'.
*    ENDIF.
*
*
*
*
*    IF lt_objects IS NOT INITIAL.
*      TRY.
*          lr_hot_hana_connector->deploy_objects_to_hana(
*            EXPORTING
*              i_objects                = lt_objects
*              i_abap_status            = 'I'
*            IMPORTING
*              e_skipped_objects        = DATA(lt_skipped_objects)
*              e_successfull_objects    = DATA(lt_successfull_objects)
*              e_failed_objects         = DATA(lt_failed_objects)
**            e_deploy_result          =
*          ).
*        CATCH cx_hana_object_transport INTO hot_exc.
*          t100_key = hot_exc->if_t100_message~t100key.
*          MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
*        CATCH cx_nhi_hana_repository INTO nhi_exc.
*          t100_key = nhi_exc->if_t100_message~t100key.
*          MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4.
*      ENDTRY.
*
*      MESSAGE |successfull/skipped/failed objects: { lines( lt_successfull_objects ) }/0/{ lines( lt_skipped_objects ) }/{ lines( lt_failed_objects ) }| TYPE 'I'.
*
*    ENDIF.
*    DATA:           ls_ok_object LIKE LINE OF lt_successfull_objects.
*
*    LOOP AT lt_successfull_objects INTO ls_ok_object.
*      lv_objname = ls_ok_object->abap_hana_package_id.
*      lv_objname+40(70) = ls_ok_object->abap_hana_object_name_suffix .
*      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
*        EXPORTING
*          "actualize_working_area = 'X'
*          immediate = 'X'
*          object    = 'HOTO'
*          obj_name  = lv_objname.
*    ENDLOOP.
*
*    IF lt_failed_objects IS NOT INITIAL.
*      DATA: ls_symsg TYPE symsg.
*      IF p_checklist IS NOT BOUND.
*        CREATE OBJECT p_checklist.
*      ENDIF.
*
*      ls_symsg-msgid = 'SCTS_HOT'.
*      ls_symsg-msgno =  '515'.
*      ls_symsg-msgty =  'I'.
*
*      p_checklist->add_t100_error_message( p_message = ls_symsg ).
**      p_checklist->add_t100_error_message( p_message = ls_symsg ).
**      p_checklist->add_t100_error_message( p_message = ls_symsg ).
**      p_checklist->add_t100_error_message( p_message = ls_symsg ).
**      p_checklist->add_t100_error_message( p_message = ls_symsg ).
*
*    ENDIF.
*
**raise EXCEPTION type CX_SWB_EXCEPTION message i515(scts_hot).
*
*
**
**FORM activate_hotx_objects USING it_dwinactiv TYPE dwinactiv_tab.
**  LOOP AT it_dwinactiv TRANSPORTING NO FIELDS WHERE object = 'HOTP' OR object = 'HOTO' OR object = 'HOTA'.
**    EXIT. " at least one inactive HOTP or HOTO object has been found
**  ENDLOOP.
**
**  IF sy-subrc = 0. " at least one inactive HOTP or HOTO object has to be activated
***   Pass over inactive HOTx object to scts_hta_activate_hotx_objects before executing (submitting) it.
**    DATA lv_exists TYPE c.
**    CALL FUNCTION 'DEV_CHECK_OBJECT_EXISTS'
**      EXPORTING
**        i_pgmid   = 'R3TR'
**        i_objtype = 'PROG'
**        i_objname = 'SCTS_HTA_ACTIVATE_HOTX_OBJECTS'
**      IMPORTING
**        e_exists  = lv_exists.
**    IF lv_exists = 'X'.
**      EXPORT pt_dwinactiv FROM it_dwinactiv TO MEMORY ID 'HTA'.
**      SUBMIT scts_hta_activate_hotx_objects AND RETURN. " WITH code = lv_tcode.
**    ENDIF.
**  ENDIF.
**
**  RETURN.
**ENDFORM.
*
**FORM activate_hotx_objects USING it_dwinactiv TYPE dwinactiv_tab.
**  LOOP AT it_dwinactiv TRANSPORTING NO FIELDS WHERE object = 'HOTP' OR object = 'HOTO' OR object = 'HOTA'.
**    EXIT. " at least one inactive HOTP or HOTO object has been found
**  ENDLOOP.
**
**  IF sy-subrc = 0. " at least one inactive HOTP or HOTO object has to be activated
***   Pass over inactive HOTx object to scts_hta_activate_hotx_objects before executing (submitting) it.
**    DATA lv_exists TYPE c.
**    CALL FUNCTION 'DEV_CHECK_OBJECT_EXISTS'
**      EXPORTING
**        i_pgmid   = 'R3TR'
**        i_objtype = 'PROG'
**        i_objname = 'SCTS_HTA_ACTIVATE_HOTX_OBJECTS'
**      IMPORTING
**        e_exists  = lv_exists.
**    IF lv_exists = 'X'.
**      EXPORT pt_dwinactiv FROM it_dwinactiv TO MEMORY ID 'HTA'.
**      SUBMIT scts_hta_activate_hotx_objects AND RETURN. " WITH code = lv_tcode.
**    ENDIF.
**  ENDIF.
**
**  RETURN.
**ENDFORM.




  ENDMETHOD.