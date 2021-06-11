FUNCTION SCTS_HTA_HANA_MIGRATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_HANA_PACKAGE_NAME) TYPE
*"        IF_CTS_HTA_TYPES=>TY_HANA_PACKAGE_NAME
*"     VALUE(I_DISPLAY_AND_SAVE_LOG) TYPE  BOOLE DEFAULT ''
*"  EXPORTING
*"     REFERENCE(E_MESSAGES) TYPE  SNHI_DU_MESSAGES
*"     REFERENCE(E_GLOBAL_MIGRATION_STATUS) TYPE  SYMSGTY
*"----------------------------------------------------------------------

  DATA: lr_hta_api                     TYPE REF TO  if_cts_hta_api_factory,
        lr_hta_f_full_packs_by_hananam TYPE REF TO  if_cts_hta_component_list,
        lt_log_messages                TYPE         if_cts_hta_types=>ty_deploy_messages,
        ls_log_message                 LIKE LINE OF lt_log_messages,
        ls_message                     LIKE LINE OF e_messages.

* TODO: Authority check for HTA migration
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'NHDU' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    MESSAGE e024(snhi_delivery_unit). " todo: adapt message to HTA
    RETURN.
  ENDIF.

* Get instance of HTA API
  lr_hta_api = cl_cts_hta_api_factory=>create_instance( ).

  TRY.
      lr_hta_f_full_packs_by_hananam = lr_hta_api->find_full_packages_by_hana_nam( EXPORTING i_hana_package_name = i_hana_package_name ). "test with 'a_test_package*'
    CATCH cx_cts_hta_no_hana_database  cx_cts_hta INTO DATA(oref). "todo cx_cts_hta_hashing
      e_global_migration_status = 'E'.
      ls_message-msgid = oref->if_t100_message~t100key-msgid.
      ls_message-msgty = 'E'.
      ls_message-msgno = oref->if_t100_message~t100key-msgno.
      ls_message-msgv1 = oref->if_t100_message~t100key-attr1.
      ls_message-msgv2 = oref->if_t100_message~t100key-attr2.
      ls_message-msgv3 = oref->if_t100_message~t100key-attr3.
      ls_message-msgv4 = oref->if_t100_message~t100key-attr4.
      INSERT ls_message INTO TABLE e_messages.
      RETURN.
  ENDTRY.

  lr_hta_f_full_packs_by_hananam->deploy(
        EXPORTING i_force                 = ' ' " no force modus
        IMPORTING e_overall_deploy_status = e_global_migration_status
                  e_deploy_messages       = lt_log_messages ).

  LOOP AT lt_log_messages INTO ls_log_message.
    IF ls_log_message-severity = space.
      ls_message-msgty = 'I'.
    ELSE.
      ls_message-msgty = ls_log_message-severity.
    ENDIF.
    ls_message-msgid = ls_log_message-ag.
    ls_message-msgno = ls_log_message-msgnr.

    ls_message-msgv1 = ls_log_message-var1.
    ls_message-msgv2 = ls_log_message-var2.
    ls_message-msgv3 = ls_log_message-var3.
    ls_message-msgv4 = ls_log_message-var4.
    APPEND ls_message TO e_messages.
  ENDLOOP.

  IF i_display_and_save_log = 'X'.
    PERFORM display_and_save_log TABLES lt_log_messages.
  ENDIF.

ENDFUNCTION.

FORM display_and_save_log TABLES t_log_messages TYPE if_cts_hta_types=>ty_deploy_messages.

  DATA:  lv_logname_file TYPE trfile,
         lv_log_dir_name TYPE eps2path,
         lv_file_name    TYPE stpa-file.

* 1) Save log first
*    Write deployment log into file system (<TRANS_DIR_LOG>/SCTS_HTA_HANA_MIGRATION_yyyymmdd.<SID>)
*    First read parameter DIR_TRANS_LOG
  CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
    EXPORTING
      eps_subdir       = '$TR_LOG' " stands for DIR_TRANS_LOG
    IMPORTING
      ev_long_dir_name = lv_log_dir_name.

  lv_file_name = |SCTS_HTA_HANA_MIGRATION_{ sy-datum }.{ sy-sysid }|.

  CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_file_name
                       ID 'PATH'     FIELD lv_log_dir_name
                       ID 'RESULT'   FIELD lv_logname_file.  " complete filename inclusive log directory

  CALL FUNCTION 'TR_WRITE_LOG'
    EXPORTING
      iv_log_type       = 'FILE'          " Protokolltyp: 'FILE', 'DB', 'MEMORY'
      iv_logname_file   = lv_logname_file " Name des Protokolls (File)
    TABLES
      it_msgs           = t_log_messages " Tabelle mit den Meldungen
    EXCEPTIONS
      invalid_input     = 1
      file_access_error = 2
      db_access_error   = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* 2) Display log
*    First write log to memory
  CALL FUNCTION 'TR_WRITE_LOG'
    EXPORTING
      iv_log_type       = 'MEMORY'
      iv_logname_memory = CONV trfilename( lv_file_name )
    TABLES
      it_msgs           = t_log_messages          " Tabelle mit den Meldungen
    EXCEPTIONS
      invalid_input     = 1
      file_access_error = 2
      db_access_error   = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   then display log from memory
  CALL FUNCTION 'TR_READ_AND_DISPLAY_LOG'
    EXPORTING
      iv_log_type            = 'MEMORY'
      iv_logname_memory      = CONV trfilename( lv_file_name )
      iv_titlebar            = 'SCTS_HTA_HANA_MIGRATION'  " Titelzeile
      """" iv_heading             = CONV logline( |Deployment Log for demo '{ i_demo_name }'| )  " Überschrift
      iv_display_level       = '2'                               " Anzeigestufe (1 bis 9)
      iv_with_long_text_icon = 'X'                               " Mit Ikone 'Langtext'
    EXCEPTIONS
      invalid_input          = 1
      access_error           = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   finally delete log in memory
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
ENDFORM.