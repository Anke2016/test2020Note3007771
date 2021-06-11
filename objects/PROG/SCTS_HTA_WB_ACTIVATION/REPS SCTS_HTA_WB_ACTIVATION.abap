REPORT scts_hta_wb_activation.

DATA: lt_dwinactiv TYPE STANDARD TABLE OF dwinactiv,
      lo_checklist TYPE REF TO cl_wb_checklist,
      ls_message   TYPE symsg.

*START-OF-SELECTION.
  SELECT * FROM dwinactiv INTO TABLE lt_dwinactiv WHERE uname = sy-uname AND ( object  = 'HOTP' OR  object  = 'HOTO'). " OR  object  = 'HOTA' ).

  IF sy-subrc <> 0.
*   message 'No inactive HTA objects found for user sy-unam
      MESSAGE ID 'SCTS_HOT' TYPE 'I' NUMBER '589' WITH sy-uname. "  DISPLAY LIKE 'S'.
  ENDIF.

  CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
    IMPORTING
      p_checklist                = lo_checklist
    TABLES
      objects                    = lt_dwinactiv
    EXCEPTIONS
      object_not_in_working_area = 1
      execution_error            = 2
      cancelled                  = 3
      insert_into_corr_error     = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
**
**  lo_checklist->get_error_message( EXPORTING p_index = 1  IMPORTING  p_message = ls_message ).
**
**  MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
**          WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
**          DISPLAY LIKE 'S'.

** todo: direkt zum Aufrufenden Programm zurück?
*  LEAVE TO current TRANSACTION. ?