*&---------------------------------------------------------------------*
*& Report scts_hta_delete_inactive
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT scts_hta_delete_inactive.

SELECTION-SCREEN COMMENT /1(83) comment1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_object TYPE trobj_name.

INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '06' ID 'OBJTYPE' FIELD 'HOTA' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    MESSAGE 'You are not authorized to delete objects of type HOTA'(001) TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  comment1 = 'Read SAP Note 2633397 before using this program'(006).

START-OF-SELECTION.
  DATA:
    lv_success_message TYPE string.

  lv_success_message = 'Inactive entry of LIMU &1 &2 deleted successfully'(002).

  IF p_object IS INITIAL.
    MESSAGE 'Enter a transport object name'(005) TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA(lr_db_access) = CAST if_cts_hot_db_access( NEW cl_cts_hot_db_access( ) ).
  IF strlen( p_object ) <= 40.
    DATA(ls_package) = lr_db_access->read_cts_hot_package( i_abap_hana_package_id = p_object(40)
                                                           i_abap_status = 'I' ).

    IF ls_package IS NOT INITIAL.
      lr_db_access->delete_cts_hot_package( i_abap_hana_package_id = p_object(40)
                                            i_abap_status = 'I' ).

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          "actualize_working_area = 'X'
          immediate = 'X'
          object    = 'HOTP'
          obj_name  = p_object.

      REPLACE '&1' IN lv_success_message WITH 'HOTP'.
      REPLACE '&2' IN lv_success_message WITH p_object.
      MESSAGE lv_success_message TYPE 'S'.
    ENDIF.
  ELSE.
    DATA(ls_object) = lr_db_access->read_cts_hot_object_wo_bcdata( i_abap_hana_package_id         = p_object(40)
                                                                   i_abap_hana_object_name_suffix = p_object+40(70)
                                                                   i_abap_status                  = 'I' ).

    IF ls_object IS NOT INITIAL.
      lr_db_access->delete_cts_hot_object( i_abap_hana_package_id         = p_object(40)
                                           i_abap_hana_object_name_suffix = p_object+40(70)
                                           i_abap_status                  = 'I' ).

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          "actualize_working_area = 'X'
          immediate = 'X'
          object    = 'HOTO'
          obj_name  = p_object.

      REPLACE '&1' IN lv_success_message WITH 'HOTO'.
      REPLACE '&2' IN lv_success_message WITH p_object.
      MESSAGE lv_success_message TYPE 'S'.
    ENDIF.
  ENDIF.

  IF ls_package IS INITIAL AND ls_object IS INITIAL.
    DATA lv_message TYPE string.
    lv_message = 'No inactive object found in HTA for &1'(003).
    REPLACE '&1' IN lv_message WITH p_object.
    MESSAGE lv_message TYPE 'S'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_object.
  TYPES:
    BEGIN OF ty_s_object,
      object TYPE trobj_name,
    END OF ty_s_object.

  DATA:
    lt_hotps      TYPE STANDARD TABLE OF ty_s_object,
    lt_hotos      TYPE STANDARD TABLE OF ty_s_object,
    lt_object_in  TYPE STANDARD TABLE OF ty_s_object WITH KEY object,
    lt_object_out TYPE STANDARD TABLE OF ddshretval WITH EMPTY KEY.

  CALL FUNCTION 'RS_OBJECTS_OF_OBJECTTYPE'
    EXPORTING
      object     = 'HOTP'
    IMPORTING
      objectlist = lt_hotps.

  CALL FUNCTION 'RS_OBJECTS_OF_OBJECTTYPE'
    EXPORTING
      object     = 'HOTO'
    IMPORTING
      objectlist = lt_hotos.

  lt_object_in = lt_hotps.
  APPEND LINES OF lt_hotos TO lt_object_in.

  SORT lt_object_in ASCENDING BY object.

  "show f4 help and get user selected value
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT'
      value_org       = 'S' " Structure
      window_title    = 'Inactive HANA repository packages/objects'(004)
    TABLES
      value_tab       = lt_object_in " F4 help values
      return_tab      = lt_object_out " F4 selected values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ELSE.
    IF lt_object_out IS NOT INITIAL.
      p_object = lt_object_out[ 1 ]-fieldval.
    ENDIF.
  ENDIF.