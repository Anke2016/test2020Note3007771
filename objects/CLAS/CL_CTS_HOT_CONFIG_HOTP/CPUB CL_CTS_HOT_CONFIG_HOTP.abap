CLASS cl_cts_hot_config_hotp DEFINITION
  PUBLIC
  INHERITING FROM cl_svrs_config_tlogo
  CREATE PUBLIC .

  PUBLIC SECTION.

* Not redifened
* - AUTH_CHECK_FOR_DISPLAY noch nicht
* - BEFORE_EXPORT
* - CHECK_OBJECT ==> Redefinieren für CWB
* - CONSTRUCTOR
* - CREATE
* - DEQUEUE_OBJECT
* - DISPLAY_OBJECT
* - DISPLAY_OBJECT_DIFF
* - ENQUEUE_OBJECT
* - GET_DDIC_TYPE
* - GET_KEY_FIELDS
* - GET_OBJECT_DEFINITION_INACTIVE
* - GET_OBJECTS_TO_BE_LOCKED
* - GET_SORT_FIELD
* - GET_SUBOBJECT_MAPPING
* - GET_SUBOBJ_LOGICAL_VIEW
* - GET_SVRS_KEY_FIELDS_NUMBER
* - GET_TITLE
* - IS_PACKED
* - PACK_OBJECT
* - READ_OBJECT_FROM_VERS
* - SHOW_DATA
* - UNPACK_OBJECT
    METHODS active_to_inactive
        REDEFINITION .
    METHODS adjust_admin_data
        REDEFINITION .
    METHODS after_import
        REDEFINITION .
    METHODS auth_check_for_modify
        REDEFINITION .
** TODO: for CWB support: eventually redefine this method
*  METHODS CHECK_OBJECT
*    REDEFINITION.
*   TODO: implementation
    METHODS dequeue_object
        REDEFINITION .
    METHODS enqueue_object
        REDEFINITION .
    METHODS get_ignore_fields
        REDEFINITION .
    METHODS get_key_fields
        REDEFINITION .
***    METHODS get_objects_to_be_locked
**
***  METHOD get_objects_to_be_locked.
****   TODO????????
****   who calls this?
***    " This method method is called in case the current object is modified
***    " and hence locked in the corresponding database.
***
***    " default case:  the current object should be locked
***    "                Sometimes further objects have to be added,
***    "                e.g. generated objects have to be considered and
***    "                     returned
***
****    ASSERT ID svrs_generic CONDITION ir_tlogo_log IS BOUND.
****
****    DATA ls_object TYPE svrs_s_missingobjects.
****
****    CLEAR rt_objects.
****
***** Always return the current object as default to be locked
****    ls_object-pgmid    = 'LIMU'.
****    ls_object-object   = ir_tlogo_log->av_objtype.
****    ls_object-obj_name = ir_tlogo_log->av_objname.
****    APPEND ls_object TO rt_objects.
****
****    ASSERT ID svrs_generic CONDITION rt_objects IS NOT INITIAL.
***    IF 1 = 1.
***    ENDIF.
***  ENDMETHOD.
***
***        REDEFINITION .
    METHODS get_object_definition
        REDEFINITION .
    METHODS get_object_header_from_repo
        REDEFINITION .
    METHODS get_subobjects
        REDEFINITION .
    METHODS get_title
        REDEFINITION .
    METHODS get_type_of_subobject
        REDEFINITION .
    METHODS has_inactive_version
        REDEFINITION .
    METHODS is_deleted_at_activation
        REDEFINITION .
    METHODS show_data
        REDEFINITION .