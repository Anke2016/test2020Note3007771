FUNCTION SCTS_HTA_DEPLOY.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IV_TARCLIENT) TYPE  MANDT
*"     REFERENCE(IV_IS_UPGRADE) TYPE  TRPARI-W_UPGRADE
*"  TABLES
*"      TT_E071 TYPE  TR_OBJECTS
*"      TT_E071K TYPE  TR_KEYS
*"      TT_ERROR_LIST TYPE  E071_T OPTIONAL
*"----------------------------------------------------------------------
*   This function module is called by switch framework (coding) if business function set is switched on
    lcl_hta_deployment=>create_instance( i_transport_objects = tt_e071[] )->execute_deployment( IMPORTING error_object_list = tt_error_list[] ).

ENDFUNCTION.