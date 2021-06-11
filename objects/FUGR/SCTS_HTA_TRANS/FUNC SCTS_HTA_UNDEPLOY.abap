FUNCTION SCTS_HTA_UNDEPLOY.
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
*   This function module is called by switch framework (coding) if business function set is switched off
*   - currently nothing is done


ENDFUNCTION.