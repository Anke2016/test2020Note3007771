*******************************************************************
*   THIS FILE IS GENERATED BY THE FUNCTION LIBRARY.               *
*   NEVER CHANGE IT MANUALLY, PLEASE!                             *
*******************************************************************
FUNCTION $$UNIT$$ SCTS_HTA_HANA_MIGRATION

    IMPORTING
       REFERENCE(I_HANA_PACKAGE_NAME) TYPE
         !IF_CTS_HTA_TYPES=>TY_HANA_PACKAGE_NAME
       VALUE(I_DISPLAY_AND_SAVE_LOG) TYPE !BOOLE DEFAULT ''
    EXPORTING
       REFERENCE(E_MESSAGES) TYPE !SNHI_DU_MESSAGES
       REFERENCE(E_GLOBAL_MIGRATION_STATUS) TYPE !SYMSGTY .