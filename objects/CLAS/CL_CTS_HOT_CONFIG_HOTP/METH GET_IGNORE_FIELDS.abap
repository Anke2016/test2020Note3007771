  METHOD get_ignore_fields.
    CLEAR rt_ignore_fields.

    DATA ls_ignore_field TYPE svrs_tlogo_ifields.

    ls_ignore_field-tabname   = 'CTS_HOT_PACKAGE'.

    ls_ignore_field-fieldname = 'ABAP_HANA_PACKAGE_ID'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_STATUS'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'HOT_STATUS'.
    APPEND ls_ignore_field TO rt_ignore_fields.

*   ls_ignore_field-fieldname = HANA_PACKAGE_ID

    ls_ignore_field-fieldname = 'HANA_READ_SYSTEM'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_SYNC_SYSTEM'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_SYNCED_AT'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_SYNCED_BY'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_DEPLOYED_AT'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_DEPLOYED_BY'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'HANA_PACK_SRC_SYSTEM'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'HANA_PACK_SRC_TENANT'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'HANA_PACK_RESPONSIBLE'.
    APPEND ls_ignore_field TO rt_ignore_fields.

*   ls_ignore_field-fieldname = HANA_PACK_ORIG_LANG

*   ls_ignore_field-fieldname = HANA_PACK_IS_STRUCTURAL

*   ls_ignore_field-fieldname = HANA_PACK_DELIVERY_UNIT

*   ls_ignore_field-fieldname = HANA_PACK_DU_VENDOR

*   ls_ignore_field-fieldname = HANA_PACK_TEXT_COLLECTION

*   ls_ignore_field-fieldname = HANA_PACK_TEXT_STATUS

*   ls_ignore_field-fieldname = HANA_PACK_TEXT_TERM_DOMAIN

*   ls_ignore_field-fieldname = HANA_PACK_HINTS_FOR_TRANSL

    ls_ignore_field-fieldname = 'ABAP_DEPLOYED_BY'.
    APPEND ls_ignore_field TO rt_ignore_fields.

    ls_ignore_field-fieldname = 'ABAP_IMPORT_TIMESTAMP'.
    APPEND ls_ignore_field TO rt_ignore_fields.

*    ls_ignore_field-fieldname = 'HOT_ACTIVATION_MODE'.
*    APPEND ls_ignore_field TO rt_ignore_fields.

  ENDMETHOD.