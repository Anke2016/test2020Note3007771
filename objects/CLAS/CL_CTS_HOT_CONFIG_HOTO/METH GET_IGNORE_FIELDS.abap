  METHOD get_ignore_fields.
*    CLEAR rt_ignore_fields.
*
*    DATA ls_ignore_field TYPE svrs_tlogo_ifields.
*
*    ls_ignore_field-tabname   = 'CTS_HOT_OBJECT'.
*
*    ls_ignore_field-fieldname = 'ABAP_STATUS'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HOT_STATUS'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_PACKAGE_ID'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_OBJECT_NAME'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_OBJECT_SUFFIX'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_SYNC_SYSTEM'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_READ_SYSTEM'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_SOURCE_OBJECT_VERSION'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_OBJECT_VERSION'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_SOURCE_BUILD_VERSION'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_ACTIVATED_AT'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'HANA_ACTIVATED_BY'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_SYNCED_AT'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_SYNCED_BY'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_DEPLOYED_AT'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_DEPLOYED_BY'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_IMPORT_TIMESTAMP'.
*    APPEND ls_ignore_field TO rt_ignore_fields.
*
*    ls_ignore_field-fieldname = 'ABAP_OBJECT_REFERENCE'.
*    APPEND ls_ignore_field TO rt_ignore_fields.

  ENDMETHOD.