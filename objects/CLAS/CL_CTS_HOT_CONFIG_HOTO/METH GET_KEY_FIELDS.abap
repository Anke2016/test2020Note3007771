  METHOD get_key_fields.
** todo: still necessary? we are working without delta calculation for HOTO
**   during delta calculation only the two first fields 'ABAP_HANA_PACKAGE_ID' and 'ABAP_HANA_OBJECT_NAME_SUFFIX'
**   shall be considered as key fields; 'ABAP_STATUS' is one of the ignored fields
*    APPEND 'ABAP_HANA_PACKAGE_ID' TO rt_key_fields.
*    APPEND 'ABAP_HANA_OBJECT_NAME_SUFFIX' TO rt_key_fields.
  ENDMETHOD.