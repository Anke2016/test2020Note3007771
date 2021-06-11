  METHOD get_key_fields.
*   during delta calculation only first field 'ABAP_HANA_PACKAGE_ID' shall be
*   considered as key field; 'ABAP_STATUS' is one of the ignored fields
    APPEND 'ABAP_HANA_PACKAGE_ID' TO rt_key_fields.
  ENDMETHOD.