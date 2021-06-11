"! HTA exception class for errors with master language maintained in
"! SAP HANA repository
CLASS cx_cts_hta_unknown_master_lang DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      "! Master language not maintained in SAP HANA repository
      BEGIN OF no_master_language,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE', "HANA Format (char[0] till char[49])
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "'PACKAGE', "HANA Format (char[50] till char[99])
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "'PACKAGE', "HANA Format (char[100] till char[149])
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "'PACKAGE', "HANA Format (char[150] till char[199])
      END OF no_master_language .
    CONSTANTS:
      "! Master language maintained in SAP HANA repository not supported or not activated(inactive) in ABAP system
      BEGIN OF unsupported_master_lang,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "language in HANA
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "'PACKAGE', "HANA Format (char[0] till char[49])
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "'PACKAGE', "HANA Format (char[50] till char[99])
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "'PACKAGE', "HANA Format (char[100] till char[159])
      END OF unsupported_master_lang .

    METHODS constructor
      IMPORTING
        !textid        LIKE if_t100_message=>t100key
        !hana_package  TYPE string
        !hana_language TYPE string OPTIONAL .