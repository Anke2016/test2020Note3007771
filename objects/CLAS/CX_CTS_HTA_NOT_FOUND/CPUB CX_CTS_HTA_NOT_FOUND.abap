"! Exception if package/object could not be found in HTA, e.g. while creating instances via transport object name.
CLASS cx_cts_hta_not_found DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF object_not_found_in_hta,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'ABAP_HANA_PACKAGE_ID' "HTA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "ABAP_HANA_OBJNAME.SUFFIX' "HTA Format
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF object_not_found_in_hta.
    CONSTANTS:
      BEGIN OF package_not_found_in_hta,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'ABAP_HANA_PACKAGE_ID, "HTA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF package_not_found_in_hta.

    METHODS constructor
      IMPORTING
        textid             LIKE if_t100_message=>t100key OPTIONAL
        previous           LIKE previous OPTIONAL
        message_variable_1 TYPE symsgv OPTIONAL
        message_variable_2 TYPE symsgv OPTIONAL
        message_variable_3 TYPE symsgv OPTIONAL
        message_variable_4 TYPE symsgv OPTIONAL.