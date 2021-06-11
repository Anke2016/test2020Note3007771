"! HTA exception class hashing errors
CLASS cx_cts_hta_hashing DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF create_package_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_package_hash_error .
    CONSTANTS:
      BEGIN OF create_object_name_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'OBJECT NAME', "HANA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "'OBJECT SUFFIX', "HANA Format
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_object_name_hash_error.
    CONSTANTS:
      BEGIN OF create_text_ref_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "text reference string
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_text_ref_hash_error.
    CONSTANTS:
      BEGIN OF create_object_ref_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "object reference string
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_object_ref_hash_error.

    METHODS constructor
      IMPORTING
        textid             LIKE if_t100_message=>t100key OPTIONAL
        previous           LIKE previous OPTIONAL
        message_variable_1 TYPE symsgv OPTIONAL
        message_variable_2 TYPE symsgv OPTIONAL
        message_variable_3 TYPE symsgv OPTIONAL
        message_variable_4 TYPE symsgv OPTIONAL.