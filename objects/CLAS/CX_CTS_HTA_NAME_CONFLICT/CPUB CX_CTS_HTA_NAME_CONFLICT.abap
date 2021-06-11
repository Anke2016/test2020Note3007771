"! HTA exception class for name conflict errors. This exception is thrown if the name of an object or package has different case in HANA and HTA .<br/>
"! For details, whether conflict is reported for an object or package, check the documentation of the constants within this class.<br/>
CLASS cx_cts_hta_name_conflict DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      "! The object name and/or suffix already exists in HTA in a different case.<br/>
      BEGIN OF object_name_conflict,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "Object name conflicting chunk1
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "Object name conflicting chunk2
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "Object name HTA chunk1
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "Object name HTA chunk2
      END OF object_name_conflict,

      "! The package name already exists in HTA in a different case.<br/>
      BEGIN OF package_name_conflict,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "Package name conflicting chunk1
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "Package name conflicting chunk2
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "Package name HTA chunk1
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "Package name HTA chunk2
      END OF package_name_conflict .

    METHODS constructor
      IMPORTING
        textid                      LIKE if_t100_message=>t100key
        previous                    LIKE previous OPTIONAL
        name_of_obj_or_package_conf TYPE string "object or package name conflicting.
        name_of_obj_or_package_hta  TYPE string "object or package name in HTA DB.
        cts_hta_component           TYPE REF TO if_cts_hta_component.