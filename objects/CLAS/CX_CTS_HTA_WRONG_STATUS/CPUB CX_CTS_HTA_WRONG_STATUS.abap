"! HTA exception class for errors when an object(package) has a wrong status for some action.<br/>
"! For example: synchronization is not possible if object was only imported to HTA but not yet deployed to SAP HANA repository.
"! So it is newer in HTA than in SAP HANA and therefore must first be deployed before being synchronizing again<br/>
CLASS cx_cts_hta_wrong_status DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      "! Object was imported to HTA but not yet deployed.<br/>
      "! So the object is newer in HTA than in HANA and therefore must first be deployed before it can be synchronized.
      BEGIN OF object_requires_deployment,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "HANA object name chunk1
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "HANA object name chunk2
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "HANA object name chunk3
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "hot_status
      END OF object_requires_deployment,

      "! Package was imported to HTA but not yet deployed.<br/>
      "! So the package is newer in HTA than in HANA and therefore must first be deployed before it can be synchronized.
      BEGIN OF package_requires_deployment,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "HANA package name chunk1
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "HANA package name chunk2
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "HANA package name chunk3
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "hot_status
      END OF package_requires_deployment.

    METHODS constructor
      IMPORTING
        textid                 LIKE if_t100_message=>t100key
        name_of_obj_or_package TYPE string "object or package name
        hot_status             TYPE cts_hot_object_status
        previous               LIKE previous OPTIONAL
        cts_hta_component      TYPE REF TO if_cts_hta_component OPTIONAL.