"! Base exception class for HANA Transport for ABAP<br/>
"! In some error cases the exception contains the cts_hta_component for which the exception was raised
"! (e.g. if_cts_hta_packge or if_cts_hta_object or if_cts_hta_full_package).
CLASS cx_cts_hta DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_exception
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! HTA component for which the exception was thrown
    DATA cts_hta_component TYPE REF TO if_cts_hta_component READ-ONLY .

*    CONSTANTS:
*      BEGIN OF cx_nhi_hana_repository_error,
*        msgid TYPE symsgid VALUE 'SCTS_HOT',
*        msgno TYPE symsgno VALUE '000',
*        attr1 TYPE scx_attrname VALUE 'msgv1',
*        attr2 TYPE scx_attrname VALUE '',
*        attr3 TYPE scx_attrname VALUE '',
*        attr4 TYPE scx_attrname VALUE '',
*      END OF cx_nhi_hana_repository_error .
    "! Raises exception cx_cts_hta filled with message from sy field.
    CLASS-METHODS raise_message_exception
      RAISING
        cx_cts_hta .
    METHODS constructor
      IMPORTING
        !textid             LIKE if_t100_message=>t100key OPTIONAL
        !previous           LIKE previous OPTIONAL
        !message_variable_1 TYPE symsgv OPTIONAL
        !message_variable_2 TYPE symsgv OPTIONAL
        !message_variable_3 TYPE symsgv OPTIONAL
        !message_variable_4 TYPE symsgv OPTIONAL
        !cts_hta_component  TYPE REF TO if_cts_hta_component OPTIONAL .
    "! Set the cts_hta_component to the passed i_hta_full_package.<br/>
    "! This should be used only to replace the cts_hta_component from object/package to full_package
    "! if object/package with current exception is processed as part of full package and thus API user
    "! should be able to e.g. remove the full package from a list.<br>
    "! Attribute cts_hta_component will only be set to i_hta_full_package if cts_hta_component is bound and
    "! belongs to same package.
    METHODS set_full_package_as_component
      IMPORTING
        !i_hta_full_package TYPE REF TO if_cts_hta_full_package .