*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS cx_cts_hot_invalid_input IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    IF textid IS INITIAL.
      "##TODO check text id
      me->textid = 123.
    ENDIF.
    me->value = value.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_timestamp_provider DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_timestamp_provider.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_timestamp_provider IMPLEMENTATION.

  METHOD lif_timestamp_provider~get_timestamp.
    GET TIME STAMP FIELD r_timestamp.
  ENDMETHOD.

ENDCLASS.