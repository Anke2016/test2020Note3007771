CLASS zcl_hello_x20 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
     types: begin of ty_data,
            key type string,
            value type string,
            end of ty_data.

            class-methods execute
            returning value(data) type ty_data.
