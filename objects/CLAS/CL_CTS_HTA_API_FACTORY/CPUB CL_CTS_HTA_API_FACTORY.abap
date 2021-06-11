CLASS cl_cts_hta_api_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_api_factory.

    CLASS-METHODS create_instance
      RETURNING
        VALUE(r_result) TYPE REF TO if_cts_hta_api_factory.
