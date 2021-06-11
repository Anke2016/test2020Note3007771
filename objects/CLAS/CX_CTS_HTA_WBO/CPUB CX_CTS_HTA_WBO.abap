"! Exception class for all WBO related exceptions from external calls like rs_corr_check or rs_corr_insert
CLASS cx_cts_hta_wbo DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Creates message exception with values of current sy field
    METHODS constructor
      IMPORTING
        cts_hta_component TYPE REF TO if_cts_hta_component.