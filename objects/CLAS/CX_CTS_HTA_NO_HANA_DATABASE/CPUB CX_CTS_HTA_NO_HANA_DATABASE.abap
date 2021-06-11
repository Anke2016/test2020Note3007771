"! HTA exception class thrown if system is not running on HANA
CLASS cx_cts_hta_no_hana_database DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL.