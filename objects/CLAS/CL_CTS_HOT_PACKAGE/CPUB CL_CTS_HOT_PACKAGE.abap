CLASS cl_cts_hot_package DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.
    CONSTANTS:
     co_hash_seperator TYPE c VALUE ';'.

    TYPES:
       ty_cl_cts_hot_package_list TYPE STANDARD TABLE OF REF TO cl_cts_hot_package WITH DEFAULT KEY.

    DATA:
      hana_package_id      TYPE cts_hot_hana_package_id READ-ONLY,
      "! Name as it is used in transport requests
      abap_hana_package_id TYPE cts_hot_package_id READ-ONLY.

    CLASS-METHODS:
      create_instance
        IMPORTING
          iv_hana_package_id TYPE string
        RETURNING
          VALUE(r_result)    TYPE REF TO cl_cts_hot_package
        RAISING
          cx_hana_object_transport,

      create_instance_from_objname
        IMPORTING
          iv_objname      TYPE sobj_name
          iv_abap_status  TYPE c DEFAULT 'A'
        RETURNING
          VALUE(r_result) TYPE REF TO cl_cts_hot_package.
