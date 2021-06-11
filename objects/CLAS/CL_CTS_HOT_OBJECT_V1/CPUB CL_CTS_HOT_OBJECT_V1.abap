CLASS cl_cts_hot_object_v1 DEFINITION PUBLIC  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.
    CONSTANTS:
      co_object_name_suffix_delimitr TYPE string VALUE '.'.

    TYPES:
       ty_cl_cts_hot_object_list TYPE STANDARD TABLE OF REF TO cl_cts_hot_object_v1 WITH DEFAULT KEY.

    DATA:
      hana_package_id              TYPE cts_hot_hana_package_id READ-ONLY,
      hana_object_name             TYPE cts_hot_hana_object_name READ-ONLY,
      hana_object_suffix           TYPE cts_hot_hana_object_suffix READ-ONLY,
      abap_hana_package_id         TYPE cts_hot_object-abap_hana_package_id READ-ONLY,
      abap_hana_object_name_suffix TYPE cts_hot_object-abap_hana_object_name_suffix READ-ONLY,
      "! Full object name as it is used in transport requests (abap_hana_package_id + abap_hana_object_name_suffix)
      transport_object_name        TYPE cts_hot_object_name READ-ONLY.

    CLASS-METHODS:
      create_instance
        IMPORTING
          iv_hana_package_id    TYPE string
          iv_hana_object_name   TYPE string
          iv_hana_object_suffix TYPE string
        RETURNING
          VALUE(r_result)       TYPE REF TO cl_cts_hot_object_v1
        RAISING
          cx_hana_object_transport,

      create_instance2
        IMPORTING
          io_cts_hot_package    TYPE REF TO cl_cts_hot_package
          iv_hana_object_name   TYPE string
          iv_hana_object_suffix TYPE string
        RETURNING
          VALUE(r_result)       TYPE REF TO cl_cts_hot_object_v1
        RAISING
          cx_hana_object_transport,


      create_instance_from_objname
        IMPORTING
          iv_objname      TYPE cts_hot_object_name
          iv_abap_status  TYPE c DEFAULT 'A' "todo: type abap_status
        RETURNING
          VALUE(r_result) TYPE REF TO cl_cts_hot_object_v1.
