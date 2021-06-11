*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ty_map_hana_2_hot_2_nhi_obj,
    package_id    TYPE string,
    object_name   TYPE string,
    object_suffix TYPE string,
    hot_object    TYPE REF TO cl_cts_hot_object_v1,
    nhi_object    TYPE REF TO cl_nhi_object_id,
  END OF ty_map_hana_2_hot_2_nhi_obj,
  ty_map_hana_2_hot_2_nhi_objs TYPE HASHED TABLE OF ty_map_hana_2_hot_2_nhi_obj WITH UNIQUE KEY package_id object_name object_suffix.

TYPES:
  BEGIN OF ty_hot_obj_status_version,
    object                     TYPE REF TO cl_cts_hot_object_v1,
    abap_status                TYPE r3state,
    hot_status                 TYPE cts_hot_object_status,
    hana_source_object_version TYPE cts_hot_hana_src_obj_version,
  END OF ty_hot_obj_status_version,
  ty_hot_obj_status_versions TYPE HASHED TABLE OF ty_hot_obj_status_version WITH UNIQUE KEY object abap_status.

CLASS cx_cts_hot_invalid_input DEFINITION
  INHERITING FROM cx_no_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA value TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !value    TYPE string OPTIONAL .
ENDCLASS.

INTERFACE lif_timestamp_provider.
  METHODS:
    get_timestamp
      RETURNING
        VALUE(r_timestamp) TYPE timestampl.
ENDINTERFACE.