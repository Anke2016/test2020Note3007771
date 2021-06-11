*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ty_object_key,
    abap_hana_package_id         TYPE cts_hot_package_id,
    abap_hana_object_name_suffix TYPE cts_hot_object_name_suffix,
    hot_status                   TYPE cts_hot_object_status,
  END OF ty_object_key,

  ty_sobj_names       TYPE STANDARD TABLE OF sobj_name WITH DEFAULT KEY,
  ty_hta_object_names TYPE STANDARD TABLE OF ty_object_key WITH DEFAULT KEY.

"##TODO move to CL_CTS_HTA_COMPONENT? (same in cl_cts_hta_object and cl_cts_hta_package but how to use this in AUnit then?)
INTERFACE lif_tadir_access.
  METHODS:
    "! Reads all obj_names from tadir for R3TR HOTA and passed i_devclasses
    read_hota_objnames_for_devclas
      IMPORTING
        i_devclasses    TYPE if_cts_hta_types=>ty_devclasses
      RETURNING
        VALUE(r_result) TYPE ty_sobj_names.
ENDINTERFACE.

INTERFACE lif_db_access.
  METHODS:
    "! Reads all objects from HTA repository that belong to passed package
    read_objects_for_package
      IMPORTING
        i_package_id    TYPE cts_hot_package_id
      RETURNING
        VALUE(r_result) TYPE ty_hta_object_names.
ENDINTERFACE.