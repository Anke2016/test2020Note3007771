*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  ty_sobj_names        TYPE STANDARD TABLE OF sobj_name WITH DEFAULT KEY,
  ty_hta_package_names TYPE STANDARD TABLE OF cts_hot_package_id WITH DEFAULT KEY.

"##TODO move to CL_CTS_HTA_COMPONENT?
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
    "! Reads all package names from cts_hot_package for passed sobj_names where abap_status='A'.<br/>
    "! If i_deployable_only is set, only those packages are returned that have HOT_STATUS NOT 'A'
    read_packages
      IMPORTING
        i_sobj_names      TYPE ty_sobj_names
        i_deployable_only TYPE abap_bool
      RETURNING
        VALUE(r_result)   TYPE ty_hta_package_names,

    "! Set prework done flag for passed package with passed value
    set_prework
      IMPORTING
        i_abap_hana_package_id TYPE cts_hot_package_id
        i_prework              TYPE REF TO ce_cts_hta_prework,

    "! Finds all packages with the name in their HANA name. i_hana_package_name can contain % so multi result possible
    find_hta_package_names
      IMPORTING
        i_hana_package_name TYPE cts_hot_hana_package_id
      RETURNING
        VALUE(r_result)     TYPE ty_hta_package_names.
ENDINTERFACE.