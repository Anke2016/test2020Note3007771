*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_hot_logger DEFINITION INHERITING FROM cl_cts_hot_logger_abstract FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_cts_hot_logger~flush REDEFINITION.

ENDCLASS.

CLASS lcl_logger DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ir_logger TYPE REF TO if_cts_hot_logger OPTIONAL,

      message
        IMPORTING
          iv_msg_nr TYPE sprot_u-msgnr
          iv_level  TYPE sprot_u-level
          iv_var1   TYPE sprot_u-var1 OPTIONAL
          iv_var2   TYPE sprot_u-var1 OPTIONAL
          iv_var3   TYPE sprot_u-var1 OPTIONAL
          iv_var4   TYPE sprot_u-var1 OPTIONAL,

      error
        IMPORTING
          iv_msg_nr TYPE sprot_u-msgnr,
      flush,

      long_text
        IMPORTING
          iv_msg_id   TYPE arbgb DEFAULT 'SCTS_HOT'
          iv_msg_nr   TYPE msgnr DEFAULT '530' "&1&2&3&4
          iv_level    TYPE protlevel DEFAULT 4
          iv_severity TYPE errortyp DEFAULT ' '
          iv_text     TYPE string,
      abnormal_termination_exception
        IMPORTING
          ir_exception TYPE REF TO cx_root.

  PRIVATE SECTION.
    DATA:
      mr_logger TYPE REF TO if_cts_hot_logger.
ENDCLASS.

CLASS lcl_helper DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor,

      get_akh_for_hota
        IMPORTING
          i_hota_name     TYPE sobj_name
        RETURNING
          VALUE(r_result) TYPE ufps_posid,

      "! Returns the devclass (abap package) for the given HOTA transport object name
      "!
      "! @parameter iv_hota_name | Name of HOTA transport object
      "! @parameter rv_devclass  | devclass / abap package of the passed HOTA or INITIAL if iv_hota_name not existing in tadir (object catalog)
      get_devclass_for_hota
        IMPORTING
          iv_hota_name       TYPE sobj_name
        RETURNING
          VALUE(rv_devclass) TYPE devclass,


      "! @parameter ev_cdata | Only filled if passed object exists in HANA Repo and has cdata
      "! @parameter ev_bdata | Only filled if passed object exists in HANA Repo and has bdata
      "! @raising cx_hana_object_transport | In case of technical errors in HANA Repo communication
      read_hana_object_data
        IMPORTING
          iv_package_id    TYPE string
          iv_object_name   TYPE string
          iv_object_suffix TYPE string
        EXPORTING
          ev_cdata         TYPE string
          ev_bdata         TYPE xstring
        RAISING
          cx_hana_object_transport,

      "! @parameter rv_version | Version of the object in HANA Repo or 0 if object does not exist
      "! @raising cx_hana_object_transport | In case of technical errors in HANA Repo communication
      read_hana_object_version
        IMPORTING
          iv_package_id     TYPE string
          iv_object_name    TYPE string
          iv_object_suffix  TYPE string
        RETURNING
          VALUE(rv_version) TYPE i
        RAISING
          cx_hana_object_transport.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_package_akh,
        package TYPE cts_hot_package_id,
        akh     TYPE ufps_posid,
      END OF ty_package_akh,
      ty_package_akhs TYPE SORTED TABLE OF ty_package_akh WITH UNIQUE KEY package.

    DATA:
      mr_hot_hana_connector TYPE REF TO cl_cts_hot_hana_connector,
      m_cache_package_akh   TYPE ty_package_akhs.
ENDCLASS.

CLASS lcx_error_prepare_redeployment DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.