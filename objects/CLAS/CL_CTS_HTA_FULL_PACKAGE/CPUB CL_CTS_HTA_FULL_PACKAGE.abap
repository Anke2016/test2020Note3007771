"! Implementation for IF_CTS_HTA_FULL_PACKAGE.<br/>
"! See IF_CTS_HTA_FULL_PACKAGE for more details.
CLASS cl_cts_hta_full_package DEFINITION
  PUBLIC
  INHERITING FROM cl_cts_hta_component_list
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_full_package.

    CLASS-METHODS create_instance_full_package
      IMPORTING
        i_cts_hta_package TYPE REF TO if_cts_hta_package
        i_cts_hta_objects TYPE if_cts_hta_types=>ty_cts_hta_objects
      RETURNING
        VALUE(r_result)   TYPE REF TO if_cts_hta_full_package
      RAISING
        cx_cts_hta.

    METHODS:
      if_cts_hta_component_list~add_component REDEFINITION,
      if_cts_hta_component_list~remove_component REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.
