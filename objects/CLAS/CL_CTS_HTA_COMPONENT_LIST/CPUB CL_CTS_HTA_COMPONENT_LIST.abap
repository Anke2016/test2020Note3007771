"! Implementation for IF_CTS_HTA_COMPONENT_LIST.<br/>
"! See IF_CTS_HTA_COMPONENT_LIST for more details.
CLASS cl_cts_hta_component_list DEFINITION
  PUBLIC
  INHERITING FROM cl_cts_hta_component
  CREATE PROTECTED .

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_component_list.

    ALIASES:
        add_component FOR if_cts_hta_component_list~add_component,
        remove_component FOR if_cts_hta_component_list~remove_component,
        get_components FOR if_cts_hta_component_list~get_components,
        deploy FOR if_cts_hta_component_list~deploy,
        get_deploy_state FOR if_cts_hta_component_list~get_deploy_state,
        get_sync_state FOR if_cts_hta_component_list~get_sync_state.

    CLASS-METHODS create_instance
      RETURNING
        VALUE(r_result) TYPE REF TO if_cts_hta_component_list.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.
    METHODS: if_cts_hta_component~get_sync_state REDEFINITION.
