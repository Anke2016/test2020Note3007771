CLASS cl_cts_hta_component DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED
  .

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_component
      FINAL METHODS:
      synchronize

      ABSTRACT METHODS:
      deploy
      get_deploy_state
      get_sync_state
      set_prework
      set_deploy_mode
      set_translation_relevance.

    CONSTANTS:
      "! Active version (ABAP_STATUS) of package/object should be used
      co_active_version   TYPE c VALUE 'A',
      "! Inactive version (ABAP_STATUS) of package/object should be used. Only valid in SNote / CWB case
      co_inactive_version TYPE c VALUE 'I'.

    DATA:
      "! Internal usage in HTA only...
      "! hot_package for internal reuse between different types of if_cts_hta_component.<br/>
      "! if_cts_hta_package, if_cts_hta_object and if_cts_hta_full_package are all based on ONE package.
      m_hot_package    TYPE REF TO cl_cts_hot_package READ-ONLY.
