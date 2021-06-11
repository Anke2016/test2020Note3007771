  PRIVATE SECTION.
    DATA:
      m_hta_packages      TYPE if_cts_hta_types=>ty_cts_hta_packages,
      m_hta_objects       TYPE if_cts_hta_types=>ty_cts_hta_objects,
      m_hta_full_packages TYPE if_cts_hta_types=>ty_cts_hta_full_packages.

    METHODS:
      "! Returns the overall deploy state for passed list of components<br/>
      "! <ul><li>ce_cts_hta_deploy_state=&gt;deployed if ALL entries of the list have deploy_state deployed</li>
      "! <li>ce_cts_hta_deploy_state=&gt;not_deployed if ALL entries of the list have deploy_state not deployed</li>
      "! <li>ce_cts_hta_deploy_state=&gt;partly_deployed if some entries of the list have deploy_state deployed and some not_deployed or some have partly_deployed</li>
      "! <li>initial if passed i_components is initial.</li></ul>
      "! @parameter i_components | the components for which aggregated deploy state should be calculated
      "! @parameter i_partly_deployed_supported | flag whether passed list supports partly_deployed or not. (should
      "!                                          be set for full_package only) - needed for performance only
      "! @parameter r_result | the aggregated deploy state
      "! @raising cx_cts_hta_not_found | in case a component (object or package) can not be found in HTA repository
      get_deploy_state_aggregated
        IMPORTING
          i_components                TYPE if_cts_hta_types=>ty_cts_hta_components
          i_partly_deployed_supported TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(r_result)             TYPE REF TO ce_cts_hta_deploy_state
        RAISING
          cx_cts_hta_not_found,

      "! Helper method to set prework on passed list of components.
      set_prework_done_once_per_pkg
        IMPORTING
          i_components         TYPE if_cts_hta_types=>ty_cts_hta_components
          i_prework_flag       TYPE REF TO ce_cts_hta_prework
        CHANGING
          c_preworked_packages TYPE ty_preworked_packages,

      "! Helper method to execute rs_corr_insert and add result to result of rs_corr_insert result
      rs_corr_insert_hta_component
        IMPORTING
          i_hta_component   TYPE REF TO if_cts_hta_component
          i_trkorr          TYPE trkorr
          i_devclass        TYPE devclass
          i_suppress_dialog TYPE c
          i_force           TYPE abap_bool
        CHANGING
          c_sync_results    TYPE if_cts_hta_types=>ty_sync_results
        RAISING
          cx_cts_hta_unknown_master_lang
          cx_cts_hta_wbo
          cx_cts_hta,

      "! Calculates the sync_state for a list of components.<br/>
      "! @parameter i_components | Component list to return sync state for.
      "! @parameter i_sync_state | sync state already reached by previous calls
      "! @parameter c_reasons_can_not_be_synced  | If any component has state can_not_be_synchronized, the reason is added to c_reasons_can_not_be_synced
      "! @parameter r_result | aggregated sync_state for all components
      get_sync_state_aggregated
        IMPORTING
          i_components                TYPE if_cts_hta_types=>ty_cts_hta_components
          i_sync_state                TYPE REF TO ce_cts_hta_sync_state
        CHANGING
          c_reasons_can_not_be_synced TYPE if_cts_hta_types=>ty_cx_cts_htas
        RETURNING
          VALUE(r_result)             TYPE REF TO ce_cts_hta_sync_state
        RAISING
          cx_cts_hta_no_hana_database.