  PROTECTED SECTION.
    METHODS:
      constructor,

      "! Executes deployment for passed packages and objects.<br/>
      "! Should not be redefined in subclasses. (only for TDD)
      execute_deploy "FINAL
        IMPORTING
          i_hta_packages    TYPE if_cts_hta_types=>ty_cts_hta_packages OPTIONAL
          i_hta_objects     TYPE if_cts_hta_types=>ty_cts_hta_objects OPTIONAL
          i_force           TYPE abap_bool DEFAULT abap_false
        EXPORTING
          e_deploy_status   TYPE if_cts_hta_types=>ty_deploy_status
          e_deploy_messages TYPE if_cts_hta_types=>ty_deploy_messages,

      "! Central check whether hot status allows sync or not.
      "! @parameter i_hot_status | Status to be checked
      "! @parameter r_result | abap_true in case sync is allowed, abap_false if sync is not allowed
      is_hot_status_ok_for_sync
        IMPORTING
          i_hot_status    TYPE cts_hot_object_status
        RETURNING
          VALUE(r_result) TYPE abap_bool,

      "! Each subclass to do some pre sync checks:<br/>
      "! <ul><li>sync allowed with regards to HOT status?</li>
      "! <li>name conflict, package/ object/suffix in HTA has different case than the package/object that should be synchronized</li>
      "! <li>package/object can be read from HANA</li></ul><br/>
      "! Requires that data from HTA and HANA was read before with read_hta_data/read_hana_data
      "! @raising cx_cts_hta_name_conflict |
      "! @raising cx_cts_hta_wrong_status |
      hta_pre_sync_check ABSTRACT
        RAISING
          cx_cts_hta_name_conflict
          cx_cts_hta_wrong_status
          cx_cts_hta_no_hana_database
          cx_hana_object_transport
          cx_cts_hta,

      "! Each subclass needs to call function module rs_corr_check with the parameter specific to the subclass..
      "!
      "! @parameter i_suppress_dialog | If set to 'X' no dialogs will be shown during call to rs_corr_check.
      "! @parameter i_force | With i_force = abap_true, the synchronization will also synchronize components that are already synchronized
      "!                      (same version in HTA repository and HANA repository) <br/>
      "! @raising cx_cts_hta_wbo | in case of errors during transport request creation/selection
      rs_corr_check ABSTRACT
        IMPORTING
          i_suppress_dialog TYPE c
          i_force           TYPE abap_bool
        RAISING
          cx_cts_hta_wbo,

      "! Each subclass needs to call function module rs_corr_insert, with the parameter specific to the subclass.<br/>
      "!
      "! @parameter i_trkorr |
      "! @parameter i_devclass |
      "! @parameter i_suppress_dialog |
      "! @parameter i_force | With i_force = abap_true, the synchronization will also synchronize components that are already synchronized
      "!                      (same version in HTA repository and HANA repository) <br/>
      "! @parameter r_result |
      "! @raising cx_cts_hta_unknown_master_lang |
      "! @raising cx_cts_hta_wbo |
      "! @raising cx_cts_hta |
      rs_corr_insert ABSTRACT
        IMPORTING
          i_trkorr          TYPE trkorr
          i_devclass        TYPE devclass OPTIONAL
          i_suppress_dialog TYPE c
          i_force           TYPE abap_bool
        RETURNING
          VALUE(r_result)   TYPE if_cts_hta_types=>ty_sync_results
        RAISING
          cx_cts_hta_unknown_master_lang
          cx_cts_hta_wbo
          cx_cts_hta,

      "! Each subclass needs to execute the synchronization specific to the subclass.
      "! @parameter i_force | With i_force = abap_true, the synchronization will also synchronize components that are already synchronized
      "!                      (same version in HTA repository and HANA repository) <br/>
      execute_sync ABSTRACT
        IMPORTING
          i_force TYPE abap_bool
        RAISING
          cx_cts_hta,

      "! Reads data from HANA into internal member variable. To be called in beginning of public API call if HANA data is required.
      read_hana_data ABSTRACT
        RAISING
          cx_cts_hta_no_hana_database
          cx_hana_object_transport
          cx_cts_hta,

      "! Reads data from HTA into internal member variable. To be called in beginning of public API call if HTA data is required.<br/>
      read_hta_data ABSTRACT.

    CLASS-DATA:
      "! Reference to HOT HANA connector which executes all calls to HANA<br/>
      "! Reference is NOT bound if the current AS ABAP is not a HANA system.<br/>
      "! Required to be class-data as it is used within class methods in subclasses and for testing
      gr_hot_hana_connector TYPE REF TO if_cts_hot_hana_conn_internal,

      "! Reference to HTA DB access to read data from HTA tables. TODO replace local implementations with lif_db_access...
      gr_hot_db_access      TYPE REF TO if_cts_hot_db_access.

    CLASS-METHODS:
      "! Helper method to instantiate gr_hot_hana_connector only in cases it is required.<br/>
      "! Also unit tests can mock the connector during test execution by setting reference of
      "! gr_hot_hana_connector to some test double<br/>
      "! This method must be called if HOT HANA Connector is required in some implementation method
      create_hot_hana_connector
        RAISING
          cx_cts_hta_no_hana_database
          cx_cts_hta.

    DATA:
      "! ABAP status of current package/object/list. So far only ALL are active / inactive are supported during deployment.
      "! ABAP_STATUS only used internally for SNote/CWB support
      m_abap_status    TYPE cts_hot_abap_status VALUE co_active_version,
      "! Reference to internal HTA interface to external tools (wbo, tadir, ...)
      m_external_calls TYPE REF TO if_cts_hot_ext_call_internal.