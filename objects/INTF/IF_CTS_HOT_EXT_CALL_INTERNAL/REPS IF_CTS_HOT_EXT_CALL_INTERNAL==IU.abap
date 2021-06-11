"! HTA internal interface to external APIs (WBO, tadir, ...)
INTERFACE if_cts_hot_ext_call_internal
  PUBLIC .

  METHODS:
    "! Checks whether R3trans version is good enough for synchronization.<br/>
    "! If it is too old, depending on i_suppress_dialog a warning dialog
    "! will be shown or exception cx_cts_hta is thrown.
    "!
    "! @parameter i_suppress_dialog | Parameter only used in case R3trans is too old.<br/>
    "!                                If not set to 'X', a warning dialog is shown.<br/>
    "!                                If set to 'X' the dialog will not be shown, instead
    "!                                an exception is thrown and user can not continue.
    "! @raising cx_cts_hta | in case of error during check of R3trans version or
    "!                       in case R3trans is too old and i_suppress_dialog is set to abap_true
    check_transport_tools_for_sync
      IMPORTING
        i_suppress_dialog TYPE c
      RAISING
        cx_cts_hta,

    "! Determines the master language to be used for tadir entry in the following sequence for passed i_hana_original_language:<br/>
    "! 1. If i_hana_original_language is initial either ask user about using logon language or throw cx_cts_hta_unknown_master_lang=&gt;no_master_language,
    "!    depending on i_suppress_dialog<br/>
    "! 2. If i_hana_original_language is set, try to convert to ABAP language. If conversion is not possible cx_cts_hta_unknown_master_lang=&gt;unsupported_master_lang
    "!    is thrown. Reason could either be language is inactive or language string from HANA can't be converted to any language at all.
    "!
    "! @parameter i_hana_package_name | HANA package name for which the language is to be determined
    "! @parameter i_hana_original_language | language string read from HANA of i_hana_package_name
    "! @parameter i_suppress_dialog | If set to 'X', no dialogs are shown
    "! @parameter i_translation_relevance | translation relveance. If set to not relevant, we allow hana package to not contain any language information.
    "! @parameter r_result | master language in ABAP format to be used for tadir
    "! @raising cx_cts_hta_unknown_master_lang | In case no language maintained in HANA and i_suppress_dialog='X' or in case i_hana_original_language can not be
    "!                                           converted to ABAP language
    "! @raising cx_cts_hta | In case user cancels action in the dialog show by HTA
    determine_masterlang_for_tadir
      IMPORTING
        i_hana_package_name      TYPE cts_hot_hana_package_id
        i_hana_original_language TYPE cts_hot_hana_orig_lang
        i_suppress_dialog        TYPE c
        i_translation_relevance  TYPE REF TO ce_cts_hta_translation
      RETURNING
        VALUE(r_result)          TYPE sy-langu
      RAISING
        cx_cts_hta_unknown_master_lang
        cx_cts_hta,

    "! Checks whether passed object could be locked on transport request or not
    "!
    "! @parameter i_pgmid | R3TR or LIMU
    "! @parameter i_object_type | type of the object to check (HOTO, HOTP, HOTA, ...)
    "! @parameter i_object_name | name of the object to check
    "! @parameter i_suppress_dialog | should dialogs be displayed or not?
    "! @parameter i_cts_hta_component | hta component calling this rs_corr_check
    "! @parameter e_tadir | TADIR (object catalog) entry if already existing.
    "! @parameter r_result | Transport request on which the object is already locked
    "! @raising cx_cts_hta_wbo | Wraps the exception returned by rs_corr_check.
    rs_corr_check
      IMPORTING
        i_pgmid             TYPE pgmid
        i_object_type       TYPE trobjtype
        i_object_name       TYPE trobj_name
        i_suppress_dialog   TYPE c
        i_cts_hta_component TYPE REF TO if_cts_hta_component OPTIONAL
      EXPORTING
        e_tadir             TYPE tadir
      RETURNING
        VALUE(r_result)     TYPE trkorr
      RAISING
        cx_cts_hta_wbo,

    "!
    "! @parameter i_pgmid |
    "! @parameter i_object_type |
    "! @parameter i_object_name |
    "! @parameter i_suppress_dialog |
    "! @parameter i_mode | 'INSERT' for new object, 'DELETION' for deleted object
    "! @parameter i_masterlang |
    "! @parameter i_devclass |
    "! @parameter i_genflag |  'X'->generiertes Objekt
    "! @parameter i_trkorr | Transport request or task which the request object should be inserted
    "! @parameter i_cts_hta_component | hta component calling this rs_corr_check
    "! @parameter r_result |
    "! @raising cx_cts_hta_wbo |
    rs_corr_insert
      IMPORTING
        i_pgmid             TYPE pgmid
        i_object_type       TYPE trobjtype
        i_object_name       TYPE trobj_name
        i_suppress_dialog   TYPE c
        i_trkorr            TYPE trkorr DEFAULT space
        i_mode              TYPE string DEFAULT space
        i_masterlang        TYPE syst_langu DEFAULT space
        i_devclass          TYPE devclass DEFAULT space
        i_genflag           TYPE genflag DEFAULT space
        i_cts_hta_component TYPE REF TO if_cts_hta_component OPTIONAL
      RETURNING
        VALUE(r_result)     TYPE trkorr
      RAISING
        cx_cts_hta_wbo,

    "! Display deploy log in SAP GUI.
    "!
    "! @parameter i_log_messages | deploy log messages as returned by HTA API/RDDHANADEPLOYMENT
    "! @parameter i_heading | Heading name for log display
    "! @parameter i_level | Level to which the log should be expanded by default
    display_deploy_log
      IMPORTING
        i_log_messages TYPE if_cts_hta_types=>ty_deploy_messages
        i_heading      TYPE logline OPTIONAL
        i_level        TYPE protlevel DEFAULT '2',

    "! Display log in SAP GUI.
    "!
    "! @parameter i_log_messages | log messages to display (table of sprot_u)
    "! @parameter i_title | Title for log display
    "! @parameter i_heading | Heading name for log display
    "! @parameter i_level | Level to which the log should be expanded by default
    "! @parameter i_condense | 'X' to condense the log messages, default is space which is no condense
    display_log
      IMPORTING
        i_log_messages TYPE if_cts_hta_types=>ty_deploy_messages
        i_title        TYPE syst_title OPTIONAL
        i_heading      TYPE logline OPTIONAL
        i_level        TYPE protlevel DEFAULT '2'
        i_condense     TYPE c DEFAULT space,

    "! Save deploy log to log file in transport directory.<br/>
    "! There will be only 1 log file per day and system, maybe containing logs for more than 1 deployment.
    "! The name of log file will be calculated from passed i_log_name_prefix to become
    "! "&lt;i_log_name_prefix&gt;_yyyymmdd.&lt;SID&gt;", e.g. HTA_WB_ACTIVATION_20151130.QAS<br/>
    "!
    "! @parameter i_log_messages | deploy log messages as returned by HTA API/RDDHANADEPLOYMENT
    "! @parameter i_log_name_prefix | prefix of the log file
    "! @parameter r_result | name of created log file (full path and name)
    save_deploy_log_to_transdir
      IMPORTING
        i_log_messages    TYPE if_cts_hta_types=>ty_deploy_messages
        i_log_name_prefix TYPE string
      RETURNING
        VALUE(r_result)   TYPE trfile,

    "! Returns the AKH for a given HOTA transport object name
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

    "! Returns the switch ID for a given devclass
    get_switch_id_for_devclass
      IMPORTING
        i_devclass      TYPE devclass
      RETURNING
        VALUE(r_result) TYPE sfw_switch_id,

    "! Returns abap_true or abap_false depending whether switch is on (abap_false) or switch is off (abap_true)
    is_switch_switched_off
      IMPORTING
        i_switch_id     TYPE sfw_switch_id
      RETURNING
        VALUE(r_result) TYPE abap_bool,

    "! Determines whether update(SP update with SPAM or SUM) or upgrade (SUM or Blue Green) is running
    "! @parameter rv_result | abap_true if update or upgrade is running, abap_false if no update or upgrade is running
    "! @raising cx_cts_hta | In case of error during update/upgrade check
    is_update_upgrade_running
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_cts_hta,

    "! Determines whether SUM update/upgrade with ZDO is running
    is_sum_with_zdo_running
      RETURNING
        VALUE(rv_result) TYPE abap_bool,

    "! Determines whether blue green deployment (zero downtime upgrade in cloud) is running
    "! @parameter rv_result | abap_true if blue green deployment is running, abap_false if no blue green deployment is running
    "! @raising cx_cts_hta | In case of error during check whether blue/green is running
    is_blue_green_update_running
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_cts_hta,

    "! Determines whether this system is running as view layer system or not. This means whether table and views are located in different schemas, view layer and data layer.
    "! @parameter rv_result | abap_true if system is a view layer system, abap_false if not
    is_view_layer_system
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDINTERFACE.