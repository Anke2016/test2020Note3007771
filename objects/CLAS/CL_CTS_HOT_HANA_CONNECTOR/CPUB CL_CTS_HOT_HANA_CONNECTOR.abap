CLASS cl_cts_hot_hana_connector DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hot_hana_conn_internal.

    "aliases required for backward compatibility...
    ALIASES:
      "method aliases
      read_objects_from_hana_to_hot FOR if_cts_hot_hana_conn_internal~read_objects_from_hana_to_hot,
      sync_packages_from_hana_to_hot FOR if_cts_hot_hana_conn_internal~sync_packages_from_hana_to_hot,
      find_active_objects_in_hana FOR if_cts_hot_hana_conn_internal~find_active_objects_in_hana,
      get_objects_of_pkg_from_hana FOR if_cts_hot_hana_conn_internal~get_objects_of_pkg_from_hana,
      read_package_data_from_hana FOR if_cts_hot_hana_conn_internal~read_package_data_from_hana,
      list_hana_packages FOR if_cts_hot_hana_conn_internal~list_hana_packages,
      read_object_data_from_hana FOR if_cts_hot_hana_conn_internal~read_object_data_from_hana,

      "type aliases
      ty_cts_hot_objects FOR if_cts_hot_hana_conn_internal~ty_cts_hot_objects.

    TYPES:
      BEGIN OF ty_log_message,
        error_code      TYPE string,
        message         TYPE string,
        timestamp       TYPE string,
        severity        TYPE string,
        location        TYPE string,
        cts_hot_object  TYPE REF TO cl_cts_hot_object_v1,
        is_hana_message TYPE abap_bool, "indicates whether log messages comes from HANA or from HOT
      END OF ty_log_message,
      ty_log_messages TYPE STANDARD TABLE OF ty_log_message WITH DEFAULT KEY,

      BEGIN OF ty_log_message_package,
        cts_hot_package TYPE REF TO cl_cts_hot_package,
        severity        TYPE string,
        error_code      TYPE string,
        message         TYPE string,
        is_hana_message TYPE abap_bool, "indicates whether log messages comes from HANA or from HOT
      END OF ty_log_message_package,
      ty_log_messages_packages TYPE STANDARD TABLE OF ty_log_message_package WITH KEY cts_hot_package,

      "! Import result for import part of a deploy_objects call
      BEGIN OF ty_import_result,
        "! Objects for which this import was last action - either revert only or import failed or wrong status or other problem
        objects_with_last_action      TYPE ty_cts_hot_objects,
        "! Objects that were successfully imported to inactive in HANA
        successfully_imported_objects TYPE ty_cts_hot_objects,
        "! Objects that were successfully deleted as inactive in HANA and require activation
        successfully_deleted_objects  TYPE ty_cts_hot_objects,
        "! Objects that were only reverted without need for activation because they did not exist as active so far. So import is also last action for these objects.
        successfully_reverted_objects TYPE ty_cts_hot_objects,
        "! Logs for error cases (each log contains the object it belongs to)
        log_messages                  TYPE ty_log_messages,
      END OF ty_import_result,

      BEGIN OF ty_activation_result,
        "! Counter of the activation HOT is trying to perform.
        "! HOT probably tries to activate the HANA Content several times:
        "! In case of errors during first activation, the content that would
        "! have been activated successfully is tried again.
        "! Usually the 2nd activation should work then, but in case of errors same
        "! procedure takes place. Retry activation with objects that would have been
        "! OK in 2nd. activation. Then 3rd try and so on.
        activation_counter       TYPE i,
        "! Number of objects that were tried to be activated with this call.
        nr_of_attempted_objects  TYPE i,
        "! abap_true in case activation was successful, abap_false if not.<br/>
        "! If abap_true then objects_with_last_action contains all successful activated objects<br/>
        "! If abap_false and ok_objects_only = abap_false then objects_with_last_action contains all failed objects
        activation_success       TYPE abap_bool,
        "! Indicates whether it was normal activation or "nachbrenner" activating only
        "! objects explicitly returned as OK in previous attempt
        ok_objects_only          TYPE abap_bool,
        hana_activation_id       TYPE string,
        hana_error_code          TYPE string,
        hana_error_msg           TYPE string,
        hana_error_arg           TYPE string,
        log_messages             TYPE ty_log_messages,
        "! Objects for which this activation attempt was last action<br/>
        "! If activation_success = abap_true then all successfully objects are contained<br/>
        "! If activation_success = abap_false and ok_objects_only = abap_false then all failed objects are contained, else failed objects are retried<br/>
        objects_with_last_action TYPE ty_cts_hot_objects,
        "! Failed objects in this activation attempt. (if ok_objects_only = abap_true these objects will also be retried in next attempt where ok_objects_only = abap_false)<br/>
        "! Might also contain failed objects that were not passed by HTA for this attempt but part of overall deployment and HANA added the object implicitly to this activation attempt.
        failed_objects           TYPE ty_cts_hot_objects,
      END OF ty_activation_result,
      ty_activation_results TYPE STANDARD TABLE OF ty_activation_result WITH DEFAULT KEY,
      BEGIN OF ty_deploy_result,
        import_result      TYPE ty_import_result,
        activation_results TYPE ty_activation_results,
      END OF ty_deploy_result,

      BEGIN OF ty_text_deploy_result,
        cts_hot_object      TYPE REF TO cl_cts_hot_object_v1,
        abap_lang           TYPE sprsl,
        hana_lang           TYPE string,
        imported_text_count TYPE i,
        hana_error_code     TYPE string,
        hana_error_message  TYPE string,
      END OF ty_text_deploy_result,
      ty_text_deploy_results TYPE HASHED TABLE OF ty_text_deploy_result WITH UNIQUE KEY cts_hot_object abap_lang hana_lang,

      "! Type for text deployment containing each object once and all languages that should to be deployed.
      "! If abap_langs is initial, all languages known in HTA for this object are deployed.
      BEGIN OF ty_text_deploy_input,
        cts_hot_object TYPE REF TO cl_cts_hot_object_v1,
        abap_langs     TYPE STANDARD TABLE OF sprsl WITH DEFAULT KEY,
      END OF ty_text_deploy_input,
      ty_text_deploy_inputs TYPE SORTED TABLE OF ty_text_deploy_input WITH UNIQUE KEY cts_hot_object.

    CLASS-DATA:
      "! HANA Timezone String, e.g.: UTC or UTC-1 or UTC+5, ...
      g_hana_timezone_string TYPE string.

    CLASS-METHODS:
      "! Initializes static fields like g_hana_timezone_string.
      class_constructor,

      "##TODO better way to prevent this to be used by somebody else?
      "! Creates an instance of this class by using local HANA DB.<br/>
      "! Optionally an nhi api and nhi user can be passed, e.g. testing using a remote HANA
      create_instance
        IMPORTING
          i_nhi_api            TYPE REF TO if_nhi_api OPTIONAL
          i_nhi_api_user       TYPE string OPTIONAL
          i_db_connection_name TYPE string DEFAULT 'DEFAULT'
        RETURNING
          VALUE(r_result)      TYPE REF TO cl_cts_hot_hana_connector
        RAISING
          cx_hana_object_transport.

    METHODS:
      "! Deploys (import/delete and activate) all passed objects (from HOT tables to HANA).
      "!
      "! In detail the passed objects will be looked up in HOT table CTS_HOT_OBJECT with specified abap_status ('A' or 'I')
      "! and then imported and activated in HANA.
      "! Make sure packages are created before calling this method. (Use method modify_packages_in_hana to create packages)
      "!
      "! @parameter i_objects | objects to be deployed (import + activate) to HANA
      "! @parameter i_abap_status | abap_status ('A' or 'I') to select in table CTS_HOT_OBJECT
      "! @parameter i_activation_mode | Activation mode to use.
      "! @parameter i_max_nr_activation_attempts | Maximum number of activation attempts to perform.
      "! @parameter i_activate_with_hints | Use new HANA repository feature returning hints, yes or no.
      "! @parameter e_skipped_objects | objects that were skipped because they are already deployed or not in HOT repository
      "! @parameter e_skipped_objects_n | objects that were skipped because HOT_STATUS = 'N' in HOT repository
      "! @parameter e_successfull_objects | objects for which deployment was successful (contains successfully reverted objects and successfully activated objects)
      "! @parameter e_failed_objects | Objects for which activation or import failed or which could not be deployed due to HOT status does not allow deployment
      "! @parameter e_deploy_result | deploy result consisting of import results (write to hana) and activation result(s)
      "! @parameter r_result | if not initial, deployment to hana was done and object is filled with one or more hana activation result(s)
      "! @raising cx_nhi_hana_repository | in case of exception
      "! @raising cx_hana_object_transport | in case of exception
      deploy_objects_to_hana
        IMPORTING
          i_objects                    TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
          i_abap_status                TYPE cts_hot_object-abap_status DEFAULT 'A'
          i_activation_mode            TYPE char1
          i_max_nr_activation_attempts TYPE i
          i_activate_with_hints        TYPE abap_bool
        EXPORTING
          e_skipped_objects            TYPE ty_cts_hot_objects "needed or will layer on top calculate this?
          e_skipped_objects_n          TYPE ty_cts_hot_objects "needed or will layer on top calculate this?
          e_successfull_objects        TYPE ty_cts_hot_objects "needed or will layer on top calculate this?
          e_failed_objects             TYPE ty_cts_hot_objects "needed or will layer on top calculate this?
          e_deploy_result              TYPE ty_deploy_result "not as returning because logs must also be available in exception case.
*        RETURNING
*          VALUE(r_result)       TYPE ty_deploy_result
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport, "##TODO Exception handling (I guess only mapping on own exception possible) outside or inside this class?

      "! creates or updates packages in HANA with values read from HOT table CTS_HOT_PACKAGE
      "!
      "! In detail the passed packages will be looked up in HOT table CTS_HOT_PACKAGE and then created/updated/deleted in HANA.
      "!
      "! @parameter i_itab_packages | packages to be created/updated/deleted in HANA
      "! @parameter i_abap_status      | abap status to be considered for reading from HOT table CTS_HOT_PACKAGE
      "! @parameter e_created_packages | successfully created packages
      "! @parameter e_updated_packages | successfully updated packages
      "! @parameter e_deleted_packages | successfully deleted packages
      "! @parameter e_skipped_packages | skipped packages, not existing in HOT repository
      "! @parameter e_skipped_packages_n | skipped packages, HOT_STATUS = 'N' in HOT repository
      "! @parameter e_failed_packages  | packages that had errors during processing
      "! @raising cx_nhi_hana_repository | in case of exception
      "! @raising cx_hana_object_transport | in case of exception
      modify_packages_in_hana
        IMPORTING
          i_itab_packages      TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_abap_status        TYPE cts_hot_package-abap_status DEFAULT 'A'
        EXPORTING
          e_created_packages   TYPE ty_log_messages_packages
          e_updated_packages   TYPE ty_log_messages_packages
          e_deleted_packages   TYPE ty_log_messages_packages
          e_skipped_packages   TYPE ty_log_messages_packages
          e_skipped_packages_n TYPE ty_log_messages_packages
          e_failed_packages    TYPE ty_log_messages_packages
        RAISING
          cx_hana_object_transport "##TODO Exception handling ok this way?
          cx_nhi_hana_repository,

      "! Deploy all texts for passed objects and languages
      "!
      "! @parameter i_hot_objects_with_lang      | table with hot objects and the language to be deployed.
      "!                                           If language table is initial all languages will be deployed
      "! @parameter e_not_active_objects         | Objects for which no text deployment was executed because objects are not active (HOT_STATUS &lt;&gt; A and &lt;&gt;N)
      "! @parameter e_unknown_objects            | Objects for which no text deployment was executed because objects are unknown in HTA
      "! @parameter e_skipped_objects            | Objects for which no text deployment was executed because either all texts already deployed or no texts for deployment at all
      "! @parameter e_failed_text_deploy_result  | Objects for which text deployment failed with HANA Error_Code not 0.<br/>
      "!                                           If an object failed for several languages, then several entries exist for same object.
      "! @parameter e_success_text_deploy_result | Objects for which text deployment was successfully executed with the number of texts deployed per language.<br/>
      "!                                           (With regards to number of imported texts, HTA does not distinguish between object and content texts, so one line
      "!                                            in result contains number of object texts plus number of content texts per language)
      "! @raising cx_nhi_hana_repository         | in case of exception
      "! @raising cx_hana_object_transport       | in case of exception
      deploy_object_texts
        IMPORTING
          i_hot_objects_with_lang      TYPE cl_cts_hot_hana_connector=>ty_text_deploy_inputs
        EXPORTING
          e_not_active_objects         TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
          e_unknown_objects            TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
          e_skipped_objects            TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
          e_success_text_deploy_result TYPE ty_text_deploy_results
          e_failed_text_deploy_result  TYPE ty_text_deploy_results
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! For all passed packages compare HTA repository with HANA repository and add all objects that are in HANA but not in HTA as deletions to CTS_HOT_OBJECT.
      "!
      "! @parameter i_packages |
      "! @parameter r_object_list | Objects that exist in HANA but not in HTA and which were added as deletions to CTS_HOT_OBJECTS table.
      check_hana_for_obosolete_objs
        IMPORTING
          i_packages           TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
        RETURNING
          VALUE(r_object_list) TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
        RAISING
          cx_hana_object_transport.

    "##TODO Exception handling (I guess only mapping on own exception possible) outside or inside this class?