  PRIVATE SECTION.
    CLASS-DATA:
      "! SID of underlying HANA DB.
      g_hana_sid             TYPE string,
      "! build version of underlying HANA DB
      g_hana_build_version   TYPE string,
      "! timezone_offset of HANA compared to UTC. In case of error during read it is set to AS ABAP timezone diff.
      g_hana_timezone_offset TYPE i.

    CLASS-METHODS:
      "! Read HANA metadat (SID, timezone_offset and build_version) from HANA and store in global variables.
      "! To be used as HANA Upload System, time conversion to UTC and HANA build version.
      read_hana_metadata,

      conv_hana_actvted_at_to_timest
        IMPORTING
          i_activated_at  TYPE string
        RETURNING
          VALUE(r_timest) TYPE timestampl,
      conv_is_structural_2_abap_bool
        IMPORTING
          i_in               TYPE cts_hot_package-hana_pack_is_structural
        RETURNING
          VALUE(r_abap_bool) TYPE abap_bool,
      conv_abap_bool_2_is_structural
        IMPORTING
          i_abap_bool TYPE abap_bool
        RETURNING
          VALUE(r_b)  TYPE cts_hot_package-hana_pack_is_structural.

    DATA: m_nhi_object_api           TYPE REF TO if_nhi_object,
          m_nhi_package_api          TYPE REF TO if_nhi_package,
          m_nhi_delivery_unit_api    TYPE REF TO if_nhi_delivery_unit,
          m_nhi_text_api             TYPE REF TO if_nhi_text,
          m_nhi_api_user             TYPE string,
          m_cts_hot_db_access        TYPE REF TO if_cts_hot_db_access,
          m_timestamp_provider       TYPE REF TO lif_timestamp_provider,
          mv_max_no_of_chars_for_log TYPE i.

    METHODS:
      constructor
        IMPORTING
          i_nhi_api      TYPE REF TO if_nhi_api
          i_nhi_api_user TYPE string,

      "! Updates object data in hot after deployment. Reads metadata from hana for deployed objects and updates hot repository.
      "! @parameter i_object_status_versions | hot_status and source_object_version before the deployment to prevent update of potentially
      "!                                       parallel imported new version (by TP)
      update_objs_in_hot_after_depl
        IMPORTING
          i_successfull_deploy_objects TYPE ty_cts_hot_objects
          i_abap_status                TYPE cts_hot_object-abap_status
          i_object_status_versions     TYPE ty_hot_obj_status_versions
        RAISING
          cx_hana_object_transport
          cx_nhi_hana_repository,

      "! Updates objects in HOT having import/activation errors
      "! @parameter i_object_status_versions | hot_status and source_object_version before the deployment to prevent update of potentially
      "!                                       parallel imported new version (by TP)
      update_error_objs_in_hot
        IMPORTING
          i_error_objects          TYPE ty_cts_hot_objects
          i_abap_status            TYPE cts_hot_object-abap_status
          i_object_status_versions TYPE ty_hot_obj_status_versions
        RAISING
          cx_hana_object_transport
          cx_nhi_hana_repository,

      "! Updates data in hot after creation / update / deletion of package.
      "! @parameter i_cts_hot_package | the package to be updated
      "! @parameter i_cts_hot_package_a | in case i_cts_hot_package-abap_status = 'I' also provide the 'A' version of the package
      "!                                  so that we can check that no parallel tp import happened.
      update_pkg_in_hot_after_modify
        IMPORTING
          i_cts_hot_package   TYPE cts_hot_package
          i_cts_hot_package_a TYPE cts_hot_package,

      "! Updates hot_status of the package in hot CTS_HOT_PACKAGE if errors occurred during creation / update / deletion of package.
      "! @parameter i_cts_hot_package | the package to be updated with hot_status 'E' or 'Z'
      update_error_pkg_in_hot
        IMPORTING
          i_cts_hot_package TYPE cts_hot_package,

      "! Reading active metadata for an object from HANA.
      "! @parameter i_cts_hot_object | Object for which the metadata should be read.
      "! @parameter r_metadata | filled with metadata of passed object or not bound if object is not active in HANA.
      read_object_metadata_from_hana
        IMPORTING
          i_cts_hot_object  TYPE REF TO cl_cts_hot_object_v1
        RETURNING
          VALUE(r_metadata) TYPE REF TO cl_nhi_metadata_active_ver
        RAISING
          cx_hana_object_transport
          cx_nhi_hana_repository,

      append_log_message
        IMPORTING
          i_severity        TYPE string
          i_error_code      TYPE string
          i_message         TYPE string
          i_timestamp       TYPE string OPTIONAL
          i_location        TYPE string OPTIONAL
          i_hot_object      TYPE REF TO cl_cts_hot_object_v1 OPTIONAL
          i_is_hana_message TYPE abap_bool DEFAULT abap_true
        CHANGING
          c_logs            TYPE cl_cts_hot_hana_connector=>ty_log_messages,

      append_log_message_package
        IMPORTING
          i_severity        TYPE string
          i_error_code      TYPE string
          i_message         TYPE string
          i_hot_package     TYPE REF TO cl_cts_hot_package
          i_is_hana_message TYPE abap_bool DEFAULT abap_true
        CHANGING
          c_logs            TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages,

      "! Returns abap_true if DU is existing in HANA, abap_false otherwise.
      "! If errors occur during DU existence check, also abap_false is returned
      is_du_existing_in_hana
        IMPORTING
                  i_du_vendor     TYPE string
                  i_du_name       TYPE string
        RETURNING VALUE(r_exists) TYPE abap_bool,

      "! Raises cx_hana_object_transport=&gt;response_is_null_error in case i_response is not bound.
      "! Uses i_what and i_action for the short message to tellwhich call went wrong
      "!
      "! @parameter i_response | response object of a nhi api call
      "! @parameter i_what | nhi_api_request=&gt;co_what
      "! @parameter i_action | nhi_api_request=&gt;co_action
      "! @raising cx_hana_object_transport | cx_hana_object_transport=&gt;response_is_null_error in case i_response is not bound
      raise_exc_if_resp_is_not_bound
        IMPORTING
          i_response TYPE REF TO object
          i_what     TYPE string
          i_action   TYPE string
        RAISING
          cx_hana_object_transport,

      "! Deletes entries in SMODI tables for passed objects. (Should be called after deletions)
      "! @parameter i_obj_type | Object type, HOTO or HOTP (= sub_type in smodi tables)
      "! @parameter i_obj_name | Full Name of the object
      delete_smodi_entries
        IMPORTING
          i_obj_type TYPE trobjtype
          i_obj_name TYPE cts_hot_object_name,

      "! Moves SMODI entries from SMODISRCI to SMODISRC. (Should be called after succssful activations but only in SNote / CWB case)
      "! @parameter i_obj_type | Object type, HOTO or HOTP (= sub_type in smodi tables)
      "! @parameter i_obj_name | Full Name of the object
      update_smodi_entries
        IMPORTING
          i_obj_type TYPE trobjtype
          i_obj_name TYPE cts_hot_object_name,

      "! Write texts to text tables of HTA
      handle_texts
        IMPORTING
          object_reference TYPE lxeobjname
          lang             TYPE spras
          text_type        TYPE c
          texts            TYPE cl_nhi_text_with_language=>ty_text_with_languages
        RAISING
          cx_hana_object_transport
          cx_cts_hta_hashing,

      "! Try to activate all objects together. If errors occur try again with those objects for
      "! which activation would have worked. Do this as long as activation is not successful and
      "! number of objects for next try is decreasing and max number of attempts not yet reached.<br/>
      "! Activation success can not be determined on error_code of activation but is successful
      "! if one of the CheckResults contains error_code = '40137' AND its message contains the
      "! string 'Finished activation phase.'<br/>
      "! If activation is not successful, objects with severity = '3' are contained in e_failed_objects
      "! and in objects_with_last_action for current activation result.<br/>
      "! Depending on the hot activation mode, all objects which were OK in an attempt, are retried in
      "! the next attempt. <br/>
      "! If activation was successful, all objects of this activation attempt are added to
      "! e_successfull_objects and to list of objects_with_last_action for current activation result.<br/>
      "! If revalidation after activation contains other objects they are only part of the log_messages.
      "!
      "! @parameter i_objects_to_be_activated | All objects to be activated, as table with mapping of HANA names to NHI object to
      "!                                        hot object
      "! @parameter i_abap_status | ABAP Status of objects for which activation is executed (distinguish normal deployment,'A', and SNOTE/SCWB deployment, 'I')
      "! @parameter i_object_status_versions | hot_status and source_object_version before the deployment to prevent update of potentially
      "!                                       parallel imported new version (by TP)
      "! @parameter i_activation_mode | Activation mode to use.<br/>
      "!                                If default is changed here, also change in CL_CTS_HOT_DB_ACCESS-&gt;read_activation_mode( )
      "!                                and CL_CTS_HOT_DB_ACCESS-&gt;read_activation_mode_rf( )
      "! @parameter i_max_nr_activation_attempts | Maximum number of activation attempts to perform.<br/>
      "!                                           If default is changed here, also change in CL_CTS_HOT_DB_ACCESS-&gt;read_max_nr_activation_atempts( )
      "!                                           and CL_CTS_HOT_DB_ACCESS-&gt;read_max_nr_act_attempts_rf( )
      "! @parameter i_activate_with_hints | Use new HANA repository feature returning hints, yes or no.<br/>
      "!                                    If default is changed here, also change in CL_CTS_HOT_DB_ACCESS-&gt;read_activate_with_hints( )
      "! @parameter c_deploy_result | The deploy result where the activation results should be appended to
      "! @parameter c_failed_objects | All objects for which activation failed are appended to this table. But only objects part of i_objects_to_be_activated
      "!                               are appended.<br/>
      "!                               Objects which where implicitly added to activation by HANA due to dependencies are not appended to this table.
      "! @parameter c_successful_objects | All objects for which activation was successful are appended to this table. But only objects part of
      "!                                   i_objects_to_be_activated are appended.<br/>
      "!                                   Objects which where implicitly added to activation by HANA due to dependencies are not appended to this table.
      "! @raising cx_nhi_hana_repository |
      "! @raising cx_hana_object_transport |
      activate_objects
        IMPORTING
          i_objects_to_be_activated    TYPE ty_map_hana_2_hot_2_nhi_objs
          i_abap_status                TYPE cts_hot_object-abap_status DEFAULT 'A'
          i_object_status_versions     TYPE ty_hot_obj_status_versions
          i_activation_mode            TYPE char1 DEFAULT if_cts_hot_db_access=>co_hot_activation_mode_ok
          i_max_nr_activation_attempts TYPE i DEFAULT 10
          i_activate_with_hints        TYPE abap_bool DEFAULT abap_false
        CHANGING
          c_deploy_result              TYPE ty_deploy_result
          c_failed_objects             TYPE ty_cts_hot_objects
          c_successful_objects         TYPE ty_cts_hot_objects
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Analyzes the i_activation_response for successful/failed objects and converts the HANA check results into HTA log messages.
      "!
      "! @parameter i_activation_response | the activation response to analyze
      "! @parameter i_map_hana_2_hot_2_nhi_obj | Table with all objects that should be activated (in overall deployment not per attempt)
      "!                                         with mapping of HANA names to NHI objects to HOT objects
      "! @parameter e_activation_success | abap_true if activation was successful or abap_false if activation was not successful.<br/>
      "!                                   activation was successful if HANA returned error_code = 0 or the check results contain a check result with text
      "!                                   'Finished activation phase.'
      "! @parameter e_successful_objects | If e_activation_success = abap_true then this table contains all successfully activated objects, but only those
      "!                                   objects for which HANA returned check results<br/>
      "!                                   If e_activation_success = abap_false then this table contains all object that could have been activated if
      "!                                   there were no error with regards to the returned check results<br/>
      "!                                   This table will only contain objects that are part of i_map_hana_2_hot_2_nhi_obj. So implicitly added objects by
      "!                                   HANA due to dependencies that were/could be successfully activated are not contained
      "! @parameter e_failed_objects | If e_activation_success = abap_true then this table is empty.<br/>
      "!                               If e_activation_success = abap_false then this table contains all objects for which activation failed<br/>
      "!                               This table will only contain objects that are part of i_map_hana_2_hot_2_nhi_obj. So implicitly added objects by
      "!                               HANA due to dependencies that could not be activated and are not known are not contained. But table might contain
      "!                               implicitly added objects if they are part of overall deployment but not part of this attempt.
      "! @parameter c_log_messages | Table where messages of HANA check results are appended to (converted as HTA log message)
      "! @parameter c_map_hana_2_hot_2_nhi_obj_un | Table with all unknown objects that were returned by HANA in check results.<br/>
      "!                                            Whenever a check result contains an object that is not part of i_map_hana_2_hot_2_nhi_obj,
      "!                                            it is added to this table to ensure that all log messages of 1 object in c_log_messages
      "!                                            contain the same cts_hot_object
      "! @raising cx_hana_object_transport |
      analyze_activation_response
        IMPORTING
          i_activation_response         TYPE REF TO cl_nhi_activate_objects_res
          i_map_hana_2_hot_2_nhi_obj    TYPE ty_map_hana_2_hot_2_nhi_objs
        EXPORTING
          e_activation_success          TYPE abap_bool
          e_successful_objects          TYPE ty_map_hana_2_hot_2_nhi_objs
          e_failed_objects              TYPE ty_map_hana_2_hot_2_nhi_objs
        CHANGING
          c_log_messages                TYPE cl_cts_hot_hana_connector=>ty_log_messages
          c_map_hana_2_hot_2_nhi_obj_un TYPE ty_map_hana_2_hot_2_nhi_objs
        RAISING
          cx_hana_object_transport.