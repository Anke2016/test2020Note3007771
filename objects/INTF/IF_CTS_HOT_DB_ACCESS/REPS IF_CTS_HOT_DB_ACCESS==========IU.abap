"! Interface encapsulating all DB access calls inside HOT implementation
"! This IF MUST not be used outside HOT coding!
INTERFACE if_cts_hot_db_access PUBLIC.

  "combined fields of cts_hot_otexts_h and cts_hot_otexts_s/cts_hot_otexts_l
  TYPES: BEGIN OF ty_object_text,
           abap_object_reference TYPE lxeobjname,
           abap_text_reference   TYPE lxetextkey,
           text_type             TYPE cts_hot_text_type,
           hana_text_id          TYPE cts_hot_hana_text_id,
           hana_text_type        TYPE cts_hot_hana_text_type,
           hana_text_max_length  TYPE cts_hot_hana_text_max_length,
           language              TYPE spras,
           hana_text_content     TYPE string,
         END OF ty_object_text,
         ty_object_texts TYPE STANDARD TABLE OF ty_object_text WITH DEFAULT KEY,
         ty_sprass       TYPE STANDARD TABLE OF spras WITH DEFAULT KEY.

  CONSTANTS:
    "! defining hot internal value for texts of type object text
    co_cts_hot_text_type_object    TYPE cts_hot_otexts_h-text_type VALUE 'O',
    "! defining hot internal value for texts of type content text
    co_cts_hot_text_type_content   TYPE cts_hot_otexts_h-text_type VALUE 'C',

    "! Object/Package was imported to HOT but not yet deployed to HANA. (waiting for deployment)
    co_hot_status_inactive         TYPE cts_hot_object_status VALUE 'I',
    "! Object/Package was deployed successfully to HANA
    co_hot_status_active           TYPE cts_hot_object_status VALUE 'A',
    "! Object/Package was marked for deletion during import and will be deleted in next deployment.
    co_hot_status_to_be_deleted    TYPE cts_hot_object_status VALUE 'D',
    "! Object/Package was synced from HANA to ABAP
    co_hot_status_new              TYPE cts_hot_object_status VALUE 'N',
    "! Object/Package was tried to be deployed from ABAP to HANA but failed
    co_hot_status_deploy_error     TYPE cts_hot_object_status VALUE 'E',
    "! Object/Package was tried to be deleted in HANA but failed
    co_hot_status_delete_error     TYPE cts_hot_object_status VALUE 'Z',

    "! Deploy mode always, package and its objects are always deployed.
    co_hot_deploy_mode_always      TYPE cts_hot_activation_mode VALUE 'A',
    "! Deploy mode prework check, package and its objects are only deployed if prework is done and maintained in CTS_HOT_PREWORK.
    co_hot_deploy_mode_prework     TYPE cts_hot_activation_mode VALUE 'P',


    "! Not relevant for translation: package and its objects are not relevant for translation.
    co_hot_not_relevant_for_transl TYPE cts_hot_abap_no_translation VALUE 'X',
    "! Relevant for translation: package and its objects are relevant for translation.
    co_hot_relevant_for_transl     TYPE cts_hot_abap_no_translation VALUE ' ',

    "! Name for reading value of HTA_ACTIVATION_MODE from table CTS_HOT_PARAMS.
    co_name_hot_activation_mode    TYPE cts_hot_params-name VALUE 'HTA_ACTIVATION_MODE',
    "! Name for reading value of MAX_NO_ACTIVATION_ATTEMPTS from table CTS_HOT_PARAMS.
    co_name_max_no_act_attempts    TYPE cts_hot_params-name VALUE 'MAX_NO_ACTIVATION_ATTEMPTS',
    "! Name for reading value of HANA_LOG_WITH_HINTS from table CTS_HOT_PARAMS.
    co_name_hana_log_with_hints    TYPE cts_hot_params-name VALUE 'HANA_LOG_WITH_HINTS',
    "! Name for reading value of RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS from table CTS_HOT_PARAMS.
    co_name_run_job_redeploy_faild TYPE cts_hot_params-name VALUE 'RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS',
    "! Name for reading value of HTA_ACTIVATION_MODE_REDEPLOY_FAILED from table CTS_HOT_PARAMS.
    co_name_hot_activation_mode_rf TYPE cts_hot_params-name VALUE 'HTA_ACTIVATION_MODE_REDEPLOY_FAILED',
    "! Name for reading value of MAX_NO_ACTIVATION_ATTEMPTS_REDEPL_FAILED from table CTS_HOT_PARAMS.
    co_name_max_no_act_attempts_rf TYPE cts_hot_params-name VALUE 'MAX_NO_ACTIVATION_ATTEMPTS_REDEPL_FAILED',
    "! Name for reading value of MAX_NO_OF_CHARS_LOGGED_PER_ERROR_WARNING from table CTS_HOT_PARAMS.
    co_name_max_no_chars_for_log TYPE cts_hot_params-name VALUE 'MAX_NO_OF_CHARS_LOGGED_PER_ERROR_WARNING',
    "! Name for reading value of MAX_NR_ATTEMPTS_HDI_MAKE from table CTS_HOT_PARAMS.
    co_name_max_no_att_hdi_make    TYPE cts_hot_params-name VALUE 'MAX_NR_ATTEMPTS_HDI_MAKE',

    "! Each activation is split in 2 activation calls:<br/>
    "! 1. Try to activate all objects that did not finish with error so far<br/>
    "! 2. Try to activate all objects that were explicitly reported as OK in previous call.
    "!    Error objects of this activation are NOT considered as error objects but are retried in next activation call
    "!    together with all objects that were not explicitly reported as OK in previous call
    co_hot_activation_mode_ok      TYPE char1 VALUE '0',
    "! Enhanced version of co_hot_activation_mode_ok in which the OK objects are retried recursively until an activation succeeds
    "! or all objects are failed.<br/>
    "! Example:<br/>
    "! First activation ends with 5 OK objects and 3 objects without success information and 1 error object.<br/>
    "! Second activation will consider only the 5 OK objects but 2 objects are failing now.<br/>
    "! Third activation will consider only the 3 successful objects of second activation and is successful.<br/>
    "! Fourth activation will consider all 3 objects of first activation without success information as well
    "! as the error objects of 2nd activation.<br/>
    co_hot_activation_mode_ok_rec  TYPE char1 VALUE '1',
    "! Activation mode as in first deliveries. As of 2nd activation attempt all not failed objects are retried.
    co_hot_activation_mode_all     TYPE char1 VALUE '2'.

  METHODS:
    "! insert or update a cts_hot_object entry in HOT repository table CTS_HOT_OBJECT
    "! Allows ABAP_STATUS = 'I' and 'A'
    modify_cts_hot_object
      IMPORTING i_cts_hot_object TYPE cts_hot_object,

    "! deletes cts_hot_object from hot_repository table CTS_HOT_OBJECT (using ABAP_STATUS = i_abap_status) <br/>
    "! If i_abap_status = 'A' also texts are deleted from cts_hot_otexts_h/_s/_l
    delete_cts_hot_object
      IMPORTING i_abap_hana_package_id         TYPE cts_hot_object-abap_hana_package_id
                i_abap_hana_object_name_suffix TYPE cts_hot_object-abap_hana_object_name_suffix
                i_abap_status                  TYPE cts_hot_object-abap_status DEFAULT 'A' ,

    "! insert or update a cts_hot_package entry in HOT repository table CTS_HOT_PACKAGE
    "! Allows ABAP_STATUS = 'I' and 'A'
    modify_cts_hot_package
      IMPORTING i_cts_hot_package TYPE cts_hot_package,

    "! reads a cts_hot_package from HOT repository table CTS_HOT_PACKAGE
    "! Allows ABAP_STATUS = 'I' and 'A'
    read_cts_hot_package
      IMPORTING i_abap_hana_package_id TYPE cts_hot_package_id
                i_abap_status          TYPE cts_hot_package-abap_status DEFAULT 'A'
      RETURNING VALUE(r_result)        TYPE cts_hot_package,

    "! deletes a cts_hot_package from HOT repository table CTS_HOT_PACKAGE
    "! Allows ABAP_STATUS = 'I' and 'A'
    delete_cts_hot_package
      IMPORTING i_abap_hana_package_id TYPE cts_hot_package_id
                i_abap_status          TYPE cts_hot_package-abap_status DEFAULT 'A',

    "! reads a cts_hot_object from HOT repository (using ABAP_STATUS = 'A')
    read_cts_hot_object
      IMPORTING i_abap_hana_package_id         TYPE cts_hot_object-abap_hana_package_id
                i_abap_hana_object_name_suffix TYPE cts_hot_object-abap_hana_object_name_suffix
                i_abap_status                  TYPE cts_hot_object-abap_status DEFAULT 'A'
      RETURNING VALUE(r_result)                TYPE cts_hot_object,

    "! read a cts_hot_object from HOT repository (using ABAP_STATUS = 'A') but WITHOUT cdata and bdata
    "! to save some memory
    read_cts_hot_object_wo_bcdata
      IMPORTING i_abap_hana_package_id         TYPE cts_hot_object-abap_hana_package_id
                i_abap_hana_object_name_suffix TYPE cts_hot_object-abap_hana_object_name_suffix
                i_abap_status                  TYPE cts_hot_object-abap_status DEFAULT 'A'
      RETURNING VALUE(r_result)                TYPE cts_hot_object,

    "! Checks existence of an object in HOT repository
    exists_object
      IMPORTING
        i_abap_hana_package_id         TYPE cts_hot_package_id
        i_abap_hana_object_name_suffix TYPE cts_hot_object_name_suffix
      RETURNING
        VALUE(r_exists)                TYPE abap_bool,

    "! deletes all cts_hot_objects from HOT repository table CTS_HOT_OBJECT for the given i_abap_hana_package_id (using ABAP_STATUS = 'A')
    "! Also all texts of passed objects are deleted from cts_hot_otexts_h/_s/_l
    delete_cts_hot_objects
      IMPORTING i_abap_hana_package_id TYPE cts_hot_package_id,

    "! Returns true if at least 1 entry exists in HTA Repository
    exists_data_in_hta
      RETURNING
        VALUE(r_exists) TYPE abap_bool,

    "! Checks whether package exists in HTA or not
    exists_package
      IMPORTING
        i_abap_hana_package_id TYPE cts_hot_package_id
      RETURNING
        VALUE(r_exists)        TYPE abap_bool,

    "! Reads the hot status for passed object. (using ABAP_STATUS = 'A')
    "!
    "! @parameter i_abap_hana_package_id | ABAP_HANA_PACKAGE_ID of the object
    "! @parameter i_abap_hana_object_name_suffix | ABAP_HANA_OBJECT_NAME_SUFFIX of the object
    "! @parameter i_abap_status | ABAP status to use for read ('I' or 'A')
    "! @parameter e_return_code | sy-subrc after query execution (to check whether object exists or not)
    "! @parameter r_hot_status | hot_status, might be one of<br/>
    "!                           <ul><li>if_cts_hot_db_access=&gt;co_hot_status_inactive</li>
    "!                            <li>if_cts_hot_db_access=&gt;co_hot_status_active</li>
    "!                            <li>if_cts_hot_db_access=&gt;co_hot_status_to_be_deleted</li>
    "!                            <li>if_cts_hot_db_access=&gt;co_hot_status_new</li>
    "!                            <li>if_cts_hot_db_access=&gt;co_hot_status_deploy_error</li>
    "!                            <li>if_cts_hot_db_access=&gt;co_hot_status_delete_error</li></ul>
    read_hot_status_for_object
      IMPORTING
        i_abap_hana_package_id         TYPE cts_hot_package_id
        i_abap_hana_object_name_suffix TYPE cts_hot_object_name_suffix
        i_abap_status                  TYPE cts_hot_object-abap_status DEFAULT 'A'
      EXPORTING
        e_return_code                  TYPE syst_subrc
      RETURNING
        VALUE(r_hot_status)            TYPE cts_hot_object_status,

    "! Reads ALL no yet deployed texts of an object from HOT repository. (HOT_STATUS = 'I')
    "!
    "! @parameter i_abap_hana_package_id         | ABAP_HANA_PACKAGE_ID of the object
    "! @parameter i_abap_hana_object_name_suffix | ABAP_HANA_OBJECT_NAME_SUFFIX of the object
    "! @parameter i_languages                    | Languages for which texts should be read. Pass empty table if all languages should be read
    "! @parameter r_object_texts                 | All texts that can be deployed to HANA
    read_deployable_texts_for_obj
      IMPORTING
        i_abap_hana_package_id         TYPE cts_hot_package_id
        i_abap_hana_object_name_suffix TYPE cts_hot_object_name_suffix
        i_languages                    TYPE ty_sprass
      RETURNING
        VALUE(r_object_texts)          TYPE ty_object_texts,

    "! Updates the tables cts_otexts_s and cts_hot_otexts_l for passed i_abap_object_reference, i_text_type and i_language
    "! by setting hot_status to 'A' and deploy time to current time and user to current user.
    "!
    "! @parameter i_abap_object_reference | abap object reference, unique object ID for translation
    "! @parameter i_text_type | text_type in HTA (if_cts_hot_db_access=&gt;co_cts_hot_text_type_object or if_cts_hot_db_access=&gt;co_cts_hot_text_type_content)
    "! @parameter i_language | language
    update_object_texts_after_dpl
      IMPORTING
        i_abap_object_reference TYPE lxeobjname
        i_text_type             TYPE cts_hot_text_type
        i_language              TYPE spras,

    "! Sets the deploy mode on a package
    set_deploy_mode
      IMPORTING
        i_abap_hana_package_id TYPE cts_hot_package_id
        i_deploy_mode          TYPE REF TO ce_cts_hta_deploy_mode,

    "! Sets the translation relevance on a package
    set_translation_relevance
      IMPORTING
        i_abap_hana_package_id  TYPE cts_hot_package_id
        i_translation_relevance TYPE REF TO ce_cts_hta_translation,

    "! Reads the value of HANA_ACTIVATE_WITH_HINTS from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, abap_false is returned.<br/>
    "! This value is used during activation of objects.<br/>
    "! Supported values are: 'Y' or 'X' for abap_true and 'N' or '' for abap_false. All other values also result in abap_true (default).
    read_activate_with_hints
      RETURNING
        VALUE(r_result) TYPE abap_bool,

    "! Reads the value of ACTIVATION_MODE from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, co_hot_activation_mode_ok is returned.<br/>
    "! This value is used during activation of objects.<br/>
    "! Support values are:
    "! <ul><li>0 - co_hot_activation_mode_ok (default)</li>
    "! <li>1 - co_hot_activation_mode_ok_rec</li>
    "! <li>2 - co_hot_activation_mode_all</li></ul>
    read_activation_mode
      RETURNING
        VALUE(r_result) TYPE char1,

    "! Reads the value of DEPLOY_MODE_DEFAULT from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, co_hot_deploy_mode_always is returned.<br/>
    "! This value is to be used for HOT_ACTIVATION_MODE in CTS_HOT_PACKAGE.
    read_default_deploy_mode
      RETURNING
        VALUE(r_result) TYPE cts_hot_activation_mode,

    "! Reads the value of TRANSLATION_RELEVANCE_DEFAULT from table CTS_HOT_PARAMS and returns it. If the value entered by the
    "! customer is unknown/unsupported or customer did not enter anything, co_hot_relevant_for_transl is returned.<br/>
    "! This value is to be used for ABAP_NO_TRANSLATION in CTS_HOT_PACKAGE.
    read_default_translation_relev
      RETURNING
        VALUE(r_result) TYPE cts_hot_abap_no_translation,

    "! Reads the value of MAX_NR_ACTIVATION_ATTEMPTS from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, the default is returned, 10.<br/>
    "! This value is used during activation of objects.
    read_max_nr_activation_atempts
      RETURNING
        VALUE(r_result) TYPE i,

    "! Activates a package in HTA after snote implementation. Move inactive to active (ABAP_STATUS).<br/>
    "! Also updates smodi entries.<br/>
    "!
    "! Only to be called in SNote/CWB cases!!!
    activate_package_cwb_snote
      IMPORTING
        i_package TYPE cts_hot_package_id,

    "! Activates an object in HTA after snote implementation. (Move inactive to active (ABAP_STATUS).)<br/>
    "! Also updates smodi entries.<br/>
    "!
    "! Only to be called in SNote/CWB cases!!!
    activate_object_cwb_snote
      IMPORTING
        i_object TYPE REF TO cl_cts_hot_object_v1,

    "! Deletes entries in SMODI tables for passed objects. (Should be called after deletions in any cases)
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

    "! Execute commit work.
    commit_work,

    "! Reads the value of RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS from table CTS_HOT_PARAMS and returns it.<br/>
    "! If the key RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS is not existing in table CTS_HOT_PARAMS 'X' is returned,
    "! because this is our delivered default because key is not existing at customers and we can't deliver it<br/>
    "! This value is used to plan/not plan the job for deployment of failed objects.<br/>
    "! Possible values:
    "! <ul><li>R=Only Repository objects to be considered for redeploy_failed</li>
    "!     <li>H=Only HDI objects only to be considered for redeploy_failed</li>
    "!     <li>X or no entry=Repository objects and HDI objects to be considered for redeploy_failed</li></ul>
    get_run_job_redeploy_failed
      RETURNING
        VALUE(r_result) TYPE char1,

    "! Reads the value of HTA_ACTIVATION_MODE_REDEPLOY_FAILED from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, co_hot_activation_mode_ok is returned.<br/>
    "! This value is used during activation of objects in job for retrying deployment for failed objects.
    read_activation_mode_rf
      RETURNING
        VALUE(r_result) TYPE char1,

    "! Reads the value of MAX_NO_ACTIVATION_ATTEMPTS_REDEPL_FAILED from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, 0 is returned.<br/>
    "! This value is used during activation of objects in job for retrying deployment for failed objects.
    read_max_nr_act_attempts_rf
      RETURNING
        VALUE(r_result) TYPE i,

    "! Reads the value of MAX_NO_OF_CHARS_LOGGED_PER_ERROR_WARNING from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is not an integer or customer did not enter anything, 10000 is returned as default. However, if the customer enters a valid integer lower
    "! than 310, then 310 is used, to have at least 3 lines of logging per CheckResult<br/>
    "! This value is used during logging to limit the logging per CheckResult to the configured number of characters.<br/>
    "! This value is only intended to be used for error/warning messages returned by HANA. For normal messages a much lower not
    "! configurable default exists.
    read_max_nr_of_chars_for_log
      RETURNING
        VALUE(r_result) TYPE i,

    "! Returns abap_true if there exists at least 1 broken object/package in HTA tables. (HOT_STATUS = 'E' or 'Z')
    exist_broken_object_or_package
      RETURNING
        VALUE(r_result) TYPE abap_bool,

    "! Updates passed package in CTS_HOT_PACKAGE with new data for passed fields,
    "! but only if i_old_package-hot_status was not yet changed (e.g. in parallel by TP)
    "! to prevent wrong status for very new imported object
    "! @parameter i_old_package | Package for which fields should be changed.
    "!                            Only key fields, hot_status, abap_deployed_at and abap_deployed_by are required to be filled.
    "! @parameter i_new_status | New status to be set.
    "! @parameter i_new_deployed_at | Timestamp when deployment was done. If not set, field will not be updated.
    "! @parameter i_new_deployed_by | User who did deployment. If not set, field will not be updated.
    update_package_after_deploymnt
      IMPORTING
        i_old_package     TYPE cts_hot_package
        i_new_status      TYPE cts_hot_object_status
        i_new_deployed_at TYPE timestampl OPTIONAL
        i_new_deployed_by TYPE cts_hot_abap_deployed_by OPTIONAL,

    "! Updates hot_status of passed object after a failed deployment to i_new_status but only if i_old_object-hot_status and i_old_object-hana_source_object_version
    "! was not yet changed (e.g. in parallel by TP) to prevent wrong status for very new object
    "! @parameter i_old_object | Object for which hot_status should be changed.
    "!                           Only key fields, hot_status and hana_source_object_version are required
    "! @parameter i_new_status | New status to be set
    update_object_after_failed_dep
      IMPORTING
        i_old_object TYPE cts_hot_object
        i_new_status TYPE cts_hot_object_status,

    "! Updates object in cts_hot_object after successful deployment with hot_status = if_cts_hot_db_access=&gt;co_hot_status_active
    "! and with passed data. But update is only done if i_old_object-hot_status and i_old_object-hana_source_object_version
    "! was not yet changed (e.g. in parallel by TP) to prevent wrong status for very parallel imported new object_version
    "! @parameter i_old_object | Object for which hot_status should be changed.
    "!                           Only key fields, hot_status and hana_source_object_version are required
    "! @parameter i_new_deployed_at | Timestamp when deployment was done.
    "! @parameter i_new_deployed_by | User who did deployment.
    "! @parameter i_new_hana_activated_at | Timestamp from HANA for activation time.
    "! @parameter i_new_hana_activated_by | User form HANA who did activation.
    "! @parameter i_new_hana_object_version | Version from HANA that was created during activation.
    update_object_after_succes_dep
      IMPORTING
        i_old_object              TYPE cts_hot_object
        i_new_deployed_at         TYPE timestampl
        i_new_deployed_by         TYPE cts_hot_abap_deployed_by
        i_new_hana_activated_at   TYPE timestampl
        i_new_hana_activated_by   TYPE cts_hot_hana_activated_by
        i_new_hana_object_version TYPE cts_hot_hana_object_version,

    "! Reads the value of MAX_NR_ATTEMPTS_HDI_MAKE from table CTS_HOT_PARAMS and returns it. If the value entered by the customer
    "! is unknown/unsupported or customer did not enter anything, then the parameter MAX_NR_ACTIVATION_ATTEMPTS is read awith
    "! read_max_nr_activation_atempts( ) which might result in their default, 10<br/>
    "! This value is used for make of HDI objects.
    read_max_nr_attempts_hdi_make
      RETURNING
        VALUE(rv_max_attempts) TYPE i.
ENDINTERFACE.