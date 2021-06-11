INTERFACE if_cts_hot_hana_conn_internal
  PUBLIC .

  TYPES:
*    BEGIN OF ty_cts_hta_package_data,
*      package_id              TYPE cts_hot_hana_package_id,
*      delivery_unit           TYPE cts_hot_pack_delivery_unit,
*      delivery_unit_vendor    TYPE cts_hot_pack_deliv_unit_vendor,
*      description             TYPE cts_hot_hana_pack_description,
*      hints_for_translation   TYPE cts_hot_pack_hints_for_transl,
*      is_structural           TYPE cts_hot_pack_is_structural,
*      original_language       TYPE cts_hot_hana_orig_lang,
*      responsible             TYPE cts_hot_pack_responsible,
*      src_system              TYPE cts_hot_pack_src_system,
*      src_tenant              TYPE cts_hot_pack_src_tenant,
*      text_collection         TYPE cts_hot_pack_text_collection,
*      text_status             TYPE cts_hot_pack_text_status,
*      text_terminology_domain TYPE cts_hot_pack_text_term_domain,
*    END OF ty_cts_hta_package_data,

    ty_cts_hot_objects    TYPE HASHED TABLE OF REF TO cl_cts_hot_object_v1 WITH UNIQUE DEFAULT KEY,
    ty_hana_package_names TYPE STANDARD TABLE OF cts_hot_hana_package_id WITH DEFAULT KEY.

  METHODS:
    "! Reads all objects from HANA into table CTS_HOT_OBJECT
    read_objects_from_hana_to_hot
      IMPORTING
        i_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
      RAISING
        cx_hana_object_transport
        cx_cts_hta_hashing, "##TODO Exception handling ok this way?

    "! Synchronizes passed packages from HANA into table CTS_HOT_PACKAGE
    "! If the package is available in HANA all metadata is inserted (updated) in CTS_HOT_PACKAGE
    "! If the package is not available in HANA (Error_Code 40132) it get deleted in CTS_HOT_PACKAGE
    "!
    "! @parameter i_hana_packages_list | list of packages with names as table of string exactly as in HANA
    sync_packages_from_hana_to_hot
      IMPORTING
        i_hana_packages_list TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
      RAISING
        cx_hana_object_transport, "##TODO Exception handling ok this way?

*    "! Deploys (import/delete and activate) all passed objects (from HOT tables to HANA).
*    "!
*    "! In detail the passed objects will be looked up in HOT table CTS_HOT_OBJECT with specified abap_status ('A' or 'I')
*    "! and then imported and activated in HANA.
*    "! Make sure packages are created before calling this method. (Use method modify_packages_in_hana to create packages)
*    "!
*    "! @parameter i_objects | objects to be deployed (import + activate) to HANA
*    "! @parameter i_abap_status | abap_status ('A' or 'I') to select in table CTS_HOT_OBJECT
*    "! @parameter e_skipped_objects | objects that were skipped because they are already deployed or not in HOT repository
*    "! @parameter e_successfull_objects | objects for which deployment was successful (contains successfully reverted objects and successfully activated objects)
*    "! @parameter e_failed_objects | Objects for which activation or import failed or which could not be deployed due to HOT status does not allow deployment
*    "! @parameter e_deploy_result | deploy result consisting of import results (write to hana) and activation result(s)
*    "! @parameter r_result | if not initial, deployment to hana was done and object is filled with one or more hana activation result(s)
*    "! @raising cx_nhi_hana_repository | in case of exception
*    "! @raising cx_hana_object_transport | in case of exception
*    deploy_objects_to_hana2
*      IMPORTING
*        i_objects             TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
*        i_abap_status         TYPE cts_hot_object-abap_status DEFAULT 'A'
*      EXPORTING
*        e_skipped_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_successfull_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_failed_objects      TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_deploy_result       TYPE cl_cts_hot_hana_connector=>ty_deploy_result "not as returning because logs must also be available in exception case.
**        RETURNING
**          VALUE(r_result)       TYPE ty_deploy_result
*      RAISING
*        cx_nhi_hana_repository
*        cx_hana_object_transport, "##TODO Exception handling (I guess only mapping on own exception possible) outside or inside this class?

    "! Returns all existing active objects of given package (does not store anything in HTA tables)
    "!
    "! @parameter I_hana_package | the package name for which all objects should be returned
    get_objects_of_pkg_from_hana
      IMPORTING
        i_hana_package       TYPE cts_hot_hana_package_id
      RETURNING
        VALUE(r_object_list) TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
      RAISING
        cx_hana_object_transport, "##TODO Exception handling ok this way?

*    "! creates or updates packages in HANA with values read from HOT table CTS_HOT_PACKAGE
*    "!
*    "! In detail the passed packages will be looked up in HOT table CTS_HOT_PACKAGE and then created/updated/deleted in HANA.
*    "!
*    "! @parameter i_itab_packages | packages to be created/updated/deleted in HANA
*    "! @parameter i_abap_status      | abap status to be considered for reading from HOT table CTS_HOT_PACKAGE
*    "! @parameter e_created_packages | successfully created packages
*    "! @parameter e_updated_packages | successfully updated packages
*    "! @parameter e_deleted_packages | successfully deleted packages
*    "! @parameter e_skipped_packages | skipped packages, not existing in HOT repository
*    "! @parameter e_failed_packages  | packages that had errors during processing
*    "! @raising cx_nhi_hana_repository | in case of exception
*    "! @raising cx_hana_object_transport | in case of exception
*    modify_packages_in_hana2
*      IMPORTING
*        i_itab_packages    TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
*        i_abap_status      TYPE cts_hot_package-abap_status DEFAULT 'A'
*      EXPORTING
*        e_created_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
*        e_updated_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
*        e_deleted_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
*        e_skipped_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
*        e_failed_packages  TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
*      RAISING
*        cx_hana_object_transport "##TODO Exception handling ok this way?
*        cx_nhi_hana_repository,
*
    "! Finds active objects in HANA for a specified package and status and returns them as list.
    "!
    "! @parameter i_hana_package_id | package_id in HANA for which all objects should be read
    "! @parameter i_object_status   | "-1" find all objects, "0" find objects with status OK, "1" find objects with status BROKEN, "2" find objects with status NEEDS_REGEN
    "! @parameter r_found_objects   | table with all objects found for input parameter
    "! @raising cx_hana_object_transport | in case of any error during find objects
    find_active_objects_in_hana
      IMPORTING
        i_hana_package_id      TYPE string
        i_object_status        TYPE string
      RETURNING
        VALUE(r_found_objects) TYPE cl_nhi_object_id_and_caption=>ty_objlist_caption
      RAISING
        cx_hana_object_transport,

*    "! Deploy all texts for passed objects and languages
*    "!
*    "! @parameter i_hot_objects_with_lang      | table with hot objects and the language to be deployed.
*    "!                                           If language table is initial all languages will be deployed
*    "! @parameter e_not_active_objects         | Objects for which no text deployment was executed because objects are not active (HOT_STATUS &lt;&gt; A and &lt;&gt;N)
*    "! @parameter e_unknown_objects            | Objects for which no text deployment was executed because objects are unknown in HTA
*    "! @parameter e_skipped_objects            | Objects for which no text deployment was executed because either all texts already deployed or no texts for deployment at all
*    "! @parameter e_failed_text_deploy_result  | Objects for which text deployment failed with HANA Error_Code not 0.<br/>
*    "!                                           If an object failed for several languages, then several entries exist for same object.
*    "! @parameter e_success_text_deploy_result | Objects for which text deployment was successfully executed with the number of texts deployed per language.<br/>
*    "!                                           (With regards to number of imported texts, HTA does not distinguish between object and content texts, so one line
*    "!                                            in result contains number of object texts plus number of content texts per language)
*    "! @raising cx_nhi_hana_repository         | in case of exception
*    "! @raising cx_hana_object_transport       | in case of exception
*    deploy_object_texts2
*      IMPORTING
*        i_hot_objects_with_lang      TYPE cl_cts_hot_hana_connector=>ty_text_deploy_inputs
*      EXPORTING
*        e_not_active_objects         TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_unknown_objects            TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_skipped_objects            TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
*        e_success_text_deploy_result TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results
*        e_failed_text_deploy_result  TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results
*      RAISING
*        cx_nhi_hana_repository
*        cx_hana_object_transport,
*
*    "! For all passed packages compare HTA repository with HANA repository and add all objects that are in HANA but not in HTA as deletions to CTS_HOT_OBJECT.
*    "!
*    "! @parameter i_packages |
*    "! @parameter r_object_list | Objects that exist in HANA but not in HTA and which were added as deletions to CTS_HOT_OBJECTS table.
*    check_hana_for_obosolete_objs2
*      IMPORTING
*        i_packages           TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
*      RETURNING
*        VALUE(r_object_list) TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
*      RAISING
*        cx_hana_object_transport.
*
    "! Lists all packages from HANA that match the given i_hana_package_name.<br/>
    "! Packages that are flagged as not transportable are NOT returned.
    "!
    "! @parameter i_hana_package_name | Name of the package in HANA repository (e.g. com.demo.package). (case sensitive!)<br/>
    "!                                  Use '*' as placeholder, e.g. to find all packages in HTA and HANA below the root package
    "!                                  com.demo use 'com.demo.*'
    "! @parameter r_result            | All packages found for i_hana_package_name.
    "! @raising cx_hana_object_transport | in case of problems during list of packages (might contain original exception as previous exception)
    list_hana_packages
      IMPORTING
        i_hana_package_name TYPE cts_hot_hana_package_id
      RETURNING
        VALUE(r_result)     TYPE ty_hana_package_names
      RAISING
        cx_hana_object_transport,

    "! Reads all package data (details) from HANA for given package_id and returns it.<br/>
    "! !!!Does not store anything in HTA DB tables!!!
    "!
    "! @parameter i_hana_package_id | HANA package for which data should be read. case-sensitive!!!
    "! @parameter r_package_data | the details of the package read from HANA or initial if package does not exist or is not transportable.
    "! @raising cx_hana_object_transport | in case of problems during read of package data (might contain original exception as previous exception)
    read_package_data_from_hana
      IMPORTING
        i_hana_package_id     TYPE cts_hot_hana_package_id
      RETURNING
        VALUE(r_package_data) TYPE cts_hot_package
      RAISING
        cx_hana_object_transport,

    "! Reads object data from HANA for given object and returns it.<br/>
    "! Does not read cdata and bdata!<br/>
    "! !!!Does not store anything in HTA DB tables!!!
    "!
    "! @parameter i_cts_hot_object | Object for which data should be read
    "! @parameter r_object_data | the details of the object read from HANA or initial if object does not exist in HANA
    "! @raising cx_hana_object_transport | in case of problems during read of object data (might contain original exception as previous exception)
    read_object_data_from_hana
      IMPORTING
        i_cts_hot_object     TYPE REF TO cl_cts_hot_object_v1
      RETURNING
        VALUE(r_object_data) TYPE cts_hot_object
      RAISING
        cx_hana_object_transport.
ENDINTERFACE.