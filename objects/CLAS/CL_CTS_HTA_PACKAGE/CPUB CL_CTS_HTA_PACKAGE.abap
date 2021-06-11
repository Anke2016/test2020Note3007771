"! Implementation for IF_CTS_HTA_PACKAGE.<br/>
"! See IF_CTS_HTA_PACKAGE for more details.
CLASS cl_cts_hta_package DEFINITION
  PUBLIC
  INHERITING FROM cl_cts_hta_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_package.

    CLASS-METHODS class_constructor.

    "! Creates an instance of if_cts_hta_package using HANA package name as input.<br/>
    "! Should only be used by cl_cts_hta_api_factory because of caching.
    "!
    "! @parameter i_hana_package_name | HANA package name for which instance should be created. Case sensitive!
    "! @parameter r_result | The created instance
    "! @raising cx_cts_hta_hashing | In case of an error during hashing of HANA package name. Hashing is done if name is longer than 40 chars
    "! @raising cx_cts_hta | Base HTA exception in case of other error than cx_cts_hta_hashing (currently not used)
    CLASS-METHODS create_instance_from_hana_key
      IMPORTING
        i_hana_package_name TYPE cts_hot_hana_package_id
      RETURNING
        VALUE(r_result)     TYPE REF TO if_cts_hta_package
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta.

    "! Creates an instance of if_cts_hta_package using a transport object name as input.<br/>
    "! Should only be used by cl_cts_hta_api_factory because of caching.
    "!
    "! @parameter i_transport_object_name | Name of the transport object (LIMU HOTP)
    "! @parameter i_abap_status | ABAP_STATUS of the package to read from HTA. Do not expose in API, usage internally only for SNote and CWB.
    "! @parameter r_result | The created instance
    "! @raising cx_cts_hta_not_found | In case the package for i_transport_object_name does not exist in HTA repository.
    CLASS-METHODS create_instance_from_obj_name
      IMPORTING
        i_transport_object_name TYPE trobj_name
        i_abap_status           TYPE c DEFAULT co_active_version
      RETURNING
        VALUE(r_result)         TYPE REF TO if_cts_hta_package
      RAISING
        cx_cts_hta_not_found.

    "! Creates instances of if_cts_hta_package for passed devclasses.<br/>
    "! 1. all R3TR HOTAs are read from tadir for passed devclasses<br/>
    "! 2. For HOTAs found in tadir check cts_hot_package and return all packages depending on i_deployable_only<br/>
    "! Should only be used by cl_cts_hta_api_factory because of caching.
    "!
    "! @parameter i_devclasses | Name of the devclasses to read R3TR HOTAs from tadir
    "! @parameter i_deployable_only | If set to abap_true, only those packages are returned that have ABAP_STATUS = 'A'
    "!                                but HOT_STATUS NOT 'A', if not set, ALL packages with ABAP_STATUS = 'A' are returned
    "! @parameter r_result | The created instances
    "! @raising cx_cts_hta_not_found | In case a package does not exist in HTA repository.
    CLASS-METHODS create_instances_from_devclass
      IMPORTING
        i_devclasses      TYPE if_cts_hta_types=>ty_devclasses
        i_deployable_only TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_result)   TYPE if_cts_hta_types=>ty_cts_hta_packages
      RAISING
        cx_cts_hta_not_found.

    "! Creates instances of if_cts_hta_package for passed i_hana_package_name that could contain '*' as placeholder.<br/>
    "! Should only be used by cl_cts_hta_api_factory because of caching.
    "!
    "! @parameter i_hana_package_name | HANA package name for which instances should be created. Case sensitive! '*' supported.
    "! @parameter i_search_in_hta_only | If set to apab_true, packages will be searched in HTA repository only.<br/>
    "!                                   If set to abap_false, packages will be searched in HTA and HANA<br/>
    "! @parameter r_result | The created instances
    "! @raising cx_cts_hta_hashing | In case of an error during hashing of HANA package name. Hashing is done if name is longer than 40 chars
    "! @raising cx_cts_hta | In case of problems reading data from HANA
    CLASS-METHODS create_instances_from_hana_key
      IMPORTING
        i_hana_package_name  TYPE if_cts_hta_types=>ty_hana_package_name
        i_search_in_hta_only TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_result)      TYPE if_cts_hta_types=>ty_cts_hta_packages
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta_no_hana_database
        cx_cts_hta.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.
    METHODS: if_cts_hta_component~get_sync_state REDEFINITION.