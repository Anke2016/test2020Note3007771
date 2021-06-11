"! Implementation for IF_CTS_HTA_OBJECT.<br/>
"! See IF_CTS_HTA_OBJECT for more details.
CLASS cl_cts_hta_object DEFINITION
  PUBLIC
  INHERITING FROM cl_cts_hta_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_object.

    DATA:
      "! Internal usage in HTA only...
      m_hot_object  TYPE REF TO cl_cts_hot_object_v1 READ-ONLY. " reuse old classes for now as they are used everywhere...

    "! Creates an instance of if_cts_hta_object using a HTA Package and HANA object name and HANA object suffix as input.
    "!
    "! @parameter i_hta_package | HTA package representing the HANA package name for which object instance should be created.
    "! @parameter i_hana_object_name | HANA object name for which instance should be created. Case sensitive!
    "! @parameter i_hana_object_suffix | HANA object suffix for which instance should be created. Case sensitive!
    "! @parameter r_result | The created instance
    "! @raising cx_cts_hta_hashing | In case of an error during hashing of HANA object name and object suffix. Hashing is done if name+suffix is longer than 70 chars
    "! @raising cx_cts_hta | Base HTA exception in case of other error than cx_cts_hta_hashing (currently not used)
    CLASS-METHODS create_instance_from_hana_key
      IMPORTING
        i_hta_package        TYPE REF TO if_cts_hta_package
        i_hana_object_name   TYPE cts_hot_hana_object_name
        i_hana_object_suffix TYPE cts_hot_hana_object_suffix
      RETURNING
        VALUE(r_result)      TYPE REF TO if_cts_hta_object
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta.

    "! Creates an instance of if_cts_hta_object using a transport object name as input.
    "!
    "! @parameter i_transport_object_name | Name of the transport object (LIMU HOTO)
    "! @parameter i_hta_package | HTA package representing the HANA package name for which instance should be created.
    "! @parameter i_abap_status | ABAP_STATUS of the object to read from HTA. Do not expose in API, usage internally only for SNote and CWB.
    "! @parameter r_result | The created instance
    "! @raising cx_cts_hta_not_found | In case the object for i_transport_object_name does not exist in HTA repository.
    "! @raising cx_cts_hta           | In other cases than cx_cts_hta_not_found. (so far not used)
    CLASS-METHODS create_instance_from_obj_name
      IMPORTING
        i_transport_object_name TYPE trobj_name
        i_hta_package           TYPE REF TO if_cts_hta_package
        i_abap_status           TYPE c DEFAULT co_active_version
      RETURNING
        VALUE(r_result)         TYPE REF TO if_cts_hta_object
      RAISING
        cx_cts_hta_not_found
        cx_cts_hta.

    "! Creates instances of if_cts_hta_object that belong to passed package.<br/>
    "! Depending on i_search_in_hta_only, only objects in HTA repository are taken into account or also objects in HANA are
    "! returned, but only if they are not yet part of r_result (meaning do not exist in HTA yet)!<br/>
    "! If i_deployable_only is set to true, parameter i_search_in_hta_only is ignored (assumed as abap_true) because
    "! use case is deployment.
    "!
    "! @parameter i_cts_hta_package  | The package the objects to be created belong to
    "! @parameter i_deployable_only  | Whether only objects should be read that are not yet deployed.
    "! @parameter i_search_in_hta_only | If set to apab_true, packages will be searched in HTA repository only.<br/>
    "!                                   If set to abap_false, packages will be searched in HTA and HANA<br/>
    "! @parameter e_all_objects_of_package | abap_true if all objects for passed package are returned or abap_false if not all objects are returned (e.g. maybe already deployed).
    "! @parameter r_result           | The created instances
    "! @raising cx_cts_hta_not_found | In case an object does not exist in HTA repository.
    "! @raising cx_cts_hta           | In other cases than cx_cts_hta_not_found. (so far not used)
    CLASS-METHODS create_instances_from_package
      IMPORTING
        i_cts_hta_package        TYPE REF TO if_cts_hta_package
        i_deployable_only        TYPE abap_bool OPTIONAL
        i_search_in_hta_only     TYPE abap_bool DEFAULT abap_true
      EXPORTING
        e_all_objects_of_package TYPE abap_bool
      RETURNING
        VALUE(r_result)          TYPE if_cts_hta_types=>ty_cts_hta_objects
      RAISING
        cx_cts_hta.

    CLASS-METHODS class_constructor.

    METHODS: if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.
    METHODS: if_cts_hta_component~get_sync_state REDEFINITION.