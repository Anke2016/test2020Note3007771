INTERFACE if_cts_hta_types
  PUBLIC .

  TYPES:
    "! Name of a package in HANA repository, case sensitive!
    ty_hana_package_name     TYPE cts_hot_hana_package_id,
    "! Name of an object in HANA repository, case sensitive!
    ty_hana_object_name      TYPE cts_hot_hana_object_name,
    "! Suffix of an object in HANA repository, case sensitive! (e.g. attributeview, calculationview, ...)
    ty_hana_object_suffix    TYPE cts_hot_hana_object_suffix,
    "! Name of a transport object in transport requests/tasks
    ty_transport_object_name TYPE trobj_name,
    "! Status/Result of a deployment.<br/>
    "!  'I' for deployment was successful (RC = 0)<br/>
    "!  'W' for deployment ended with warning(s) (RC = 4)<br/>
    "!  'E' for deployment ended with error(s) (RC = 8)<br/>
    "!  'A' for deployment was aborted/fatal error (RC = 12)<br/>
    ty_deploy_status         TYPE symsgty,
    "! Single deploy message (protocol), created during deployment of HANA packages/objects
    ty_deploy_message        TYPE sprot_u,
    "! Table of all messages (T100) that are created during a deployment of HANA packages/objects.
    ty_deploy_messages       TYPE STANDARD TABLE OF ty_deploy_message WITH DEFAULT KEY,
    "! Devclass (tadir entry)
    ty_devclass              TYPE devclass,
    "! List of devclasses
    ty_devclasses            TYPE STANDARD TABLE OF ty_devclass WITH DEFAULT KEY,
    "! List of hta components
    ty_cts_hta_components    TYPE STANDARD TABLE OF REF TO if_cts_hta_component WITH DEFAULT KEY,
    "! List of hta packages (LIMU HOTP)
    ty_cts_hta_packages      TYPE STANDARD TABLE OF REF TO if_cts_hta_package WITH DEFAULT KEY,
    "! List of hta objects (LIMU HOTO)
    ty_cts_hta_objects       TYPE STANDARD TABLE OF REF TO if_cts_hta_object WITH DEFAULT KEY,
    "! List of hta full packages (R3TR HOTA)
    ty_cts_hta_full_packages TYPE STANDARD TABLE OF REF TO if_cts_hta_full_package WITH DEFAULT KEY,
    "! Flag for prework was done or not done on package level.<br/>
    "! Values are co_prework_done or co_prework_not_done
    ty_prework_flag          TYPE cts_hot_prework_flag,
    "! Transport request/task
    ty_trkorr                TYPE trkorr,
    "! Mapping transport request and HTA components
    BEGIN OF ty_sync_result,
      trkorr         TYPE ty_trkorr,
      hta_components TYPE ty_cts_hta_components,
    END OF ty_sync_result,
    "! Sorted table for mapping of trkorr with its HTA components
    ty_sync_results TYPE SORTED TABLE OF ty_sync_result WITH UNIQUE KEY trkorr,
    "! List of cx_cts_hta exceptions
    ty_cx_cts_htas  TYPE STANDARD TABLE OF REF TO cx_cts_hta WITH DEFAULT KEY.

  TYPES:
    "! Type for key fields of an object of type IF_CTS_HTA_OBJECT.
    BEGIN OF ty_cts_hta_object_key,
      "! Name of a package in HANA repository, case sensitive!
      hana_package_name  TYPE ty_hana_package_name,
      "! Name of an object in HANA repository, case sensitive!
      hana_object_name   TYPE ty_hana_object_name,
      "! Suffix of an object in HANA repository, case sensitive! (e.g. attributeview, calculationview, ...)
      hana_object_suffix TYPE ty_hana_object_suffix,
    END OF ty_cts_hta_object_key.

ENDINTERFACE.