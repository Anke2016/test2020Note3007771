"! Interface for the "entry" to HANA Transport for ABAP (HTA) API.<br/>
"! <br/>
"! To create an instance of this interface use CL_CTS_HTA_API_FACTORY=&gt;create_instance( ).<br/>
"! <br/>
"! Do not implement this interface but use CL_CTS_HTA_API_FACTORY to get an instance of it.<br/>
"! Exception: In test code (TDD), you can implement it.<br/>
"! <br/>
"! This interface might be extended in next deliveries.
INTERFACE if_cts_hta_api_factory
  PUBLIC .

  METHODS:
    "! Creates an instance of if_cts_hta_object with the passed key data (case sensitive!).<br/>
    "! During object creation it is not checked whether the object really exists in HANA repository or in HTA repository.
    "!
    "! @parameter i_hana_package_name  | Name of the package in HANA repository where the object resides (e.g. com.demo.package). Case sensitive!
    "! @parameter i_hana_object_name   | Name of the object in HANA repository (e.g. AT_VIEW1). Case sensitive!
    "! @parameter i_hana_object_suffix | Suffix of the object in HANA repository (e.g. attributeview). Case sensitive!
    "! @parameter r_result             | Instance of if_cts_hta_object
    "! @raising cx_cts_hta_hashing     | In case of an error during hashing of HANA package name or HANA object name and object suffix.
    "!                                   Hashing is done if package is longer than 40 chars or object name+suffix is longer than 70 chars
    "! @raising cx_cts_hta             | Base HTA exception in case of other error than cx_cts_hta_hashing (currently not used)
    create_object_by_hana_name
      IMPORTING
        i_hana_package_name  TYPE if_cts_hta_types=>ty_hana_package_name
        i_hana_object_name   TYPE if_cts_hta_types=>ty_hana_object_name
        i_hana_object_suffix TYPE if_cts_hta_types=>ty_hana_object_suffix
      RETURNING
        VALUE(r_result)      TYPE REF TO if_cts_hta_object
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_object for the passed transport object name.<br/>
    "! Object creation is only successful if a hana object with the passed transport object name exists in HTA repository.
    "! Package data AND object data must exist in HTA repository.
    "!
    "! @parameter i_transport_object_name  | Name of the transport object of LIMU HOTO
    "! @parameter r_result                 | Instance of if_cts_hta_object
    "! @raising cx_cts_hta_not_found       | In case the package or object for i_transport_object_name does not exist in HTA repository.
    "! @raising cx_cts_hta                 | Base HTA exception in case of other error than cx_cts_hta_not_found (currently not used)
    create_object_by_trobj_name
      IMPORTING
        i_transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name
      RETURNING
        VALUE(r_result)         TYPE REF TO if_cts_hta_object
      RAISING
        cx_cts_hta_not_found
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_package with the passed HANA package name.<br/>
    "! During object creation it is not checked whether the package really exists in HANA repository or in HTA repository.
    "!
    "! @parameter i_hana_package_name  | Name of the package in HANA repository (e.g. com.demo.package). (case sensitive!)
    "! @parameter r_result             | Instance of if_cts_hta_package
    "! @raising cx_cts_hta_hashing     | In case of an error during hashing of HANA package name. Hashing is done if name is longer than 40 chars
    "! @raising cx_cts_hta             | Base HTA exception in case of other error than cx_cts_hta_hashing (currently not used)
    create_package_by_hana_name
      IMPORTING
        i_hana_package_name TYPE if_cts_hta_types=>ty_hana_package_name
      RETURNING
        VALUE(r_result)     TYPE REF TO if_cts_hta_package
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_package for the passed transport object name.<br/>
    "! Object creation is only successful if a hana package with the passed transport object name exists in HTA repository.
    "!
    "! @parameter i_transport_object_name | Name of the transport object of LIMU HOTP
    "! @parameter r_result                | The created instance
    "! @raising cx_cts_hta_not_found      | In case the package for i_transport_object_name does not exist in HTA repository.
    "! @raising cx_cts_hta                | Base HTA exception in case of other error than cx_cts_hta_not_found (currently not used)
    create_package_by_trobj_name
      IMPORTING
        i_transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name
      RETURNING
        VALUE(r_result)         TYPE REF TO if_cts_hta_package
      RAISING
        cx_cts_hta_not_found
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_component_list and fills it with ALL packages and objects of HTA repository that
    "! belong to passed ABAP packages (devclasses).<br/>
    "! If ALL objects of a HANA package and the package itself are to be returned, the HANA package and the objects are contained
    "! as IF_CTS_HTA_FULL_PACKAGE in the returned list.
    "!
    "! @parameter i_devclasses       | List of devclasses for which the list of packages and objects should be created.
    "! @parameter i_deployable_only  | Setting this to true will return only packages/objects from HTA repository that are not yet deployed to HANA.
    "! @parameter r_result           | The instance of the created if_cts_hta_component_list.
    "! @raising cx_cts_hta_not_found | In case the packages/objects found for devclasses do not exist in HTA repository.
    "! @raising cx_cts_hta           | Base HTA exception in case of other error than cx_cts_hta_not_found (currently not used)
    create_list_by_abap_packages
      IMPORTING
        i_devclasses      TYPE if_cts_hta_types=>ty_devclasses
        i_deployable_only TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_result)   TYPE REF TO if_cts_hta_component_list
      RAISING
        cx_cts_hta_not_found
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_full_package (R3TR HOTA) for the passed i_transport_object_name containing the 1 HANA package (LIMU HOTP)
    "! and all objects existing in this package (LIMU HOTO). Instance is only created if the passed i_transport_object_name can be found in
    "! HTA repository. If the package is found in HTA, all objects from HTA and HANA are returned within this if_cts_hta_full_package<br/>
    "!
    "! @parameter i_transport_object_name | Name of the transport object of R3TR HOTA
    "! @parameter r_result                | The created instance
    "! @raising cx_cts_hta_not_found      | In case the package for i_transport_object_name does not exist in HTA repository.
    "! @raising cx_cts_hta                | Base HTA exception in case of other error than cx_cts_hta_not_found (currently not used)
    create_full_package_trobj_name
      IMPORTING
        i_transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name
      RETURNING
        VALUE(r_result)         TYPE REF TO if_cts_hta_full_package
      RAISING
        cx_cts_hta_not_found
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_component_list.
    "! @parameter r_result | The created instance
    create_component_list
      RETURNING
        VALUE(r_result) TYPE REF TO if_cts_hta_component_list,

    "! Creates an instance of if_cts_hta_component_list and adds all if_cts_hta_full_packages that can be found for the
    "! passed i_hana_package_name.<br/>
    "! The search for packages with i_hana_package_name is either done only in HTA or in HTA and HANA. (parameter i_search_in_hta_only)<br/>
    "! For each found package one instance of if_cts_hta_full_package (R3TR HOTA) is created containing the package as
    "! if_cts_hta_package (LIMU HOTP) and all objects as if_cts_hta_object (LIMU HOTO) that exist for this package in HTA and/or HANA.<br/>
    "! So the returned list might contain if_cts_hta_full_packages that exist only in HTA or only in HANA or a mixture, e.g. some objects exist
    "! only in HTA, some only in HANA<br/>
    "!
    "! @parameter i_hana_package_name  | Name of the package in HANA repository (e.g. com.demo.package). (case sensitive!)<br/>
    "!                                   Use '*' as placeholder, e.g. to find all packages in HTA and HANA below the root package
    "!                                   com.demo use 'com.demo.*'
    "! @parameter i_search_in_hta_only | If set to apab_true, packages will be searched in HTA repository only.<br/>
    "!                                   If set to abap_false, packages will be searched in HTA and HANA<br/>
    "!                                   But in both cases the contained objects are looked up in HTA and HANA.
    "! @parameter r_result             | Instance of if_cts_hta_component_list but only containing if_cts_hta_full_packages
    "! @raising cx_cts_hta_hashing     | In case of an error during hashing of HANA package name. Hashing is done if name is longer than 40 chars
    "! @raising cx_cts_hta             | Base HTA exception in case of other error than cx_cts_hta_hashing
    find_full_packages_by_hana_nam
      IMPORTING
        i_hana_package_name  TYPE if_cts_hta_types=>ty_hana_package_name
        i_search_in_hta_only TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_result)      TYPE REF TO if_cts_hta_component_list
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta_no_hana_database
        cx_cts_hta,

    "! Creates an instance of if_cts_hta_full_package (R3TR HOTA) with the passed HANA package name as if_cts_hta_package(LIMU HOTO).<br/>
    "! The contained objects as if_cts_hta_object (LIMU HOTO) will be looked up in HTA and HANA repository.
    "!
    "! @parameter i_hana_package_name  | Name of the package in HANA repository (e.g. com.demo.package). (case sensitive!)
    "! @parameter r_result             | Instance of if_cts_hta_full_package
    "! @raising cx_cts_hta_hashing     | In case of an error during hashing of HANA package name. Hashing is done if name is longer than 40 chars
    "! @raising cx_cts_hta             | Base HTA exception in case of other error than cx_cts_hta_hashing
    create_full_package_hana_name
      IMPORTING
        i_hana_package_name TYPE if_cts_hta_types=>ty_hana_package_name
      RETURNING
        VALUE(r_result)     TYPE REF TO if_cts_hta_full_package
      RAISING
        cx_cts_hta_hashing
        cx_cts_hta.

ENDINTERFACE.