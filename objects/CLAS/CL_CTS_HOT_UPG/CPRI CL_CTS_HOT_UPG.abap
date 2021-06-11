  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_package,
             abap_hana_package_id TYPE cts_hot_package-abap_hana_package_id,
             hana_package_id      TYPE cts_hot_package-hana_package_id,
           END OF ty_s_package.

    METHODS:
      constructor
        IMPORTING
          ir_logger TYPE REF TO lcl_logger OPTIONAL
          ir_helper TYPE REF TO lcl_helper OPTIONAL,

      "! Checks that all HANA objects are deployed, meaning they have either HOT_STATUS 'A' or 'N'<br/>
      "! If there are objects with another HOT_STATUS, an error message is logged and these objects are also logged
      "! @parameter rv_success | abap_true if all objects are deployed, abap_false if at least one object is not deployed
      check_not_deployed_objects
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! Checks that all HANA packages are deployed, meaning they have either HOT_STATUS 'A' or 'N'<br/>
      "! If there are packages with another HOT_STATUS, an error message is logged and these packages are also logged
      "! @parameter rv_success | abap_true if all packages are deployed, abap_false if at least one package is not deployed
      check_not_deployed_packages
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! Checks that for all HANA packages with activation mode P (prework required) the prework is set to done.<br/>
      "! If there are packages with missing prework, an error message is logged and the packages with missing prework are also logged
      "! @parameter rv_success | abap_true if prework is set for all packages, abap_false if at least one package has missing prework
      check_missing_prework_for_pkgs
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! Checks whether the objects known in HTA have same version in HANA Repository. First by comparing the version number used by
      "! HANA repository. If the version is different, compare the content (cdata and bdata). If the content is different, consider
      "! this object as different in HTA and HANA.<br/>
      "! Only the objects are checked for which their packages have prework set to done<br/>
      "! Based on parameter iv_check_consistency the objects that are different are logged or not.<br/>
      "! @parameter iv_all_objects_deployed | Indicates whether all objects in HTA are deployed or not. (result of check_not_deployed_objects)
      "!                                      Parameter is used to log different detail message in case all other objects are same in HTA and HANA
      "! @parameter iv_check_consistency | If abap_true, the method is executed as part of check_consistency and all
      "!                                   objects that are different in HTA and HANA Repository are logged. Only objects
      "!                                   with HOT_STATUS 'A' or 'N' are checked<br/>
      "!                                   If abap_false, the method is executed as part of prepare_redeployment. In this
      "!                                   scenario no logging of object differences should be made (apart from exceptions),
      "!                                   execution should stop with first difference found. All HOT_STATUS apart from
      "!                                   'D' and 'Z' are taken into account.
      "! @parameter rv_success | abap_true if all objects are same in HTA and HANA, abap_false if at least one object is different in HANA and HTA
      check_object_versions
        IMPORTING
          iv_all_objects_deployed TYPE abap_bool
          iv_check_consistency    TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_success)       TYPE abap_bool,

      check_bck_table_existence
        RAISING
          lcx_error_prepare_redeployment,

      "! Find all packages/objects that only exist in ~bck table (V2) and add them to cts_hot_* table (V1) as to be deleted
      add_to_be_deleted_pkgs_objs
        RAISING
          lcx_error_prepare_redeployment,

      "! If any package is different in HANA and HTA, set all packages to HOT_STATUS=I but not packages with HOT_STATUS='D' or 'Z'.<br/>
      "! If any object is different in HANA and HTA, set all objects to HOT_STATUS=I but not objects with HOT_STATUS='D' or 'Z'.<br/>
      set_to_be_deployed_pkgs_objs
        RAISING
          lcx_error_prepare_redeployment,

      execute_query
        IMPORTING
          iv_query  TYPE string
        EXPORTING
          et_result TYPE ANY TABLE
        RAISING
          cx_sql_exception.

    DATA:
      mr_helper                   TYPE REF TO lcl_helper,
      mr_logger                   TYPE REF TO lcl_logger,
      "! Filled in method check_missing_prework
      mt_packages_without_prework TYPE STANDARD TABLE OF ty_s_package.