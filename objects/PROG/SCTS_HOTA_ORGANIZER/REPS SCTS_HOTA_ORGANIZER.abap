REPORT scts_hota_organizer.

DATA: gv_sync TYPE c,
      gv_depl      TYPE c,
      gv_pack(255) TYPE c.

INTERFACE lif_data_provider.

  METHODS:
    "! Finds active objects in HANA for a specified package and status and returns them as list.
    "!
    "! @parameter i_hana_package_id | package_id in HANA for which all objects should be read
    "! @parameter i_object_status | "-1" find all objects, "0" find objects with status OK, "1" find objects with status BROKEN, "2" find objects with status NEEDS_REGEN
    "! @parameter r_objects | table with all objects found for input parameter
    find_active_objects_in_hana
      IMPORTING
        i_hana_package_id TYPE string
        i_object_status   TYPE string
      RETURNING
        VALUE(r_objects)  TYPE cl_nhi_object_id_and_caption=>ty_objlist_caption
      RAISING
        cx_hana_object_transport.

ENDINTERFACE.

CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_data_provider.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.

  METHOD lif_data_provider~find_active_objects_in_hana.

    r_objects = cl_cts_hot_hana_connector=>create_instance( )->find_active_objects_in_hana(
                                  i_hana_package_id = i_hana_package_id
                                  i_object_status   = i_object_status ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_deployment DEFINITION DEFERRED.
CLASS lcl_hota_organizer DEFINITION FRIENDS ltc_deployment.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_filter_data,
             show_green           TYPE abap_bool,
             show_yellow          TYPE abap_bool,
             show_red             TYPE abap_bool,
             show_inactive        TYPE abap_bool,
             nr_packages_green    TYPE i,
             nr_packages_yellow   TYPE i,
             nr_packages_red      TYPE i,
             nr_packages_inactive TYPE i,
             nr_objects_green     TYPE i,
             nr_objects_yellow    TYPE i,
             nr_objects_red       TYPE i,
             nr_objects_inactive  TYPE i,
           END OF ty_filter_data.

    DATA:
      "! Stores the search/select string of the user
      mv_search_string       TYPE string READ-ONLY,
      "! Stores whether subpackages should also be taken into account
      mv_include_subpackages TYPE abap_bool READ-ONLY.

    METHODS:
      constructor
        RAISING
          cx_hana_object_transport,

      "! Displays the data in hierarchy table
      display_data,

      "! Read user values for passed parameters if not initial and returns in corresponding changing parameter
      read_user_value
        IMPORTING
          i_param_name    TYPE string
        RETURNING
          VALUE(r_result) TYPE string,

      "! Displays f4 help for packages using passed package id as search string and returns user selected value.
      "! F4 help shows all packages from HANA repository AND all packages available in HOT that are not in HANA
      f4help_packages
        IMPORTING
          i_hana_package_name   TYPE cts_hot_package-hana_package_id
          i_include_subpackages TYPE abap_bool
        RETURNING
          VALUE(r_result)       TYPE cts_hot_package-hana_package_id
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Reads data from HANA repository and HOTA repository and stores them in mt_master and mt_slave. Also sets the sync status.
      select_data
        IMPORTING
          i_hana_package_name   TYPE cts_hot_package-hana_package_id
          i_include_subpackages TYPE abap_bool
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Filter packages/objects using the passed selection. Each importing parameter might contain 'X'
      "! for checkbox is checked (display) or not 'X' for not checked (do not display)
      "! @parameter i_green | if 'X', show green packages/objects, else do not show green packages/objects
      "! @parameter i_yellow | if 'X', show yellow packages/objects, else do not show yellow packages/objects
      "! @parameter i_red | if 'X', show red packages/objects, else do not show red packages/objects
      "! @parameter i_inactive | if 'X', show inactive packages/objects, else do not show inactive packages/objects
      apply_filter
        IMPORTING
          i_green    TYPE c
          i_yellow   TYPE c
          i_red      TYPE c
          i_inactive TYPE c,

      "! Returns filter data about which filter is active as well as how many objects / packages are there per status
      get_filter_data
        RETURNING VALUE(r_filter_data) TYPE ty_filter_data,

      is_data_filtered
        RETURNING
          VALUE(r_result) TYPE abap_bool,

      sync_data,

      "! Selects package and its objects for the package/object the cursor is currently placed in
      select_all_for_package,

      "! Deselect package and its objects for the package/object the cursor is currently placed in
      deselect_all_for_package,

      preselect_all_out_of_sync,

      select_all,

      deselect_all,

      refresh_data,

      "! Reads current selected cell (package or object) and displays versioning UI for this.
      show_versioning_for_currnt_cll,

      hide_name_columns,

      show_name_columns,

      deploy,

      set_deploy_mode,

      set_transl_mode.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_hana_package_id,
        hana_package_id TYPE cts_hot_package-hana_package_id,
      END OF ty_hana_package_id.
    TYPES: tt_cts_hot_hana_package_id TYPE STANDARD TABLE OF cts_hot_hana_package_id,
           ty_trkorrs_sorted          TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line.
    TYPES: BEGIN OF g_type_s_master.    " package
    " icon_led_green (in sync/ not deployed), icon_led_yellow (out of sync/deployable),
    " icon_led_red (not syncable/not deployable) icon_led_inactive (not syncable/ not deployable)
    TYPES: sync_deploy_state            TYPE icon_d,
           "! Transport Object name (HANA package in upper case or with hash)
           transport_obj_name           TYPE cts_hot_package_id,
           "! package_id to display in ALV List. Either package id from HANA or if package only exists in HOTA, the package_id from HOTA. Important: This package_id must not be shown in details view!
           "! This package_id is required to be able to have &lt;unbekannt&gt; for hana_package_id or hot_hana_package_id in details view.
           package_id                   TYPE cts_hot_hana_package_id,
           "! Name of the hana package in HANA
           hana_package_id              TYPE cts_hot_hana_package_id,
           "! name of hana package in HOTA repository that is already known for the transport_obj_name  or initial if it is equal to hana_package_id
           hot_hana_package_id          TYPE cts_hot_hana_package_id,
           "! Devclass in tadir
           devclass                     TYPE tadir-devclass,
           "! HANA package description in HANA
           hana_description             TYPE cts_hot_hana_pack_description,
           "! HANA package description in HOTA
           hot_description              TYPE cts_hot_hana_pack_description,
           "! HANA package src_system in HANA
           hana_src_system              TYPE cts_hot_pack_src_system,
           "! HANA package src_system in HOTA
           hot_src_system               TYPE cts_hot_pack_src_system,
           "! HANA package src_tenant in HANA
           hana_src_tenant              TYPE cts_hot_pack_src_tenant,
           "! HANA package src_tenant in HOTA
           hot_src_tenant               TYPE cts_hot_pack_src_tenant,
           "! HANA package responsible in HANA
           hana_responsible             TYPE cts_hot_pack_responsible,
           "! HANA package responsible in HOTA
           hot_responsible              TYPE cts_hot_pack_responsible,
           "! HANA package original language in HANA
           hana_original_language       TYPE cts_hot_hana_orig_lang,
           "! HANA package original language in HOTA
           hot_original_language        TYPE cts_hot_hana_orig_lang,
           "! HANA package is structural in HANA
           hana_is_structural           TYPE char40,  "CTS_HOT_PACK_IS_STRUCTURAL not possible as it is number. char40 because max length of header text
           "! HANA package is structural in HOTA
           hot_is_structural            TYPE char40,  "CTS_HOT_PACK_IS_STRUCTURAL not possible as it is number. char40 because max length of header text
           "! HANA package delivery unit in HANA
           hana_delivery_unit           TYPE cts_hot_pack_delivery_unit,
           "! HANA package delivery unit in HOTA
           hot_delivery_unit            TYPE cts_hot_pack_delivery_unit,
           "! HANA package delivery unit vendor in HANA
           hana_delivery_unit_vendor    TYPE cts_hot_pack_deliv_unit_vendor,
           "! HANA package delivery unit vendor in HOTA
           hot_delivery_unit_vendor     TYPE cts_hot_pack_deliv_unit_vendor,
           "! HANA package text collection in HANA
           hana_text_collection         TYPE cts_hot_pack_text_collection,
           "! HANA package text collection in HOTA
           hot_text_collection          TYPE cts_hot_pack_text_collection,
           "! HANA package text status in HANA
           hana_text_status             TYPE cts_hot_pack_text_status,
           "! HANA package text status in HOTA
           hot_text_status              TYPE cts_hot_pack_text_status,
           "! HANA package terminology domain in HANA
           hana_text_terminology_domain TYPE cts_hot_pack_text_term_domain,
           "! HANA package terminology domain in HOTA
           hot_text_terminology_domain  TYPE cts_hot_pack_text_term_domain,
           "! HANA package hints for translation in HANA
           hana_hints_for_translation   TYPE cts_hot_pack_hints_for_transl,
           "! HANA package hints for translation in HOTA
           hot_hints_for_translation    TYPE cts_hot_pack_hints_for_transl,
           hana_read_system             TYPE cts_hot_hana_read_system,
           hot_abap_sync_system         TYPE cts_hot_abap_sync_system,
           hot_abap_synced_by           TYPE cts_hot_abap_synced_by,
           hot_abap_synced_at           TYPE char40, "char40 because timestamp is in ms but we would like to see human readable data
           hot_abap_deployed_by         TYPE cts_hot_abap_deployed_by,
           hot_abap_deployed_at         TYPE char40, "char40 because timestamp is in ms but we would like to see human readable data
           "! R3trans UTC??? import timestamp in milli seconds
           abap_import_timestamp        TYPE char40,
           "! Deploy mode, whether deployment should be done always or only if prework was done,...
           hot_deploy_mode              TYPE cts_hot_activation_mode,  "technical for calculation only
           "! Text for deploy mode for UI
           hot_deploy_mode_as_text      TYPE char100,
           "! translation relevance, whether translation should be done or not
           abap_no_translation          TYPE cts_hot_abap_no_translation,
           "! Text for translation relevance for UI
           abap_translation_as_text     TYPE char100,
           hot_status                   TYPE cts_hot_object_status, "technical for calculation only
           hot_status_as_text           TYPE char70,
           cts_hot_package_ref          TYPE REF TO cl_cts_hot_package,
           exists_in_hana               TYPE abap_bool,
           exists_in_hota               TYPE abap_bool,
           masterlang                   TYPE sy-langu,
           t_color                      TYPE lvc_t_scol,
           END OF g_type_s_master.

    TYPES: BEGIN OF g_type_s_slave.   "object
    TYPES:
      "! hana package as transport object name, only needed to link objects in objects table (slave) to packages table (master)
      abap_hana_package_id       TYPE cts_hot_package_id,
      sync_deploy_state          TYPE icon_d,
      "! Transport Object name (HANA package(40) + hana object name + '.' + hana object suffix(70) in upper case or with hash(es))
      transport_obj_name         TYPE cts_hot_object_name,
      "! Name of the hana package in HANA
      hana_package_id            TYPE cts_hot_hana_package_id,
      "! name of hana package in HOTA repository that is already known for the transport_obj_name or initial if it is equal to hana_package_id
      hot_hana_package_id        TYPE cts_hot_hana_package_id,
      "! object_name to display in ALV List. Either object name from HANA or if object only exists in HOTA, the object name from HOTA. Important: This object_name must not be shown in details view!
      "! This object_name is required to be able to have &lt;unbekannt&gt; for hana_object_name or hot_hana_object_name in details view.
      object_name                TYPE cts_hot_hana_object_name,
      "! Name of the object in HANA
      hana_object_name           TYPE cts_hot_hana_object_name,
      "! Name of the object in HOTA repository that is already known for the transport_obj_name or initial if it is equal to hana_object_name
      hot_hana_object_name       TYPE cts_hot_hana_object_name,
      "! object_suffix to display in ALV List. Either object suffix from HANA or if object only exists in HOTA, the object suffix from HOTA. Important: This object_suffix must not be shown in details view!
      "! This object_suffix is required to be able to have &lt;unbekannt&gt; for hana_object_suffix or hot_hana_object_suffix in details view.
      object_suffix              TYPE cts_hot_hana_object_suffix,
      "! Suffix of the object in HANA
      hana_object_suffix         TYPE cts_hot_hana_object_suffix,
      "! Suffix of the object in HOTA repository that is already known for the transport_obj_name or initial if it is equal to hana_object_suffix
      hot_hana_object_suffix     TYPE cts_hot_hana_object_suffix,
      "! Status of the object in HANA (OK, BROKEN; NEEDS_REGEN)
      hana_object_status         TYPE icon_d,
      hana_source_object_version TYPE cts_hot_hana_src_obj_version,
      hana_version               TYPE char40, "cts_hot_hana_object_version not possible due to assign &lt;unbekannt&gt; to this entry in some cases
      hot_version                TYPE char40, "cts_hot_hana_object_version not possible due to assign &lt;unbekannt&gt; to this entry in some cases
      hana_activated_by          TYPE cts_hot_hana_activated_by,
      hot_activated_by           TYPE cts_hot_hana_activated_by,
      hana_activated_at          TYPE char40, "string not working for manually setting column length, char40 because max length of header text
      hot_activated_at           TYPE char40, "string not working for manually setting column length, char40 because max length of header text
      hana_hana_read_system      TYPE cts_hot_hana_read_system,
      hot_hana_read_system       TYPE cts_hot_hana_read_system,
      hot_abap_sync_system       TYPE cts_hot_abap_sync_system,
      hot_abap_synced_by         TYPE cts_hot_abap_synced_by,
      hot_abap_synced_at         TYPE char40, "string not working for manually setting column length, char40 because max length of header text
      hot_abap_deployed_by       TYPE cts_hot_abap_deployed_by,
      hot_abap_deployed_at       TYPE string,
      abap_import_timestamp      TYPE char40,
      hot_status                 TYPE cts_hot_object_status, "technical for calculation only
      hot_status_as_text         TYPE char70,
      cts_hot_object_ref         TYPE REF TO cl_cts_hot_object_v1,
      exists_in_hana             TYPE abap_bool,
      abap_object_reference      TYPE lxeobjname, "name of translation object
      exists_in_hota             TYPE abap_bool,
      t_color                    TYPE lvc_t_scol,
      END OF g_type_s_slave.

    DATA: mt_master                      TYPE STANDARD TABLE OF g_type_s_master WITH UNIQUE HASHED KEY pack_ref COMPONENTS cts_hot_package_ref,
          mt_slave                       TYPE STANDARD TABLE OF g_type_s_slave WITH UNIQUE HASHED KEY obj_ref COMPONENTS cts_hot_object_ref,
          mr_hierseq                     TYPE REF TO cl_salv_hierseq_table,
          mv_max_length_package_id       TYPE lvc_outlen, "needed to reduce output length from 256 to maximum used length of current dataset
          mv_max_length_devclass         TYPE lvc_outlen,
          mv_max_length_pack_desc        TYPE lvc_outlen,
          mv_max_length_pack_src_system  TYPE lvc_outlen,
          mv_max_length_pack_src_tenant  TYPE lvc_outlen,
          mv_max_length_pack_responsible TYPE lvc_outlen,
          mv_max_length_pack_du_name     TYPE lvc_outlen,
          mv_max_length_pack_du_vendor   TYPE lvc_outlen,
          mv_max_length_pack_text_coll   TYPE lvc_outlen,
          mv_max_length_pack_text_status TYPE lvc_outlen,
          mv_max_length_pack_text_domain TYPE lvc_outlen,
          mv_max_length_pack_hint        TYPE lvc_outlen,
          mv_max_length_object_name      TYPE lvc_outlen,
          mv_max_length_object_suffix    TYPE lvc_outlen,
          mv_any_object_not_ok_in_hana   TYPE abap_bool,
          mr_data_provider               TYPE REF TO lif_data_provider,
          mr_hot_hana_connector          TYPE REF TO if_cts_hot_hana_conn_internal,
          mr_external_calls              TYPE REF TO if_cts_hot_ext_call_internal,
          mv_system_timezone             TYPE timezone,
          "! Filter data, current used filter and number of packages/objects by status
          ms_filter_data                 TYPE ty_filter_data,
          "! All packages found for user input string. Currently it will be only filled and use in apply_filter
          mt_master_all                  LIKE mt_master,
          "! All objects found for user input string. Currently it will be only filled and use in apply_filter.
          mt_slave_all                   LIKE mt_slave.

    METHODS:
      "! select objects from hana of all packages in mt_master into global output table mt_slaves
      read_object_data_from_hana
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport ,

      "! select objects from hot of all packages in mt_master into global table mt_slaves
      "! Before all objects were read from HANA so that global table usually contain already values.
      read_object_data_from_hot
        RAISING
          cx_hana_object_transport,

      "! Calculates the sync_status icon.
      calculate_sync_status,

      "! Calculates the depl_status icon.
      calculate_depl_status,

      "! Mark the difference in the ALV list for packages.
      mark_difference_package EXPORTING ev_master TYPE any,

      "! Mark the difference in the ALV list for objects.
      mark_difference_object EXPORTING ev_slave TYPE any,

      "! Calculates the numbers of packages/objects per status
      calc_number_pkg_obj,

      "! Searches all packages in HANA and HOT repository and returns them for each repository. So both e_xx_packages might contain exact same data.
      "!
      "! @parameter i_hana_package_name | i_hana_package_name
      "! @parameter i_include_subpackages | i_include_subpackages
      "! @parameter e_hana_packages | packages found in HANA for the i_hana_package_name (exact case)
      "! @parameter e_hot_packages | packages found in HOT for the i_hana_package_name (exact case)
      "! @raising cx_hana_object_transport | cx_hana_object_transport
      search_packges_in_hana_and_hot
        IMPORTING
          i_hana_package_name   TYPE cts_hot_package-hana_package_id
          i_include_subpackages TYPE abap_bool
        EXPORTING
          e_hana_packages       TYPE tt_cts_hot_hana_package_id
          e_hot_packages        TYPE tt_cts_hot_hana_package_id
        RAISING
          cx_hana_object_transport,

      "! Returns the selected data from UI
      "!
      "! @parameter i_check_deletion_consistency | flag whether to check or not to check for deletion consistency
      "!                                           (if package should be deleted, show error if not all objects are also selected)
      "! @parameter e_hotp_packages | selected packages for which NOT all objects are selected
      "! @parameter e_hota_packages | selected packages for which also all objects are selected
      "! @parameter e_hoto_objects | objects that are selected individually (not together with all other objects of a package as HOTA)
      "! @parameter e_hota_objects | objects that are selected as part of a selection of all objects of a package
      "! @raising cx_hana_object_transport | cx_hana_object_transport
      get_selected_data
        IMPORTING
          i_check_deletion_consistency TYPE abap_bool DEFAULT abap_false
        EXPORTING
          e_hotp_packages              TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          e_hota_packages              TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          e_hoto_objects               TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
          e_hota_objects               TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
        RAISING
          cx_hana_object_transport,
      add_to_transport_request
        IMPORTING
          i_hotp_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hota_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hoto_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
        RETURNING
          VALUE(r_result) TYPE ty_trkorrs_sorted,

      "! Adds the passed object in i_obj_name to TR or new TR if TR is not specified
      "!
      "! @parameter i_pgmid | R3TR or LIMU
      "! @parameter i_obj_type | HOTA or HOTP or HOTO
      "! @parameter i_obj_name | Object name for TR
      "! @parameter i_transport_request | i_transport_request
      "! @parameter i_is_deletion | i_is_deletion
      "! @parameter r_result | r_result
      add_to_tr
        IMPORTING
          i_pgmid             TYPE pgmid
          i_obj_type          TYPE trobjtype
          i_obj_name          TYPE cts_hot_object_name
          i_transport_request TYPE trkorr
          i_is_deletion       TYPE abap_bool
        RETURNING
          VALUE(r_result)     TYPE trkorr,

      "! Reads package attributes from hana of all packages in mt_master into global output table mt_master
      read_package_data_from_hana
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Reads package attributes from hot of all packages in mt_master into global output table mt_master
      read_package_data_from_hot,

      create_local_time_string
        IMPORTING
          i_timestamp_utc     TYPE timestampl
        RETURNING
          VALUE(r_local_time) TYPE string,

      conv_hana_actvted_at_to_timest
        IMPORTING
          i_activated_at  TYPE string
        RETURNING
          VALUE(r_timest) TYPE timestampl,
      read_tadir_for_packages,

      "! ALV does not show data that has a 'space', therefore write meaningfull data if either HANA or HOTA has data
      "! Also calculate gv_max_length for some columns
      replace_spaces_for_display,

      "! Sets output length of passed column to at least i_min_length or i_max_length if i_max_length &gt; i_min_length
      set_column_length
        IMPORTING
          i_column     TYPE REF TO cl_salv_column_hierseq
          i_min_length TYPE lvc_outlen
          i_max_length TYPE lvc_outlen,

      find_package_with_diffrnt_case
        IMPORTING
          i_hot_hana_package_id    TYPE string
        RETURNING
          VALUE(r_hana_package_id) TYPE string
        RAISING
          cx_nhi_hana_repository,

      "! Marks the passed columns as color columns in ch_color_table
      "! @parameter i_name1 | Name of column1 (HANA column or general column)
      "! @parameter i_name2 | Name of column2 (HOTA column)
      "! @parameter ch_color_table | ch_color_table
      append_change_color
        IMPORTING
          i_name1        TYPE string
          i_name2        TYPE string OPTIONAL
        CHANGING
          ch_color_table TYPE lvc_t_scol,

      "! Adds or updates the passed HANA objects to/in mt_slave
      append_hana_objcts_to_mt_slave
        IMPORTING
          i_master        TYPE REF TO g_type_s_master
          i_hana_objects  TYPE cl_nhi_object_id_and_caption=>ty_objlist_caption
          i_object_status TYPE icon_d
        RAISING
          cx_hana_object_transport,

      "! Do column settings for columns and their layout that is based on content
      handle_dynamic_columns,

      "! Helper method to set max value on gv_xxx_length variables.
      "! Calculates length of i_text and if longer than c_length, c_length is updated to this value
      "! @parameter i_text | text to calculate length from
      "! @parameter c_length | current length to be changed if strlen( i_text ) &gt; c_length
      set_max_length
        IMPORTING
          i_text   TYPE lcl_hota_organizer=>g_type_s_slave-hana_object_name
        CHANGING
          c_length TYPE lvc_outlen,

      "! First appending the packages of i_package_names_hana to mt_master.
      "! Then for packages in i_package_names_hot either modify existing entries in mt_master
      "! (same key on transport_obj_name) or append as well.
      append_packages_to_mt_master
        IMPORTING
          i_package_names_hana TYPE tt_cts_hot_hana_package_id
          i_package_names_hot  TYPE tt_cts_hot_hana_package_id
        RAISING
          cx_hana_object_transport,

      "! Loops over mt_master to check whether the packages that do exist only in either HANA or HOTA
      "! do exist in different case in the other repository
      check_packgs_for_diffrnt_cases
        RAISING
          cx_nhi_hana_repository,

      set_tooltips,

      "! Sets the long text for the column with the specified template (&1 in HANA or &1 in HOTA)
      set_column_long_text
        IMPORTING
          i_text_template TYPE string
          i_column        TYPE REF TO cl_salv_column_hierseq,

      build_header
        CHANGING
          cr_content TYPE REF TO cl_salv_form_element,

      "! Set master language for all objects/packages in the mt_master table (language is an attribute on package/tadir level)<br/>
      "! If setting language fails because of unknown language code on HANA side or there is no language maintained on HANA and user
      "! decides to cancel, an error system message is shown which leads to complete cancel of current action. (e.g. sync)
      set_masterlang_in_mt_master
        IMPORTING
          i_hota_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hotp_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hoto_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,

      "! Checks all passed objects with tr_object_check. If any problem occurs or user does cancel, complete sync will be canceled. All actions done
      "! until then remain (but this might be only add task to existing request).
      "! @parameter i_hota_packages | i_hota_packages
      "! @parameter i_hotp_packages | i_hotp_packages
      "! @parameter i_hoto_objects | i_hoto_objects
      "! @parameter r_result | all different trkorrs returned by tr_object_check
      check_objects
        IMPORTING
          i_hota_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hotp_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
          i_hoto_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
        RETURNING
          VALUE(r_result) TYPE ty_trkorrs_sorted,

      "! Checks passed object with rs_corr_check. If any problem occurs or user does cancel, complete sync will be canceled. All actions done
      "! until then remain (but this might be only add task to existing request).
      "! @parameter i_pgmid | i_hoto_objects
      "! @parameter i_obj_type | i_obj_type
      "! @parameter i_obj_name | i_obj_name
      "! @parameter r_result | trkorr used if object was already locked on any request, otherwise initial.
      check_object
        IMPORTING
          i_pgmid         TYPE pgmid
          i_obj_type      TYPE trobjtype
          i_obj_name      TYPE cts_hot_object_name
        RETURNING
          VALUE(r_result) TYPE trkorr,

      "! Checks passed object with tr_object_check for LANG object. If any problem occurs or user does cancel, complete sync will be canceled. All actions done
      "! until then remain (but this might be only add task to existing request).
      "! @parameter i_obj_type | i_obj_type
      "! @parameter i_obj_name | i_obj_name
      "! @parameter r_result | trkorr used if object was already locked on any request, otherwise initial.
      check_langu_object
        IMPORTING
          i_obj_type      TYPE trobjtype
          i_obj_name      TYPE cts_hot_object_name
          i_masterlang    TYPE spras
        RETURNING
          VALUE(r_result) TYPE trkorr,

      "! Adds the passed object in i_obj_name as LANG object to TR or new TR if TR is not specified
      "!
      "! @parameter i_obj_type | HOTA or HOTP or HOTO
      "! @parameter i_obj_name | Object name for TR
      "! @parameter i_masterlang | language for the object
      "! @parameter i_transport_request | i_transport_request
      "! @parameter r_result | r_result
      add_langu_to_tr
        IMPORTING
          i_obj_type          TYPE trobjtype
          i_obj_name          TYPE cts_hot_object_name
          i_masterlang        TYPE spras
          i_transport_request TYPE trkorr
        RETURNING
          VALUE(r_result)     TYPE trkorr,

      "! Checks whether TP and R3trans are up to date with regards to required HOTA functionality.
      check_transport_tools,

      "! Tries to read HANA package name for passed transport object name.
      "!
      "! @parameter i_hana_package_name | HANA package as transport object name
      "! @parameter r_result | hana package name for passed i_abap_hana_package_id or i_abap_hana_package_id itself.
      read_hana_pack_name_for_trobjn
        IMPORTING
          i_hana_package_name TYPE cts_hot_hana_package_id
        RETURNING
          VALUE(r_result)     TYPE cts_hot_hana_package_id.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_hot_organizer TYPE REF TO lcl_hota_organizer,

      on_before_salv_function FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING
            e_salv_function,

      on_after_salv_function FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING
            e_salv_function,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING
            e_salv_function.

  PRIVATE SECTION.
    DATA: gr_hot_organizer  TYPE REF TO lcl_hota_organizer.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'SELECT_ALL' OR 'SEL_ALL_D'.
        gr_hot_organizer->select_all( ).
      WHEN 'DESEL_ALL' OR 'DES_ALL_D'.
        gr_hot_organizer->deselect_all( ).
      WHEN 'SEL_SYNC' OR 'SEL_DEPL'.
        gr_hot_organizer->preselect_all_out_of_sync( ).
      WHEN 'SYNC'.
        gr_hot_organizer->sync_data( ).
      WHEN 'DEPL'.
        gr_hot_organizer->deploy( ).
      WHEN 'SEL_PACK'.
        gr_hot_organizer->select_all_for_package( ).
      WHEN 'DESEL_PACK'.
        gr_hot_organizer->deselect_all_for_package( ).
      WHEN 'REFRESH'.
        gr_hot_organizer->refresh_data( ).
      WHEN 'VERS'.
        gr_hot_organizer->show_versioning_for_currnt_cll( ).
      WHEN 'SE09'.
        TRY.
            CALL TRANSACTION 'SE09' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error.
            DATA text TYPE string.
            text = 'Keine Berechtigung für Transport Organizer (SE09)'(165).
            MESSAGE text TYPE 'E'.
        ENDTRY.
      WHEN 'INFO'.
        IF gv_sync = 'X'.
          CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
            EXPORTING
              dokclass = 'TX' " allgemeiner Text - SE61
              dokname  = 'CTS_HOTA_SYNC_UI_ALV_DOCU'
*             short_text = 'X'
            EXCEPTIONS
              OTHERS   = 1.
        ELSEIF gv_depl = 'X'.
          CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
            EXPORTING
              dokclass = 'TX' " allgemeiner Text - SE61
              dokname  = 'CTS_HOTA_DEPL_UI_ALV_DOCU'
*             short_text = 'X'
            EXCEPTIONS
              OTHERS   = 1.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN 'SETDEPMODE'.
        gr_hot_organizer->set_deploy_mode( ).
      WHEN 'TRANSLMODE'.
        gr_hot_organizer->set_transl_mode( ).
      WHEN 'LEGEND'.
        IF gv_sync = 'X'.
          CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
            EXPORTING
              dokclass = 'TX' " allgemeiner Text - SE61
              dokname  = 'CTS_HTA_SYNC_UI_LEGEND'
*             short_text = 'X'
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ELSEIF gv_depl = 'X'.
          CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
            EXPORTING
              dokclass = 'TX' " allgemeiner Text - SE61
              dokname  = 'CTS_HTA_DEPL_UI_LEGEND'
*             short_text = 'X'
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      WHEN 'DEPLOY'.
        MESSAGE 'Um SAP-HANA-Pakete und -Objekte zu deployen, verwenden Sie Transaktion SCTS_HTA_DEPLOY'(235) TYPE 'I'.
      WHEN 'FILTER'.
        CALL SCREEN 2000 STARTING AT 40 9.
      WHEN 'SYNC_T'.
        CALL TRANSACTION 'SCTS_HTA' WITH AUTHORITY-CHECK.
      WHEN 'DEPL_T'.
        CALL TRANSACTION 'SCTS_HTA_DEPLOY' WITH AUTHORITY-CHECK.
    ENDCASE.
  ENDMETHOD.                    "on_user_command

  METHOD constructor.
    gr_hot_organizer = i_hot_organizer.
  ENDMETHOD.


  METHOD on_before_salv_function.
    IF e_salv_function = '&ETA'.
      gr_hot_organizer->hide_name_columns( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_after_salv_function.
    CASE e_salv_function.
      WHEN '&ETA'.
        gr_hot_organizer->show_name_columns( ).
      WHEN '&OL0' OR '&OAD'  OR '&AVE'.
        gr_hot_organizer->preselect_all_out_of_sync( ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

CLASS lcl_hota_organizer IMPLEMENTATION.

  METHOD f4help_packages.
    DATA:
      lt_package_names_hot_and_hana  TYPE tt_cts_hot_hana_package_id, "will contain all names from HANA and HOT with given name
      ls_package_name_in_hot_and_han LIKE LINE OF lt_package_names_hot_and_hana,
      lt_package_names_out           TYPE STANDARD TABLE OF ddshretval WITH EMPTY KEY,
      ls_package_name                LIKE LINE OF lt_package_names_out,
      lt_package_names_in            TYPE STANDARD TABLE OF ty_hana_package_id,
      ls_f4help_wa                   TYPE ty_hana_package_id,
      lv_hana_package_name           TYPE cts_hot_hana_package_id.

    lv_hana_package_name = read_hana_pack_name_for_trobjn( i_hana_package_name ).

    search_packges_in_hana_and_hot(
      EXPORTING
        i_hana_package_name = lv_hana_package_name
        i_include_subpackages = i_include_subpackages
      IMPORTING
        e_hana_packages = lt_package_names_hot_and_hana
        e_hot_packages  = DATA(lt_package_names_hot)
    ).

    INSERT LINES OF lt_package_names_hot INTO TABLE lt_package_names_hot_and_hana.
    SORT lt_package_names_hot_and_hana.
    DELETE ADJACENT DUPLICATES FROM lt_package_names_hot_and_hana.

    " add all HANA and HOT packages to F4 help input table
    LOOP AT lt_package_names_hot_and_hana INTO ls_package_name_in_hot_and_han.
      ls_f4help_wa-hana_package_id = ls_package_name_in_hot_and_han.
      APPEND ls_f4help_wa TO lt_package_names_in.
    ENDLOOP.

    "show f4 help and get user selected value
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'HANA_PACKAGE_ID'
        value_org       = 'S' " Structure
      TABLES
        value_tab       = lt_package_names_in " F4 help values
        return_tab      = lt_package_names_out " F4 selected values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_package_names_out INDEX 1 INTO ls_package_name.
      r_result = ls_package_name-fieldval.
    ENDIF.

  ENDMETHOD.

  METHOD read_user_value.
    DATA: dynpfield  TYPE dynpread,
          dynpfields TYPE STANDARD TABLE OF dynpread.

    IF i_param_name IS NOT INITIAL.
      dynpfield-fieldname = i_param_name.
      APPEND dynpfield TO dynpfields.
    ENDIF.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
        translate_to_upper   = ' '
      TABLES
        dynpfields           = dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE dynpfields WITH KEY fieldname = i_param_name INTO dynpfield.
      IF sy-subrc = 0.
        r_result = dynpfield-fieldvalue.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD select_data.
    DATA lv_hana_package_name TYPE cts_hot_hana_package_id.

    FREE: mt_master, mt_slave, mt_master_all, mt_slave_all, ms_filter_data.
    ms_filter_data-show_green = abap_true.
    ms_filter_data-show_yellow = abap_true.
    ms_filter_data-show_red = abap_true.
    ms_filter_data-show_inactive = abap_true.

    "Store search string and subpackage selection to print to top of list
    mv_search_string = i_hana_package_name.
    mv_include_subpackages = i_include_subpackages.

* 0. Check if input is transport object name
    lv_hana_package_name = read_hana_pack_name_for_trobjn( i_hana_package_name ).

* 1. Prepare mt_master by reading all packages from HANA and HOTA that match user input string, also check for different cases if it exists only in 1 repository

    "can be called for multi packages with * like mypackage*
    search_packges_in_hana_and_hot( EXPORTING i_hana_package_name = lv_hana_package_name
                                              i_include_subpackages = i_include_subpackages
                                    IMPORTING e_hana_packages     = DATA(lt_package_names_hana)
                                              e_hot_packages      = DATA(lt_package_names_hot) ).

    append_packages_to_mt_master( i_package_names_hana = lt_package_names_hana i_package_names_hot = lt_package_names_hot ).

    check_packgs_for_diffrnt_cases( ).

* 2. Read package, tadir and object details from HANA, HOTA and tadir
    read_package_data_from_hana( ).

    read_package_data_from_hot( ).

    read_tadir_for_packages( ).

    read_object_data_from_hana( ).

    read_object_data_from_hot( ).

* 3. Set all unknown(initial) data to <unknown> because initial values are not shown in details view in ALV
    replace_spaces_for_display( ).

* 4. Calculate the sync/depl status for packages and objects
    IF gv_sync = 'X'.
      calculate_sync_status( ).
    ELSEIF gv_depl = 'X'.
      calculate_depl_status( ).
    ENDIF.

    calc_number_pkg_obj(  ).
  ENDMETHOD.

  METHOD calc_number_pkg_obj.
    DATA: lt_master TYPE STANDARD TABLE OF g_type_s_master,
          lt_slave  TYPE STANDARD TABLE OF g_type_s_slave.

    LOOP AT mt_master REFERENCE INTO DATA(lr_master) GROUP BY ( sync_deploy_state = lr_master->sync_deploy_state lv_count = GROUP SIZE )
                                                        WITHOUT MEMBERS REFERENCE INTO DATA(lr_master_group).
      CASE lr_master_group->sync_deploy_state.
        WHEN icon_led_green.
          ms_filter_data-nr_packages_green = lr_master_group->lv_count.
        WHEN icon_led_yellow.
          ms_filter_data-nr_packages_yellow = lr_master_group->lv_count.
        WHEN icon_led_red.
          ms_filter_data-nr_packages_red = lr_master_group->lv_count.
        WHEN icon_led_inactive.
          ms_filter_data-nr_packages_inactive = lr_master_group->lv_count.
        WHEN OTHERS.
          "todo: OK to use assert in this case and how to use assert?
          ASSERT 0 = 0. "new state not implemented...
      ENDCASE.
    ENDLOOP.

    LOOP AT mt_slave REFERENCE INTO DATA(lr_slave) GROUP BY ( sync_deploy_state = lr_slave->sync_deploy_state lv_count = GROUP SIZE )
                                                        WITHOUT MEMBERS REFERENCE INTO DATA(lr_slave_group).
      CASE lr_slave_group->sync_deploy_state.
        WHEN icon_led_green.
          ms_filter_data-nr_objects_green = lr_slave_group->lv_count.
        WHEN icon_led_yellow.
          ms_filter_data-nr_objects_yellow = lr_slave_group->lv_count.
        WHEN icon_led_red.
          ms_filter_data-nr_objects_red = lr_slave_group->lv_count.
        WHEN icon_led_inactive.
          ms_filter_data-nr_objects_inactive = lr_slave_group->lv_count.
        WHEN OTHERS.
          "todo: OK to use assert in this case and how to use assert?
          ASSERT 0 = 0. "new state not implemented...
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD apply_filter.
    DATA: lr_content TYPE REF TO cl_salv_form_element.

    "Only filter if filter has changed?
    IF i_green = ms_filter_data-show_green
          AND i_yellow = ms_filter_data-show_yellow
          AND i_red = ms_filter_data-show_red
          AND i_inactive = ms_filter_data-show_inactive.
      RETURN.
    ENDIF.

    TRY.
        ms_filter_data-show_green = i_green.
        ms_filter_data-show_yellow = i_yellow.
        ms_filter_data-show_red = i_red.
        ms_filter_data-show_inactive = i_inactive.

        " if not yet filtered, remember all packages / objects. If filter is used 2nd time,
        " get all packages / objects back  into mt_master and mt_slave and filter again.
        IF mt_master_all IS INITIAL.
          mt_master_all = mt_master.
        ELSE.
          mt_master = mt_master_all.
        ENDIF.

        IF mt_slave_all IS INITIAL.
          mt_slave_all = mt_slave.
        ELSE.
          mt_slave = mt_slave_all.
        ENDIF.

* 1. delete all objects that should not be shown
        IF ms_filter_data-show_green = abap_false.
          DELETE mt_slave WHERE sync_deploy_state = icon_led_green.
        ENDIF.
        IF  ms_filter_data-show_yellow = abap_false.
          DELETE mt_slave WHERE sync_deploy_state = icon_led_yellow.
        ENDIF.
        IF  ms_filter_data-show_red = abap_false.
          DELETE mt_slave WHERE sync_deploy_state = icon_led_red.
        ENDIF.
        IF  ms_filter_data-show_inactive = abap_false.
          DELETE mt_slave WHERE sync_deploy_state = icon_led_inactive.
        ENDIF.

* 2. delete all packages that should not be shown, BUT only if there is no object to be shown for this package
*    Else the object will not be shown as it requires the package to be shown!
        LOOP AT mt_master REFERENCE INTO DATA(lr_master).
          IF ( ( ms_filter_data-show_green = abap_false AND lr_master->sync_deploy_state = icon_led_green )
                OR ( ms_filter_data-show_yellow = abap_false AND lr_master->sync_deploy_state = icon_led_yellow )
                OR ( ms_filter_data-show_red = abap_false AND lr_master->sync_deploy_state = icon_led_red )
                OR ( ms_filter_data-show_inactive = abap_false AND lr_master->sync_deploy_state = icon_led_inactive )
             ) AND NOT line_exists( mt_slave[ abap_hana_package_id = lr_master->transport_obj_name ] ).
            DELETE mt_master. "delete current table line
          ENDIF.
        ENDLOOP.

        preselect_all_out_of_sync( ).

*       in case number of packages/objects has been changed, top of list has to be changed as well
        build_header( CHANGING cr_content = lr_content ).
        mr_hierseq->set_top_of_list( lr_content ).

*... refresh the table in order to see the new data
*        mr_hierseq->refresh( ). "with refresh there are scrolling issues...
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        DATA(t100_key) = hot_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
      CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
        t100_key = nhi_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4.
    ENDTRY.
  ENDMETHOD.

  METHOD display_data.
    DATA:
      lt_binding      TYPE salv_t_hierseq_binding,
      ls_binding      TYPE salv_s_hierseq_binding,
      lr_functions    TYPE REF TO cl_salv_functions_list,
      lr_columns      TYPE REF TO cl_salv_columns_hierseq,
      lr_column       TYPE REF TO cl_salv_column_hierseq,
      lr_layout       TYPE REF TO cl_salv_layout,
      lr_events       TYPE REF TO cl_salv_events_hierseq,
      ls_layout_key   TYPE salv_s_layout_key,
      lv_text_in_hana TYPE string,
      lv_text_in_hota TYPE string,
      lv_title        TYPE lvc_title,
      lr_content      TYPE REF TO cl_salv_form_element.

*    lv_text_in_sap_hana = '&1 in SAP HANA'(106).
    lv_text_in_hana = '&1 in HANA'(101).
    lv_text_in_hota = '&1 in ABAP'(102).

*... create the binding information between master and slave
    ls_binding-master = 'TRANSPORT_OBJ_NAME'.
    ls_binding-slave  = 'ABAP_HANA_PACKAGE_ID'.
    APPEND ls_binding TO lt_binding.

*... create an ALV hierseq table
    TRY.
        cl_salv_hierseq_table=>factory(
          EXPORTING
            t_binding_level1_level2 = lt_binding
          IMPORTING
            r_hierseq               = mr_hierseq
          CHANGING
            t_table_level1           = mt_master
            t_table_level2           = mt_slave ).

        IF gv_sync = 'X'.
          mr_hierseq->set_screen_status(
            pfstatus   =  'HOTA_SYNC'
            report     =  sy-repid ).
          lv_title = 'SAP HANA Transport for ABAP - Synchronisierung'(004).
        ELSEIF gv_depl = 'X'.
          mr_hierseq->set_screen_status(
            pfstatus   =  'HOTA_DEPL'
            report     =  sy-repid ).
          lv_title = 'SAP HANA Transport for ABAP - Deployment'(229).
        ENDIF.
        mr_hierseq->get_display_settings( )->set_list_header( lv_title ).

        build_header( CHANGING cr_content = lr_content ).
        mr_hierseq->set_top_of_list( lr_content ).

      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      CATCH cx_salv_data_error INTO DATA(exc1).
        ls_msg = exc1->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... §3.1 activate ALV generic Functions
    lr_functions = mr_hierseq->get_functions( ).
*      lr_functions->set_all( abap_true ).
*      lr_functions->set_default( ).
    lr_functions->set_detail( if_salv_c_bool_sap=>true ).
*      lr_functions->set_filter( if_salv_c_bool_sap=>true ).
    lr_functions->set_layout_change( if_salv_c_bool_sap=>true ).
    lr_functions->set_layout_load( if_salv_c_bool_sap=>true ).
    lr_functions->set_layout_save( if_salv_c_bool_sap=>true ).
*      lr_functions->set_sort_asc( if_salv_c_bool_sap=>true ).
*      lr_functions->set_sort_desc( if_salv_c_bool_sap=>true ).
    lr_functions->set_find( if_salv_c_bool_sap=>true ).
*    lr_functions->set_group_layout( if_salv_c_bool_sap=>true ).


*... set tooltips
    set_tooltips( ).

*enable selections
    TRY.
*... set selection mode for both levels to single
        mr_hierseq->get_selections( 1 )->set_selection_mode( if_salv_c_selection_mode=>single ).
        mr_hierseq->get_selections( 2 )->set_selection_mode( if_salv_c_selection_mode=>single ).
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*handle master columns
    TRY.
        lr_columns = mr_hierseq->get_columns( 1 ).

        TRY.
            lr_columns->set_color_column( 'T_COLOR' ).
          CATCH cx_salv_data_error.                     "#EC NO_HANDLER
        ENDTRY.

*... optimze the columns
*          lr_columns->set_optimize( abap_true ).

*... hide technical columns
        lr_column ?= lr_columns->get_column( 'EXISTS_IN_HANA' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'EXISTS_IN_HOTA' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'HOT_STATUS' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'HOT_DEPLOY_MODE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'ABAP_NO_TRANSLATION' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'MASTERLANG' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

*...change column settings
        IF gv_sync = 'X'.
          lr_column ?= lr_columns->get_column( 'SYNC_DEPLOY_STATE' ).
          lr_column->set_short_text( 'Status'(107) ).
          lr_column->set_medium_text( 'Sync.status Paket'(108) ).
          lr_column->set_long_text( 'Synchronisierungsstatus Paket'(109) ).
          lr_column->set_output_length( 6 ).
          lr_column->set_icon( if_salv_c_bool_sap=>true ).
        ELSEIF gv_depl = 'X'.
          lr_column ?= lr_columns->get_column( 'SYNC_DEPLOY_STATE' ).
          lr_column->set_short_text( 'Status'(107) ).
          lr_column->set_medium_text( 'Depl.status Paket'(218) ).
          lr_column->set_long_text( 'Deploymentstatus Paket'(219) ).
          lr_column->set_output_length( 6 ).
          lr_column->set_icon( if_salv_c_bool_sap=>true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'PACKAGE_ID' ).
        set_column_length( i_column = lr_column i_min_length = 9 i_max_length = mv_max_length_package_id ).

        lr_column ?= lr_columns->get_column( 'HANA_PACKAGE_ID' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_package_id ).

        lr_column ?= lr_columns->get_column( 'HOT_HANA_PACKAGE_ID' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_package_id ).

        lr_column ?= lr_columns->get_column( 'TRANSPORT_OBJ_NAME' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS' ).
        lr_column->set_short_text( 'ABAP-Paket'(103) ).
        lr_column->set_medium_text( 'ABAP-Paket'(104) ).
        lr_column->set_long_text( 'ABAP-Paket'(105) ).
        set_column_length( i_column = lr_column i_min_length = 10 i_max_length = mv_max_length_devclass ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'HANA_DESCRIPTION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_desc ).

        lr_column ?= lr_columns->get_column( 'HOT_DESCRIPTION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_desc ).

        lr_column ?= lr_columns->get_column( 'HANA_SRC_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_src_system ).

        lr_column ?= lr_columns->get_column( 'HOT_SRC_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_src_system ).

        lr_column ?= lr_columns->get_column( 'HANA_SRC_TENANT' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_src_tenant ).

        lr_column ?= lr_columns->get_column( 'HOT_SRC_TENANT' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_src_tenant ).

        lr_column ?= lr_columns->get_column( 'HANA_RESPONSIBLE' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_responsible ).

        lr_column ?= lr_columns->get_column( 'HOT_RESPONSIBLE' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_responsible ).

        lr_column ?= lr_columns->get_column( 'HANA_ORIGINAL_LANGUAGE' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        lr_column->set_output_length( 20 ).

        lr_column ?= lr_columns->get_column( 'HOT_ORIGINAL_LANGUAGE' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        lr_column->set_output_length( 20 ).

        lr_column ?= lr_columns->get_column( 'HANA_IS_STRUCTURAL' ).
        lr_column->set_short_text( 'Struk.pak.'(110) ).
        lr_column->set_medium_text( 'Strukturpaket'(111) ).
        lr_column->set_long_text( 'Strukturpaket'(112) ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        lr_column->set_output_length( 20 ).

        lr_column ?= lr_columns->get_column( 'HOT_IS_STRUCTURAL' ).
        lr_column->set_short_text( 'Struk.pak.'(110) ).
        lr_column->set_medium_text( 'Strukturpaket'(111) ).
        lr_column->set_long_text( 'Strukturpaket'(112) ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        lr_column->set_output_length( 20 ).

        lr_column ?= lr_columns->get_column( 'HANA_DELIVERY_UNIT' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_du_name ).

        lr_column ?= lr_columns->get_column( 'HOT_DELIVERY_UNIT' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_du_name ).

        lr_column ?= lr_columns->get_column( 'HANA_DELIVERY_UNIT_VENDOR' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_du_vendor ).

        lr_column ?= lr_columns->get_column( 'HOT_DELIVERY_UNIT_VENDOR' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_du_vendor ).

        lr_column ?= lr_columns->get_column( 'HANA_TEXT_COLLECTION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_coll ).

        lr_column ?= lr_columns->get_column( 'HOT_TEXT_COLLECTION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_coll ).

        lr_column ?= lr_columns->get_column( 'HANA_TEXT_STATUS' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_status ).

        lr_column ?= lr_columns->get_column( 'HOT_TEXT_STATUS' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_status ).

        lr_column ?= lr_columns->get_column( 'HANA_TEXT_TERMINOLOGY_DOMAIN' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_domain ).

        lr_column ?= lr_columns->get_column( 'HOT_TEXT_TERMINOLOGY_DOMAIN' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_text_domain ).

        lr_column ?= lr_columns->get_column( 'HANA_HINTS_FOR_TRANSLATION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_hint ).

        lr_column ?= lr_columns->get_column( 'HOT_HINTS_FOR_TRANSLATION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_pack_hint ).

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNC_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        lr_column->set_output_length( 15 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNCED_AT' ).
        lr_column->set_short_text( 'Sync.zeit'(113) ).
        lr_column->set_medium_text( 'Synchronis.zeit'(114) ).
        lr_column->set_long_text( 'Synchronisierungszeit'(115) ).
        lr_column->set_output_length( 19 ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNCED_BY' ).
        lr_column->set_output_length( 25 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_DEPLOYED_AT' ).
        lr_column->set_short_text( 'Deployzeit'(116) ).
        lr_column->set_medium_text( 'Deployzeit'(117) ).
        lr_column->set_long_text( 'Deployzeit'(118) ).
        lr_column->set_output_length( 19 ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_DEPLOYED_BY' ).
        lr_column->set_output_length( 25 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HANA_READ_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'HOT_STATUS_AS_TEXT' ).
        lr_column->set_short_text( 'Status'(107) ).
        lr_column->set_medium_text( 'Status in ABAP'(119) ).
        lr_column->set_long_text( 'Status in ABAP'(120) ).
        lr_column->set_output_length( 15 ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'HOT_DEPLOY_MODE_AS_TEXT' ).
        lr_column->set_short_text( 'Deploymod.'(166) ).
        lr_column->set_medium_text( 'Deploymodus'(167) ).
        lr_column->set_long_text( 'Deploymodus'(168) ).
        lr_column->set_output_length( 15 ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'ABAP_TRANSLATION_AS_TEXT' ).
*        lr_column->set_short_text( 'Deploymod.'(166) ).
*        lr_column->set_medium_text( 'Deploymodus'(167) ).
        lr_column->set_long_text( 'Übersetzungsrelevanz'(201) ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'ABAP_IMPORT_TIMESTAMP' ).
        lr_column->set_short_text( 'Importzeit'(236) ).
        lr_column->set_medium_text( 'Importzeit'(237) ).
        lr_column->set_long_text( 'Importzeit'(238) ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ). " not visible directly in ALV List

      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*handle slave columns
    TRY.
        lr_columns = mr_hierseq->get_columns( 2 ).

        TRY.
            lr_columns->set_color_column( 'T_COLOR' ).
          CATCH cx_salv_data_error.                     "#EC NO_HANDLER
        ENDTRY.

*... optimze the columns
*          lr_columns->set_optimize( abap_true ).

*      lr_column = lr_columns->get_column( 'OBJECT_NAME' ).
*      lr_column->set_output_length( 50 ).

*... hide technical columns
        lr_column ?= lr_columns->get_column( 'ABAP_HANA_PACKAGE_ID' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'EXISTS_IN_HANA' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'EXISTS_IN_HOTA' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'HOT_STATUS' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

*...change column settings
        IF gv_sync = 'X'.
          lr_column ?= lr_columns->get_column( 'SYNC_DEPLOY_STATE' ).
          lr_column->set_short_text( 'Status'(107) ).
          lr_column->set_medium_text( 'Sync.status Objekt'(121) ).
          lr_column->set_long_text( 'Synchronisierungsstatus Objekt'(122) ).
          lr_column->set_output_length( 6 ).
          lr_column->set_icon( ).
        ELSEIF gv_depl = 'X'.
          lr_column ?= lr_columns->get_column( 'SYNC_DEPLOY_STATE' ).
          lr_column->set_short_text( 'Status'(107) ).
          lr_column->set_medium_text( 'Depl.status Objekt'(220) ).
          lr_column->set_long_text( 'Deploymentstatus Objekt'(221) ).
          lr_column->set_output_length( 6 ).
          lr_column->set_icon( ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HANA_PACKAGE_ID' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_package_id ).

        lr_column ?= lr_columns->get_column( 'HOT_HANA_PACKAGE_ID' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 20 i_max_length = mv_max_length_package_id ).

        lr_column ?= lr_columns->get_column( 'OBJECT_NAME' ).
        set_column_length( i_column = lr_column i_min_length = 10 i_max_length = mv_max_length_object_name ).

        lr_column ?= lr_columns->get_column( 'HANA_OBJECT_NAME' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 10 i_max_length = mv_max_length_object_name ).

        lr_column ?= lr_columns->get_column( 'HOT_HANA_OBJECT_NAME' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 10 i_max_length = mv_max_length_object_name ).

        lr_column ?= lr_columns->get_column( 'OBJECT_SUFFIX' ).
        set_column_length( i_column = lr_column i_min_length = 12 i_max_length = mv_max_length_object_suffix ).

        lr_column ?= lr_columns->get_column( 'HANA_OBJECT_SUFFIX' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 12 i_max_length = mv_max_length_object_suffix ).

        lr_column ?= lr_columns->get_column( 'HOT_HANA_OBJECT_SUFFIX' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).
        set_column_length( i_column = lr_column i_min_length = 12 i_max_length = mv_max_length_object_suffix ).

        lr_column ?= lr_columns->get_column( 'TRANSPORT_OBJ_NAME' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'HANA_OBJECT_STATUS' ).
        lr_column->set_short_text( 'Obj.status'(123) ).
        lr_column->set_medium_text( 'Objektstatus in HANA'(124) ).
        lr_column->set_long_text( 'Objektstatus in HANA'(125) ).
        lr_column->set_icon( if_salv_c_bool_sap=>true ).
        lr_column->set_output_length( 20 ).

        lr_column ?= lr_columns->get_column( 'HANA_SOURCE_OBJECT_VERSION' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
*       not really necessary
*        lr_column->set_short_text( 'ObjVersQu'(184) ).
*        lr_column->set_medium_text( 'Objektvers/Quellsyst'(185) ).
        lr_column->set_long_text( 'Objektversion aus Quellsystem'(186) ).
        lr_column->set_output_length( 21 ). " 4 should be enough?!?

        lr_column ?= lr_columns->get_column( 'HANA_VERSION' ).
        lr_column->set_short_text( 'Obj.vers.'(130) ).
        lr_column->set_medium_text( 'Objektversion'(131) ).
        lr_column->set_long_text( 'Objektversion'(132) ).
        lr_column->set_output_length( 22 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HOT_VERSION' ).
        lr_column->set_short_text( 'Obj.vers.'(130) ).
        lr_column->set_medium_text( 'Objektversion'(131) ).
        lr_column->set_long_text( 'Objektversion'(132) ).
        lr_column->set_output_length( 22 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HANA_HANA_READ_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        lr_column->set_output_length( 20 ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HOT_HANA_READ_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        lr_column->set_output_length( 20 ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HANA_ACTIVATED_AT' ).
        lr_column->set_short_text( 'Aktiv.zeit'(126) ).
        lr_column->set_medium_text( 'Aktivierungszeit'(127) ).
        lr_column->set_long_text( 'Aktivierungszeit'(128) ).
        lr_column->set_output_length( 25 ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HOT_ACTIVATED_AT' ).
        lr_column->set_short_text( 'Aktiv.zeit'(126) ).
        lr_column->set_medium_text( 'Aktivierungszeit'(127) ).
        lr_column->set_long_text( 'Aktivierungszeit'(128) ).
        lr_column->set_output_length( 25 ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HANA_ACTIVATED_BY' ).
        lr_column->set_output_length( 23 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        set_column_long_text( i_text_template = lv_text_in_hana i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HOT_ACTIVATED_BY' ).
        lr_column->set_output_length( 23 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        set_column_long_text( i_text_template = lv_text_in_hota i_column = lr_column ).

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNC_SYSTEM' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
        lr_column->set_output_length( 15 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNCED_AT' ).
        lr_column->set_short_text( 'Sync.zeit'(113) ).
        lr_column->set_medium_text( 'Synchronis.zeit'(114) ).
        lr_column->set_long_text( 'Synchronisierungszeit'(115) ).
        lr_column->set_output_length( 19 ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_SYNCED_BY' ).
        lr_column->set_output_length( 25 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_DEPLOYED_AT' ).
        lr_column->set_short_text( 'Deployzeit'(116) ).
        lr_column->set_medium_text( 'Deployzeit'(117) ).
        lr_column->set_long_text( 'Deployzeit'(118) ).
        lr_column->set_output_length( 19 ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_ABAP_DEPLOYED_BY' ).
        lr_column->set_output_length( 25 ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        IF gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ELSEIF gv_depl = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'HOT_STATUS_AS_TEXT' ).
        lr_column->set_short_text( 'Status'(107) ).
        lr_column->set_medium_text( 'Status in ABAP'(119) ).
        lr_column->set_long_text( 'Status in ABAP'(120) ).
        lr_column->set_output_length( 15 ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'ABAP_IMPORT_TIMESTAMP' ).
        lr_column->set_short_text( 'Importzeit'(236) ).
        lr_column->set_medium_text( 'Importzeit'(237) ).
        lr_column->set_long_text( 'Importzeit'(238) ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ). " not visible directly in ALV List

        lr_column ?= lr_columns->get_column( 'ABAP_OBJECT_REFERENCE' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found INTO DATA(exc3).
        ls_msg = exc3->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

    handle_dynamic_columns( ).

*... *** GENERAL Settings ***

*... set layout settings
    lr_layout = mr_hierseq->get_layout( ).

    ls_layout_key-report = sy-repid.
    lr_layout->set_key( ls_layout_key ).

    lr_layout->set_default( abap_true ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*... register to the events
    lr_events = mr_hierseq->get_event( ).

    DATA(lr_event_handler) = NEW lcl_handle_events( i_hot_organizer = me ).
    SET HANDLER lr_event_handler->on_user_command FOR lr_events.
    SET HANDLER lr_event_handler->on_user_command FOR lr_events.

*... §6.2 register to the event BEFORE_SALV_FUNCTION
    SET HANDLER lr_event_handler->on_before_salv_function FOR lr_events.
*... §6.3 register to the event AFTER_SALV_FUNCTION
    SET HANDLER lr_event_handler->on_after_salv_function FOR lr_events.

*.... default sorting otherwise preselection does not work...
    SORT mt_master BY package_id ASCENDING.
    SORT mt_slave BY abap_hana_package_id object_name ASCENDING.

    preselect_all_out_of_sync( ).

*... display the table
    mr_hierseq->display( ).

  ENDMETHOD.



  METHOD read_object_data_from_hana.

    DATA: dr_master       TYPE REF TO g_type_s_master,
          lt_hana_objects TYPE cl_nhi_object_id_and_caption=>ty_objlist_caption.

    LOOP AT mt_master REFERENCE INTO dr_master WHERE hana_package_id IS NOT INITIAL.
      lt_hana_objects = mr_hot_hana_connector->find_active_objects_in_hana( i_hana_package_id = dr_master->hana_package_id i_object_status = '0' ).
      append_hana_objcts_to_mt_slave( i_master = dr_master i_hana_objects = lt_hana_objects i_object_status = icon_okay ).

      lt_hana_objects = mr_hot_hana_connector->find_active_objects_in_hana( i_hana_package_id = dr_master->hana_package_id i_object_status = '1' ).

      IF lt_hana_objects IS NOT INITIAL.
        mv_any_object_not_ok_in_hana = abap_true.
        append_hana_objcts_to_mt_slave( i_master = dr_master i_hana_objects = lt_hana_objects i_object_status = icon_alert ).
      ENDIF.

      IF lt_hana_objects IS NOT INITIAL.
        mv_any_object_not_ok_in_hana = abap_true.
        lt_hana_objects = mr_hot_hana_connector->find_active_objects_in_hana( i_hana_package_id = dr_master->hana_package_id i_object_status = '2' ).

        append_hana_objcts_to_mt_slave( i_master = dr_master i_hana_objects = lt_hana_objects i_object_status = icon_generate ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD read_object_data_from_hot.

    DATA: lt_hot_objects        TYPE STANDARD TABLE OF cts_hot_object,
          ls_slave              TYPE g_type_s_slave,
          lr_cts_hot_object     TYPE REF TO cl_cts_hot_object_v1,
          lv_transport_obj_name TYPE g_type_s_slave-transport_obj_name.

    LOOP AT mt_master ASSIGNING FIELD-SYMBOL(<master>).
      CLEAR lt_hot_objects.
      SELECT hana_package_id, hana_object_name, hana_object_suffix, hana_read_system, hana_source_object_version,
             hana_object_version, abap_sync_system, hot_status, hana_activated_at, hana_activated_by, abap_synced_at,
             abap_synced_by, abap_deployed_at, abap_deployed_by, abap_import_timestamp, abap_object_reference
            FROM cts_hot_object
            INTO CORRESPONDING FIELDS OF TABLE @lt_hot_objects WHERE abap_hana_package_id = @<master>-transport_obj_name
                                                                 AND abap_status = 'A'.

      IF sy-subrc = 0.
        LOOP AT lt_hot_objects ASSIGNING FIELD-SYMBOL(<hot_object>).
          lr_cts_hot_object = cl_cts_hot_object_v1=>create_instance2(
                                io_cts_hot_package = <master>-cts_hot_package_ref
                                iv_hana_object_name = <hot_object>-hana_object_name
                                iv_hana_object_suffix = <hot_object>-hana_object_suffix ).
          lv_transport_obj_name = lr_cts_hot_object->transport_object_name.

          READ TABLE mt_slave WITH KEY transport_obj_name = lv_transport_obj_name ASSIGNING FIELD-SYMBOL(<slave>).

          IF sy-subrc <> 0.
            "if object only exists in HOT, create an entry in mt_slave
            CLEAR ls_slave.
            ls_slave-abap_hana_package_id = <master>-transport_obj_name.
            ls_slave-cts_hot_object_ref = lr_cts_hot_object.
            ls_slave-transport_obj_name = lv_transport_obj_name.
            ls_slave-object_name = <hot_object>-hana_object_name.
            ls_slave-object_suffix = <hot_object>-hana_object_suffix.
            APPEND  ls_slave TO mt_slave.

            "read entry again to have a pointer to the data in the table for the rest of assignments after the if.
            READ TABLE mt_slave INDEX lines( mt_slave ) ASSIGNING <slave>.

            set_max_length( EXPORTING i_text = <slave>-hana_object_name CHANGING c_length = mv_max_length_object_name ).
            set_max_length( EXPORTING i_text = <slave>-hana_object_suffix CHANGING c_length = mv_max_length_object_suffix ).
          ENDIF.

          "assign general data independent whether mt_slave had already an entry or not
          <slave>-hot_hana_package_id = <hot_object>-hana_package_id.
          <slave>-hot_hana_object_name = <hot_object>-hana_object_name.
          <slave>-hot_hana_object_suffix = <hot_object>-hana_object_suffix.
          <slave>-hot_abap_deployed_at = create_local_time_string( <hot_object>-abap_deployed_at ).
          <slave>-hot_abap_deployed_by = <hot_object>-abap_deployed_by.
          <slave>-hot_abap_synced_at = create_local_time_string( <hot_object>-abap_synced_at ).
          <slave>-hot_abap_synced_by = <hot_object>-abap_synced_by.
          <slave>-hot_abap_sync_system = <hot_object>-abap_sync_system.
          <slave>-hot_activated_at = create_local_time_string( <hot_object>-hana_activated_at ).
          <slave>-hot_activated_by = <hot_object>-hana_activated_by.
          <slave>-hot_hana_read_system = <hot_object>-hana_read_system.
          <slave>-hot_status = <hot_object>-hot_status.
          <slave>-hot_version = <hot_object>-hana_object_version.
          <slave>-abap_import_timestamp = create_local_time_string( CONV timestampl( <hot_object>-abap_import_timestamp(14) ) ).
          IF <master>-abap_no_translation IS INITIAL.
            <slave>-abap_object_reference = <hot_object>-abap_object_reference.
          ELSE.
            CLEAR <slave>-abap_object_reference.
          ENDIF.
          CONDENSE <slave>-hot_version.
          <slave>-hana_source_object_version = <hot_object>-hana_source_object_version.
          <slave>-exists_in_hota = abap_true.

          "Fix display of wrong timestamp in HOT created with older version of HTA organizer
          IF <slave>-exists_in_hana = abap_true AND <slave>-exists_in_hota = abap_true
             AND <slave>-hot_activated_at <> <slave>-hana_activated_at
             AND <slave>-hana_version = <slave>-hot_version
             AND <slave>-hana_package_id = <slave>-hot_hana_package_id
             AND <slave>-hana_object_name = <slave>-hot_hana_object_name
             AND <slave>-hana_object_suffix = <slave>-hot_hana_object_suffix.
            "if same version and same case of file name and package, we assume timestamps must be the same
            <slave>-hot_activated_at = <slave>-hana_activated_at.
          ENDIF.
        ENDLOOP.
      ELSE.
        "no data in HOT
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_sync_status.
    FIELD-SYMBOLS: <master> TYPE g_type_s_master,
                   <slave>  TYPE g_type_s_slave.

* ... 1.  handle packages
    LOOP AT mt_master ASSIGNING <master>.
      "set sync status
      IF <master>-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        <master>-sync_deploy_state = icon_led_inactive.
      ELSEIF "<master>-hana_delivery_unit <> <master>-hot_delivery_unit "DU difference is not a difference because HTA does not write DU data to HANA
         "OR <master>-hana_delivery_unit_vendor <> <master>-hot_delivery_unit_vendor
         <master>-hana_description <> <master>-hot_description
         OR <master>-hana_hints_for_translation <> <master>-hot_hints_for_translation
         OR <master>-hana_is_structural <> <master>-hot_is_structural
         OR <master>-hana_original_language <> <master>-hot_original_language
         OR <master>-hana_responsible <> <master>-hot_responsible
         OR <master>-hana_src_system <> <master>-hot_src_system
         OR <master>-hana_src_tenant <> <master>-hot_src_tenant
         OR <master>-hana_text_collection <> <master>-hot_text_collection
         OR <master>-hana_text_status <> <master>-hot_text_status
         OR <master>-hana_text_terminology_domain <> <master>-hot_text_terminology_domain.
        <master>-sync_deploy_state = icon_led_yellow.
      ELSE.
        <master>-sync_deploy_state = icon_led_green.
      ENDIF.

      "set color in case package id in HANA is different than in HOTA (but will be same transport object name!!!)
      IF <master>-exists_in_hana = abap_true AND <master>-exists_in_hota = abap_true AND <master>-hana_package_id <> <master>-hot_hana_package_id.
        "set color to blue in case HANA package id is different in HANA and HOTA
        append_change_color( EXPORTING i_name1 = 'HANA_PACKAGE_ID' i_name2 = 'HOT_HANA_PACKAGE_ID'
                             CHANGING ch_color_table = <master>-t_color  ).
        <master>-sync_deploy_state = icon_led_red. "Do not allow sync for case different packages
      ENDIF.

      mark_difference_package( IMPORTING ev_master = <master> ).
    ENDLOOP.

* ... 2. handle objects
    LOOP AT mt_slave ASSIGNING <slave>.

      IF <master>-transport_obj_name <> <slave>-abap_hana_package_id.
        READ TABLE mt_master ASSIGNING <master> WITH KEY transport_obj_name = <slave>-abap_hana_package_id.
      ENDIF.

      IF <master>-sync_deploy_state = icon_led_red.
        "set status to red if package has case difference, because this is currently not support.
        "no colors to be set for these slaves as the objects can not be compared
        <slave>-sync_deploy_state = icon_led_red.
      ELSEIF <slave>-exists_in_hota = abap_false AND <slave>-exists_in_hana = abap_true.
        <slave>-sync_deploy_state = icon_led_yellow.
      ELSEIF <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
            OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
            OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
            OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        <slave>-sync_deploy_state = icon_led_inactive.
      ELSEIF <slave>-hana_package_id <> <slave>-hot_hana_package_id
            OR <slave>-hana_object_name <> <slave>-hot_hana_object_name
            OR <slave>-hana_version <> <slave>-hot_version "Stephan
            OR <slave>-hana_activated_at <> <slave>-hot_activated_at
            OR <slave>-hana_activated_by <> <slave>-hot_activated_by.
        IF <slave>-exists_in_hana = abap_true AND <slave>-exists_in_hota = abap_true
            AND ( <slave>-hana_package_id <> <slave>-hot_hana_package_id
                  OR <slave>-hana_object_name <> <slave>-hot_hana_object_name
                  OR <slave>-hana_object_suffix <> <slave>-hot_hana_object_suffix ).
          <slave>-sync_deploy_state = icon_led_red.
        ELSE.
          <slave>-sync_deploy_state = icon_led_yellow.
        ENDIF.
      ELSE.
        <slave>-sync_deploy_state = icon_led_green.
      ENDIF.

      mark_difference_object( IMPORTING ev_slave = <slave> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD search_packges_in_hana_and_hot.

    CLEAR: e_hana_packages, e_hot_packages.

*...  read all packages with i_package name from HANA
    e_hana_packages = mr_hot_hana_connector->list_hana_packages( i_hana_package_name ).

*...  read all packages with i_package name from HOT
    "convert * to % for select with like
    DATA(lv_package) = i_hana_package_name.
    REPLACE ALL OCCURRENCES OF '*' IN lv_package WITH '%'.

    IF sy-subrc = 0 OR sy-subrc = 2. "switch for select with 'LIKE' or '='
      REPLACE ALL OCCURRENCES OF '_' IN lv_package WITH '\_'.
      SELECT hana_package_id FROM cts_hot_package INTO TABLE @e_hot_packages WHERE hana_package_id LIKE @lv_package ESCAPE '\' AND abap_status = 'A'.
      DATA(lv_case_like) = 'X'.
    ELSE.
      SELECT hana_package_id FROM cts_hot_package INTO TABLE @e_hot_packages WHERE hana_package_id = @lv_package AND abap_status = 'A'.
    ENDIF.

*    IF e_hana_packages IS INITIAL AND e_hot_packages IS INITIAL. "???? AND lv_leng < 41.
*      IF lv_case_like = 'X'. " AND strlen( lv_package ) < 81.
*        SELECT hana_package_id FROM cts_hot_package INTO TABLE @e_hot_packages WHERE abap_hana_package_id LIKE @lv_package ESCAPE '\' AND abap_status = 'A'.
*      ELSE.
**      ELSEIF strlen( i_hana_package_name ) < 41.
*        SELECT hana_package_id FROM cts_hot_package INTO TABLE @e_hot_packages WHERE abap_hana_package_id = @lv_package AND abap_status = 'A'.
*      ENDIF.
*      IF e_hot_packages IS NOT INITIAL.
*        LOOP AT e_hot_packages ASSIGNING FIELD-SYMBOL(<hot_package>).
*          DATA(l_hana_packages) = mr_hot_hana_connector->list_hana_packages( <hot_package> ).
*          APPEND LINES OF l_hana_packages TO e_hana_packages.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.

    "if subpackages should be selected, execute this method again for all found packages so far plus '.*' as input string. (with '.*' all subpackages will bes elected)
    IF i_include_subpackages = abap_true.
      "if last character of search string was a '*' then we do not need to search for subpackages anymore...
      DATA(lv_length) = strlen( i_hana_package_name ).
      IF lv_length > 0 AND substring( val = i_hana_package_name off = lv_length - 1 len = 1 ) <> '*'.
        DATA(lt_all_packages) = e_hana_packages.
        APPEND LINES OF e_hot_packages TO lt_all_packages.
        SORT lt_all_packages ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_all_packages.

        LOOP AT lt_all_packages INTO DATA(lv_hana_package).
          search_packges_in_hana_and_hot(
            EXPORTING
              i_hana_package_name      = lv_hana_package && '.*'
              i_include_subpackages   = abap_false "abap_false to stop recursive function call because '.*' will already contain all subpackages of all levels.
            IMPORTING
              e_hana_packages          = DATA(lt_hana_packages)
              e_hot_packages           = DATA(lt_hot_packages)
          ).
          APPEND LINES OF lt_hana_packages TO e_hana_packages.
          SORT e_hana_packages ASCENDING.
          DELETE ADJACENT DUPLICATES FROM e_hana_packages.

          APPEND LINES OF lt_hot_packages TO e_hot_packages.
          SORT e_hot_packages ASCENDING.
          DELETE ADJACENT DUPLICATES FROM e_hot_packages.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD sync_data.
    DATA: lv_text TYPE string.

    TRY.
        get_selected_data( EXPORTING i_check_deletion_consistency = abap_true
                           IMPORTING e_hotp_packages = DATA(lt_hotp_packages) e_hota_packages = DATA(lt_hota_packages) e_hota_objects = DATA(lt_hota_objects) e_hoto_objects = DATA(lt_hoto_objects) ).

        IF lt_hotp_packages IS INITIAL AND lt_hota_packages IS INITIAL AND lt_hoto_objects IS INITIAL AND lt_hota_objects IS INITIAL.
          MESSAGE 'Synchronisierung nicht möglich; markieren Sie Einträge mit gelbem oder grünem Status'(240) TYPE 'S'.
        ELSE.
          "if lt_hoto_objects should be synced check whether package is already known in HOT repo or part of HOTP and if not, add it to lt_hotp_packages

          "each package needs to be added only once, so group all objects by package
          LOOP AT lt_hoto_objects INTO DATA(lr_hoto_object) GROUP BY lr_hoto_object->abap_hana_package_id REFERENCE INTO DATA(lr_hoto_group).
            LOOP AT GROUP lr_hoto_group INTO DATA(lr_hoto_member).
              "check if package already in hotp
              DATA(package_part_of_hotp) = abap_false.
              LOOP AT lt_hotp_packages ASSIGNING FIELD-SYMBOL(<hotp>).
                IF <hotp>->abap_hana_package_id = lr_hoto_member->abap_hana_package_id.
                  package_part_of_hotp = abap_true.
                  EXIT.
                ENDIF.
              ENDLOOP.

              IF package_part_of_hotp = abap_false.
                READ TABLE mt_master REFERENCE INTO DATA(lr_master) WITH KEY transport_obj_name = lr_hoto_member->abap_hana_package_id.
                IF sy-subrc = 0 AND lr_master->cts_hot_package_ref IS BOUND.
                  IF lr_master->exists_in_hota = abap_false.
                    APPEND lr_master->cts_hot_package_ref TO lt_hotp_packages.
                  ENDIF.
                ELSE.
                  SELECT SINGLE abap_hana_package_id FROM cts_hot_package INTO @DATA(lv_abap_hana_package_id) WHERE abap_hana_package_id = @lr_hoto_member->abap_hana_package_id AND abap_status = 'A'.
                  IF sy-subrc <> 0.
                    APPEND cl_cts_hot_package=>create_instance( lr_hoto_member->hana_package_id ) TO lt_hotp_packages.
                  ENDIF.
                ENDIF.
                EXIT. "only one loop run per abap_hana_package_id needed.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          "add all as R3TR/LIMUs to TR or reuse TR if already part of a TR.
          DATA(lt_used_trkorrs) = add_to_transport_request( i_hotp_packages = lt_hotp_packages i_hota_packages = lt_hota_packages i_hoto_objects = lt_hoto_objects ).

          mr_hot_hana_connector->sync_packages_from_hana_to_hot( lt_hota_packages ).
          mr_hot_hana_connector->sync_packages_from_hana_to_hot( lt_hotp_packages ).

          mr_hot_hana_connector->read_objects_from_hana_to_hot( lt_hota_objects ).
          mr_hot_hana_connector->read_objects_from_hana_to_hot( lt_hoto_objects ).

          IF lines( lt_used_trkorrs ) = 0. " e.g.for $tmp objekte
            lv_text = '&1 Pakete/&2 Objekte synchronisiert'(184).
            REPLACE '&1' IN lv_text WITH |{ lines( lt_hota_packages ) + lines( lt_hotp_packages ) }|.
            REPLACE '&2' IN lv_text WITH |{ lines( lt_hota_objects ) + lines( lt_hoto_objects ) }|.
          ELSEIF lines( lt_used_trkorrs ) = 1.
            lv_text = '&1 Pakete/&2 Objekte zum Transportauftrag &3 hinzugefügt und synchronisiert'(180).
            REPLACE '&1' IN lv_text WITH |{ lines( lt_hota_packages ) + lines( lt_hotp_packages ) }|.
            REPLACE '&2' IN lv_text WITH |{ lines( lt_hota_objects ) + lines( lt_hoto_objects ) }|.
            REPLACE '&3' IN lv_text WITH lt_used_trkorrs[ 1 ].
          ELSE.
            lv_text = '&1 Pakete/&2 Objekte zu &3 Transportaufträgen hinzugefügt und synchronisiert'(181).
            REPLACE '&1' IN lv_text WITH |{ lines( lt_hota_packages ) + lines( lt_hotp_packages ) }|.
            REPLACE '&2' IN lv_text WITH |{ lines( lt_hota_objects ) + lines( lt_hoto_objects ) }|.
            REPLACE '&3' IN lv_text WITH |{ lines( lt_used_trkorrs ) }|.
          ENDIF.
          MESSAGE lv_text TYPE 'S'.

          " Refresh data to show sync result (status from yellow to green for items that were not in sync before)
          refresh_data( ).
        ENDIF.
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        DATA(t100_key) = hot_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
      CATCH cx_cts_hta INTO DATA(hta_exc).
        t100_key = hta_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hta_exc->message_variable_1 hta_exc->message_variable_2 hta_exc->message_variable_3 hta_exc->message_variable_4.
    ENDTRY.

  ENDMETHOD.

  METHOD get_selected_data.
    DATA: lr_selections        TYPE REF TO cl_salv_selections,
          lt_rows_packages     TYPE salv_t_row,
          lt_rows_objects      TYPE salv_t_row,
          lt_rows_hota_objects TYPE salv_t_row,
          l_row                TYPE i,
          lv_style             TYPE lvc_s_styl,
          lv_hota              TYPE abap_bool.

    CLEAR: e_hota_packages, e_hotp_packages, e_hota_objects, e_hoto_objects.

*... get selected packages
    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lt_rows_packages = lr_selections->get_selected_rows( ).
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... get selected objects
    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lt_rows_objects = lr_selections->get_selected_rows( ).
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... check for hota or hotp and fill exporting parameter
    LOOP AT lt_rows_packages INTO l_row.
      READ TABLE mt_master INDEX l_row REFERENCE INTO DATA(lr_master).

      "skip package if it is not allowed to be synced.
      IF lr_master->sync_deploy_state = icon_led_inactive OR lr_master->sync_deploy_state = icon_led_red.
        CONTINUE.
      ENDIF.

      "determine whether hotp or hota should be used (hota if all objects of this package are also selected and syncable, hotp otherwise)
      lv_hota = abap_true.
      CLEAR lt_rows_hota_objects.
      LOOP AT mt_slave REFERENCE INTO DATA(lr_slave) WHERE abap_hana_package_id = lr_master->transport_obj_name.
        IF NOT line_exists( lt_rows_objects[ table_line = sy-tabix ] )
                OR lr_slave->sync_deploy_state = icon_led_inactive OR lr_slave->sync_deploy_state = icon_led_red.
          lv_hota = abap_false.
          EXIT.
        ELSE.
          APPEND sy-tabix TO lt_rows_hota_objects.
        ENDIF.
      ENDLOOP.

      "If package should be deleted, check whether also all objects are selected. (all objects are selected if lv_hota = abap_true)
      IF i_check_deletion_consistency = abap_true AND lr_master->exists_in_hana = abap_false AND lv_hota = abap_false.
        DATA lv_text TYPE string.
        lv_text = 'Paket kann nur als Gesamtpaket gelöscht werden; selektieren Sie auch die Objekte von Paket &1'(182).
        REPLACE '&1' IN lv_text WITH lr_master->hot_hana_package_id.
        MESSAGE lv_text TYPE 'E'.
        "Ende Programmausfuehrung
      ENDIF.

      IF lv_hota = abap_true.
        APPEND lr_master->cts_hot_package_ref TO e_hota_packages.

        "add all obects of this package to e_hota_objects
        LOOP AT lt_rows_hota_objects INTO l_row.
          READ TABLE mt_slave INDEX l_row REFERENCE INTO lr_slave.

          "add objects only if allowed to be synched.
          IF lr_slave->sync_deploy_state <> icon_led_inactive AND lr_slave->sync_deploy_state <> icon_led_red.
            APPEND lr_slave->cts_hot_object_ref TO e_hota_objects.
            DELETE TABLE lt_rows_objects FROM l_row. "object selection not needed anymore
          ENDIF.
        ENDLOOP.
      ELSE.
        APPEND lr_master->cts_hot_package_ref TO e_hotp_packages.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_rows_objects INTO l_row.
      READ TABLE mt_slave INDEX l_row REFERENCE INTO lr_slave.
      "add objects only if allowed to be synched.
      IF lr_slave->sync_deploy_state <> icon_led_inactive AND lr_slave->sync_deploy_state <> icon_led_red.
        APPEND lr_slave->cts_hot_object_ref TO e_hoto_objects.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_to_transport_request.
    DATA: lv_transport_request TYPE trkorr,
          lt_used_trkorrs      TYPE ty_trkorrs_sorted,
          lv_is_deletion       TYPE abap_bool.

    set_masterlang_in_mt_master( i_hota_packages = i_hota_packages
                                 i_hotp_packages = i_hotp_packages
                                 i_hoto_objects  = i_hoto_objects ).

    lt_used_trkorrs = check_objects( i_hota_packages = i_hota_packages
                                     i_hotp_packages = i_hotp_packages
                                     i_hoto_objects  = i_hoto_objects ).

    "Only reuse transport request if there was only 1 request so far. otherwise we don't know which request was used last and
    "we even do not know whether order of tr_object_check was 'correct', because we process in order hota, hotp, hoto and not as selected by user
    IF lines( lt_used_trkorrs ) = 1.
      lv_transport_request = lt_used_trkorrs[ 1 ].
    ENDIF.

    LOOP AT i_hota_packages INTO DATA(lo_hota_package).
      CLEAR lv_is_deletion.
      READ TABLE mt_master REFERENCE INTO DATA(lr_master) WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lo_hota_package.
      IF lr_master->exists_in_hana = abap_false AND lr_master->exists_in_hota = abap_true.
        lv_is_deletion = abap_true.
      ENDIF.

      lv_transport_request = add_to_tr( i_pgmid = 'R3TR' i_obj_type = 'HOTA'
                                        i_obj_name = CONV #( lo_hota_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
                                        i_transport_request = lv_transport_request
                                        i_is_deletion = lv_is_deletion ).
      IF lv_transport_request NE space.
        INSERT lv_transport_request INTO TABLE lt_used_trkorrs.
      ENDIF.
    ENDLOOP.

    LOOP AT i_hotp_packages INTO DATA(lo_hotp_package).
      CLEAR lv_is_deletion.
      READ TABLE mt_master REFERENCE INTO lr_master WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lo_hotp_package.
      IF lr_master->exists_in_hana = abap_false AND lr_master->exists_in_hota = abap_true.
        lv_is_deletion = abap_true.
      ENDIF.

      lv_transport_request = add_to_tr( i_pgmid = 'LIMU' i_obj_type = 'HOTP'
                                        i_obj_name = CONV #( lo_hotp_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
                                        i_transport_request = lv_transport_request
                                        i_is_deletion = lv_is_deletion ).
      IF lv_transport_request NE space.
        INSERT lv_transport_request INTO TABLE lt_used_trkorrs.
      ENDIF.
    ENDLOOP.

    LOOP AT i_hoto_objects INTO DATA(lo_hoto_object).
      CLEAR lv_is_deletion.
      READ TABLE mt_slave REFERENCE INTO DATA(lr_slave) WITH KEY obj_ref COMPONENTS cts_hot_object_ref = lo_hoto_object.
      IF lr_slave->exists_in_hana = abap_false AND lr_slave->exists_in_hota = abap_true.
        lv_is_deletion = abap_true.
      ENDIF.

      lv_transport_request = add_to_tr( i_pgmid = 'LIMU' i_obj_type = 'HOTO'
                                        i_obj_name = lo_hoto_object->transport_object_name
                                        i_transport_request = lv_transport_request
                                        i_is_deletion = lv_is_deletion ).
      IF lv_transport_request NE space.
        INSERT lv_transport_request INTO TABLE lt_used_trkorrs.
      ENDIF.
    ENDLOOP.

    r_result = lt_used_trkorrs.
  ENDMETHOD.


  METHOD add_to_tr.
    DATA:
      lv_text     TYPE string,
      lv_mode     TYPE string,
      lv_devclass TYPE lcl_hota_organizer=>g_type_s_master-devclass.

    r_result = i_transport_request.

    READ TABLE mt_master REFERENCE INTO DATA(lr_master) WITH KEY transport_obj_name = i_obj_name(40).

    IF i_is_deletion = abap_true.
      lv_mode = 'DELETION'.
    ENDIF.

    IF lr_master->exists_in_hota = abap_true. "if so, tadir must also exist already
      lv_devclass = lr_master->devclass.
    ENDIF.


    TRY.
        r_result = mr_external_calls->rs_corr_insert(
          EXPORTING
            i_pgmid             = i_pgmid
            i_object_type       = i_obj_type
            i_object_name       = CONV #( i_obj_name )
            i_suppress_dialog   = space
            i_trkorr            = i_transport_request
            i_mode              = lv_mode
            i_masterlang        = lr_master->masterlang
            i_devclass          = lv_devclass "do not use lr_master->devclass as it might contain <unbekannt>
        ).
      CATCH cx_cts_hta_wbo INTO DATA(lr_exc).
        DATA(t100_key) = lr_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH lr_exc->message_variable_1 lr_exc->message_variable_2 lr_exc->message_variable_3 lr_exc->message_variable_4.
        "Ende Programmausfuehrung
    ENDTRY.

    IF r_result IS NOT INITIAL.
      DATA(tr) = |{ r_result }|.
      CONDENSE tr.

      lv_text = '&1 &2 &3 zum Transportauftrag &4 hinzugefügt'(137).
      REPLACE '&1' IN lv_text WITH i_pgmid.
      REPLACE '&2' IN lv_text WITH i_obj_type.
      REPLACE '&3' IN lv_text WITH i_obj_name.
      REPLACE '&4' IN lv_text WITH tr.

      MESSAGE lv_text TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD read_package_data_from_hana.
    LOOP AT mt_master REFERENCE INTO DATA(lr_master) WHERE exists_in_hana = abap_true.
      DATA(ls_package_data) = mr_hot_hana_connector->read_package_data_from_hana( lr_master->hana_package_id ).

      IF ls_package_data IS NOT INITIAL.
        lr_master->hana_delivery_unit = ls_package_data-hana_pack_delivery_unit.
        lr_master->hana_delivery_unit_vendor = ls_package_data-hana_pack_du_vendor.
        lr_master->hana_description = ls_package_data-hana_pack_description.
        lr_master->hana_hints_for_translation = ls_package_data-hana_pack_hints_for_transl.
        IF ls_package_data-hana_pack_is_structural = 1.
          lr_master->hana_is_structural = 'Ja'(149).
        ELSE.
          lr_master->hana_is_structural = 'Nein'(150).
        ENDIF.
        lr_master->hana_original_language = ls_package_data-hana_pack_orig_lang.
        lr_master->hana_responsible = ls_package_data-hana_pack_responsible.
        lr_master->hana_src_system = ls_package_data-hana_pack_src_system.
        lr_master->hana_src_tenant = ls_package_data-hana_pack_src_tenant.
        lr_master->hana_text_collection = ls_package_data-hana_pack_text_collection.
        lr_master->hana_text_status = ls_package_data-hana_pack_text_status.
        lr_master->hana_text_terminology_domain = ls_package_data-hana_pack_text_term_domain.
      ELSE.
        lr_master->exists_in_hana = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD read_package_data_from_hot.

    DATA: ls_hot_package     TYPE cts_hot_package,
          lr_cts_hot_package TYPE REF TO cl_cts_hot_package.

    LOOP AT mt_master REFERENCE INTO DATA(lr_master).
      CLEAR ls_hot_package.
      SELECT SINGLE * FROM cts_hot_package INTO ls_hot_package WHERE abap_hana_package_id = lr_master->cts_hot_package_ref->abap_hana_package_id AND abap_status = 'A'.
      IF ls_hot_package IS NOT INITIAL.
        lr_master->hot_delivery_unit = ls_hot_package-hana_pack_delivery_unit.
        lr_master->hot_delivery_unit_vendor = ls_hot_package-hana_pack_du_vendor.
        lr_master->hot_description = ls_hot_package-hana_pack_description.
        lr_master->hot_hints_for_translation = ls_hot_package-hana_pack_hints_for_transl.
        IF ls_hot_package-hana_pack_is_structural = 0.
          lr_master->hot_is_structural = 'Nein'(150).
        ELSE.
          lr_master->hot_is_structural = 'Ja'(149).
        ENDIF.
        lr_master->hot_original_language = ls_hot_package-hana_pack_orig_lang.
        lr_master->hot_responsible = ls_hot_package-hana_pack_responsible.
        lr_master->hot_src_system = ls_hot_package-hana_pack_src_system.
        lr_master->hot_src_tenant = ls_hot_package-hana_pack_src_tenant.
        lr_master->hot_text_collection = ls_hot_package-hana_pack_text_collection.
        lr_master->hot_text_status = ls_hot_package-hana_pack_text_status.
        lr_master->hot_text_terminology_domain = ls_hot_package-hana_pack_text_term_domain.
        lr_master->hot_abap_sync_system = ls_hot_package-abap_sync_system.
        lr_master->hot_abap_synced_at = create_local_time_string( ls_hot_package-abap_synced_at ).
        lr_master->hot_abap_synced_by = ls_hot_package-abap_synced_by.
        lr_master->hot_abap_deployed_at = create_local_time_string( ls_hot_package-abap_deployed_at ).
        lr_master->hot_abap_deployed_by = ls_hot_package-abap_deployed_by.
        lr_master->hana_read_system = ls_hot_package-hana_read_system.
        lr_master->hot_status           = ls_hot_package-hot_status.
        lr_master->hot_deploy_mode      = ls_hot_package-hot_activation_mode.
        lr_master->abap_no_translation  = ls_hot_package-abap_no_translation.
        lr_master->abap_import_timestamp  = create_local_time_string( CONV timestampl( ls_hot_package-abap_import_timestamp(14) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_local_time_string.

    DATA: lv_loc_date            TYPE d,
          lv_loc_time            TYPE t,
          lv_timestamp_as_string TYPE string,
          lv_milli_seconds       TYPE string,
          lv_text                TYPE string.

    IF i_timestamp_utc IS NOT INITIAL. "in initial case it contains 0.0000 that would be displayed...
      CONVERT TIME STAMP i_timestamp_utc TIME ZONE sy-zonlo INTO DATE lv_loc_date TIME lv_loc_time.
      IF sy-subrc <> 0.
        lv_text = 'Fehler (&1) bei der Umwandlung des Zeitstempels &2'(140).
        REPLACE '&1' IN lv_text WITH '' && sy-subrc.
        REPLACE '&2' IN lv_text WITH '' && i_timestamp_utc.
        MESSAGE lv_text TYPE 'E'.
      ENDIF.

*      lv_timestamp_as_string = i_timestamp_utc.
*      IF strlen( lv_timestamp_as_string ) >= 18.
*        lv_milli_seconds = lv_timestamp_as_string+15(3).
*      ENDIF.
*
*      IF lv_milli_seconds IS NOT INITIAL.
*        r_local_time = |{ lv_loc_date(4) }-{ lv_loc_date+4(2) }-{ lv_loc_date+6(2) } { lv_loc_time(2) }:{ lv_loc_time+2(2) }:{ lv_loc_time+4(2) }.{ lv_milli_seconds }|.
*      ELSE.
      r_local_time = |{ lv_loc_date(4) }-{ lv_loc_date+4(2) }-{ lv_loc_date+6(2) } { lv_loc_time(2) }:{ lv_loc_time+2(2) }:{ lv_loc_time+4(2) }|.
*      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD conv_hana_actvted_at_to_timest.
    DATA: activated_at TYPE string,
          day          TYPE d,
          time         TYPE t,
          timest       TYPE timestampl.

    activated_at = i_activated_at.

    "remove all unwanted chars in activated_at string (2014-04-14 14:26.13.1000000)
    REPLACE ALL OCCURRENCES OF REGEX '[\-\:\.\s]' IN activated_at WITH ''.

    day = activated_at(8).
    time = activated_at+8(6).

    IF mv_system_timezone IS INITIAL.
      CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
        IMPORTING
          timezone            = mv_system_timezone " Zeitzone
        EXCEPTIONS
          customizing_missing = 1
          OTHERS              = 2.

      IF sy-subrc <> 0.
        mv_system_timezone = sy-zonlo.
      ENDIF.
    ENDIF.

    "We do not use HANA timezone as we saw cases that at CET time (November) still CEST was returned (customizing issue SAP internally?).
    "in addition old coding also did not work, just using HANA timezone_offset (cl_cts_hot_hana_connector=>g_hana_timezone_string)
    "because with the currrent timezone_offset we can not create UTC timestamps for times created with DaySavingTime.
    "HANA system and ABAP system must run on same zimezone as per instguide therefore we use ABAP timezone instead
    CONVERT DATE day TIME time INTO TIME STAMP r_timest TIME ZONE mv_system_timezone.

    r_timest = r_timest + ( activated_at+14(3) / 1000 ). "adds the ms to the timestamp
  ENDMETHOD.

  METHOD preselect_all_out_of_sync.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row.

*... preselect packages
    LOOP AT mt_master REFERENCE INTO DATA(lr_master).
      IF lr_master->sync_deploy_state = icon_led_yellow.
        APPEND sy-tabix TO lt_rows.
      ENDIF.
    ENDLOOP.

    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... preselect objects
    CLEAR lt_rows.
    LOOP AT mt_slave REFERENCE INTO DATA(lr_slave).
      IF lr_slave->sync_deploy_state = icon_led_yellow.
        APPEND sy-tabix TO lt_rows.
      ENDIF.
    ENDLOOP.

    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD select_all_for_package.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          ls_cell       TYPE salv_s_cell,
          lt_rows       TYPE salv_t_row,
          lv_row        TYPE i,
          lv_text       TYPE string,
          ls_master     LIKE LINE OF mt_master.

*...get cell of cursor in packages.
    TRY.
        ls_cell = mr_hierseq->get_selections( 1 )->get_current_cell( ). " 1 stands for hierarchy level 'package'
        IF ls_cell IS NOT INITIAL.
          lv_row = ls_cell-row.
          "reset current cell as it might be user moves to legend and then still old cell is set
          CLEAR ls_cell.
          mr_hierseq->get_selections( 1 )->set_current_cell( ls_cell ).
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*...get cell of cursor in objects if it was not in packages.
    IF lv_row IS INITIAL.
      TRY.
          ls_cell = mr_hierseq->get_selections( 2 )->get_current_cell( ). " 2 stands for hierarchy level 'object'
          IF ls_cell IS NOT INITIAL.
            READ TABLE mt_master TRANSPORTING NO FIELDS WITH KEY transport_obj_name = mt_slave[ ls_cell-row ]-abap_hana_package_id.
            lv_row = sy-tabix.
            "reset current cell as it might be user moves to legend and then still old cell is set
            CLEAR ls_cell.
            mr_hierseq->get_selections( 2 )->set_current_cell( ls_cell ).
          ENDIF.
        CATCH cx_salv_not_found INTO exc.
          "should not happen because we always have 2 levels of data
          ls_msg = exc->get_message( ).
          MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      ENDTRY.
    ENDIF.

    IF lv_row IS INITIAL.
      lv_text = 'Setzen Sie den Cursor in eine Paket- oder Objektzeile der zu markierenden Daten'(141).
      MESSAGE lv_text TYPE 'S'.
      RETURN.
    ENDIF.

*...select package
    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lt_rows = lr_selections->get_selected_rows( ).

        READ TABLE mt_master INTO ls_master INDEX lv_row.
        IF ls_master-sync_deploy_state = icon_led_red OR ls_master-sync_deploy_state = icon_led_inactive. "icon_led_red(not synconizable) or ICON_LED_INACTIVE
          MESSAGE 'Keine Einträge markiert; markieren Sie Einträge mit gelbem oder grünem Status'(241) TYPE 'S'.
          " do nothing
          RETURN.
        ENDIF.

        IF NOT line_exists( lt_rows[ table_line = lv_row ] ).
          APPEND lv_row TO lt_rows.
          lr_selections->set_selected_rows( lt_rows ).
        ENDIF.
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*...select objects of package
    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lt_rows = lr_selections->get_selected_rows( ).
        DATA(row_added) = abap_false.
        READ TABLE mt_master INDEX lv_row REFERENCE INTO DATA(lr_master).
        LOOP AT mt_slave REFERENCE INTO DATA(lr_slave) WHERE abap_hana_package_id = lr_master->transport_obj_name.
          IF NOT line_exists( lt_rows[ table_line = sy-tabix ] ).
            APPEND sy-tabix TO lt_rows.
            row_added = abap_true.
          ENDIF.
        ENDLOOP.

        IF row_added = abap_true.
          lr_selections->set_selected_rows( lt_rows ).
        ENDIF.
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD deselect_all_for_package.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          ls_cell       TYPE salv_s_cell,
          lt_rows       TYPE salv_t_row,
          ls_row        TYPE i,
          lv_text       TYPE string.

*...get cell of cursor in packages.
    TRY.
        ls_cell = mr_hierseq->get_selections( 1 )->get_current_cell( ).
        IF ls_cell IS NOT INITIAL.
          ls_row = ls_cell-row.
          "reset current cell as it might be user moves to legend and then still old cell is set
          CLEAR ls_cell.
          mr_hierseq->get_selections( 1 )->set_current_cell( ls_cell ).
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*...get cell of cursor in objectsif it was not in packages.
    IF ls_row IS INITIAL.
      TRY.
          ls_cell = mr_hierseq->get_selections( 2 )->get_current_cell( ).
          IF ls_cell IS NOT INITIAL.
            READ TABLE mt_master TRANSPORTING NO FIELDS WITH KEY hana_package_id = mt_slave[ ls_cell-row ]-hana_package_id.
            ls_row = sy-tabix.
            "reset current cell as it might be user moves to legend and then still old cell is set
            CLEAR ls_cell.
            mr_hierseq->get_selections( 2 )->set_current_cell( ls_cell ).
          ENDIF.
        CATCH cx_salv_not_found INTO exc.
          "should not happen because we always have 2 levels of data
          ls_msg = exc->get_message( ).
          MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      ENDTRY.
    ENDIF.

    IF ls_row IS INITIAL.
      lv_text = 'Setzen Sie den Cursor in eine Paket- oder Objektzeile der entzumarkierenden Daten'(142).
      MESSAGE lv_text TYPE 'S'.
      RETURN.
    ENDIF.

*...deselect package
    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lt_rows = lr_selections->get_selected_rows( ).
        DELETE TABLE lt_rows FROM ls_row.
        IF sy-subrc = 0.
          lr_selections->set_selected_rows( lt_rows ).
        ENDIF.
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*...deselect objects of package
    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lt_rows = lr_selections->get_selected_rows( ).
        DATA(row_deleted) = abap_false.
        READ TABLE mt_master INDEX ls_row REFERENCE INTO DATA(lr_master).
        LOOP AT mt_slave REFERENCE INTO DATA(lr_slave) WHERE abap_hana_package_id = lr_master->transport_obj_name.
          DELETE TABLE lt_rows FROM sy-tabix.
          IF sy-subrc = 0.
            row_deleted = abap_true.
          ENDIF.
        ENDLOOP.

        IF row_deleted = abap_true.
          lr_selections->set_selected_rows( lt_rows ).
        ENDIF.
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD select_all.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row,
          nr_of_rows    TYPE i.

*... preselect packages
    LOOP AT mt_master ASSIGNING FIELD-SYMBOL(<master>).
      IF <master>-sync_deploy_state <> icon_led_red AND <master>-sync_deploy_state <> icon_led_inactive.
        APPEND sy-tabix TO lt_rows.
      ENDIF.
    ENDLOOP.

    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... preselect objects
    CLEAR lt_rows.
    LOOP AT mt_slave ASSIGNING FIELD-SYMBOL(<slave>).
      IF <slave>-sync_deploy_state <> icon_led_red AND <slave>-sync_deploy_state <> icon_led_inactive.
        APPEND sy-tabix TO lt_rows.
      ENDIF.
    ENDLOOP.
    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD deselect_all.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row.

*... deselect all packages
    TRY.
        lr_selections = mr_hierseq->get_selections( 1 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*... deselect all objects
    TRY.
        lr_selections = mr_hierseq->get_selections( 2 ).
        lr_selections->set_selected_rows( lt_rows ).
      CATCH cx_salv_not_found INTO exc.
        "should not happen because we always have 2 levels of data
        ls_msg = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD read_tadir_for_packages.

    DATA: lv_devclass TYPE tadir-devclass.

    LOOP AT mt_master ASSIGNING FIELD-SYMBOL(<master>).
      "##TODO TODO better to use: call function 'TR_TADIR_INTERFACE'
      SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid = 'R3TR' AND object = 'HOTA' AND obj_name = <master>-transport_obj_name.
      IF sy-subrc = 0.
        <master>-devclass = lv_devclass.

        set_max_length( EXPORTING i_text = CONV #( <master>-devclass ) CHANGING c_length = mv_max_length_devclass ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD replace_spaces_for_display.

    LOOP AT mt_master ASSIGNING FIELD-SYMBOL(<master>).
      IF <master>-hana_package_id IS INITIAL.
        <master>-hana_package_id = '<unbekannt>'(129).
      ENDIF.

      IF <master>-hana_delivery_unit <> <master>-hot_delivery_unit.
        IF <master>-hana_delivery_unit IS INITIAL.
          <master>-hana_delivery_unit = '<unbekannt>'(129).
        ELSEIF <master>-hot_delivery_unit IS INITIAL.
          <master>-hot_delivery_unit = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_delivery_unit_vendor <> <master>-hot_delivery_unit_vendor.
        IF <master>-hana_delivery_unit_vendor IS INITIAL.
          <master>-hana_delivery_unit_vendor = '<unbekannt>'(129).
        ELSEIF <master>-hot_delivery_unit_vendor IS INITIAL.
          <master>-hot_delivery_unit_vendor = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_description <> <master>-hot_description.
        IF <master>-hana_description IS INITIAL.
          <master>-hana_description = '<unbekannt>'(129).
        ELSEIF <master>-hot_description IS INITIAL.
          <master>-hot_description = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_hints_for_translation <> <master>-hot_hints_for_translation.
        IF <master>-hana_hints_for_translation IS INITIAL.
          <master>-hana_hints_for_translation = '<unbekannt>'(129).
        ELSEIF <master>-hot_hints_for_translation IS INITIAL.
          <master>-hot_hints_for_translation = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_is_structural <> <master>-hot_is_structural.
        IF <master>-hana_is_structural IS INITIAL.
          <master>-hana_is_structural = '<unbekannt>'(129).
        ELSEIF <master>-hot_is_structural IS INITIAL.
          <master>-hot_is_structural = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_original_language <> <master>-hot_original_language.
        IF <master>-hana_original_language IS INITIAL.
          <master>-hana_original_language = '<unbekannt>'(129).
        ELSEIF <master>-hot_original_language IS INITIAL.
          <master>-hot_original_language = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_responsible <> <master>-hot_responsible.
        IF <master>-hana_responsible IS INITIAL.
          <master>-hana_responsible = '<unbekannt>'(129).
        ELSEIF <master>-hot_responsible IS INITIAL.
          <master>-hot_responsible = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_src_system <> <master>-hot_src_system.
        IF <master>-hana_src_system IS INITIAL.
          <master>-hana_src_system = '<unbekannt>'(129).
        ELSEIF <master>-hot_src_system IS INITIAL.
          <master>-hot_src_system = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_src_tenant <> <master>-hot_src_tenant.
        IF <master>-hana_src_tenant IS INITIAL.
          <master>-hana_src_tenant = '<unbekannt>'(129).
        ELSEIF <master>-hot_src_tenant IS INITIAL.
          <master>-hot_src_tenant = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_text_collection <> <master>-hot_text_collection.
        IF <master>-hana_text_collection IS INITIAL.
          <master>-hana_text_collection = '<unbekannt>'(129).
        ELSEIF <master>-hot_text_collection IS INITIAL.
          <master>-hot_text_collection = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_text_status <> <master>-hot_text_status.
        IF <master>-hana_text_status IS INITIAL.
          <master>-hana_text_status = '<unbekannt>'(129).
        ELSEIF <master>-hot_text_status IS INITIAL.
          <master>-hot_text_status = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-hana_text_terminology_domain <> <master>-hot_text_terminology_domain.
        IF <master>-hana_text_terminology_domain IS INITIAL.
          <master>-hana_text_terminology_domain = '<unbekannt>'(129).
        ELSEIF <master>-hot_text_terminology_domain IS INITIAL.
          <master>-hot_text_terminology_domain = '<unbekannt>'(129).
        ENDIF.
      ENDIF.

      IF <master>-exists_in_hota = abap_false.
        <master>-hot_hana_package_id = '<unbekannt>'(129).
        <master>-devclass = '<unbekannt>'(129).
        <master>-hot_abap_synced_at = '<unbekannt>'(129).
        <master>-hot_abap_synced_by = '<unbekannt>'(129).
        <master>-hot_abap_sync_system = '<unbekannt>'(129).
        <master>-hot_status_as_text = '<unbekannt>'(129).
        <master>-hot_deploy_mode_as_text = '<unbekannt>'(129).
        <master>-abap_translation_as_text = '<unbekannt>'(129).
      ELSE.
        CASE <master>-hot_status.
          WHEN if_cts_hot_db_access=>co_hot_status_new.
            <master>-hot_status_as_text = 'N - Status gesetzt von Synchronisierung. Kann nicht deployt werden'(160).
          WHEN if_cts_hot_db_access=>co_hot_status_to_be_deleted.
            <master>-hot_status_as_text = 'D - Wird beim Deployment gelöscht'(161).
          WHEN if_cts_hot_db_access=>co_hot_status_inactive.
            <master>-hot_status_as_text = 'I - Noch nicht ins SAP HANA Repository deployt'(162).
          WHEN if_cts_hot_db_access=>co_hot_status_active.
            <master>-hot_status_as_text = 'A - Ins SAP HANA Repository deployt'(163).
          WHEN if_cts_hot_db_access=>co_hot_status_deploy_error.
            <master>-hot_status_as_text = 'E - Deployment ins SAP HANA Repository fehlgeschlagen'(243).
          WHEN if_cts_hot_db_access=>co_hot_status_delete_error.
            <master>-hot_status_as_text = 'Z - Löschung im SAP HANA Repository fehlgeschlagen'(244).
        ENDCASE.

        CASE <master>-hot_deploy_mode.
          WHEN ' ' OR if_cts_hot_db_access=>co_hot_deploy_mode_always.
            <master>-hot_deploy_mode_as_text = 'A - Paket und zugehörige Objekte werden beim Import direkt deployt'(169).
          WHEN if_cts_hot_db_access=>co_hot_deploy_mode_prework.
            <master>-hot_deploy_mode_as_text = 'P - Paket und zugehörige Objekte werden nur nach erfolgter Vorarbeit deployt'(170).
        ENDCASE.
        CASE <master>-abap_no_translation.
          WHEN if_cts_hot_db_access=>co_hot_relevant_for_transl. " ' '
            <master>-abap_translation_as_text = 'Paket und zugehörige Objekte sind relevant für Übersetzung'(197).
          WHEN if_cts_hot_db_access=>co_hot_not_relevant_for_transl. " 'X'
            <master>-abap_translation_as_text = 'Paket und zugehörige Objekte sind nicht relevant für Übersetzung'(198).
        ENDCASE.
      ENDIF.

      set_max_length( EXPORTING i_text = <master>-hana_description CHANGING c_length = mv_max_length_pack_desc ).
      set_max_length( EXPORTING i_text = <master>-hot_description CHANGING c_length = mv_max_length_pack_desc ).
      set_max_length( EXPORTING i_text = <master>-hana_delivery_unit CHANGING c_length = mv_max_length_pack_du_name ).
      set_max_length( EXPORTING i_text = <master>-hot_delivery_unit CHANGING c_length = mv_max_length_pack_du_name ).
      set_max_length( EXPORTING i_text = <master>-hana_delivery_unit_vendor CHANGING c_length = mv_max_length_pack_du_vendor ).
      set_max_length( EXPORTING i_text = <master>-hot_delivery_unit_vendor CHANGING c_length = mv_max_length_pack_du_vendor ).
      set_max_length( EXPORTING i_text = <master>-hana_hints_for_translation CHANGING c_length = mv_max_length_pack_hint ).
      set_max_length( EXPORTING i_text = <master>-hot_hints_for_translation CHANGING c_length = mv_max_length_pack_hint ).
      set_max_length( EXPORTING i_text = <master>-hana_responsible CHANGING c_length = mv_max_length_pack_responsible ).
      set_max_length( EXPORTING i_text = <master>-hot_responsible CHANGING c_length = mv_max_length_pack_responsible ).
      set_max_length( EXPORTING i_text = <master>-hana_src_system CHANGING c_length = mv_max_length_pack_src_system ).
      set_max_length( EXPORTING i_text = <master>-hot_src_system CHANGING c_length = mv_max_length_pack_src_system ).
      set_max_length( EXPORTING i_text = <master>-hana_src_tenant CHANGING c_length = mv_max_length_pack_src_tenant ).
      set_max_length( EXPORTING i_text = <master>-hot_src_tenant CHANGING c_length = mv_max_length_pack_src_tenant ).
      set_max_length( EXPORTING i_text = <master>-hana_text_collection CHANGING c_length = mv_max_length_pack_text_coll ).
      set_max_length( EXPORTING i_text = <master>-hot_text_collection CHANGING c_length = mv_max_length_pack_text_coll ).
      set_max_length( EXPORTING i_text = <master>-hana_text_terminology_domain CHANGING c_length = mv_max_length_pack_text_domain ).
      set_max_length( EXPORTING i_text = <master>-hot_text_terminology_domain CHANGING c_length = mv_max_length_pack_text_domain ).
      set_max_length( EXPORTING i_text = <master>-hana_text_status CHANGING c_length = mv_max_length_pack_text_status ).
      set_max_length( EXPORTING i_text = <master>-hot_text_status CHANGING c_length = mv_max_length_pack_text_status ).
    ENDLOOP.

    LOOP AT mt_slave ASSIGNING FIELD-SYMBOL(<slave>).
      IF <slave>-exists_in_hana = abap_false.
        "usually not possible
        <slave>-hana_package_id = '<unbekannt>'(129).
        <slave>-hana_object_name = '<unbekannt>'(129).
        <slave>-hana_object_suffix = '<unbekannt>'(129).
        <slave>-hana_activated_at = '<unbekannt>'(129).
        <slave>-hana_activated_by = '<unbekannt>'(129).
        <slave>-hana_hana_read_system = '<unbekannt>'(129).
        <slave>-hana_version = '<unbekannt>'(129). "Stephan
      ENDIF.

      IF <slave>-exists_in_hota = abap_false.
        <slave>-hot_hana_package_id = '<unbekannt>'(129).
        <slave>-hot_hana_object_name = '<unbekannt>'(129).
        <slave>-hot_hana_object_suffix = '<unbekannt>'(129).
        <slave>-hot_hana_read_system = '<unbekannt>'(129).
        <slave>-hot_activated_at = '<unbekannt>'(129).
        <slave>-hot_activated_by = '<unbekannt>'(129).
        <slave>-hot_abap_synced_at = '<unbekannt>'(129).
        <slave>-hot_abap_synced_by = '<unbekannt>'(129).
        <slave>-hot_abap_sync_system = '<unbekannt>'(129).
        <slave>-hot_status_as_text = '<unbekannt>'(129).
        <slave>-hot_version = '<unbekannt>'(129).
      ELSE.
        CASE <slave>-hot_status.
          WHEN if_cts_hot_db_access=>co_hot_status_new.
            <slave>-hot_status_as_text = 'N - Status gesetzt von Synchronisierung. Kann nicht deployt werden'(160).
          WHEN if_cts_hot_db_access=>co_hot_status_to_be_deleted.
            <slave>-hot_status_as_text = 'D - Wird beim Deployment gelöscht'(161).
          WHEN if_cts_hot_db_access=>co_hot_status_inactive.
            <slave>-hot_status_as_text = 'I - Noch nicht ins SAP HANA Repository deployt'(162).
          WHEN if_cts_hot_db_access=>co_hot_status_active.
            <slave>-hot_status_as_text = 'A - Ins SAP HANA Repository deployt'(163).
          WHEN if_cts_hot_db_access=>co_hot_status_deploy_error.
            <slave>-hot_status_as_text = 'E - Deployment ins SAP HANA Repository fehlgeschlagen'(243).
          WHEN if_cts_hot_db_access=>co_hot_status_delete_error.
            <slave>-hot_status_as_text = 'Z - Löschung im SAP HANA Repository fehlgeschlagen'(244).
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_column_length.

    IF i_max_length < i_min_length.
      i_column->set_output_length( i_min_length ).
    ELSE.
      i_column->set_output_length( i_max_length ).
    ENDIF.

  ENDMETHOD.


  METHOD find_package_with_diffrnt_case.

    CLEAR r_hana_package_id.

    DATA(lr_package_api) = cl_nhi_api=>create_instance( )->get_package( ).
    DATA(lv_hot_hana_package_id_upper) = to_upper( i_hot_hana_package_id ).
    DATA(lv_hota_length) = strlen( lv_hot_hana_package_id_upper ).
    DATA(lr_exists_response) = lr_package_api->exists(
          lr_package_api->create_exists_package_req(
                tenant = ''
                package = i_hot_hana_package_id
                ignore_case = abap_true ) ).
*        CATCH cx_nhi_hana_repository.    " .

    "1. check if package exists at all in different case
    IF lr_exists_response->error_code IS INITIAL AND lr_exists_response->exists = abap_true.
      "2. if package exists check for spelling (TODO: explain more clearly)
      "    - compare rootpackages in uppercase with first n chars of input package in upper case (n = length of current rootpackage)
      "    - if root package was found, get its sub packages and compare again in upper case mode.
      "    - redo get sub packages until package was found
      DATA(lr_root_package_response) = lr_package_api->list_root_packages( lr_package_api->create_list_root_packages_req( tenant = '' ) ).
*        CATCH cx_nhi_hana_repository.    " .
      IF lr_root_package_response->error_code = '0'.
        LOOP AT lr_root_package_response->rootpackages ASSIGNING FIELD-SYMBOL(<root_package>).
          DATA(lv_hanap_length) = strlen( <root_package>->package ).
          IF lv_hota_length = lv_hanap_length AND lv_hot_hana_package_id_upper = to_upper( <root_package>->package ).
            r_hana_package_id = <root_package>->package.
            EXIT. "we found our package, exit loop "Test-done
*          ELSEIF lv_hota_length >= lv_hanap_length AND lv_hot_hana_package_id_upper(lv_hanap_length) = to_upper( <root_package>->package ) AND "TODO: why lv_hota_length >= lv_hanap_length and not lv_hota_length > lv_hanap_length ?
          ELSEIF lv_hota_length > lv_hanap_length AND lv_hot_hana_package_id_upper(lv_hanap_length) = to_upper( <root_package>->package ) AND
                 lv_hot_hana_package_id_upper+lv_hanap_length(1) = '.'. " '.' is delimeter between package and sub package
            DATA(lv_package) = <root_package>->package.
            WHILE r_hana_package_id IS INITIAL.
              DATA(lr_get_sub_packages_response) = lr_package_api->get_sub_packages( lr_package_api->create_get_sub_packages_req( package = lv_package ) ).
*               CATCH cx_nhi_hana_repository.    "
              IF lr_get_sub_packages_response->error_code = '0'.
                IF lr_get_sub_packages_response->subpackages IS INITIAL.
                  EXIT. "TODO: test
                ENDIF.
                LOOP AT lr_get_sub_packages_response->subpackages ASSIGNING FIELD-SYMBOL(<sub_package>).
                  lv_hanap_length = strlen( <sub_package>->package ).
                  IF lv_hota_length = lv_hanap_length AND lv_hot_hana_package_id_upper = to_upper( <sub_package>->package ).
                    r_hana_package_id = <sub_package>->package.
                    EXIT. "we found our package, exit loop "Test-done
                  ELSEIF lv_hota_length >= lv_hanap_length AND lv_hot_hana_package_id_upper(lv_hanap_length) = to_upper( <sub_package>->package ) AND
                         lv_hot_hana_package_id_upper+lv_hanap_length(1) = '.'.
                    lv_package = <sub_package>->package.
                    EXIT. "exit loop and continue while loop with found package "Test-done
                  ENDIF.
                ENDLOOP.
              ELSE.
                "##todo error handling
                EXIT.
              ENDIF.
            ENDWHILE.
            EXIT. "end loop because we found root package "Test-done
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD append_change_color.

    DATA ls_color TYPE lvc_s_scol.

    "default setting for all color columns, blue and intensified
    ls_color-color-col = col_key.
    ls_color-color-int = 1.
    ls_color-color-inv = 0.

    ls_color-fname = i_name1.
    APPEND ls_color TO ch_color_table.

    IF i_name2 IS NOT INITIAL.
      ls_color-fname = i_name2.
      APPEND ls_color TO ch_color_table.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    mr_hot_hana_connector = cl_cts_hot_hana_connector=>create_instance( ).
    mr_external_calls = NEW cl_cts_hot_ext_call_intenal( ).
    CREATE OBJECT mr_data_provider TYPE lcl_data_provider.
  ENDMETHOD.

  METHOD refresh_data.
    DATA: lr_content     TYPE REF TO cl_salv_form_element,
          ls_filter_data TYPE ty_filter_data.
    TRY.
        "first clear global data so that layout might be adapted as well
        CLEAR: mt_master,
               mt_slave,
               mv_max_length_devclass,
               mv_max_length_object_name,
               mv_max_length_object_suffix,
               mv_max_length_package_id,
               mv_max_length_pack_desc,
               mv_max_length_pack_du_name,
               mv_max_length_pack_du_vendor,
               mv_max_length_pack_hint,
               mv_max_length_pack_responsible,
               mv_max_length_pack_src_system,
               mv_max_length_pack_src_tenant,
               mv_max_length_pack_text_coll,
               mv_max_length_pack_text_domain,
               mv_max_length_pack_text_status,
               mv_any_object_not_ok_in_hana.

        ls_filter_data = ms_filter_data. "preserve current filter settings

        select_data( i_hana_package_name = mv_search_string i_include_subpackages = mv_include_subpackages ).

        handle_dynamic_columns( ).

        SORT mt_master BY package_id ASCENDING.
        SORT mt_slave BY abap_hana_package_id object_name ASCENDING.

        preselect_all_out_of_sync( ).

*       in case number of packages/objects has been changed, top of list has to be changed as well
        build_header( CHANGING cr_content = lr_content ).
        mr_hierseq->set_top_of_list( lr_content ).

*... refresh the table in order to see the new data
        mr_hierseq->refresh( ).

        apply_filter(
            i_green    = ls_filter_data-show_green
            i_yellow   = ls_filter_data-show_yellow
            i_red      = ls_filter_data-show_red
            i_inactive = ls_filter_data-show_inactive
        ).
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        DATA(t100_key) = hot_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
      CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
        t100_key = nhi_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD show_versioning_for_currnt_cll.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          ls_cell       TYPE salv_s_cell,
          lt_rows       TYPE salv_t_row,
          ls_row        TYPE i,
          lv_obj_type   TYPE vrsd-objtype,
          lv_obj_name   TYPE vrsd-objname.

*...get cell of cursor in packages.
    TRY.
        ls_cell = mr_hierseq->get_selections( 1 )->get_current_cell( ).
        IF ls_cell IS NOT INITIAL.
          lv_obj_type = 'HOTP'.
          lv_obj_name = mt_master[ ls_cell-row ]-transport_obj_name.
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

*...get cell of cursor in objects if it was not in packages.
    IF lv_obj_type IS INITIAL.
      TRY.
          ls_cell = mr_hierseq->get_selections( 2 )->get_current_cell( ).
          IF ls_cell IS NOT INITIAL.
            lv_obj_type = 'HOTO'.
            lv_obj_name = mt_slave[ ls_cell-row ]-transport_obj_name.
          ENDIF.
        CATCH cx_salv_not_found INTO exc.
          "should not happen because we always have 2 levels of data
          ls_msg = exc->get_message( ).
          MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      ENDTRY.
    ENDIF.

    IF lv_obj_type IS INITIAL.
      MESSAGE 'Zur Anzeige der Versionsverwaltung müssen Sie den Cursor in eine Paket- oder Objektzeile setzen'(164) TYPE 'S'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SVRS_DISPLAY_DIRECTORY_NEW'
      EXPORTING
        object_type  = lv_obj_type   " Objekttyp (REPS, TABD, ...)
        object_name  = lv_obj_name   " Objektname
        recall       = ' '    " 'X' Version zurueckholen wird angeboten
      EXCEPTIONS
        no_directory = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD append_hana_objcts_to_mt_slave.

    DATA: ls_slave  TYPE g_type_s_slave.

    LOOP AT i_hana_objects INTO DATA(lr_hana_object).
      DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance2(
                                  io_cts_hot_package = i_master->cts_hot_package_ref
                                  iv_hana_object_name = lr_hana_object->name
                                  iv_hana_object_suffix = lr_hana_object->suffix ).
      CLEAR ls_slave.
      ls_slave-abap_hana_package_id = i_master->transport_obj_name.
      ls_slave-object_name = lr_hana_object->name.
      ls_slave-object_suffix = lr_hana_object->suffix.
      ls_slave-hana_package_id = i_master->hana_package_id.
      ls_slave-hana_object_name = lr_hana_object->name.
      ls_slave-hana_object_suffix = lr_hana_object->suffix.
      ls_slave-cts_hot_object_ref = lo_cts_hot_object.
      ls_slave-transport_obj_name = lo_cts_hot_object->transport_object_name.
      ls_slave-hana_version = lr_hana_object->metadata->version_id.
      ls_slave-hana_activated_by = CAST cl_nhi_metadata_active_ver( lr_hana_object->metadata )->activated_by.
      ls_slave-hana_activated_at = create_local_time_string( conv_hana_actvted_at_to_timest( CAST cl_nhi_metadata_active_ver( lr_hana_object->metadata )->activated_at ) ).
      ls_slave-sync_deploy_state = icon_light_out.
      ls_slave-exists_in_hana = abap_true.
      ls_slave-hana_object_status = i_object_status.

      APPEND ls_slave TO mt_slave.

      set_max_length( EXPORTING i_text = ls_slave-hana_object_name CHANGING c_length = mv_max_length_object_name ).
      set_max_length( EXPORTING i_text = ls_slave-hana_object_suffix CHANGING c_length = mv_max_length_object_suffix ).
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_dynamic_columns.

    TRY.
        "get columns for objects
        DATA(lr_columns) = mr_hierseq->get_columns( 2 ).

        "set column HANA_OBJECT_STATUS to invisible if all objects are OK but only in sync UI, always show in deploy UI
        DATA(lr_column) = lr_columns->get_column( 'HANA_OBJECT_STATUS' ).
        IF mv_any_object_not_ok_in_hana IS INITIAL AND gv_sync = 'X'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ELSE.
          lr_column->set_visible( ).
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD hide_name_columns.

    DATA: lv_current_cell TYPE salv_s_cell.

    TRY.
        lv_current_cell = mr_hierseq->get_selections( 1 )->get_current_cell( ).

        IF lv_current_cell IS NOT INITIAL.
          "package selected
          CLEAR mt_master[ lv_current_cell-row ]-package_id.
        ELSE.
          "object selected?
          lv_current_cell = mr_hierseq->get_selections( 2 )->get_current_cell( ).

          IF lv_current_cell IS NOT INITIAL.
            "object selected?
            CLEAR mt_slave[ lv_current_cell-row ]-object_name.
            CLEAR mt_slave[ lv_current_cell-row ]-object_suffix.
          ELSE.
            "nothing selected...
          ENDIF.
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD show_name_columns.

    DATA: lv_current_cell TYPE salv_s_cell.

    TRY.
        lv_current_cell = mr_hierseq->get_selections( 1 )->get_current_cell( ).

        IF lv_current_cell IS NOT INITIAL.
          "package was selected
          DATA(ls_master) = mt_master[ lv_current_cell-row ].
          IF ls_master-exists_in_hana = abap_true.
            mt_master[ lv_current_cell-row ]-package_id = ls_master-hana_package_id.
          ELSEIF ls_master-exists_in_hota = abap_true.
            mt_master[ lv_current_cell-row ]-package_id = ls_master-hot_hana_package_id.
          ENDIF.
        ELSE.
          "object selected?
          lv_current_cell = mr_hierseq->get_selections( 2 )->get_current_cell( ).

          IF lv_current_cell IS NOT INITIAL.
            "object selected?
            DATA(ls_slave) = mt_slave[ lv_current_cell-row ].
            IF ls_slave-exists_in_hana = abap_true.
              mt_slave[ lv_current_cell-row ]-object_name = ls_slave-hana_object_name.
              mt_slave[ lv_current_cell-row ]-object_suffix = ls_slave-hana_object_suffix.
            ELSEIF ls_slave-exists_in_hota = abap_true.
              mt_slave[ lv_current_cell-row ]-object_name = ls_slave-hot_hana_object_name.
              mt_slave[ lv_current_cell-row ]-object_suffix = ls_slave-hot_hana_object_suffix.
            ENDIF.
          ELSE.
            "nothing selected...
          ENDIF.
        ENDIF.
      CATCH cx_salv_not_found INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD set_max_length.

    DATA: lv_length TYPE i.

    lv_length = strlen( i_text ).
    IF lv_length > c_length.
      c_length = lv_length.
    ENDIF.

  ENDMETHOD.

  METHOD append_packages_to_mt_master.

    DATA: lr_cts_hot_package TYPE REF TO cl_cts_hot_package,
          ls_master          TYPE g_type_s_master.

    LOOP AT i_package_names_hana INTO DATA(lv_package_name_hana).
      "create cl_cts_hotpackage to get abap_hana_packageid, the key for HOTA
      lr_cts_hot_package = cl_cts_hot_package=>create_instance( lv_package_name_hana ).

      CLEAR ls_master.
      ls_master-package_id = lv_package_name_hana.
      ls_master-hana_package_id = lv_package_name_hana.
      ls_master-cts_hot_package_ref = lr_cts_hot_package.
      ls_master-transport_obj_name = lr_cts_hot_package->abap_hana_package_id.
      ls_master-exists_in_hana = abap_true.
      APPEND ls_master TO mt_master.

      set_max_length( EXPORTING i_text = lv_package_name_hana CHANGING c_length = mv_max_length_package_id ).
    ENDLOOP.

    "add all packages found in HOTA to mt_master but do not duplicate if package was already in HANA, only set HOTA exists then
    LOOP AT i_package_names_hot INTO DATA(lv_package_name_hot).
      "create cl_cts_hotpackage to get abap_hana_package_id, the key for HOTA
      lr_cts_hot_package = cl_cts_hot_package=>create_instance( lv_package_name_hot ).

      READ TABLE mt_master ASSIGNING FIELD-SYMBOL(<master>) WITH KEY transport_obj_name = lr_cts_hot_package->abap_hana_package_id.
      IF sy-subrc = 0.
        <master>-exists_in_hota = abap_true.
        <master>-hot_hana_package_id = lv_package_name_hot.
      ELSE.
        CLEAR ls_master.
        ls_master-package_id = lv_package_name_hot.
        ls_master-hot_hana_package_id = lv_package_name_hot.
        ls_master-cts_hot_package_ref = lr_cts_hot_package.
        ls_master-transport_obj_name = lr_cts_hot_package->abap_hana_package_id.
        ls_master-exists_in_hota = abap_true.
        ls_master-exists_in_hana = abap_false.
        APPEND ls_master TO mt_master.

        set_max_length( EXPORTING i_text = lv_package_name_hot CHANGING c_length = mv_max_length_package_id ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_packgs_for_diffrnt_cases.

    DATA lv_db_read_done TYPE abap_bool. "to only read from DB once and only in case there is data with exists_in_hota = abap_false

    "for all packages that are in HANA but not in HOTA check if they exist with different name in HOTA
    "1. get all hana_package_ids from HOTA (only once)
    "2. for all packages not in HOTA check for different name in HOT
    LOOP AT mt_master ASSIGNING FIELD-SYMBOL(<master>) WHERE exists_in_hota = abap_false.
      IF lv_db_read_done = abap_false.
        SELECT abap_hana_package_id, hana_package_id FROM cts_hot_package INTO TABLE @DATA(lt_all_hot_packages) WHERE abap_status = 'A'.  "#EC CI_NOFIRST
        lv_db_read_done = abap_true.
      ENDIF.

      READ TABLE lt_all_hot_packages WITH KEY abap_hana_package_id = <master>-transport_obj_name ASSIGNING FIELD-SYMBOL(<hot_package>).
*      SELECT SINGLE hana_package_id FROM cts_hot_package INTO lr_master->hot_hana_package_id WHERE abap_hana_package_id = lr_master->transport_obj_name AND abap_status = 'A'.
      IF sy-subrc = 0.
        <master>-exists_in_hota = abap_true.
        IF <master>-hana_package_id <> <hot_package>-hana_package_id.
          <master>-hot_hana_package_id = <hot_package>-hana_package_id.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "for all packages that are not in HANA but in HOTA search for different cases in HANA
    LOOP AT mt_master ASSIGNING <master> WHERE exists_in_hota = abap_true AND exists_in_hana = abap_false.
      DATA(tmp_hana_package_id) = find_package_with_diffrnt_case( <master>-hot_hana_package_id ).
      IF tmp_hana_package_id IS NOT INITIAL.
        <master>-hana_package_id = tmp_hana_package_id.
        <master>-exists_in_hana = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_tooltips.

    DATA:
      lr_tooltips TYPE REF TO cl_salv_tooltips,
      l_value     TYPE lvc_value.

    TRY.
        IF gv_sync = 'X'.
          lr_tooltips = mr_hierseq->get_functional_settings( )->get_tooltips( ).
          l_value = icon_led_green.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt ist in sync'(143) ).
          l_value = icon_led_yellow.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt ist nicht in sync'(144) ).
          l_value = icon_led_inactive.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt kann nicht synchron. werden'(145) ).
          l_value = icon_led_red.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt kann nicht synchron. werden'(145) ).
        ELSEIF gv_depl = 'X'.
          lr_tooltips = mr_hierseq->get_functional_settings( )->get_tooltips( ).
          l_value = icon_led_green.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt ist deployt'(230) ).
          l_value = icon_led_yellow.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt ist nicht deployt'(211) ).
          l_value = icon_led_inactive.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt kann nicht deployt werden'(212) ).
          l_value = icon_led_red.
          lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Paket/Objekt kann nicht deployt werden'(212) ).
        ENDIF.
        l_value = icon_alert.
        lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Objekt ist defekt'(146) ).
        l_value = icon_okay.
        lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Objekt ist OK'(147) ).
        l_value = icon_generate.
        lr_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = l_value tooltip = 'Objekt muss neu generiert werden'(148) ).
      CATCH cx_salv_existing INTO DATA(exc).
        "should not happen because we always have 2 levels of data
        DATA(ls_msg) = exc->get_message( ).
        MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD set_column_long_text.

    DATA: lv_old_text TYPE scrtext_l,
          lv_new_text TYPE string,
          lv_pos      TYPE i.

    lv_new_text = i_text_template.
    lv_old_text = i_column->get_long_text( ).

    IF strlen( lv_old_text ) > 40 - strlen( i_text_template ).
      lv_pos = 40 - strlen( i_text_template ) - 1. "-1 because of the '.' that we add
      lv_old_text = lv_old_text(lv_pos) && '.'.
    ENDIF.

    REPLACE '&1' IN lv_new_text WITH lv_old_text.
    i_column->set_long_text( CONV #( lv_new_text ) ).

  ENDMETHOD.

  METHOD build_header.
    DATA: lv_text     TYPE string,
          lv_tmp_text TYPE string,
          lv_filter   TYPE string,
          lv_info     TYPE string.

    IF me->is_data_filtered( ) = abap_true.
      lv_filter = '   Filter aktiv'(233).
    ENDIF.

    IF mv_include_subpackages = abap_true.
      lv_info = 'Anzeige für ''&1'' mit Unterpaketen: &2 Pakete/&3 Objekte'(133).
    ELSE.
      lv_info = 'Anzeige für ''&1'': &2 Pakete/&3 Objekte'(152).
    ENDIF.
    REPLACE '&1' IN lv_info WITH mv_search_string.
    lv_tmp_text = lines( mt_master ).
    CONDENSE lv_tmp_text.
    REPLACE '&2' IN lv_info WITH lv_tmp_text.
    lv_tmp_text = lines( mt_slave ).
    CONDENSE lv_tmp_text.
    REPLACE '&3' IN lv_info WITH lv_tmp_text.
    CONCATENATE lv_info lv_filter INTO lv_text.
    CREATE OBJECT cr_content TYPE cl_salv_form_text
      EXPORTING
        text    = lv_text
        tooltip = lv_text.
  ENDMETHOD.


  METHOD deploy.
    DATA: lr_selections         TYPE REF TO cl_salv_selections,
          lt_rows               TYPE salv_t_row,
          l_row                 TYPE i,
          lv_text               TYPE string,
          lv_popup_question     TYPE string,
          lv_popup_answer       TYPE c LENGTH 1,
          lv_force_deploy       TYPE abap_bool VALUE abap_false,
          lv_ask_for_force_depl TYPE abap_bool VALUE abap_false. "indicates whether user should be asked for force deployment or not (user gets asked if at least one selected package/object has hot_stauts = 'A')

    AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'HOTA' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

    IF sy-subrc <> 0.
      MESSAGE ID 'SCTS_HOT' type'E' NUMBER '031'.
      RETURN.
    ENDIF.

    TRY.
        DATA(lr_hta_api) = cl_cts_hta_api_factory=>create_instance( ).
        DATA(lr_deploy_list) = lr_hta_api->create_component_list( ).

        me->get_selected_data(
          IMPORTING
            e_hotp_packages = DATA(lt_hotps)
            e_hota_packages = DATA(lt_hotas)
            e_hoto_objects = DATA(lt_hotos)
            e_hota_objects = DATA(lt_hotos_hota)
        ).

        "for deployment we only care about hotps and hotos and not HOTAs
        APPEND LINES OF lt_hotas TO lt_hotps.
        APPEND LINES OF lt_hotos_hota TO lt_hotos.
        FREE: lt_hotas, lt_hotos_hota.

        IF lt_hotps IS INITIAL AND lt_hotos IS INITIAL.
          MESSAGE 'Deployment nicht möglich; markieren Sie Einträge mit gelbem oder grünem Status'(135) TYPE 'S'.
          RETURN.
        ENDIF.

        LOOP AT lt_hotps INTO DATA(lr_package).
          IF lv_ask_for_force_depl = abap_false
             AND line_exists( mt_master[ transport_obj_name = lr_package->abap_hana_package_id sync_deploy_state = icon_led_green ] ).
            lv_ask_for_force_depl = abap_true.
          ENDIF.

          lr_deploy_list->add_component( lr_hta_api->create_package_by_trobj_name( CONV #( lr_package->abap_hana_package_id ) ) ).
        ENDLOOP.

        LOOP AT lt_hotos INTO DATA(lr_object).
          IF lv_ask_for_force_depl = abap_false
               AND line_exists( mt_slave[ transport_obj_name = lr_object->transport_object_name sync_deploy_state = icon_led_green ] ).
            lv_ask_for_force_depl = abap_true.
          ENDIF.

          lr_deploy_list->add_component( lr_hta_api->create_object_by_trobj_name( CONV #( lr_object->transport_object_name ) ) ).
        ENDLOOP.

        IF lv_ask_for_force_depl = abap_true.
          lv_popup_question = 'Sie haben ein oder mehrere Pakete/Objekte selektiert, die bereits deployt wurden. Sollen diese erneut deployt werden?'(206).

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Pakete/Objekte erneut deployen'(207)    " Titel des Popup* Laenge 60
              text_question         = lv_popup_question    " Fragetext im Popup
              text_button_1         = 'Ja'(192)      " Text auf der ersten Drucktaste - 12 Zeichen
              text_button_2         = 'Nein'(193)    " Text auf der zweiten Drucktaste
              default_button        = '1'    " Cursorposition
              display_cancel_button = 'X'    " Schalter,ob Abbrechen-Drucktaste angezeigt wird
              userdefined_f1_help   = 'CTS_HTA_FORCE_DEPLOY_F1'    " Benutzerdefinierte F1-Hilfe
            IMPORTING
              answer                = lv_popup_answer   " Rückgabewerte: '1', '2', 'A'
            EXCEPTIONS
              OTHERS                = 1.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lv_popup_answer = '1'.
            lv_force_deploy = abap_true.

          ELSEIF lv_popup_answer = '2'.
            DATA: lv_onlygreen_or_nopacks_selctd TYPE c VALUE 'X',
                  lv_onlygreen_or_noobjs_selctd  TYPE c VALUE 'X'.
            LOOP AT lt_hotps INTO lr_package.
              IF NOT line_exists( mt_master[ transport_obj_name = lr_package->abap_hana_package_id sync_deploy_state = icon_led_green ] ).
                CLEAR lv_onlygreen_or_nopacks_selctd.
                EXIT.
              ELSE.
*               todo: in future lr_deploy_list->remove_component that shall not be deployed a second time
*                lr_deploy_list->remove_component( lr_hta_api->create_package_by_trobj_name( CONV #( lr_package->abap_hana_package_id ) ) ).
              ENDIF.
            ENDLOOP.

            LOOP AT lt_hotos INTO lr_object.
              IF NOT line_exists( mt_slave[ transport_obj_name = lr_object->transport_object_name sync_deploy_state = icon_led_green ] ).
                CLEAR lv_onlygreen_or_noobjs_selctd.
                EXIT.
              ELSE.
*               todo: in future lr_deploy_list->remove_component that shall not be deployed a second time
*                lr_deploy_list->remove_component( lr_hta_api->create_object_by_trobj_name( CONV #( lr_object->transport_object_name ) ) ).
              ENDIF.
            ENDLOOP.

            IF lv_onlygreen_or_nopacks_selctd = 'X' AND lv_onlygreen_or_noobjs_selctd = 'X'.
              MESSAGE 'Bereits deployte Pakete/Objekte wurden nicht erneut deployt'(239) TYPE 'S'.
              RETURN.
            ENDIF.

          ELSEIF lv_popup_answer = 'A'.
            MESSAGE 'Aktion durch Benutzer abgebrochen'(159) TYPE 'E'.
            "Ende Programmausfuehrung
          ENDIF.
        ENDIF.

        lr_deploy_list->deploy( EXPORTING i_force = lv_force_deploy
                                IMPORTING e_overall_deploy_status = DATA(lv_deploy_status)
                                          e_deploy_messages       = DATA(lt_deploy_logs)
        ).

        DATA(lv_logname) = mr_external_calls->save_deploy_log_to_transdir( i_log_messages  = lt_deploy_logs
                                                                           i_log_name_prefix = 'HTA_MANUAL_ACTIVATION' ).
        DATA(lv_level) = '2'.
        IF lv_deploy_status = 'I'.
          lv_text = 'Erfolg'(208).
        ELSEIF lv_deploy_status = 'W'.
          lv_text = 'Warnung'(203).
          lv_level = '3'.
        ELSEIF lv_deploy_status = 'E'.
          lv_text = 'Fehler'(209).
        ELSEIF lv_deploy_status = 'A'.
          lv_text = 'Fataler Fehler'(210).
        ENDIF.

        DATA(ls_splitted_text) = cl_cts_hot_utility=>split_text_50_chars( CONV #( lv_logname ) ).
        MESSAGE ID 'SCTS_HOT' TYPE 'S' NUMBER '030' WITH lv_text ls_splitted_text-chunk1 ls_splitted_text-chunk2 ls_splitted_text-chunk3.


        mr_external_calls->display_deploy_log( i_log_messages = lt_deploy_logs
                                               i_heading = CONV #( lv_logname )
                                               i_level = CONV #( lv_level ) ).

        me->refresh_data( ).
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        MESSAGE ID hot_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER hot_exc->if_t100_message~t100key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
      CATCH cx_cts_hta INTO DATA(lr_hta_exc).
        MESSAGE ID lr_hta_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lr_hta_exc->if_t100_message~t100key-msgno WITH lr_hta_exc->message_variable_1 lr_hta_exc->message_variable_2 lr_hta_exc->message_variable_3 lr_hta_exc->message_variable_4.
    ENDTRY.
  ENDMETHOD.

  METHOD set_deploy_mode.
    DATA: lv_popup_question     TYPE string,
          lv_popup_answer       TYPE c LENGTH 1,
          lv_target_deploy_mode TYPE cts_hot_activation_mode,
          lr_hot_package        TYPE REF TO cl_cts_hot_package,
          lr_master             TYPE REF TO g_type_s_master,
          lv_transport_request  TYPE trkorr,
          lv_transport_request2 TYPE trkorr,
          lv_count_changed_pkgs TYPE i,
          lv_text               TYPE string,
          lv_changed            TYPE abap_bool, "indicates whether at least one deploy mode was changed
          lv_popup_p_to_a_shown TYPE abap_bool. "indicates whether popup for change from prework to always was shown, to show only once

    TRY.
        get_selected_data( IMPORTING e_hotp_packages = DATA(lt_hotp_packages) e_hota_packages = DATA(lt_hota_packages) e_hota_objects = DATA(lt_hota_objects) e_hoto_objects = DATA(lt_hoto_objects) ).
        "in this method we do not need to distinguish between hota and hotp
        APPEND LINES OF lt_hota_packages TO lt_hotp_packages.

        IF lt_hotp_packages IS INITIAL.
          MESSAGE 'Deploymodus kann für markierte Pakete nicht festgelegt werden; markieren Sie Pakete mit gelbem oder grünem Status'(171) TYPE 'S'.
          RETURN.
        ENDIF.

        SORT lt_hotp_packages BY table_line->hana_package_id ASCENDING.

        lv_popup_question = 'Welcher Deploymodus soll für die markierten Pakete festgelegt werden?'(172).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Deploymodus festlegen'(173)    " Titel des Popup
            diagnose_object       = 'CTS_HTA_SYNC_UI_DEPLOY_MODE'    " Diagnosetext (Pflege über SE61)
            text_question         = lv_popup_question    " Fragetext im Popup
            text_button_1         = 'Modus A'(174)    " Text auf der ersten Drucktaste - 12 Zeichen
*           icon_button_1         = 'ICON_CHECKED'    " Ikone auf der ersten Drucktaste - 12 Zeichen
            text_button_2         = 'Modus P'(175)    " Text auf der zweiten Drucktaste
*           icon_button_2         = 'ICON_CANCEL'    " Ikone auf der zweiten Drucktaste
            default_button        = '1'    " Cursorposition
            display_cancel_button = 'X'    " Schalter,ob Abbrechen-Drucktaste angezeigt wird
            userdefined_f1_help   = 'CTS_HTA_SYNC_UI_DPL_MODE_F1'    " Benutzerdefinierte F1-Hilfe
*           start_column          =     " Startspalte, in der das POPUP beginnt
*           start_row             =     " Startzeile, in der das POPUP beginnt
*           popup_type            = 'ICON_MESSAGE_WARNING'    " Ikonentyp
            iv_quickinfo_button_1 = 'Modus A (Standard)'(176)   " Quickinfo auf der ersten Drucktaste
            iv_quickinfo_button_2 = 'Modus P (mit Vorarbeit)'(177)    " Quickinfo auf der zweiten Drucktaste
          IMPORTING
            answer                = lv_popup_answer   " Rückgabewerte: '1', '2', 'A'
*          TABLES
*           parameter             = lt_popup_params   " Übergabetabelle für Parameter im Text
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lv_popup_answer = '1'.
          lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_always.
        ELSEIF lv_popup_answer = '2'.
          lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework.
        ELSEIF lv_popup_answer = 'A'.
          MESSAGE 'Aktion durch Benutzer abgebrochen'(159) TYPE 'E'.
          "Ende Programmausfuehrung
        ENDIF.

        "if target is P, which is not the default, check whether all packages are known in HTA so that P can be set at all.
        IF lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework.
          LOOP AT lt_hotp_packages INTO lr_hot_package.
            READ TABLE mt_master REFERENCE INTO lr_master WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lr_hot_package.
            IF sy-subrc = 0 AND lr_master->exists_in_hota = abap_false AND lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework.
              MESSAGE 'Deploymodus kann erst nach erstmaliger Synchronisierung festgelegt werden; führen Sie zunächst die Synchronisierung durch'(178) TYPE 'E'.
              "Ende Programmausfuehrung
            ENDIF.
          ENDLOOP.
        ENDIF.

        "add package to TR and set deploy mode in case it is changed for some package
        LOOP AT lt_hotp_packages INTO lr_hot_package.
          READ TABLE mt_master REFERENCE INTO lr_master WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lr_hot_package.
          IF sy-subrc = 0 AND                                                                     "A is default. So if package not exist yet in HOTA, it does not matter because A will be used as default during sync
               ( ( lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_always AND lr_master->hot_deploy_mode <> if_cts_hot_db_access=>co_hot_deploy_mode_always AND lr_master->exists_in_hota = abap_true )
                OR ( lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework AND lr_master->hot_deploy_mode <> if_cts_hot_db_access=>co_hot_deploy_mode_prework ) ). "For setting P the entry needs to exist in HOTA which is checked before.

            lv_transport_request2 = check_object( i_pgmid = 'LIMU' i_obj_type = 'HOTP' i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) ).

            "in case check_object does not return any request, reuse request of previous object if there was a previous object.
            IF lv_transport_request2 IS NOT INITIAL.
              lv_transport_request = lv_transport_request2.
            ENDIF.

            lv_transport_request = add_to_tr( i_pgmid = 'LIMU' i_obj_type = 'HOTP'
                                              i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
                                              i_transport_request = lv_transport_request
                                              i_is_deletion = abap_false ).

            "update deploy mode in DB
            "##TODO move to cl_ctshot_dbaccess
            UPDATE cts_hot_package SET hot_activation_mode = lv_target_deploy_mode WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id AND abap_status = 'A'.

            "update deploy mode global table.
            lr_master->hot_deploy_mode = lv_target_deploy_mode.
            CASE lr_master->hot_deploy_mode.
              WHEN ' ' OR if_cts_hot_db_access=>co_hot_deploy_mode_always.
                lr_master->hot_deploy_mode_as_text = 'A - Paket und zugehörige Objekte werden beim Import direkt deployt'(169).
              WHEN if_cts_hot_db_access=>co_hot_deploy_mode_prework.
                lr_master->hot_deploy_mode_as_text = 'P - Paket und zugehörige Objekte werden nur nach erfolgter Vorarbeit deployt'(170).
            ENDCASE.
            lv_changed = abap_true.

            lv_count_changed_pkgs = lv_count_changed_pkgs + 1.

            IF lv_target_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_always
               AND lv_popup_p_to_a_shown = abap_false.
              MESSAGE ID 'SCTS_HOT' TYPE 'I' NUMBER 029.
              lv_popup_p_to_a_shown = abap_true.
            ENDIF.

            lv_text = 'Deploymodus festgelegt auf &1 für Paket &2'(179).
            REPLACE '&1' IN lv_text WITH lv_target_deploy_mode.
            REPLACE '&2' IN lv_text WITH lr_hot_package->hana_package_id.
            MESSAGE lv_text TYPE 'S'.
          ENDIF.
        ENDLOOP.

        IF lv_count_changed_pkgs > 1.
          lv_text = 'Deploymodus festgelegt auf &1 für &2 Pakete'(183).
          REPLACE '&1' IN lv_text WITH lv_target_deploy_mode.
          REPLACE '&2' IN lv_text WITH |{ lv_count_changed_pkgs }|.
          MESSAGE lv_text TYPE 'S'.
        ENDIF.
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        DATA(t100_key) = hot_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
    ENDTRY.

    IF lv_changed = abap_true.
      "preserve selection... therefore do not use refresh_data().
      TRY.
          DATA(lt_rows_packages) = mr_hierseq->get_selections( 1 )->get_selected_rows( ).
          DATA(lt_rows_objects) = mr_hierseq->get_selections( 2 )->get_selected_rows( ).

          mr_hierseq->refresh( ). "selektierte Pakete/Objekte gehen verloren...

          mr_hierseq->get_selections( 1 )->set_selected_rows( lt_rows_packages ).
          mr_hierseq->get_selections( 2 )->set_selected_rows( lt_rows_objects ).
        CATCH cx_salv_not_found INTO DATA(exc).
          "should not happen because we always have 2 levels of data
          DATA(ls_msg) = exc->get_message( ).
          MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD set_transl_mode.

    DATA: lv_popup_question            TYPE string,
          lv_popup_answer              TYPE c LENGTH 1,
          lr_hot_package               TYPE REF TO cl_cts_hot_package,
          lr_master                    TYPE REF TO g_type_s_master,
          lv_transport_request         TYPE trkorr,
          lv_transport_request2        TYPE trkorr,
          lv_count_changed_pkgs        TYPE i,
          lv_text                      TYPE string,
          lv_changed                   TYPE abap_bool,
          lt_languages                 TYPE STANDARD TABLE OF spras,
          lv_not_relevant_for_transl   TYPE cts_hot_abap_no_translation,
          lv_sync_required_popup_shown TYPE abap_bool VALUE abap_false.

    TRY.
        get_selected_data( IMPORTING e_hotp_packages = DATA(lt_hotp_packages) e_hota_packages = DATA(lt_hota_packages) e_hota_objects = DATA(lt_hota_objects) e_hoto_objects = DATA(lt_hoto_objects) ).
        "in this method we do not need to distinguish between hota and hotp
        APPEND LINES OF lt_hota_packages TO lt_hotp_packages.

        IF lt_hotp_packages IS INITIAL.
          MESSAGE 'Übersetzungsrelevanz kann für markierte Pakete nicht festgelegt werden; markieren Sie Pakete mit gelbem oder grünem Status'(242) TYPE 'S'.
          RETURN.
        ENDIF.

        SORT lt_hotp_packages BY table_line->hana_package_id ASCENDING.

        lv_popup_question = 'Sind die markierten Pakete und die zugehörigen Objekte übersetzungsrelevant?'(190).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Übersetzungsrelevanz festlegen'(191)    " Titel des Popup*
*           diagnose_object       = 'CTS_HTA_..........'    " Diagnosetext (Pflege über SE61)
            text_question         = lv_popup_question    " Fragetext im Popup
            text_button_1         = 'Ja'(192)      " Text auf der ersten Drucktaste - 12 Zeichen
*           icon_button_1         = 'ICON_CHECKED' " Ikone auf der ersten Drucktaste - 12 Zeichen
            text_button_2         = 'Nein'(193)    " Text auf der zweiten Drucktaste
*           icon_button_2         = 'ICON_CANCEL'  " Ikone auf der zweiten Drucktaste
            default_button        = '1'    " Cursorposition
            display_cancel_button = 'X'    " Schalter,ob Abbrechen-Drucktaste angezeigt wird
            userdefined_f1_help   = 'CTS_HTA_SET_TRANSL_MODE_F1'    " Benutzerdefinierte F1-Hilfe
*           start_column          =     " Startspalte, in der das POPUP beginnt
*           start_row             =     " Startzeile, in der das POPUP beginnt
*           popup_type            = 'ICON_MESSAGE_WARNING'    " Ikonentyp
            iv_quickinfo_button_1 = 'Pakete und zugehörige Objekte übersetzen'(194)   " Quickinfo auf der ersten Drucktaste
            iv_quickinfo_button_2 = 'Pakete und zugeh. Objekte n. übersetzen'(195) " Quickinfo auf der zweiten Drucktaste
          IMPORTING
            answer                = lv_popup_answer   " Rückgabewerte: '1', '2', 'A'
*          TABLES
*           parameter             = lt_popup_params   " Übergabetabelle für Parameter im Text
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lv_popup_answer = '1'.
          lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_relevant_for_transl.
        ELSEIF lv_popup_answer = '2'.
          lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_not_relevant_for_transl. " 'X'
        ELSEIF lv_popup_answer = 'A'.
          MESSAGE 'Aktion durch Benutzer abgebrochen'(159) TYPE 'E'.
          "Ende Programmausfuehrung
        ENDIF.

        "check if all packages are known in HTA so that the appropriate value can be set at all.
        LOOP AT lt_hotp_packages INTO lr_hot_package.
          READ TABLE mt_master REFERENCE INTO lr_master WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lr_hot_package.
          IF sy-subrc = 0 AND lr_master->exists_in_hota = abap_false.
            MESSAGE 'Übersetzungssrelevanz kann erst nach erstmaliger Synchronisierung festgelegt werden; führen Sie zunächst die Synchronisierung durch'(196) TYPE 'E'.
            "Ende Programmausfuehrung
          ENDIF.
        ENDLOOP.

        "add package to TR and set translation relevance in case it is changed for some package
        LOOP AT lt_hotp_packages INTO lr_hot_package.
          READ TABLE mt_master REFERENCE INTO lr_master WITH KEY pack_ref COMPONENTS cts_hot_package_ref = lr_hot_package.
          IF sy-subrc = 0 AND  " TODO: ist sy-subrc nicht immer = 0???
            lr_master->abap_no_translation <> lv_not_relevant_for_transl. " only proceed if translation relevance has changed

            lv_transport_request2 = check_object( i_pgmid = 'LIMU' i_obj_type = 'HOTP' i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) ).

            "in case check_object does not (todo: not?????) return any request, reuse request of previous object if there was a previous object.
            IF lv_transport_request2 IS NOT INITIAL.
              lv_transport_request = lv_transport_request2.
            ENDIF.

            lv_transport_request = add_to_tr( i_pgmid = 'LIMU' i_obj_type = 'HOTP'
                                              i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
                                              i_transport_request = lv_transport_request
                                              i_is_deletion = abap_false ).

            "update translation relevance in DB
            "##TODO/Daniel move to cl_ctshot_dbaccess
            UPDATE cts_hot_package SET abap_no_translation = lv_not_relevant_for_transl WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id AND abap_status = 'A'.

            "if translation is switched off, texts must be deleted and therefore LANG HOTA to be added to TR
            IF lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
****** start block without LANG HOTA for deletion (delete in 7.40 SP 13/7.50 SP 2)
              lv_transport_request2 = check_object( i_pgmid = 'R3TR' i_obj_type = 'HOTA' i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) ).  "##TODO: LANG instead of R3TR

              IF lv_transport_request2 IS NOT INITIAL.
                lv_transport_request = lv_transport_request2.
              ENDIF.

              lv_transport_request = add_to_tr( i_pgmid = 'R3TR' i_obj_type = 'HOTA' "##TODO: LANG instead of R3TR
                                                i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
                                                i_transport_request = lv_transport_request
                                                i_is_deletion = abap_false ). "isn't it a deletion because We delete all texts?

****** Ende block without LANG HOTA for deletion (delete in 7.40 SP 13/7.50 SP 2)
****** Start block with LANG HOTA for deletion (enable in 7.40 SP 13/7.50 SP 2)
*              "get master language from tadir
*              IF lr_master->masterlang IS INITIAL.
*                "##TODO better to use: call function 'TR_TADIR_INTERFACE'
*                SELECT SINGLE masterlang FROM tadir INTO lr_master->masterlang WHERE pgmid = 'R3TR' AND object = 'HOTA' AND obj_name = lr_master->transport_obj_name.
*                IF sy-subrc <> 0.
*                  "no tadir yet, so package was never added to transport before.
*                  MESSAGE 'Übersetzungssrelevanz kann erst nach erstmaligem Synchronisieren gesetzt werden; führen Sie zunächst die Synchronisierung durch'(196) TYPE 'E'.
*                  "Ende Programmausfuehrung
*                ENDIF.
*              ENDIF.
*
*              "check if there is only master language in HTA so far. If so, add LANG HOTA to transport request.
*              " If there are several languages already there, add R3TR HOTA to transport request (because of
*              " possible exports with mode LSM=master which would not allow different LANG HOTA than master language)
*              CLEAR lt_languages.
*              SELECT DISTINCT language FROM cts_hot_otexts_s INTO TABLE lt_languages WHERE abap_object_reference IN
*                                            ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id
*                                                                                                 AND abap_status = 'A' ).
*              SELECT DISTINCT language FROM cts_hot_otexts_l APPENDING TABLE lt_languages WHERE abap_object_reference IN
*                                            ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id
*                                                                                                 AND abap_status = 'A' ).
*
*              SORT lt_languages.
*              DELETE ADJACENT DUPLICATES FROM lt_languages.
*
*              IF lt_languages IS INITIAL OR ( lines( lt_languages ) = 1 AND lt_languages[ 1 ] = lr_master->masterlang ).
*                lv_transport_request2 = check_langu_object( i_obj_type = 'HOTA'
*                                                            i_obj_name = CONV #( lr_hot_package->abap_hana_package_id )
*                                                            i_masterlang = lr_master->masterlang ).
*
*                IF lv_transport_request2 IS NOT INITIAL.
*                 lv_transport_request = lv_transport_request2.
*                ENDIF.
*
*                lv_transport_request = add_langu_to_tr( i_obj_type = 'HOTA'
*                                                        i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
*                                                        i_masterlang = lr_master->masterlang
*                                                        i_transport_request = lv_transport_request ).
*              ELSE.
*                lv_transport_request2 = check_object( i_pgmid = 'R3TR' i_obj_type = 'HOTA' i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) ).
*
*                IF lv_transport_request2 IS NOT INITIAL.
*                  lv_transport_request = lv_transport_request2.
*                ENDIF.
*
*                lv_transport_request = add_to_tr( i_pgmid = 'R3TR' i_obj_type = 'HOTA'
*                                                  i_obj_name = CONV #( lr_hot_package->abap_hana_package_id ) "conv needed to extend c length 40 to c length 110
*                                                  i_transport_request = lv_transport_request
*                                                  i_is_deletion = abap_false ). "isn't it a deletion because We delete all texts?
*              ENDIF.
*
****** End block with LANG HOTA For deletion (enable in 7.40 SP 13/7.50 SP 2)*

              "Finally delete masterlang and text references from DB for all objects of the package for which language is switched off
              DELETE FROM cts_hot_otexts_h WHERE abap_object_reference IN
                                            ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id
                                                                                                 AND abap_status = 'A' ).
              DELETE FROM cts_hot_otexts_s WHERE abap_object_reference IN
                                            ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id
                                                                                                 AND abap_status = 'A' ).
              DELETE FROM cts_hot_otexts_l WHERE abap_object_reference IN
                                            ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id
                                                                                                 AND abap_status = 'A' ).

              UPDATE cts_hot_object SET abap_object_reference = '' WHERE abap_hana_package_id = lr_hot_package->abap_hana_package_id AND abap_status = 'A'.
            ENDIF.

            "if translation is switched on, show a popup that texts will by synced during next normal object sync only.
            IF lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_relevant_for_transl AND lv_sync_required_popup_shown = abap_false.
              lv_text = 'Die Texte der Objekte der gewählten Pakete stehen erst nach der nächsten Synchronisierung der Objekte im ABAP zur Verfügung'(202).
              MESSAGE lv_text TYPE 'I'.
              lv_sync_required_popup_shown = abap_true.
            ENDIF.

            "update translation relevance in  global table.
            lr_master->abap_no_translation = lv_not_relevant_for_transl.
            CASE lr_master->abap_no_translation.
              WHEN if_cts_hot_db_access=>co_hot_relevant_for_transl.
                lr_master->abap_translation_as_text = 'Paket und zugehörige Objekte sind relevant für Übersetzung'(197).
              WHEN if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
                lr_master->abap_translation_as_text = 'Paket und zugehörige Objekte sind nicht relevant für Übersetzung'(198).
            ENDCASE.
            lv_changed = abap_true.

            IF lr_master->abap_no_translation = if_cts_hot_db_access=>co_hot_relevant_for_transl.
              lv_text = 'Paket &1 wurde festgelegt auf übersetzungsrelevant'(199).
              REPLACE '&1' IN lv_text WITH lr_hot_package->hana_package_id.
              MESSAGE lv_text TYPE 'S'.
              lv_count_changed_pkgs = lv_count_changed_pkgs + 1.
            ELSEIF lr_master->abap_no_translation = if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
              lv_text = 'Paket &1 wurde festgelegt auf nicht übersetzungsrelevant'(200).
              REPLACE '&1' IN lv_text WITH lr_hot_package->hana_package_id.
              MESSAGE lv_text TYPE 'S'.
              lv_count_changed_pkgs = lv_count_changed_pkgs + 1.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lv_count_changed_pkgs > 1.
          IF lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_relevant_for_transl.
            lv_text = '&1 Pakete wurden festgelegt auf übersetzungsrelevant'(204).
          ELSEIF lv_not_relevant_for_transl = if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
            lv_text = '&1 Pakete wurden festgelegt auf nicht übersetzungsrelevant'(205).
          ENDIF.
          REPLACE '&1' IN lv_text WITH |{ lv_count_changed_pkgs }|.
          MESSAGE lv_text TYPE 'S'.
        ENDIF.
      CATCH cx_hana_object_transport INTO DATA(hot_exc).
        DATA(t100_key) = hot_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hot_exc->msgv1 hot_exc->msgv2 hot_exc->hana_error_code hot_exc->hana_error_msg.
    ENDTRY.

    IF lv_changed = abap_true.
      "preserve selection... therefore do not use refresh_data().
      TRY.
          DATA(lt_rows_packages) = mr_hierseq->get_selections( 1 )->get_selected_rows( ).
          DATA(lt_rows_objects) = mr_hierseq->get_selections( 2 )->get_selected_rows( ).

          mr_hierseq->refresh( ). "selektierte Pakete/Objekte gehen verloren...

          mr_hierseq->get_selections( 1 )->set_selected_rows( lt_rows_packages ).
          mr_hierseq->get_selections( 2 )->set_selected_rows( lt_rows_objects ).
        CATCH cx_salv_not_found INTO DATA(exc).
          "should not happen because we always have 2 levels of data
          DATA(ls_msg) = exc->get_message( ).
          MESSAGE ID ls_msg-msgid TYPE 'E' NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD set_masterlang_in_mt_master.
    DATA: lt_sobj_names              TYPE SORTED TABLE OF sobj_name WITH UNIQUE KEY table_line,
          lv_popup_question          TYPE string,
          lv_popup_button1_quickinfo TYPE text132,
          lv_popup_answer            TYPE c LENGTH 1,
          ls_popup_param             TYPE spar,
          lt_popup_params            TYPE STANDARD TABLE OF spar,
          lv_fallback                TYPE i,
          lv_text                    TYPE string.

    "1. create table with all sobj_names of passed data (tadir name)
    LOOP AT i_hota_packages INTO DATA(lr_package).
      INSERT lr_package->abap_hana_package_id INTO TABLE lt_sobj_names.
    ENDLOOP.
    LOOP AT i_hotp_packages INTO lr_package.
      INSERT lr_package->abap_hana_package_id INTO TABLE lt_sobj_names.
    ENDLOOP.
    LOOP AT i_hoto_objects INTO DATA(lr_object).
      INSERT lr_object->abap_hana_package_id INTO TABLE lt_sobj_names.
    ENDLOOP.

    "2. For all sobj_names set ABAP language on package if not yet set during this user session.
    LOOP AT lt_sobj_names INTO DATA(lv_sobj_name).
      READ TABLE mt_master REFERENCE INTO DATA(lr_master) WITH KEY transport_obj_name = lv_sobj_name.

      "##TODO maybe ask user every time whether masterlang in tadir is still OK if not yet maintained in HANA?
      IF lr_master->exists_in_hana = abap_false "deletion case. language not needed because tadir should already exists
          OR lr_master->exists_in_hota = abap_true "already exists in HOTA, language not needed because tadir already exists
          OR lr_master->masterlang IS NOT INITIAL. "language already set
        CONTINUE.
      ENDIF.

      TRY.
          lr_master->masterlang = mr_external_calls->determine_masterlang_for_tadir(
            EXPORTING
              i_hana_package_name            = lr_master->hana_package_id
              i_hana_original_language       = lr_master->hana_original_language
              i_suppress_dialog              = space
              i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
          ).

          IF lr_master->masterlang = sy-langu AND lr_master->hana_original_language IS INITIAL.
            lr_master->abap_no_translation = if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
            lr_master->abap_translation_as_text = 'Paket und zugehörige Objekte sind nicht relevant für Übersetzung'(198).
          ENDIF.
        CATCH cx_cts_hta_unknown_master_lang cx_cts_hta INTO DATA(lr_exc).
          DATA(t100_key) = lr_exc->if_t100_message~t100key.
          MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH lr_exc->message_variable_1 lr_exc->message_variable_2 lr_exc->message_variable_3 lr_exc->message_variable_4.
          "Ende Programmausfuehrung
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_objects.
    DATA: lv_transport_request TYPE trkorr.

    LOOP AT i_hota_packages INTO DATA(lr_package).
      lv_transport_request = check_object( i_pgmid = 'R3TR' i_obj_type = 'HOTA'
                                           i_obj_name = CONV cts_hot_object_name( lr_package->abap_hana_package_id ) ).
      IF lv_transport_request IS NOT INITIAL.
        INSERT lv_transport_request INTO TABLE r_result.
      ENDIF.
    ENDLOOP.

    LOOP AT i_hotp_packages INTO lr_package.
      lv_transport_request = check_object( i_pgmid = 'LIMU' i_obj_type = 'HOTP'
                                           i_obj_name = CONV cts_hot_object_name( lr_package->abap_hana_package_id ) ).
      IF lv_transport_request IS NOT INITIAL.
        INSERT lv_transport_request INTO TABLE r_result.
      ENDIF.
    ENDLOOP.

    LOOP AT i_hoto_objects INTO DATA(lr_object).
      lv_transport_request = check_object( i_pgmid = 'LIMU' i_obj_type = 'HOTO'
                                           i_obj_name = lr_object->transport_object_name ).
      IF lv_transport_request IS NOT INITIAL.
        INSERT lv_transport_request INTO TABLE r_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_object.
    DATA:
      lv_text        TYPE string,
      lv_global_lock TYPE c.

    check_transport_tools( ).

    READ TABLE mt_master REFERENCE INTO DATA(lr_master) WITH KEY transport_obj_name = i_obj_name(40).

    TRY.
        r_result = mr_external_calls->rs_corr_check(
            i_pgmid           = i_pgmid
            i_object_type     = i_obj_type
            i_object_name     = CONV #( i_obj_name )
            i_suppress_dialog = space
        ).
      CATCH cx_cts_hta_wbo INTO DATA(lr_exc).
        DATA(t100_key) = lr_exc->if_t100_message~t100key.
        MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH lr_exc->message_variable_1 lr_exc->message_variable_2 lr_exc->message_variable_3 lr_exc->message_variable_4.
        "Ende Programmausfuehrung
    ENDTRY.
  ENDMETHOD.


  METHOD check_langu_object.
    DATA:
      lv_text        TYPE string,
      lv_global_lock TYPE c,
      ls_wi_ko200    TYPE ko200.

    check_transport_tools( ).

    ls_wi_ko200-pgmid = 'LANG'.
    ls_wi_ko200-object = i_obj_type.
    ls_wi_ko200-obj_name = i_obj_name.
    ls_wi_ko200-lang = i_masterlang.

    CALL FUNCTION 'TR_OBJECT_CHECK'
      EXPORTING
        wi_ko200                = ls_wi_ko200    " Eingabe zu editierendes Objekt
*       iv_no_standard_editor   = ' '    " Allgemeines Flag
*       iv_no_show_option       = ' '    " Kein 'Anzeigen' auf Fehlerpopup
*       iv_externalps           = ' '
*       iv_externalid           = ' '
*       iv_no_ps                = ' '    " Allgemeines Flag
*       it_e071k_str            =     " (obsolet - IT_OBJ_ENTRIES verwenden) Tabellenschlüssel mit S
*       it_obj_entry            =     " Schnittstellenstruktur für Objekte mit Keys im CTS
      IMPORTING
        we_order                = r_result    " gewählter Auftrag (Transportauftrag)
*       we_task                 =     " gewählte Aufgabe (Korrektur/Reparatur)
*       we_ko200                =     " Ausgabe zu editierendes Objekt
*       we_object_appendable    =     " 'X': TR_OBJECT_INSERT wird Objekt aufnehmen
*       es_tadir                =     " Ausgabe Objektkatalogeintrag
*      TABLES
*       wt_e071k                =     " Eingabetabelle zu editierender Objekt-keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "Ende Programmausfuehrung
    ENDIF.
  ENDMETHOD.


  METHOD add_langu_to_tr.
    DATA: ls_wi_ko200 TYPE ko200.

    ls_wi_ko200-pgmid = 'LANG'.
    ls_wi_ko200-object = i_obj_type.
    ls_wi_ko200-obj_name = i_obj_name.
    ls_wi_ko200-lang = i_masterlang.

    CALL FUNCTION 'TR_OBJECT_INSERT'
      EXPORTING
        wi_order                = i_transport_request    " vorgeschlagener Auftrag (Prio vor Auftragssuche)
        wi_ko200                = ls_wi_ko200   " Eingabe editiertes Objekt
*       iv_no_standard_editor   = ' '    " Allgemeines Flag
*       iv_no_show_option       = ' '    " Kein 'Anzeigen' auf Fehlerpopup
*       iv_externalps           = ' '
*       iv_externalid           = ' '
*       iv_no_ps                = ' '    " Allgemeines Flag
*       iv_old_call             =     " Alte Semantik
*       it_e071k_str            =     " (obsolet - IT_OBJ_ENTRIES verwenden) Tabellenschlüssel mit S
*       it_obj_entry            =     " Schnittstellenstruktur für Objekte mit Keys im CTS
      IMPORTING
        we_order                = r_result   " gewählter Auftrag (Transportauftrag)
*       we_task                 =     " gewählte Aufgabe (Korrektur/Reparatur)
*       we_ko200                =     " Ausgabe editiertes Objekt
*       es_tadir                =     " Ausgabe Objektkatalogeintrag
*      TABLES
*       wt_e071k                =     " Eingabetabelle editierter Objekt-keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "Ende Programmausfuehrung
    ENDIF.
  ENDMETHOD.


  METHOD check_transport_tools.
    TRY.
        mr_external_calls->check_transport_tools_for_sync( i_suppress_dialog = abap_false ).
      CATCH cx_cts_hta INTO DATA(lo_exc).
        MESSAGE ID lo_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lo_exc->if_t100_message~t100key-msgno
                WITH lo_exc->message_variable_1 lo_exc->message_variable_2 lo_exc->message_variable_3 lo_exc->message_variable_4.
    ENDTRY.
  ENDMETHOD.


  METHOD calculate_depl_status.
    FIELD-SYMBOLS: <master> TYPE g_type_s_master,
                   <slave>  TYPE g_type_s_slave.

* ... 1.  handle packages
    LOOP AT mt_master ASSIGNING <master>.
      "set sync status
      IF <master>-exists_in_hana = abap_true AND <master>-exists_in_hota = abap_true AND <master>-hana_package_id <> <master>-hot_hana_package_id.
        <master>-sync_deploy_state = icon_led_red. "Do not allow sync for case different packages
      ELSEIF <master>-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
        OR <master>-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        <master>-sync_deploy_state = icon_led_yellow.
      ELSEIF <master>-hot_status = if_cts_hot_db_access=>co_hot_status_active.
        <master>-sync_deploy_state = icon_led_green.
      ELSEIF <master>-hot_status = if_cts_hot_db_access=>co_hot_status_new
        OR  <master>-exists_in_hota = abap_false.
        <master>-sync_deploy_state = icon_led_inactive.
      ELSE. "for other cases
        <master>-sync_deploy_state = icon_led_inactive.
      ENDIF.

      mark_difference_package( IMPORTING ev_master = <master> ).
    ENDLOOP.

* ... 2. handle objects
    UNASSIGN <master>.
    LOOP AT mt_slave ASSIGNING <slave>.

      IF <master> IS NOT ASSIGNED OR <master>-transport_obj_name <> <slave>-abap_hana_package_id.
        READ TABLE mt_master ASSIGNING <master> WITH KEY transport_obj_name = <slave>-abap_hana_package_id.
      ENDIF.

      IF <master>-sync_deploy_state = icon_led_red "set object status to red if package has case difference, because this is currently not supported
        OR ( <slave>-exists_in_hana = abap_true AND <slave>-exists_in_hota = abap_true
            AND ( <slave>-hana_package_id <> <slave>-hot_hana_package_id
                  OR <slave>-hana_object_name <> <slave>-hot_hana_object_name
                  OR <slave>-hana_object_suffix <> <slave>-hot_hana_object_suffix ) ).
        <slave>-sync_deploy_state = icon_led_red.
      ELSEIF <slave>-exists_in_hota = abap_false
        OR   <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_new.
        <slave>-sync_deploy_state = icon_led_inactive.
      ELSEIF <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_inactive
        OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
        OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
        OR <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
        <slave>-sync_deploy_state = icon_led_yellow.
      ELSEIF <slave>-hot_status = if_cts_hot_db_access=>co_hot_status_active.
        <slave>-sync_deploy_state = icon_led_green.
      ELSE.
        <slave>-sync_deploy_state = icon_led_inactive.
      ENDIF.

      IF <slave>-sync_deploy_state <> icon_led_red.
        "no colors to be set for these slaves as the objects can not be compared
        mark_difference_object( IMPORTING ev_slave = <slave> ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD mark_difference_package.
    FIELD-SYMBOLS <master_c> TYPE g_type_s_master.
    ASSIGN ev_master TO  <master_c>.

    IF <master_c>-hana_package_id <> <master_c>-hot_hana_package_id.
      append_change_color( EXPORTING i_name1 = 'HANA_PACKAGE_ID' i_name2 = 'HOT_HANA_PACKAGE_ID'
                           CHANGING ch_color_table = <master_c>-t_color  ).
      append_change_color( EXPORTING i_name1 = 'PACKAGE_ID' i_name2 = 'PACKAGE_ID'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_description <> <master_c>-hot_description.
      append_change_color( EXPORTING i_name1 = 'HANA_DESCRIPTION' i_name2 = 'HOT_DESCRIPTION'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_hints_for_translation <> <master_c>-hot_hints_for_translation.
      append_change_color( EXPORTING i_name1 = 'HANA_HINTS_FOR_TRANSLATION' i_name2 = 'HOT_HINTS_FOR_TRANSLATION'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_is_structural <> <master_c>-hot_is_structural.
      append_change_color( EXPORTING i_name1 = 'HANA_IS_STRUCTURAL' i_name2 = 'HOT_IS_STRUCTURAL'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_original_language <> <master_c>-hot_original_language.
      append_change_color( EXPORTING i_name1 = 'HANA_ORIGINAL_LANGUAGE' i_name2 = 'HOT_ORIGINAL_LANGUAGE'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_responsible <> <master_c>-hot_responsible.
      append_change_color( EXPORTING i_name1 = 'HANA_RESPONSIBLE' i_name2 = 'HOT_RESPONSIBLE'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_src_system <> <master_c>-hot_src_system.
      append_change_color( EXPORTING i_name1 = 'HANA_SRC_SYSTEM' i_name2 = 'HOT_SRC_SYSTEM'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_src_tenant <> <master_c>-hot_src_tenant.
      append_change_color( EXPORTING i_name1 = 'HANA_SRC_TENANT' i_name2 = 'HOT_SRC_TENANT'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_text_collection <> <master_c>-hot_text_collection.
      append_change_color( EXPORTING i_name1 = 'HANA_TEXT_COLLECTION' i_name2 = 'HOT_TEXT_COLLECTION'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_text_status <> <master_c>-hot_text_status.
      append_change_color( EXPORTING i_name1 = 'HANA_TEXT_STATUS' i_name2 = 'HOT_TEXT_STATUS'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.
    IF <master_c>-hana_text_terminology_domain <> <master_c>-hot_text_terminology_domain.
      append_change_color( EXPORTING i_name1 = 'HANA_TEXT_TERMINOLOGY_DOMAIN' i_name2 = 'HOT_TEXT_TERMINOLOGY_DOMAIN'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

    IF <master_c>-hana_hints_for_translation <> <master_c>-hot_hints_for_translation.
      append_change_color( EXPORTING i_name1 = 'HANA_HINTS_FOR_TRANSLATION' i_name2 = 'HOT_HINTS_FOR_TRANSLATION'
                           CHANGING ch_color_table = <master_c>-t_color  ).
    ENDIF.

  ENDMETHOD.

  METHOD mark_difference_object.

    FIELD-SYMBOLS <slave_c> TYPE g_type_s_slave.
    ASSIGN ev_slave TO  <slave_c>.

    IF <slave_c>-hana_package_id <> <slave_c>-hot_hana_package_id.
      append_change_color( EXPORTING i_name1 = 'PACKAGE_ID'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
      append_change_color( EXPORTING i_name1 = 'HANA_PACKAGE_ID' i_name2 = 'HOT_HANA_PACKAGE_ID'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_object_name <> <slave_c>-hot_hana_object_name.
      append_change_color( EXPORTING i_name1 = 'OBJECT_NAME'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
      append_change_color( EXPORTING i_name1 = 'HANA_OBJECT_NAME' i_name2 = 'HOT_HANA_OBJECT_NAME'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_object_suffix <> <slave_c>-hot_hana_object_suffix.
      append_change_color( EXPORTING i_name1 = 'HANA_OBJECT_SUFFIX' i_name2 = 'HOT_HANA_OBJECT_SUFFIX'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
      append_change_color( EXPORTING i_name1 = 'OBJECT_SUFFIX' i_name2 = 'HANA_OBJECT_SUFFIX'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_version <> <slave_c>-hot_version. "Stephan
      append_change_color( EXPORTING i_name1 = 'HANA_VERSION' i_name2 = 'HOT_VERSION'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_activated_at <> <slave_c>-hot_activated_at.
      append_change_color( EXPORTING i_name1 = 'HANA_ACTIVATED_AT' i_name2 = 'HOT_ACTIVATED_AT'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_activated_by <> <slave_c>-hot_activated_by.
      append_change_color( EXPORTING i_name1 = 'HANA_ACTIVATED_BY' i_name2 = 'HOT_ACTIVATED_BY'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

    IF <slave_c>-hana_hana_read_system <> <slave_c>-hot_hana_read_system.
      append_change_color( EXPORTING i_name1 = 'HANA_HANA_READ_SYSTEM' i_name2 = 'HOT_HANA_READ_SYSTEM'
                           CHANGING ch_color_table = <slave_c>-t_color  ).
    ENDIF.

  ENDMETHOD.


  METHOD is_data_filtered.
    IF ms_filter_data-show_green = abap_false
          OR ms_filter_data-show_yellow = abap_false
          OR ms_filter_data-show_red = abap_false
          OR ms_filter_data-show_inactive = abap_false.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_filter_data.
    r_filter_data = ms_filter_data.
  ENDMETHOD.


  METHOD read_hana_pack_name_for_trobjn.
    IF strlen( i_hana_package_name ) <= 40. "transport object names are maximum 40 chars long
      SELECT SINGLE hana_package_id FROM cts_hot_package INTO r_result WHERE abap_hana_package_id = i_hana_package_name. "#EC CI_NOORDER
    ENDIF.

    IF r_result IS INITIAL.
      r_result = i_hana_package_name.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


"! Test class for checking status icons in the Deployment UI
CLASS ltc_deployment DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION long.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! to check whether we are on HANA or not. Currently required as instantiating o_cut will try to connect to HANA.
      class_setup RAISING cx_static_check. "todo: Check for running on HANA to be removed if we support read only mode on NOT HANA DBs
    METHODS:
      deploy_yellow FOR TESTING RAISING cx_static_check,
      deploy_grey FOR TESTING RAISING cx_static_check,
      deploy_red FOR TESTING RAISING cx_static_check,
      deploy_green FOR TESTING RAISING cx_static_check.
ENDCLASS.

*
CLASS ltc_deployment IMPLEMENTATION.
  METHOD class_setup.
    TRY.
        cl_nhi_api=>create_instance( ).
      CATCH cx_nhi_not_supported.
        cl_abap_unit_assert=>abort( msg = 'Tests can only be executed on an ABAP on HANA System, but this system is not an ABAP on HANA system' ).
    ENDTRY.
  ENDMETHOD.

  METHOD deploy_yellow.
    DATA: o_cut TYPE REF TO lcl_hota_organizer.

    TRY.
        CREATE OBJECT o_cut.
      CATCH cx_hana_object_transport INTO DATA(hota_exc).
        MESSAGE ID hota_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER hota_exc->if_t100_message~t100key-msgno.
    ENDTRY.

    FIELD-SYMBOLS: <master> TYPE o_cut->g_type_s_master,
                   <slave>  TYPE o_cut->g_type_s_slave.

    TRY.
        DATA(pkg1) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_1` ).
        DATA(pkg2) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_2` ).
        DATA(pkg3) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_3` ).
        DATA(pkg4) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_4` ).

        DATA(obj1) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_1'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj2) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_2'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj3) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_3'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj4) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_4'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
      CATCH cx_hana_object_transport INTO DATA(hota_exc_p).
        DATA(t100_key) = hota_exc_p->if_t100_message~t100key.
        IF t100_key = cx_hana_object_transport=>cx_nhi_hana_repository_error.
          DATA(nhi_exc) = CAST cx_nhi_hana_repository( hota_exc->previous ).
          t100_key = nhi_exc->if_t100_message~t100key.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
        ELSE.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc_p->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg DISPLAY LIKE 'E'.
        ENDIF.
      CATCH cx_nhi_hana_repository INTO nhi_exc.
        t100_key = nhi_exc->if_t100_message~t100key.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
    ENDTRY.

    o_cut->mt_master = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_inactive
    transport_obj_name = 'depl_test_1' hana_package_id = 'depl_test_1'
    hot_hana_package_id = 'depl_test_1' package_id = 'depl_test_1'
    cts_hot_package_ref = pkg1 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
    transport_obj_name = 'depl_test_2' hana_package_id = 'depl_test_2'
    hot_hana_package_id = 'depl_test_2' package_id = 'depl_test_2'
    cts_hot_package_ref = pkg2 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
    transport_obj_name = 'depl_test_3' hana_package_id = 'depl_test_3'
    hot_hana_package_id = 'depl_test_3' package_id = 'depl_test_3'
    cts_hot_package_ref = pkg3 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_delete_error
    transport_obj_name = 'depl_test_4' hana_package_id = 'depl_test_4'
    hot_hana_package_id = 'depl_test_4' package_id = 'depl_test_4'
    cts_hot_package_ref = pkg4 )
    ).

    o_cut->mt_slave = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_inactive
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_1' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_1' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj1 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_2' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_2' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj2 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_3' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_3' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj3 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_delete_error
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_4' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_4' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj4 )
    ).

    o_cut->calculate_depl_status( ).

* All packages should be marked yellow (inactive, to be delete, deploy error and delete error)
    LOOP AT o_cut->mt_master ASSIGNING <master>.
      cl_aunit_assert=>assert_equals( act    = <master>-sync_deploy_state
                                    exp    = icon_led_yellow
                                    msg    = 'The status is wrong.' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD deploy_grey.
    DATA: result TYPE icon_d.
    DATA: o_cut TYPE REF TO lcl_hota_organizer.

    TRY.
        CREATE OBJECT o_cut.
      CATCH cx_hana_object_transport INTO DATA(lr_hot_exc).
        MESSAGE ID lr_hot_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lr_hot_exc->if_t100_message~t100key-msgno.
    ENDTRY.

    FIELD-SYMBOLS: <master> TYPE o_cut->g_type_s_master,
                   <slave>  TYPE o_cut->g_type_s_slave.

    TRY.
        DATA(pkg1) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_1` ).
        DATA(pkg2) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_2` ).

        DATA(obj1) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_1'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj2) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_2'
                      iv_hana_object_name      = 'depl_test_a.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).

      CATCH cx_hana_object_transport INTO DATA(hota_exc).
        DATA(t100_key) = hota_exc->if_t100_message~t100key.
        IF t100_key = cx_hana_object_transport=>cx_nhi_hana_repository_error.
          DATA(nhi_exc) = CAST cx_nhi_hana_repository( hota_exc->previous ).
          t100_key = nhi_exc->if_t100_message~t100key.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
        ELSE.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg DISPLAY LIKE 'E'.
        ENDIF.
      CATCH cx_nhi_hana_repository INTO nhi_exc.
        t100_key = nhi_exc->if_t100_message~t100key.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
    ENDTRY.

    o_cut->mt_master = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_new
    transport_obj_name = 'depl_test_1' hana_package_id = 'depl_test_1'
    hot_hana_package_id = 'depl_test_1' package_id = 'depl_test_1'
    cts_hot_package_ref = pkg1 )
    (
    exists_in_hota = abap_false
    transport_obj_name = 'depl_test_2' hana_package_id = 'depl_test_2'
    hot_hana_package_id = 'depl_test_2' package_id = 'depl_test_2'
    cts_hot_package_ref = pkg2 ) ).

    o_cut->mt_slave = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_new
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_1' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_1' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj1 )
    (
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_2' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_2' object_suffix = 'depl_test_a.attributeview'
    exists_in_hota = abap_false
    hana_object_status = 'A' cts_hot_object_ref = obj2 ) ).

    o_cut->calculate_depl_status( ).

* Test case 1: hot_status = new
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_1'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_inactive
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_1'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_inactive
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

* Test case 2: exists in hota = false
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_2'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_inactive
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_2'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_inactive
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

  ENDMETHOD.

  METHOD deploy_red.
    DATA: result TYPE icon_d.
    DATA: o_cut TYPE REF TO lcl_hota_organizer.

    TRY.
        CREATE OBJECT o_cut.
      CATCH cx_hana_object_transport INTO DATA(lr_hot_exc).
        MESSAGE ID lr_hot_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lr_hot_exc->if_t100_message~t100key-msgno.
    ENDTRY.

    FIELD-SYMBOLS: <master> TYPE o_cut->g_type_s_master,
                   <slave>  TYPE o_cut->g_type_s_slave.

    TRY.
        DATA(pkg1) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_1` ).
        DATA(obj1) = cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id       = 'depl_test_1'
                  iv_hana_object_name      = 'depl_test_a.attributeview'
                  iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj2) = cl_cts_hot_object_v1=>create_instance(
                      iv_hana_package_id       = 'depl_test_1'
                      iv_hana_object_name      = 'depl_test_b.attributeview'
                      iv_hana_object_suffix    = 'attributeview' ).
      CATCH cx_hana_object_transport INTO DATA(hota_exc).
        DATA(t100_key) = hota_exc->if_t100_message~t100key.
        IF t100_key = cx_hana_object_transport=>cx_nhi_hana_repository_error.
          DATA(nhi_exc) = CAST cx_nhi_hana_repository( hota_exc->previous ).
          t100_key = nhi_exc->if_t100_message~t100key.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
        ELSE.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg DISPLAY LIKE 'E'.
        ENDIF.
      CATCH cx_nhi_hana_repository INTO nhi_exc.
        t100_key = nhi_exc->if_t100_message~t100key.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
    ENDTRY.

    o_cut->mt_master = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_new
    exists_in_hana = abap_true exists_in_hota = abap_true
    transport_obj_name = 'depl_test_1' hana_package_id = 'depl_test_1'
    hot_hana_package_id = 'depl_test_2' package_id = 'depl_test_1'
    cts_hot_package_ref = pkg1 ) ).

    o_cut->mt_slave = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_new
   transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
   hana_package_id = 'depl_test_1' hot_hana_package_id = 'depl_test_2'
   hana_object_name = 'depl_test_a.attributeview'
   abap_hana_package_id = 'depl_test_1' object_suffix = 'depl_test_a.attributeview'
   exists_in_hana = abap_true exists_in_hota = abap_true
   hana_object_status = 'A' cts_hot_object_ref = obj1 )
   ( hot_status = if_cts_hot_db_access=>co_hot_status_new
   transport_obj_name = 'depl_test_b' hot_hana_object_suffix = 'attributeview'
   hana_package_id = 'depl_test_1' hana_object_name = 'depl_test_b.attributeview'
   abap_hana_package_id = 'depl_test_1' object_suffix = 'depl_test_b.attributeview'
   exists_in_hana = abap_false exists_in_hota = abap_true
   hana_object_status = 'A' cts_hot_object_ref = obj2 ) ).

    o_cut->calculate_depl_status( ).

* Test case 1: hana package id <> hot_hana_package_id
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_1'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_red
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_1'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_red
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

* Test case 2: <master>-sync_deploy_state = red
    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE transport_obj_name = 'depl_test_b'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_red
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

  ENDMETHOD.

  METHOD deploy_green.
    DATA: result TYPE icon_d.
    DATA: o_cut TYPE REF TO lcl_hota_organizer.

    TRY.
        CREATE OBJECT o_cut.
      CATCH cx_hana_object_transport INTO DATA(lr_hot_exc).
        MESSAGE ID lr_hot_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lr_hot_exc->if_t100_message~t100key-msgno.
    ENDTRY.

    FIELD-SYMBOLS: <master> TYPE o_cut->g_type_s_master,
                   <slave>  TYPE o_cut->g_type_s_slave.


    TRY.
        DATA(pkg1) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_1` ).
        DATA(pkg2) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_2` ).
        DATA(pkg3) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `depl_test_3` ).
        DATA(obj1) = cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id       = 'depl_test_1'
                  iv_hana_object_name      = 'depl_test_a.attributeview'
                  iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj2) = cl_cts_hot_object_v1=>create_instance(
    iv_hana_package_id       = 'depl_test_2'
    iv_hana_object_name      = 'depl_test_a.attributeview'
    iv_hana_object_suffix    = 'attributeview' ).
        DATA(obj3) = cl_cts_hot_object_v1=>create_instance(
iv_hana_package_id       = 'depl_test_3'
iv_hana_object_name      = 'depl_test_a.attributeview'
iv_hana_object_suffix    = 'attributeview' ).
      CATCH cx_hana_object_transport INTO DATA(hota_exc).
        DATA(t100_key) = hota_exc->if_t100_message~t100key.
        IF t100_key = cx_hana_object_transport=>cx_nhi_hana_repository_error.
          DATA(nhi_exc) = CAST cx_nhi_hana_repository( hota_exc->previous ).
          t100_key = nhi_exc->if_t100_message~t100key.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
        ELSE.
          "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
          MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg DISPLAY LIKE 'E'.
        ENDIF.
      CATCH cx_nhi_hana_repository INTO nhi_exc.
        t100_key = nhi_exc->if_t100_message~t100key.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
    ENDTRY.

    o_cut->mt_master = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_active
    exists_in_hana = abap_true exists_in_hota = abap_true
    transport_obj_name = 'depl_test_1' hana_package_id = 'depl_test_1'
    hot_hana_package_id = 'depl_test_1' package_id = 'depl_test_1'
    cts_hot_package_ref = pkg1 )
               ( hot_status = if_cts_hot_db_access=>co_hot_status_active
    exists_in_hana = abap_true exists_in_hota = abap_true
    transport_obj_name = 'depl_test_2' hana_package_id = 'depl_test_2'
    hot_hana_package_id = 'depl_test_2' package_id = 'depl_test_2'
    hana_src_system = 'YI3' cts_hot_package_ref = pkg2 )
            ( hot_status = if_cts_hot_db_access=>co_hot_status_active
    exists_in_hana = abap_false exists_in_hota = abap_true
    transport_obj_name = 'depl_test_3' hana_package_id = 'depl_test_3'
    hot_hana_package_id = 'depl_test_3' package_id = 'depl_test_3'
    cts_hot_package_ref = pkg3 )
    ).

    o_cut->mt_slave = VALUE #( ( hot_status = if_cts_hot_db_access=>co_hot_status_active
   transport_obj_name = 'depl_test_a'
   hot_hana_object_suffix = 'attributeview' hana_object_suffix = 'attributeview'
   hana_package_id = 'depl_test_1' hot_hana_package_id = 'depl_test_1'
   hana_object_name = 'depl_test_a.attributeview' hot_hana_object_name  = 'depl_test_a.attributeview'
   abap_hana_package_id = 'depl_test_1' object_suffix = 'depl_test_a.attributeview'
   exists_in_hana = abap_true exists_in_hota = abap_true
   hana_version = 1 hot_version = 1
   hana_object_status = 'A' cts_hot_object_ref = obj1 )
          ( hot_status = if_cts_hot_db_access=>co_hot_status_active transport_obj_name = 'depl_test_a'
    hot_hana_object_suffix = 'attributeview' hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_2' hot_hana_package_id = 'depl_test_2'
    hana_object_name = 'depl_test_a.attributeview' hot_hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_2' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_true exists_in_hota = abap_true
    hana_version = 1 hot_version = 2 hana_activated_by = 'test'
    hana_object_status = 'A' cts_hot_object_ref = obj2 )
    ( hot_status = if_cts_hot_db_access=>co_hot_status_active
    transport_obj_name = 'depl_test_a' hot_hana_object_suffix = 'attributeview'
    hana_package_id = 'depl_test_3' hana_object_name = 'depl_test_a.attributeview'
    abap_hana_package_id = 'depl_test_3' object_suffix = 'depl_test_a.attributeview'
    exists_in_hana = abap_false exists_in_hota = abap_true
    hana_object_status = 'A' cts_hot_object_ref = obj3 )
).
    o_cut->calculate_depl_status( ).

* Test case 1: hot_status = active, equivalent versions
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_1'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_1'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

*     Test case 2: hot_status = active, varying versions
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_2'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_2'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

* Test case 3: hot_status = active, exists in hana = false
    LOOP AT o_cut->mt_master ASSIGNING <master> WHERE hana_package_id = 'depl_test_3'.
      result = <master>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

    LOOP AT o_cut->mt_slave ASSIGNING <slave> WHERE hana_package_id = 'depl_test_3'.
      result = <slave>-sync_deploy_state .
    ENDLOOP.

    cl_aunit_assert=>assert_equals( act    = result
                                    exp    = icon_led_green
                                    msg    = 'The status is wrong.' ).
    CLEAR result.

  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
DATA gr_hot_organizer TYPE REF TO lcl_hota_organizer.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  IF sy-tcode = 'SCTS_HTA_DEPLOY'.
    SET PF-STATUS 'START_DEPL'.
    SET TITLEBAR 'DEPLOYMENT'.
    gv_sync = ' '.
    gv_depl = 'X'.
  ELSE.
    SET PF-STATUS 'START_SYNC'.
    SET TITLEBAR 'SYNCHRONISATION'.
    gv_sync = 'X'.
    gv_depl = ' '.
  ENDIF.

  TRY.
      CREATE OBJECT gr_hot_organizer.
    CATCH cx_hana_object_transport INTO DATA(lr_hot_exc).
      MESSAGE ID lr_hot_exc->if_t100_message~t100key-msgid TYPE 'E' NUMBER lr_hot_exc->if_t100_message~t100key-msgno.
  ENDTRY.
  "TODO: HOTA READ ONLY MODUS ???? not yet possible for DB not equal HANA
*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE lv_title.
  PARAMETERS:
    pv_pack TYPE cts_hot_package-hana_package_id LOWER CASE MEMORY ID hta_pack  ##EXISTS ,
    pv_sub  TYPE abap_bool AS CHECKBOX DEFAULT abap_false MEMORY ID hta_sub  ##EXISTS. "handle ALL subpackages as well
  SELECTION-SCREEN END OF BLOCK b1.

  GET PARAMETER ID 'hta_pack' FIELD gv_pack  ##EXISTS  .
  pv_pack = gv_pack.

  IF sy-tcode = 'SCTS_HTA_DEPLOY'.
    lv_title = 'Paketauswahl für Deployment'(231).
  ELSE.
    lv_title = 'Paketauswahl für Synchronisierung'(100).
  ENDIF.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.                                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'AUSF'.
      IF pv_pack IS INITIAL OR pv_pack = '*'.
        MESSAGE 'Geben Sie ein SAP-HANA-Repository-Paket an'(003) TYPE 'E'.
      ELSE.
        TRY.
            gr_hot_organizer->select_data( i_hana_package_name = pv_pack i_include_subpackages = pv_sub ).
          CATCH cx_hana_object_transport INTO DATA(hota_exc).
            DATA(t100_key) = hota_exc->if_t100_message~t100key.
            MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg.
          CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
            t100_key = nhi_exc->if_t100_message~t100key.
            MESSAGE ID t100_key-msgid TYPE 'E' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4.
        ENDTRY.

        gv_pack = pv_pack.
        SET PARAMETER ID 'hta_pack' FIELD gv_pack  ##EXISTS.

        gr_hot_organizer->display_data( ).
      ENDIF.

    WHEN '&F03' OR '&F12' OR '&F15'.
      LEAVE PROGRAM.

    WHEN 'INFO_S'.
      CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
        EXPORTING
          dokclass = 'TX' " allgemeiner Text - SE61
          dokname  = 'CTS_HTA_INTRO_SYNC'
*         short_text = 'X'
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'INFO_D'.
      CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
        EXPORTING
          dokclass = 'TX' " allgemeiner Text - SE61
          dokname  = 'CTS_HTA_INTRO_DEPL'
*         short_text = 'X'
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'SYNC_T'.
      gv_pack = pv_pack.
      SET PARAMETER ID: 'hta_pack' FIELD gv_pack ##EXISTS .

      CALL TRANSACTION 'SCTS_HTA' WITH AUTHORITY-CHECK.

    WHEN 'DEPL_T'.
      gv_pack = pv_pack.
      SET PARAMETER ID: 'hta_pack' FIELD gv_pack ##EXISTS .

      CALL TRANSACTION 'SCTS_HTA_DEPLOY' WITH AUTHORITY-CHECK.
  ENDCASE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.                                          *
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*   comment = 'Der Organizer zeigt fuer das gewaehlte Paket bzw. die gewaehlten Pakete die Daten aus dem SAP HANA Repository an und vergleicht diese mit den Daten die aktuell im HOTA Repository liegen. Bei der Paketwahl wird * als Wildcard untesrtuetzt.'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN - F4 Help                                                     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pv_pack.
  DATA(lv_user_input_string) = gr_hot_organizer->read_user_value( 'PV_PACK' ).
  DATA(lv_include_subpackages) = gr_hot_organizer->read_user_value( 'PV_SUB' ).

  TRY.
      pv_pack = gr_hot_organizer->f4help_packages( i_hana_package_name = lv_user_input_string i_include_subpackages = CONV abap_bool( lv_include_subpackages ) ).
    CATCH cx_hana_object_transport INTO DATA(hota_exc).
      DATA(t100_key) = hota_exc->if_t100_message~t100key.
      IF t100_key = cx_hana_object_transport=>cx_nhi_hana_repository_error.
        DATA(nhi_exc) = CAST cx_nhi_hana_repository( hota_exc->previous ).
        t100_key = nhi_exc->if_t100_message~t100key.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
      ELSE.
        "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
        MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH hota_exc->msgv1 hota_exc->msgv2 hota_exc->hana_error_code hota_exc->hana_error_msg DISPLAY LIKE 'E'.
      ENDIF.
    CATCH cx_nhi_hana_repository INTO nhi_exc.
      t100_key = nhi_exc->if_t100_message~t100key.
      "Type 'E' not possible (dumps) in AT SELECTION-SCREEN ON VALUE-REQUEST
      MESSAGE ID t100_key-msgid TYPE 'I' NUMBER t100_key-msgno WITH nhi_exc->msgv1 nhi_exc->msgv2 nhi_exc->msgv3 nhi_exc->msgv4 DISPLAY LIKE 'E'.
  ENDTRY.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

  DATA: fcode    TYPE TABLE OF sy-ucomm,
        wa_fcode TYPE sy-ucomm.
  DATA: green    TYPE c,
        inactive TYPE c,
        yellow   TYPE c,
        red      TYPE c.
  DATA: lv_initialized TYPE c.
  DATA: no_p_green(6)    TYPE c,
        no_o_green(6)    TYPE c,
        no_p_yellow(6)   TYPE c,
        no_o_yellow(6)   TYPE c,
        no_p_red(6)      TYPE c,
        no_o_red(6)      TYPE c,
        no_p_inactive(6) TYPE c,
        no_o_inactive(6) TYPE c,
        text_green       TYPE string,
        text_yellow      TYPE string,
        text_red         TYPE string,
        text_inactive    TYPE string,
        text_pakete_1    TYPE string,
        text_pakete_2    TYPE string,
        text_pakete_3    TYPE string,
        text_pakete_4    TYPE string,
        text_objekte_1   TYPE string,
        text_objekte_2   TYPE string,
        text_objekte_3   TYPE string,
        text_objekte_4   TYPE string.

  text_pakete_1 = text_pakete_2 = text_pakete_3 = text_pakete_4 = 'Pakete /'(232).
  text_objekte_1 = text_objekte_2 = text_objekte_3 = text_objekte_4 = 'Objekte'(234).

  wa_fcode = 'SYNC '. APPEND wa_fcode TO fcode.
  wa_fcode = '&ETA '. APPEND wa_fcode TO fcode.
  wa_fcode = 'REFRESH '. APPEND wa_fcode TO fcode.
  wa_fcode = 'DESEL_ALL '. APPEND wa_fcode TO fcode.
  wa_fcode = 'SEL_PACK '. APPEND wa_fcode TO fcode.
  wa_fcode = 'DESEL_PACK '. APPEND wa_fcode TO fcode.
  wa_fcode = 'SEL_SYNC '. APPEND wa_fcode TO fcode.
  wa_fcode = '&OL0 '. APPEND wa_fcode TO fcode.
  wa_fcode = 'SELECT_ALL '. APPEND wa_fcode TO fcode.
  wa_fcode = '&OAD '. APPEND wa_fcode TO fcode.
  wa_fcode = '&AVE '. APPEND wa_fcode TO fcode.
  wa_fcode = '&CRL '. APPEND wa_fcode TO fcode.
  wa_fcode = '&CRR '. APPEND wa_fcode TO fcode.
  wa_fcode = '&CRB '. APPEND wa_fcode TO fcode.
  wa_fcode = '&CRE '. APPEND wa_fcode TO fcode.
  wa_fcode = 'LEGEND '. APPEND wa_fcode TO fcode.
  wa_fcode = 'INFO'. APPEND wa_fcode TO fcode.

  SET PF-STATUS 'HOTA_SYNC' EXCLUDING fcode.
  SET TITLEBAR 'FILTER_TITEL'.

  IF gv_sync = 'X'.
    text_green = 'Paket/Objekt ist in sync'(143).
    text_yellow = 'Paket/Objekt ist nicht in sync'(144).
    text_red = 'Paket/Objekt kann nicht synchron. werden'(145).
    text_inactive = 'Paket/Objekt kann nicht synchron. werden'(145).
  ELSEIF gv_depl = 'X'.
    text_green = 'Paket/Objekt ist deployt'(230).
    text_yellow = 'Paket/Objekt ist nicht deployt'(211).
    text_red = 'Paket/Objekt kann nicht deployt werden'(212).
    text_inactive = 'Paket/Objekt kann nicht deployt werden'(212).
  ENDIF.

  DATA(ls_filter_data) = gr_hot_organizer->get_filter_data( ).

  green = ls_filter_data-show_green.
  yellow = ls_filter_data-show_yellow.
  red = ls_filter_data-show_red.
  inactive = ls_filter_data-show_inactive.
  no_p_green = ls_filter_data-nr_packages_green.
  no_p_yellow = ls_filter_data-nr_packages_yellow.
  no_p_red = ls_filter_data-nr_packages_red.
  no_p_inactive = ls_filter_data-nr_packages_inactive.
  no_o_green = ls_filter_data-nr_objects_green.
  no_o_yellow = ls_filter_data-nr_objects_yellow.
  no_o_red = ls_filter_data-nr_objects_red.
  no_o_inactive = ls_filter_data-nr_objects_inactive.

  IF sy-ucomm = '&F12'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  IF sy-ucomm = 'FILTER'.
    gr_hot_organizer->apply_filter( i_green = green
                                    i_yellow = yellow
                                    i_red = red
                                    i_inactive = inactive ).
  ENDIF.

  LEAVE TO SCREEN 0.
ENDMODULE.