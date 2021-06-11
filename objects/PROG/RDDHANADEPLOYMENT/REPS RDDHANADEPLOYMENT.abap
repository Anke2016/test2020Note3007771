REPORT rddhanadeployment.

**************************************************************************
* HANA Deployment for R3TR/LANG HOTA, LIMU/LANG HOTO and LIMU HOTP transport objects
**************************************************************************

**************************************************************************
* CONSTANTS: Definitions                                                       *
**************************************************************************
CONSTANTS: gc_ag    TYPE sprot_u-ag      VALUE 'SCTS_HOT',
*             langu   TYPE sprot_u-langu   VALUE 'E'.          "TODO Jens nötig?
           gc_langu TYPE sprot_u-langu   VALUE 'E'.        "TODO Jens nötig? oder sollte sy-lang genommen werden?

**************************************************************************
* Type Definitions                                                       *
**************************************************************************
TYPES:
  "! type mapping e071 entries to logfile per request and all contained objects to hot objects/packages
  BEGIN OF ty_e071_hot_refs,
    trbat_order     TYPE trbat-timestmp, "order of the queue (buffer)
    trkorr          TYPE e071-trkorr,
    as4pos          TYPE e071-as4pos,
    pgmid           TYPE e071-pgmid,
    object          TYPE e071-object,
    obj_name        TYPE e071-obj_name,
    lockflag        TYPE e071-lockflag,
    lang            TYPE e071-lang,
    logfile         TYPE trfile,
    logger          TYPE REF TO if_cts_hot_logger,
    cts_hot_object  TYPE REF TO cl_cts_hot_object_v1, "either cts_hot_object or cts_hot_package will be filled! (package for hotp and hota case)
    cts_hot_package TYPE REF TO cl_cts_hot_package,
  END OF ty_e071_hot_refs,
  "secondary keys? zugriffe mit:
  "       WHERE object = 'HOTA' GROUP BY ( key1 = ls_e071-obj_name )
  "       WHERE object = 'HOTP' GROUP BY ( key1 = lr_e071->obj_name ).
  "       WHERE object = 'HOTO' GROUP BY ( key1 = lr_e071->obj_name ).
  "       WHERE cts_hot_package = 'xyz'
  "       WHERE logfile = xyz
  "       where pgmid = 'LIMU' and object = 'HOTP'
  "       where pgmid = 'LIMU' and object = 'HOTO' and obj_name = xyz
  "       where object = 'HOTO' and obj_name = xyz
  tt_e071_hot_refs            TYPE SORTED TABLE OF ty_e071_hot_refs WITH UNIQUE KEY trbat_order trkorr as4pos pgmid object obj_name lang, "unique key different to e071 because we expand hota to hoto and hotps which will all have same as4pos
  ty_dref_log_message_package TYPE REF TO cl_cts_hot_hana_connector=>ty_log_message_package, "Performance in form log_package_results
  tt_dref_log_message_package TYPE STANDARD TABLE OF ty_dref_log_message_package, "Performance in form log_package_results
  tt_logfile                  TYPE STANDARD TABLE OF trfile,
  tt_sprot_u                  TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY,

  "! type mapping e071 AMHC entries
  BEGIN OF ty_s_e071_amhc_ref,
    trbat_order TYPE trbat-timestmp, "order of the queue (buffer)
    trkorr      TYPE e071-trkorr,
    as4pos      TYPE e071-as4pos,
    pgmid       TYPE e071-pgmid,
    object      TYPE e071-object,
    obj_name    TYPE e071-obj_name,
    lockflag    TYPE e071-lockflag,
*   lang        TYPE e071-lang, "  necessary?
    logger      TYPE REF TO if_cts_hot_logger,
  END OF ty_s_e071_amhc_ref,

  ty_t_e071_amhc_ref TYPE SORTED TABLE OF ty_s_e071_amhc_ref WITH UNIQUE KEY trbat_order trkorr as4pos pgmid object obj_name.


**************************************************************************
* Local Class Definitions and Implementations                            *
**************************************************************************

"!Local helper class for rddhanadeployment
CLASS lcl_external_persistency DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_devclass_for_package
      IMPORTING
        i_package       TYPE cts_hot_package_id
      RETURNING
        VALUE(r_result) TYPE devclass.
    METHODS get_switch_id_for_devclass
      IMPORTING
        i_devclass      TYPE devclass
      RETURNING
        VALUE(r_result) TYPE sfw_switch_id.
    METHODS is_switch_switched_off
      IMPORTING
        i_switch_id     TYPE sfw_switch_id
      RETURNING
        VALUE(r_result) TYPE abap_bool.
    METHODS get_akh_for_package
      IMPORTING
        i_package       TYPE cts_hot_package_id
      RETURNING
        VALUE(r_result) TYPE ufps_posid.
    METHODS is_view_layer_system
      RETURNING
        VALUE(r_result) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mr_hot_ext_call_internal TYPE REF TO if_cts_hot_ext_call_internal.
ENDCLASS.

CLASS lcl_external_persistency IMPLEMENTATION.

  METHOD constructor.
    mr_hot_ext_call_internal = NEW cl_cts_hot_ext_call_intenal( ).
  ENDMETHOD.

  METHOD get_devclass_for_package.
    r_result = mr_hot_ext_call_internal->get_devclass_for_hota( i_package ).
  ENDMETHOD.

  METHOD get_switch_id_for_devclass.
    r_result = mr_hot_ext_call_internal->get_switch_id_for_devclass( i_devclass ).
  ENDMETHOD.

  METHOD is_switch_switched_off.
    r_result = mr_hot_ext_call_internal->is_switch_switched_off( i_switch_id ).
  ENDMETHOD.

  METHOD get_akh_for_package.
    r_result = mr_hot_ext_call_internal->get_akh_for_hota( i_package ).
  ENDMETHOD.

  METHOD is_view_layer_system.
    r_result = mr_hot_ext_call_internal->is_view_layer_system( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_switch_framework_accessor DEFINITION DEFERRED.
CLASS ltd_external_persistency DEFINITION DEFERRED.
CLASS lcl_switch_framework_accessor DEFINITION FINAL FRIENDS ltcl_switch_framework_accessor ltd_external_persistency.
  "1 devclass belongs to 1 switchID
  "1 switchID can have n devclasses
  "1 hot_package belongs to 1 devclass
  "1 devclass can have n hot_packages
  "1 hot package belongs to 1 switch (because 1 devclass = 1 switch)
  "1 hot package can have n hot_objects
  PUBLIC SECTION.
    " Types for exporting parameter of public methods
    TYPES:
      BEGIN OF ty_switch_id_devc_hot_package,
        switch_id       TYPE sfw_switch_id,
        devclass        TYPE devclass,
        hana_package_id TYPE cts_hot_hana_package_id,
        hot_package     TYPE REF TO cl_cts_hot_package,
      END OF ty_switch_id_devc_hot_package,
      tt_switch_id_hot_package TYPE SORTED TABLE OF ty_switch_id_devc_hot_package WITH UNIQUE KEY switch_id devclass hana_package_id,

      BEGIN OF ty_switch_id_devc_hot_object,
        switch_id          TYPE sfw_switch_id,
        devclass           TYPE devclass,
        hana_package_id    TYPE cts_hot_hana_package_id,
        hana_object_name   TYPE cts_hot_hana_object_name,
        hana_object_suffix TYPE cts_hot_hana_object_suffix,
        hot_object         TYPE REF TO cl_cts_hot_object_v1,
      END OF ty_switch_id_devc_hot_object,
      tt_switch_id_devc_hot_object TYPE SORTED TABLE OF ty_switch_id_devc_hot_object WITH UNIQUE KEY switch_id devclass hana_package_id hana_object_name hana_object_suffix.

    METHODS:
      constructor,

      "! Returns a table with all relevant switches that are OFF and the packages that are not to be deployed because of switch being OFF.<br/>
      "! Return object also contains the devclass the switch belongs to.<br/>
      "! All packages that can be deployed are NOT returned
      get_off_switches_for_packages
        IMPORTING
          i_hot_packages           TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
        EXPORTING
          e_switch_id_hot_packages TYPE tt_switch_id_hot_package,

      "! Returns a table with all relevant switches that are OFF and the objects that are not to be deployed because of switch being OFF<br/>
      "! Return object also contains the devclass the switch belongs to.<br/>
      "! All objects that can be deployed are NOT returned
      get_off_switches_for_objects
        IMPORTING
          i_hot_objects           TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
        EXPORTING
          e_switch_id_hot_objects TYPE tt_switch_id_devc_hot_object.

  PROTECTED SECTION.

  PRIVATE SECTION.
    " Types for internal caching of switch and tadir results
    TYPES:
      BEGIN OF ty_package_switch_id,
        package   TYPE cts_hot_hana_package_id,
        switch_id TYPE sfw_switch_id,
        devclass  TYPE devclass,
      END OF ty_package_switch_id,
      tt_package_switch_id TYPE SORTED TABLE OF ty_package_switch_id WITH UNIQUE KEY package,

      BEGIN OF ty_devclass_switch_id,
        devclass  TYPE devclass,
        switch_id TYPE sfw_switch_id,
      END OF ty_devclass_switch_id,
      tt_devclass_switch_id TYPE SORTED TABLE OF ty_devclass_switch_id WITH UNIQUE KEY devclass,

      BEGIN OF ty_switch_id_switch_state,
        switch_id    TYPE sfw_switch_id,
        switched_off TYPE abap_bool,
      END OF ty_switch_id_switch_state,
      tt_switch_id_switch_state TYPE SORTED TABLE OF ty_switch_id_switch_state WITH UNIQUE KEY switch_id.

    DATA: m_persistency_access           TYPE REF TO lcl_external_persistency,
          "! Caching all packages for which the switch settings were already read.<br/>
          "! If package does not yet exist, switch settings were not yet read.<br/>
          "! If package exists and the switch is off, the switch_id is filled.<br/>
          "! If package exists and the switch is on or does not exist, the switch_id is initial.<br/>
          m_cache_package_switch_id      TYPE tt_package_switch_id,
          "! Caching all devclasses for which the switch settings were already read.<br/>
          "! If devclass does not yet exist, switch settings were not yet read.<br/>
          "! If devclass exists and the switch is off, the switch_id is filled.<br/>
          "! If devclass exists and the switch is on or does not exist, the switch_id is initial.<br/>
          m_cache_devclass_switch_id     TYPE tt_devclass_switch_id,
          "! Caching all switch_ids for which the switch settings were already read.<br/>
          "! If switch_id does not yet exist, switch settings were not yet read.<br/>
          "! If switch_id exists, data was read and state is contained<br/>
          m_cache_switch_id_switch_state TYPE tt_switch_id_switch_state.

    "! If the switch for passed package is off, the switch and devclass it belongs to is returned. If the switch is on or not existing, r_result is initial.
    METHODS read_off_switch_for_package
      IMPORTING
        i_abap_hana_package_id TYPE cts_hot_package_id
      RETURNING
        VALUE(r_result)        TYPE ty_devclass_switch_id.

    "! If the switch for passed devclass is off, the switch is returned. If the switch is on or not existing, r_result is initial.
    METHODS read_off_switch_for_devclass
      IMPORTING
        i_devclass      TYPE devclass
      RETURNING
        VALUE(r_result) TYPE sfw_switch_id.

    "! If the switch for passed switch_id is off, the switch is returned again. If the switch is on or not existing, r_result is initial.
    METHODS read_off_switch_for_switch_id
      IMPORTING
        i_switch_id     TYPE sfw_switch_id
      RETURNING
        VALUE(r_result) TYPE sfw_switch_id.

ENDCLASS.

CLASS lcl_switch_framework_accessor IMPLEMENTATION.

  METHOD constructor.
    me->m_persistency_access = NEW lcl_external_persistency( ).
  ENDMETHOD.

  METHOD get_off_switches_for_packages.
    DATA: ls_switch_packages TYPE ty_switch_id_devc_hot_package,
          lr_hot_package     TYPE REF TO cl_cts_hot_package,
          ls_switch_id       TYPE ty_devclass_switch_id.

    FREE e_switch_id_hot_packages.

    LOOP AT i_hot_packages INTO lr_hot_package.
      ls_switch_id = read_off_switch_for_package( lr_hot_package->abap_hana_package_id ).

      IF ls_switch_id IS NOT INITIAL.
        ls_switch_packages-switch_id = ls_switch_id-switch_id.
        ls_switch_packages-devclass = ls_switch_id-devclass.
        ls_switch_packages-hana_package_id = lr_hot_package->hana_package_id.
        ls_switch_packages-hot_package = lr_hot_package.
        INSERT ls_switch_packages INTO TABLE e_switch_id_hot_packages.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_off_switches_for_objects.
    DATA: ls_switch_object TYPE ty_switch_id_devc_hot_object,
          lr_hot_object    TYPE REF TO cl_cts_hot_object_v1,
          ls_switch        TYPE ty_devclass_switch_id.

    FREE e_switch_id_hot_objects.

    LOOP AT i_hot_objects INTO lr_hot_object.
      ls_switch = read_off_switch_for_package( lr_hot_object->abap_hana_package_id ).

      IF ls_switch IS NOT INITIAL.
        ls_switch_object-switch_id = ls_switch-switch_id.
        ls_switch_object-devclass = ls_switch-devclass.
        ls_switch_object-hana_package_id = lr_hot_object->hana_package_id.
        ls_switch_object-hana_object_name = lr_hot_object->hana_object_name.
        ls_switch_object-hana_object_suffix = lr_hot_object->hana_object_suffix.
        ls_switch_object-hot_object = lr_hot_object.
        INSERT ls_switch_object INTO TABLE e_switch_id_hot_objects.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_off_switch_for_package.
    DATA: lv_devclass       TYPE devclass,
          ls_package_switch TYPE ty_package_switch_id.

    "check cache whether switch was already read for this package
    READ TABLE me->m_cache_package_switch_id WITH KEY package = i_abap_hana_package_id INTO ls_package_switch.
    IF sy-subrc = 0.
      r_result-switch_id = ls_package_switch-switch_id.
      r_result-devclass = ls_package_switch-devclass.
      RETURN.
    ENDIF.

    lv_devclass = me->m_persistency_access->get_devclass_for_package( i_abap_hana_package_id ).
    IF lv_devclass IS INITIAL.
      "same as in HTC, assume switch is ON!
    ELSE.
      r_result-switch_id = read_off_switch_for_devclass( lv_devclass ).
      IF r_result-switch_id IS NOT INITIAL.
        r_result-devclass = lv_devclass.
        ls_package_switch-devclass = lv_devclass.
      ENDIF.

      "cache result whether switch is off or on(or not existing).
      ls_package_switch-switch_id = r_result-switch_id.
    ENDIF.

    "cache switch settings (only if switch_id is set, it is OFF, if it is initial, it was checked but switch does either not exist or is ON)
    ls_package_switch-package = i_abap_hana_package_id.
    INSERT ls_package_switch INTO TABLE me->m_cache_package_switch_id.
  ENDMETHOD.

  METHOD read_off_switch_for_devclass.
    DATA: lv_switch_id          TYPE sfw_switch_id,
          ls_devclass_switch_id TYPE ty_devclass_switch_id.

    "check cache, if switch setting for iv_devclass was already read before?
    READ TABLE me->m_cache_devclass_switch_id WITH KEY devclass = i_devclass INTO ls_devclass_switch_id.
    IF sy-subrc = 0.
      "return found switch settings(initial = ON, existing = OFF)
      r_result = ls_devclass_switch_id-switch_id.
      RETURN.
    ENDIF.

    lv_switch_id = me->m_persistency_access->get_switch_id_for_devclass( i_devclass ).
    IF lv_switch_id IS INITIAL.
      "if switch does not exist, it is ON.
    ELSE.
      r_result = read_off_switch_for_switch_id( lv_switch_id ).

      IF r_result IS NOT INITIAL.
        "cache information that switch was read for devclass and is OFF
        ls_devclass_switch_id-switch_id = lv_switch_id.
      ENDIF.
    ENDIF.

    "cache information that switch was read for devclass with either initial or switch_id in case it is off.
    ls_devclass_switch_id-devclass = i_devclass.
    INSERT ls_devclass_switch_id INTO TABLE me->m_cache_devclass_switch_id.
  ENDMETHOD.

  METHOD read_off_switch_for_switch_id.
    DATA: ls_switch_id_switch_state TYPE ty_switch_id_switch_state,
          lv_is_switched_off        TYPE abap_bool.

    "check cache, if switch setting for i_switch_id was already read before?
    READ TABLE me->m_cache_switch_id_switch_state WITH KEY switch_id = i_switch_id INTO ls_switch_id_switch_state.
    IF sy-subrc = 0.
      IF ls_switch_id_switch_state-switched_off = abap_true.
        r_result = i_switch_id.
      ELSE.
        "switch is on, so return initial result
      ENDIF.
      RETURN.
    ENDIF.

    "read switch state from persistency
    lv_is_switched_off = me->m_persistency_access->is_switch_switched_off( i_switch_id ).

    IF lv_is_switched_off = abap_true.
      r_result = i_switch_id.
    ELSE.
      CLEAR r_result.
    ENDIF.

    "cache switch state
    ls_switch_id_switch_state-switch_id = i_switch_id.
    ls_switch_id_switch_state-switched_off = lv_is_switched_off.
    INSERT ls_switch_id_switch_state INTO TABLE me->m_cache_switch_id_switch_state.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_rddhanadeployment_helper DEFINITION.
  PUBLIC SECTION.
    "chunk size 32 but only 31 are used. 1 is reserved for adding a space in the beginning
    "if previous variable ended with space because space ending is cut off later in logging
    "and leads to words being merged without space
    CONSTANTS msg_para_chunk_size TYPE i VALUE 32.

    TYPES:
      BEGIN OF ty_split_message,
        var1 TYPE c LENGTH msg_para_chunk_size,
        var2 TYPE c LENGTH msg_para_chunk_size,
        var3 TYPE c LENGTH msg_para_chunk_size,
        var4 TYPE c LENGTH msg_para_chunk_size,
      END OF ty_split_message,
      tt_split_message TYPE STANDARD TABLE OF ty_split_message WITH DEFAULT KEY.

    CLASS-METHODS:
      "! Splits a message in maximum 4 pieces each of max length msg_para_chunk_size.
      split_message IMPORTING message TYPE string EXPORTING et_split_message TYPE tt_split_message. "copied from cl_nhi_dup_utility

  PRIVATE SECTION.
    CLASS-METHODS:
      "! Internal helper method to shift trailing spaces to next variable because trailing spaces of variables are deleted when being logged
      fix_trailing_spaces CHANGING ch_line TYPE ty_split_message.
ENDCLASS.

CLASS lcl_rddhanadeployment_helper IMPLEMENTATION.
  METHOD split_message.
    DATA:
      lv_message    LIKE message,
      lt_messages   TYPE STANDARD TABLE OF string,
      lv_var_length TYPE i,
      lv_var        TYPE i,
      lv_line       TYPE ty_split_message.

    CLEAR: et_split_message.
    SPLIT message AT cl_abap_char_utilities=>newline INTO TABLE lt_messages IN CHARACTER MODE. "HANA might return lines with line break...

    LOOP AT lt_messages INTO lv_message.
      CLEAR lv_line.

      DATA(lv_current_length) = strlen( lv_message ).
      WHILE lv_current_length > 0.

        IF lv_current_length < msg_para_chunk_size - 1.
          lv_var_length = lv_current_length.
        ELSE.
          lv_var_length = msg_para_chunk_size - 1. " -1 because we must reserve 1 space for shifting (see description of msg_para_chunk_size)
        ENDIF.

        lv_var = sy-index MOD 4.
        CASE lv_var.
          WHEN 1.
            lv_line-var1 = substring( val = lv_message len = lv_var_length ).
          WHEN 2.
            lv_line-var2 = substring( val = lv_message len = lv_var_length ).
          WHEN 3.
            lv_line-var3 = substring( val = lv_message len = lv_var_length ).
          WHEN 0.
            lv_line-var4 = substring( val = lv_message len = lv_var_length ).
            fix_trailing_spaces( CHANGING ch_line = lv_line ).
            APPEND lv_line TO et_split_message.
            CLEAR lv_line.
        ENDCASE.
        lv_message = substring( val = lv_message off = lv_var_length ).
        IF lv_var = 0. "as of second line add 2 spaces in front to better see in the log that it belongs to first row line (first row is where sy-index = 1)
          lv_message = |  { lv_message }|.
        ENDIF.
        lv_current_length = strlen( lv_message ).
      ENDWHILE.

      IF lv_line IS NOT INITIAL.
        fix_trailing_spaces( CHANGING ch_line = lv_line ).
        APPEND lv_line TO et_split_message.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fix_trailing_spaces.

    DATA: space_check_pos_var1 TYPE i,
          space_check_pos_var2 TYPE i,
          space_check_pos_var3 TYPE i.

    space_check_pos_var1 = msg_para_chunk_size - 2. "Position in line-var1 to check for space ending and shifting of next variable to prevent merge of 2 words.
    space_check_pos_var2 = msg_para_chunk_size - 2. "Position in line-var2 to check for space ending and shifting of next variable to prevent merge of 2 words.
    space_check_pos_var3 = msg_para_chunk_size - 2. "Position in line-var3 to check for space ending and shifting of next variable to prevent merge of 2 words.

    IF ch_line IS NOT INITIAL.
      IF ch_line-var1+space_check_pos_var1(1) = space AND strlen( ch_line-var2 ) > 0.
        SHIFT ch_line-var2 RIGHT.
        space_check_pos_var2 = space_check_pos_var2 + 1. "increase because we shifted var-2 to the right.
      ENDIF.
      IF ch_line-var2+space_check_pos_var2(1) = space AND strlen( ch_line-var3 ) > 0.
        SHIFT ch_line-var3 RIGHT.
        space_check_pos_var3 = space_check_pos_var3 + 1. "increase because we shifted var-3 to the right.
      ENDIF.
      IF ch_line-var3+space_check_pos_var3(1) = space AND strlen( ch_line-var4 ) > 0.
        SHIFT ch_line-var4 RIGHT.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_external_viewhandler DEFINITION DEFERRED.
CLASS lcl_external_viewhandler DEFINITION FINAL FRIENDS ltcl_external_viewhandler.

  PUBLIC SECTION.
    TYPES:
      ty_view_results_with_message TYPE STANDARD TABLE OF if_dd_view_types=>ty_s_viresult_with_mess WITH DEFAULT KEY,
      BEGIN OF ty_s_ext_view_result_with_hoto,
        hot_object   TYPE REF TO cl_cts_hot_object_v1,
        max_severity TYPE if_dd_view_types=>ty_s_viresult_with_mess-severity, "max_Severity for all contained view_results of current hot_object
        view_results TYPE ty_view_results_with_message,
      END OF ty_s_ext_view_result_with_hoto,
      ty_t_ext_view_result_with_hoto TYPE STANDARD TABLE OF ty_s_ext_view_result_with_hoto.

    METHODS:
      "! Creates aliases (link between ABAP external views and HANA views) for all passed objects.
      "! First objects are filtered to match only view types and then external code is called to create the views
      "!
      "! @parameter i_hoto_objects | All successful objects of the import for which aliases should be created
      "! @parameter e_ext_views_results | result of alias creation
      create_aliases_for_ext_views
        IMPORTING
          i_hoto_objects      TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
        EXPORTING
          e_ext_views_results TYPE ty_t_ext_view_result_with_hoto.

  PRIVATE SECTION.
    METHODS:
      "! Checks whether i_severity is higher than c_severity and in case it is, changes c_severity to i_severity.<br/>
      "! Order is 'A' greater than 'E' greater than 'W' greater than 'I' or space.<br/>
      "! In addition space is replaced by 'I'
      set_max_severity
        IMPORTING
          i_severity TYPE if_dd_view_types=>ty_s_viresult_with_mess-severity
        CHANGING
          c_severity TYPE if_dd_view_types=>ty_s_viresult_with_mess-severity.
ENDCLASS.

CLASS lcl_external_viewhandler IMPLEMENTATION.

  METHOD create_aliases_for_ext_views.

    TYPES:
      BEGIN OF ty_s_hoto_dbview,
        dbviewname TYPE if_dd_view_types=>ty_s_dbview-dbviewname,
        hot_object TYPE REF TO cl_cts_hot_object_v1,
      END OF ty_s_hoto_dbview,
      ty_t_hoto_dbview TYPE SORTED TABLE OF ty_s_hoto_dbview WITH UNIQUE KEY dbviewname.

    DATA:
      ls_hoto_dbview               TYPE ty_s_hoto_dbview,
      lv_suffix_upper              TYPE string,
      lr_hoto_object               TYPE REF TO cl_cts_hot_object_v1,
      ls_dbview                    TYPE if_dd_view_types=>ty_s_dbview,
      lt_dbviews                   TYPE if_dd_view_types=>ty_t_dbview,
      ls_view_result               TYPE if_dd_view_types=>ty_s_viresult_with_mess,
      lt_view_results              TYPE if_dd_view_types=>ty_t_viresult_with_mess,
      lt_hoto_dbviews              TYPE ty_t_hoto_dbview,
      ls_ext_view_result_with_hoto TYPE ty_s_ext_view_result_with_hoto,
      ls_ext_view_result_wo_hoto   TYPE ty_s_ext_view_result_with_hoto, "Only in case DDIC returns results of alias creation for objects we do not know...
      lr_dd_view                   TYPE REF TO if_dd_view.

    "initialize exporting parameter
    FREE e_ext_views_results.

    LOOP AT i_hoto_objects INTO lr_hoto_object.
*     Filter for supported object types (suffixes)
      lv_suffix_upper = to_upper( lr_hoto_object->hana_object_suffix ).
      IF lv_suffix_upper = 'ATTRIBUTEVIEW'
          OR lv_suffix_upper = 'HDBVIEW'
          OR lv_suffix_upper = 'ANALYTICVIEW'
          OR lv_suffix_upper = 'CALCULATIONVIEW'.

        "create expected input of DD API
        CONCATENATE lr_hoto_object->hana_package_id '/' lr_hoto_object->hana_object_name INTO ls_dbview-dbviewname.
        ls_dbview-dbviewtype = lv_suffix_upper.
        APPEND ls_dbview TO lt_dbviews.

        "remember association of dbview to hot object to be able to map result of external view creation back to a specific hot object
        ls_hoto_dbview-dbviewname = ls_dbview-dbviewname.
        ls_hoto_dbview-hot_object = lr_hoto_object.
        INSERT ls_hoto_dbview INTO TABLE lt_hoto_dbviews.
      ENDIF.
    ENDLOOP.

    IF lines( lt_dbviews ) = 0.
      RETURN.
    ENDIF.

    lr_dd_view = cl_dd_view_factory=>create_ex_view_access( ).
    lr_dd_view->ex_view_create_alias_objs(
      EXPORTING
        dbviews       = lt_dbviews " Namen der DB-View in Notation /<package>/viewname
      IMPORTING
        views_results = lt_view_results " Ergebnis der View Behandlung
    ).

    "map extview_results of DD to our exporting parameter, per hot_object.
    LOOP AT lt_view_results REFERENCE INTO DATA(lr_view_result) GROUP BY ( dbviewname = lr_view_result->dbviewname ) REFERENCE INTO DATA(lr_view_result_group).
      CLEAR ls_ext_view_result_with_hoto.
      READ TABLE lt_hoto_dbviews WITH KEY dbviewname = lr_view_result_group->dbviewname REFERENCE INTO DATA(lr_hoto_dbview).
      IF sy-subrc = 0.
        ls_ext_view_result_with_hoto-hot_object = lr_hoto_dbview->hot_object.
      ENDIF.

      LOOP AT GROUP lr_view_result_group INTO ls_view_result.
        IF ls_ext_view_result_with_hoto IS NOT INITIAL.
          APPEND ls_view_result TO ls_ext_view_result_with_hoto-view_results.
          set_max_severity( EXPORTING i_severity = ls_view_result-severity
                            CHANGING  c_severity = ls_ext_view_result_with_hoto-max_severity ).
        ELSE.
          "Only in case DDIC returns results of alias creation for objects we do not know...
          APPEND ls_view_result TO ls_ext_view_result_wo_hoto-view_results.
          set_max_severity( EXPORTING i_severity = ls_view_result-severity
                            CHANGING  c_severity = ls_ext_view_result_wo_hoto-max_severity ).
        ENDIF.
      ENDLOOP.

      INSERT ls_ext_view_result_with_hoto INTO TABLE e_ext_views_results.
    ENDLOOP.

    "Only in case DDIC returns results of alias creation for objects we do not know...
    IF ls_ext_view_result_wo_hoto IS NOT INITIAL.
      INSERT ls_ext_view_result_wo_hoto INTO TABLE e_ext_views_results.
    ENDIF.
  ENDMETHOD.

  METHOD set_max_severity.
    IF c_severity = 'A'.
      "nothing to do, max already reached
    ELSEIF c_severity = 'E'.
      IF i_severity = 'A'.
        c_severity = 'A'.
      ENDIF.
    ELSEIF c_severity = 'W'.
      IF i_severity = 'A' OR i_severity = 'E'.
        c_severity = i_severity.
      ENDIF.
    ELSEIF c_severity = space OR c_severity = 'I'.
      IF i_severity <> space AND i_severity <> 'I'.
        c_severity = i_severity.
      ELSEIF c_severity = space.
        c_severity = 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

"! Helper class to perform logging to a log file.
CLASS lcl_logger DEFINITION.
  PUBLIC SECTION.
    METHODS:
      "! Create logger instance for passed logfile
      constructor
        IMPORTING i_logfile TYPE trfile,

      "! Logger method to create message in log with passed values.
      log_message
        IMPORTING
          i_level      TYPE c
          i_severity   TYPE c DEFAULT space
          i_msg_nr     TYPE char3
          i_new_object TYPE c DEFAULT space
          i_var1       TYPE string OPTIONAL
          i_var2       TYPE string OPTIONAL
          i_var3       TYPE string OPTIONAL
          i_var4       TYPE string OPTIONAL,

      "! Writes all collected log messgaes to log file.
      write_log,

      "! Logs an empty line (msgnr 507), by default on level 4, if other level needed, it can be set.
      log_empty_line
        IMPORTING
          i_level TYPE c DEFAULT '4',

      "! Logs a long text log line. (protokoll_longtext)
      log_long_text
        IMPORTING
          i_level      TYPE c
          i_severity   TYPE c DEFAULT space
          i_msg_nr     TYPE char3
          i_new_object TYPE c DEFAULT space
          i_long_text  TYPE string.

  PROTECTED SECTION.
    DATA:
      "! Log file of the logger instance
      mv_logfile  TYPE trfile,
      "! Log messages collected so far to be used by next flush
      mt_messages TYPE tt_sprot_u.
ENDCLASS.

CLASS lcl_logger IMPLEMENTATION.

  METHOD constructor.
    mv_logfile = i_logfile.
  ENDMETHOD.

  METHOD log_message.
    "##TODO bad style to collect log here and in report... later during write_log
    APPEND VALUE #( level = i_level
                    severity = i_severity
                    langu = gc_langu
                    ag = gc_ag
                    msgnr = i_msg_nr
                    newobj = i_new_object
                    var1 = i_var1
                    var2 = i_var2
                    var3 = i_var3
                    var4 = i_var4 ) TO mt_messages.
  ENDMETHOD.

  METHOD write_log.
    "##TODO bad style to use perform... but need to be used so far because of gv_max_severity
    LOOP AT mt_messages REFERENCE INTO DATA(lr_message).
      PERFORM protokoll USING lr_message->level lr_message->severity lr_message->msgnr lr_message->newobj lr_message->var1 lr_message->var2 lr_message->var3 lr_message->var4.
    ENDLOOP.
    PERFORM write_prot_no_condense_to_file USING mv_logfile.
    FREE mt_messages.
  ENDMETHOD.


  METHOD log_empty_line.
    log_message( i_level = i_level i_severity = space i_msg_nr = '507' ).
  ENDMETHOD.


  METHOD log_long_text.
    lcl_rddhanadeployment_helper=>split_message(
                      EXPORTING message = i_long_text
                      IMPORTING et_split_message = DATA(lt_split_message)
    ).

    LOOP AT lt_split_message REFERENCE INTO DATA(lr_split_message).
      "create new line seperator only for first line
      IF sy-tabix = 1.
        log_message( i_level = i_level i_severity = i_severity i_msg_nr = i_msg_nr i_new_object = i_new_object
                     i_var1 = CONV #( lr_split_message->var1 ) i_var2 = CONV #( lr_split_message->var2 )
                     i_var3 = CONV #( lr_split_message->var3 ) i_var4 = CONV #( lr_split_message->var4 ) ).
      ELSE.
        log_message( i_level = i_level i_severity = i_severity i_msg_nr = i_msg_nr
                   i_var1 = CONV #( lr_split_message->var1 ) i_var2 = CONV #( lr_split_message->var2 )
                   i_var3 = CONV #( lr_split_message->var3 ) i_var4 = CONV #( lr_split_message->var4 ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_text_deploy_result_logger DEFINITION DEFERRED.
"! Helper class to create log messages after text deployment.<br/>
"! Each instance of this class is responsible to write log for one log file (1 transport request).<br/>
"! During initialization the logfile for the instance and all e071 entries (tt_e071_hot_refs) of current import must be provided.
"! The logger instance then filters for its own logfile (by trkorr), lockflag = '2' and object = 'HOTO' (LIMU or LANG) and uses
"! these items during later method calls.<br/>
"! Remark: items with lockflag = '3' should have been logged already earlier as "successfully processed in a previous import".
CLASS lcl_text_deploy_result_logger DEFINITION FRIENDS ltcl_text_deploy_result_logger.

  PUBLIC SECTION.
    METHODS:
      "! Initializes the current logger instance by filtering out only the entries it should
      "! use, filtered by i_trkorr.
      constructor
        IMPORTING i_e071_entries TYPE tt_e071_hot_refs
                  i_trkorr       TYPE trkorr,

      "! Log text deploy header (message 593) for request if at least 1 HOTO (LIMU or LANG) is part of the request.
      write_header,

      "! Log text deploy footer (message 594) for request if at least 1 HOTO (LIMU or LANG) is part of the request.
      write_footer,

      "! Filters passed table i_not_active_objects for relevant not_active_objects for this logger and stores result
      set_not_active_objects
        IMPORTING
          i_not_active_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,

      "! Filters passed table i_unknown_objects for relevant unknown_objects for this logger and stores result
      set_unknown_objects
        IMPORTING
          i_unknown_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,

      "! Filters passed table i_skipped_objects for relevant skipped_objects for this logger and stores result
      set_skipped_objects
        IMPORTING
          i_skipped_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,

      "! Filters passed table i_failed_text_deploy_result for relevant deploy_result for this logger and stores result
      set_failed_text_deploy_results
        IMPORTING
          i_failed_text_deploy_result TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results,

      "! Filters passed table i_ok_text_deploy_result for relevant deploy_result for this logger and stores result
      set_ok_text_deploy_results
        IMPORTING
          i_ok_text_deploy_result TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results,

      "! Writes log for the data passed in other methods before.
      write_log.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_object_with_lang,
        cts_hot_object TYPE REF TO cl_cts_hot_object_v1,
        lang           TYPE spras,
        touched        TYPE abap_bool, "marks this object that it got any result set via set... method. If no method did mark this object,
        "it is to be handled as skipped. Example: 2 requests for same HANA object: TR1 with LIMU HOTO,
        "TR2 with LANG HOTO for language F. In HTA there is only language D and E so set_skipped will not be
        "called because D and E could be deployed successfully and deployment of textx for F is not explicitly
        "tried because LIMU HOTO implies all languages as ' '
      END OF ty_object_with_lang,
      ty_objects_with_lang TYPE HASHED TABLE OF ty_object_with_lang WITH UNIQUE KEY cts_hot_object lang.

*      ty_dref_e071_hot_ref  TYPE REF TO ty_e071_hot_refs,
*      ty_dref_e071_hot_refs TYPE STANDARD TABLE OF ty_dref_e071_hot_ref WITH NON-UNIQUE SORTED KEY obj_lang_key COMPONENTS ty_dref_e071_hot_ref-cts_hot_object lang.
    DATA:
*      m_e071_entries      TYPE ty_dref_e071_hot_refs,
      "! All objects and their languages this logger is responsible for logging.
      mt_objects_with_lang  TYPE ty_objects_with_lang,
      mt_not_active_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
      mt_unknown_objects    TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
      mt_skipped_objects    TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
      mt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results,
      "! All text_deploy_results where text deployment finished successfully, already filtered for this logger.
      mt_ok_objects         TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results,
      mr_logger             TYPE REF TO lcl_logger.


    METHODS:
      "! returns the name of the passed object as string in the following format:
      "! object_name.suffix (package name)
      get_object_name_for_log
        IMPORTING
          i_cts_hot_object TYPE REF TO cl_cts_hot_object_v1
        RETURNING
          VALUE(r_result)  TYPE string.

ENDCLASS.

CLASS lcl_text_deploy_result_logger IMPLEMENTATION.
  METHOD constructor.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_e071_entries REFERENCE INTO DATA(lr_e071_hot_ref) WHERE trkorr = i_trkorr AND object = 'HOTO' AND lockflag = '2'.
      "check whether object already exists in mt_objects_with_lang, maybe different language?
      "language space means all languages which is set for LIMU HOTOs. Language DE or EN or ... is set on LANG HOT only but will only be use if not all languages should be deployed.
      IF lr_e071_hot_ref->pgmid = 'LIMU' AND lr_e071_hot_ref->lang IS INITIAL. "LIMU HOTO overrules LANG HOTO for any language
        "Delete all existing entries with specific language
        LOOP AT mt_objects_with_lang REFERENCE INTO lr_object_with_lang WHERE cts_hot_object = lr_e071_hot_ref->cts_hot_object.
          DELETE TABLE mt_objects_with_lang WITH TABLE KEY cts_hot_object = lr_object_with_lang->cts_hot_object lang = lr_object_with_lang->lang.
        ENDLOOP.

        "add as language independent
        INSERT VALUE #( cts_hot_object = lr_e071_hot_ref->cts_hot_object ) INTO TABLE mt_objects_with_lang. "without lang
      ELSE.
        "if LANG HOTO only insert if not yet inserted as LIMU HOTO (all languages)
        IF lr_e071_hot_ref->pgmid = 'LANG' AND lr_e071_hot_ref->lang IS NOT INITIAL.
          IF line_exists( mt_objects_with_lang[ cts_hot_object = lr_e071_hot_ref->cts_hot_object lang = space ] ).
            "nothing to insert because already part of list with all languages which is ''
          ELSE.
            INSERT VALUE #( cts_hot_object = lr_e071_hot_ref->cts_hot_object lang = lr_e071_hot_ref->lang ) INTO TABLE mt_objects_with_lang.
          ENDIF.
        ENDIF.
      ENDIF.

      IF mr_logger IS INITIAL.
        mr_logger = NEW lcl_logger( i_logfile = lr_e071_hot_ref->logfile ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_not_active_objects.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_not_active_objects INTO DATA(lr_hot_object).
      "check whether object exists in mt_objects_with_lang.
      CLEAR lr_object_with_lang.
      lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object ] DEFAULT lr_object_with_lang ).
      IF lr_object_with_lang IS BOUND.
        INSERT lr_hot_object INTO TABLE mt_not_active_objects.
        lr_object_with_lang->touched = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_unknown_objects.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_unknown_objects INTO DATA(lr_hot_object).
      "check whether object exists in mt_objects_with_lang.
      CLEAR lr_object_with_lang.
      lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object ] DEFAULT lr_object_with_lang ).
      IF lr_object_with_lang IS BOUND.
        INSERT lr_hot_object INTO TABLE mt_unknown_objects.
        lr_object_with_lang->touched = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_skipped_objects.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_skipped_objects INTO DATA(lr_hot_object).
      "check whether object exists in mt_objects_with_lang.
      CLEAR lr_object_with_lang.
      lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object ] DEFAULT lr_object_with_lang ).
      IF lr_object_with_lang IS BOUND.
        INSERT lr_hot_object INTO TABLE mt_skipped_objects.
        lr_object_with_lang->touched = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_failed_text_deploy_results.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_failed_text_deploy_result REFERENCE INTO DATA(lr_hot_object).
      "check whether object exists in mt_objects_with_lang for specific language (LANG HOTO) or all languages (LIMU HOTO with '' as lang).
      CLEAR lr_object_with_lang.
      lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object->cts_hot_object lang = lr_hot_object->abap_lang ] DEFAULT lr_object_with_lang ). "LANG HOTO
      IF lr_object_with_lang IS BOUND.
        INSERT lr_hot_object->* INTO TABLE mt_failed_objects.
        lr_object_with_lang->touched = abap_true.
      ELSE.
        lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object->cts_hot_object lang = '' ] DEFAULT lr_object_with_lang ). "LIMU HOTO
        IF lr_object_with_lang IS BOUND.
          INSERT lr_hot_object->* INTO TABLE mt_failed_objects.
          lr_object_with_lang->touched = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_ok_text_deploy_results.
    DATA lr_object_with_lang TYPE REF TO ty_object_with_lang.

    LOOP AT i_ok_text_deploy_result REFERENCE INTO DATA(lr_hot_object).
      "check whether object exists in mt_objects_with_lang for specific language (LANG HOTO) or all languages (LIMU HOTO with '' as lang).
      CLEAR lr_object_with_lang.
      lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object->cts_hot_object lang = lr_hot_object->abap_lang ] DEFAULT lr_object_with_lang ). "LANG HOTO
      IF lr_object_with_lang IS BOUND.
        INSERT lr_hot_object->* INTO TABLE mt_ok_objects.
        lr_object_with_lang->touched = abap_true.
      ELSE.
        lr_object_with_lang = REF #( mt_objects_with_lang[ cts_hot_object = lr_hot_object->cts_hot_object lang = '' ] DEFAULT lr_object_with_lang ). "LIMU HOTO
        IF lr_object_with_lang IS BOUND.
          INSERT lr_hot_object->* INTO TABLE mt_ok_objects.
          lr_object_with_lang->touched = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_header.
    IF mr_logger IS BOUND.
      mr_logger->log_message( i_level = '3' i_msg_nr = '593' i_new_object = 'X' ). "Begin details of text import
      mr_logger->write_log( ).
    ENDIF.
  ENDMETHOD.


  METHOD write_footer.
    IF mr_logger IS BOUND.
      mr_logger->log_message( i_level = '3' i_msg_nr = '594' ). "End details of text import
      mr_logger->write_log( ).
    ENDIF.
  ENDMETHOD.


  METHOD write_log.
    DATA: lv_tmptext             TYPE string,
          lt_deployed_languages  TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY,
          lv_count               TYPE i,
          lv_deployed_text_count TYPE i,
          "! nr of objects not touched and therefore skipped. (see explanation for type ty_object_with_lang
          lv_not_touched         TYPE i,
          lv_598_written         TYPE abap_bool,
          ls_split_text          TYPE cl_cts_hot_utility=>ty_split_text_50.

    IF mr_logger IS NOT BOUND.
      RETURN.
    ENDIF.

*1. create overview of imported texts
    LOOP AT mt_ok_objects REFERENCE INTO DATA(lr_text_deploy_result).
      INSERT |{ lr_text_deploy_result->abap_lang }/{ lr_text_deploy_result->hana_lang }| INTO TABLE lt_deployed_languages.
      lv_deployed_text_count = lv_deployed_text_count + lr_text_deploy_result->imported_text_count.
    ENDLOOP.

    " Find out how many different objects are there where text was successfully deployed
    CLEAR lv_count.
    LOOP AT mt_ok_objects REFERENCE INTO lr_text_deploy_result GROUP BY ( cts_hot_object = lr_text_deploy_result->cts_hot_object )
                                                               WITHOUT MEMBERS
                                                               REFERENCE INTO DATA(lr_result_group_ok).
      lv_count = lv_count + 1.
    ENDLOOP.
    mr_logger->log_message( i_level = '3' i_msg_nr = '596' i_var1 = |{ lv_count }| ). "Objects for which text was deployed: &1

    " Log number of deployed texts
    mr_logger->log_message( i_level = '3' i_msg_nr = '597' i_var1 = |{ lv_deployed_text_count }| ). "Deployed texts: &1

    " Log deployed languages summary (if required in 2 lines, number 598 and 533)
    CLEAR lv_tmptext.
    lv_598_written = abap_false.
    LOOP AT lt_deployed_languages INTO DATA(lv_lang).
      IF strlen( lv_tmptext ) + strlen( lv_lang ) > 75. "75 chars can be put in 598, more to be added as seperate line
        ls_split_text = cl_cts_hot_utility=>split_text_50_chars( lv_tmptext ).
        mr_logger->log_message( i_level = '3' i_msg_nr = '598' i_var1 = CONV #( ls_split_text-chunk1 ) i_var2 = CONV #( ls_split_text-chunk2 ) ). "Deployed languages: &1
        lv_598_written = abap_true.
        CLEAR lv_tmptext.
      ENDIF.
      lv_tmptext = |{ lv_tmptext }{ lv_lang } |.
    ENDLOOP.

    IF lv_598_written = abap_false.
      ls_split_text = cl_cts_hot_utility=>split_text_50_chars( lv_tmptext ).
      mr_logger->log_message( i_level = '3' i_msg_nr = '598' i_var1 = CONV #( ls_split_text-chunk1 ) i_var2 = CONV #( ls_split_text-chunk2 ) ). "Deployed languages: &1
    ELSE.
      mr_logger->log_long_text( i_level = '3' i_msg_nr = '533' i_long_text = lv_tmptext ). "rest of deployed languages
    ENDIF.

    " Log number of skipped objects due to errors during import / activation
    lv_tmptext = |{ lines( mt_not_active_objects ) }|.
    IF mt_not_active_objects IS NOT INITIAL.
      mr_logger->log_message( i_level = '3' i_severity = 'W' i_msg_nr = '600' i_var1 = lv_tmptext ). "Skipped objects due to import/activation errors: &1
    ELSE.
      mr_logger->log_message( i_level = '4' i_msg_nr = '600' i_var1 = lv_tmptext ). "Skipped objects due to import/activation errors: &1
    ENDIF.

    " Log number of objects without textdeployment because either object missing in HTA or no text found for language to be deployed.
    LOOP AT mt_objects_with_lang REFERENCE INTO DATA(lr_object_with_lang) WHERE touched = ' '
                                                                          GROUP BY ( cts_hot_object = lr_object_with_lang->cts_hot_object )
                                                                          WITHOUT MEMBERS
                                                                          REFERENCE INTO DATA(lr_result_group_skipped).
      lv_not_touched = lv_not_touched + 1.
    ENDLOOP.
    lv_tmptext = |{ lv_not_touched + lines( mt_skipped_objects ) + lines( mt_unknown_objects ) }|.
    mr_logger->log_message( i_level = '3' i_msg_nr = '601' i_var1 = lv_tmptext ). "Objects without textdeployment: &1

    " Find out how many different objects are there that had errors during text deployment
    CLEAR lv_count.
    LOOP AT mt_failed_objects REFERENCE INTO lr_text_deploy_result GROUP BY ( cts_hot_object = lr_text_deploy_result->cts_hot_object )
                                                                   WITHOUT MEMBERS
                                                                   REFERENCE INTO DATA(lr_result_group_failed).
      lv_count = lv_count + 1.
    ENDLOOP.
    IF lv_count > 0.
      mr_logger->log_message( i_level = '2' i_severity = 'E' i_msg_nr = '603' i_var1 = |{ lv_count }| ). "Objects with errors during textdeployment: &1
    ELSE.
      mr_logger->log_message( i_level = '3' i_msg_nr = '603' i_var1 = '0' ). "Objects with errors during textdeployment: &1
    ENDIF.

* 2. Log details of imported texts
    " Log all failed objects with the language for which the error did occur and the hana error code and error message
    IF mt_failed_objects IS NOT INITIAL.
      mr_logger->log_message( i_level = '2' i_severity = 'E' i_msg_nr = '604' i_new_object = 'X' ). "Following objects have errors durign text deployment:
      LOOP AT mt_failed_objects REFERENCE INTO DATA(lr_failed_object).
        lv_tmptext = |{ get_object_name_for_log( lr_failed_object->cts_hot_object ) } language: { lr_failed_object->abap_lang }/{ lr_failed_object->hana_lang }|.
        mr_logger->log_long_text( i_level = '2' i_severity = 'E' i_msg_nr = '533' i_long_text = lv_tmptext ). "&1&2&3&4

        lv_tmptext = |  { lr_failed_object->hana_error_code }: { lr_failed_object->hana_error_message }|.
        mr_logger->log_long_text( i_level = '2' i_severity = 'E' i_msg_nr = '533' i_long_text = lv_tmptext ). "&1&2&3&4
      ENDLOOP.
    ENDIF.

    " Log all objects that had import / activation errors
    IF mt_not_active_objects IS NOT INITIAL.
      mr_logger->log_empty_line( i_level = '3' ).
      mr_logger->log_message( i_level = '3' i_severity = 'W' i_msg_nr = '606' ). "Objects with import/activation errors:
      LOOP AT mt_not_active_objects INTO DATA(lr_not_active_object).
        mr_logger->log_long_text( i_level = '3' i_severity = 'W' i_msg_nr = '533' i_long_text = get_object_name_for_log( lr_not_active_object ) ). "&1&2&3&4
      ENDLOOP.
    ENDIF.

    " Log all objects where text was deployed successfully with language and number of texts for this language
    IF mt_ok_objects IS NOT INITIAL.
      mr_logger->log_empty_line( ).
      mr_logger->log_message( i_level = '4' i_msg_nr = '599' ). "Following objects had text deployment:
      SORT mt_ok_objects BY cts_hot_object->hana_package_id ASCENDING cts_hot_object->hana_object_name ASCENDING cts_hot_object->hana_object_suffix ASCENDING abap_lang ASCENDING.
      LOOP AT mt_ok_objects REFERENCE INTO DATA(lr_ok_object_group) GROUP BY ( cts_hot_object = lr_ok_object_group->cts_hot_object ).
        CLEAR lv_tmptext.
        " Use two loop at groups to get the languages order by abap_lang
        LOOP AT GROUP lr_ok_object_group REFERENCE INTO DATA(lr_ok_group_member_group) GROUP BY ( abap_lang = lr_ok_group_member_group->abap_lang ) ASCENDING.
          LOOP AT GROUP lr_ok_group_member_group REFERENCE INTO DATA(lr_lang_group_member).
            lv_tmptext = |{ lv_tmptext } { lr_lang_group_member->abap_lang }/{ lr_lang_group_member->hana_lang }:{ lr_lang_group_member->imported_text_count }|.
          ENDLOOP.
        ENDLOOP.

        lv_tmptext = |{ get_object_name_for_log( lr_ok_object_group->cts_hot_object ) } -{ lv_tmptext }|.
        mr_logger->log_long_text( i_level = '4' i_msg_nr = '533' i_long_text = lv_tmptext ). "&1&2&3&4
      ENDLOOP.
    ENDIF.

    " Log objects not known in HTA
    IF mt_unknown_objects IS NOT INITIAL.
      mr_logger->log_empty_line( ).
      mr_logger->log_message( i_level = '4' i_msg_nr = '605' ). "Following objects were not found in HTA-Repository:
      SORT mt_unknown_objects BY table_line->hana_package_id ASCENDING table_line->hana_object_name ASCENDING table_line->hana_object_suffix ASCENDING.
      LOOP AT mt_unknown_objects INTO DATA(lr_unknown_object).
        mr_logger->log_long_text( i_level = '4' i_msg_nr = '533' i_long_text = get_object_name_for_log( lr_unknown_object ) ). "&1&2&3&4
      ENDLOOP.
    ENDIF.

    " Log objects for which no texts were found in HTA
    IF mt_skipped_objects IS NOT INITIAL OR lv_not_touched IS NOT INITIAL.
      mr_logger->log_empty_line( ).
      mr_logger->log_message( i_level = '4' i_msg_nr = '602' ). "No texts found for following objects:
      SORT mt_skipped_objects BY table_line->hana_package_id ASCENDING table_line->hana_object_name ASCENDING table_line->hana_object_suffix ASCENDING.
      LOOP AT mt_skipped_objects INTO DATA(lr_skipped_object).
        mr_logger->log_long_text( i_level = '4' i_msg_nr = '533' i_long_text = get_object_name_for_log( lr_skipped_object ) ). "&1&2&3&4
      ENDLOOP.
      IF lv_not_touched IS NOT INITIAL.
        SORT mt_objects_with_lang BY cts_hot_object->hana_package_id ASCENDING cts_hot_object->hana_object_name ASCENDING cts_hot_object->hana_object_suffix ASCENDING lang ASCENDING.
        LOOP AT mt_objects_with_lang REFERENCE INTO DATA(lr_obj_with_lang) WHERE touched = abap_false
                                                                           GROUP BY ( cts_hot_object = lr_obj_with_lang->cts_hot_object )
                                                                           REFERENCE INTO DATA(lr_obj_with_lang_group).
          CLEAR lv_tmptext.
          LOOP AT GROUP lr_obj_with_lang_group REFERENCE INTO DATA(lr_obj_with_lang_group_member).
            lv_tmptext = |{ lv_tmptext } { lr_obj_with_lang_group_member->lang }|.
          ENDLOOP.
          lv_tmptext = |{ get_object_name_for_log( lr_obj_with_lang_group->cts_hot_object ) } -{ lv_tmptext }|.
          mr_logger->log_long_text( i_level = '4' i_msg_nr = '533' i_long_text = lv_tmptext ). "&1&2&3&4
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_object_name_for_log.
    IF i_cts_hot_object IS NOT INITIAL.
      r_result = |{ i_cts_hot_object->hana_object_name }.{ i_cts_hot_object->hana_object_suffix } ({ i_cts_hot_object->hana_package_id })|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cts_hot_logger_rdd DEFINITION INHERITING FROM cl_cts_hot_logger_abstract FINAL.
  PUBLIC SECTION.
    METHODS:
      if_cts_hot_logger~flush REDEFINITION.
ENDCLASS.

CLASS lcl_cts_hot_logger_rdd IMPLEMENTATION.
  METHOD if_cts_hot_logger~flush.
    "##TODO bad style to use perform... but need to be used so far because of gv_max_severity
    LOOP AT mt_messages REFERENCE INTO DATA(lr_message).
      PERFORM protokoll USING lr_message->level lr_message->severity lr_message->msgnr lr_message->newobj lr_message->var1 lr_message->var2 lr_message->var3 lr_message->var4.
    ENDLOOP.
    PERFORM write_prot_to_all_logfiles.
    FREE mt_messages.
  ENDMETHOD.
ENDCLASS.

"! Log helper for AMHC deployments and deletions for different logging during updates/upgrades.<br/>
"! During updates/upgrades NO errors must be logged because this would stop the updates/upgrade. Instead, if there are
"! errors that need to be processed after update/upgrade, e.g. during SPAU, only one log message with severity 'P' (postproccessing
"! necessary) should be written to the log.<br/>
"! However the real HDI error messages should also be logged. Therefore the standard logfile from trbat is used
"! for P messages only and a new logfile is created by this class per request to contain the HDI log. This log
"! has the same name as the trbat logfile but before '&gt;.SID&lt;' the string '_HDI' is added. Log is stored in
"! &lt;upgrade&gt;/log folder and not in &lt;upgrade&gt;/tmp folder.<br/>
"! The constructor already determines whether update/upgrade is running or not and in case creates the _HDI log file and replaces
"! the logger with this hdi logger in the gt_e071* tables.<br/>
"! Expected usage of this class:<br/>
"! <ol>
"! <li>Instantiate an instances before logging during deployment/deletion is started</li>
"! <li>call to log_begin_deployment or log_begin_deletion to get header entries written to the log. (Start deployment/deletion of
"! HDI containers)</li>
"! <li>call to log_end_deployment or log_end_deletion to get summary (deployment/deletion finished successfully/with warnings/with
"! errors) and header entries (End deployment/deletion of HDI containers) written to the log.</li>
"! <li>call to reset_logger to get the hdi loggers replaced in gt_e071_* by the trbat loggers</li>
"! </ol>
CLASS lcl_amhc_log_helper DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ir_e071_amhc_refs TYPE REF TO ty_t_e071_amhc_ref,

      "! Log begin deletion in trbat logger and in hdi logger if it is an update/upgrade
      log_begin_deletion,

      "! Log begin deployment in trbat logger and in hdi logger if it is an update/upgrade
      log_begin_deployment,

      "! Log end deletion in trbat logger and in hdi logger if it is an update/upgrade. In update/upgrade case also add a message
      "! to trbat logger if deletion of containers was successful or not
      log_end_deletion,

      "! Log end deployment in trbat logger and in hdi logger if it is an update/upgrade. In update/upgrade case also add a message
      "! to trbat logger if deployment of containers was successful or not
      log_end_deployment,

      "! Replace hdi logger instances in gt_e071_amhc tables with trbat logger
      reset_logger.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_trkorr_logger,
        "! the trkorr of the logger
        trkorr           TYPE trkorr,
        "! the original logger of trbat for P-messages only in case of errors
        original_logger  TYPE REF TO if_cts_hot_logger,
        "! the hdi logger for normal Deployment logging
        hdi_logger       TYPE REF TO cl_cts_hot_logger_tr,
        "! Name of the HDI log file (name and suffix, no path)
        hdi_logfile_name TYPE string,
      END OF ty_s_trkorr_logger,
      ty_t_trkorr_logger TYPE STANDARD TABLE OF ty_s_trkorr_logger WITH EMPTY KEY.

    DATA:
      "! Contains the original and hdi logger per trkorr
      mt_trkorr_logger            TYPE ty_t_trkorr_logger,
      "! Overall deploy_data. Filled within execute method.
      mr_e071_amhc_refs           TYPE REF TO ty_t_e071_amhc_ref,
      "! Is Update or Upgrade active?
      mv_is_update_upgrade_active TYPE abap_bool.

    METHODS:
      "! If current deployment/deletion is happening in update/upgrade, create separate hdi log files and logger instances
      "! and replace logger instances in gt_e071_amhc tables. original logger instances are stored in mt_trkorr_logger
      create_hdi_logger,

      "! Log a warning, when checking if system is in update/upgrade failed
      log_update_check_warning
        IMPORTING
          is_sy  TYPE syst OPTIONAL
          ir_exc TYPE REF TO cx_root OPTIONAL,

      "! Determines whether any kind of update or upgrade is running.<br/>
      "! Result is stored in mv_is_update_upgrade_active
      set_is_update_upgrade_active.

ENDCLASS.

CLASS lcl_amhc_log_helper IMPLEMENTATION.

  METHOD constructor.
    mr_e071_amhc_refs = ir_e071_amhc_refs.

    me->set_is_update_upgrade_active( ).

    me->create_hdi_logger( ).
  ENDMETHOD.


  METHOD create_hdi_logger.
    DATA:
      ls_e071_amhc_ref    TYPE ty_s_e071_amhc_ref,
      lv_hdi_logfile_name TYPE string.

    LOOP AT mr_e071_amhc_refs->* REFERENCE INTO DATA(lr_e071)
                                                       GROUP BY ( trkorr = lr_e071->trkorr logger = lr_e071->logger )
                                                       WITHOUT MEMBERS
                                                       REFERENCE INTO DATA(lr_e071_group).

      APPEND INITIAL LINE TO mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      lr_trkorr_logger->trkorr = lr_e071_group->trkorr.
      lr_trkorr_logger->original_logger = lr_e071_group->logger.

      IF mv_is_update_upgrade_active = abap_true.
        IF lr_e071_group->logger IS INSTANCE OF cl_cts_hot_logger_tr.
          DATA(lv_logfile) = CAST cl_cts_hot_logger_tr( lr_e071_group->logger )->get_logfile( ).
          lv_hdi_logfile_name = cl_cts_hot_logger_tr=>create_hdi_logfile_name( lv_logfile ).
        ELSE.
          lv_hdi_logfile_name = 'HTA' && sy-datum && sy-uzeit && '_HDI.' && sy-sysid.
        ENDIF.

        DATA(lr_hdi_logger) = cl_cts_hot_logger_tr=>create_instance_for_file( lv_hdi_logfile_name ).
        lr_hdi_logger->if_cts_hot_logger~set_msg_id( 'SCTS_HDI' ).

        ls_e071_amhc_ref-logger = lr_hdi_logger.
        MODIFY mr_e071_amhc_refs->* FROM ls_e071_amhc_ref TRANSPORTING logger WHERE trkorr = lr_e071_group->trkorr.

        lr_trkorr_logger->hdi_logger = lr_hdi_logger.
        lr_trkorr_logger->hdi_logfile_name = lv_hdi_logfile_name.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD reset_logger.
    DATA:
      ls_e071_amhc_ref TYPE ty_s_e071_amhc_ref.

    IF mv_is_update_upgrade_active = abap_false.
      RETURN.
    ENDIF.

    LOOP AT mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      ls_e071_amhc_ref-logger = lr_trkorr_logger->original_logger.
      MODIFY mr_e071_amhc_refs->* FROM ls_e071_amhc_ref TRANSPORTING logger WHERE trkorr = lr_trkorr_logger->trkorr.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_begin_deletion.
    GET TIME STAMP FIELD DATA(lv_timestamp).

    LOOP AT mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      lr_trkorr_logger->original_logger->new_log_section( ).
      lr_trkorr_logger->original_logger->message( iv_msg_nr = '533'   "Start of HDI container deletion: &1
                                                  iv_level = if_cts_hot_logger=>co_level_2
                                                  iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).

      IF mv_is_update_upgrade_active = abap_true.
        DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( lr_trkorr_logger->hdi_logger->get_logfile( ) ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = '590'  "  HDI container deletion log: &1&2
                                                    iv_level = if_cts_hot_logger=>co_level_3
                                                    iv_var1 = ls_split-chunk1
                                                    iv_var2 = ls_split-chunk2 ).

        IF lr_trkorr_logger->hdi_logger IS BOUND.
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~new_log_section( ).
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~message( iv_msg_nr = '533'  "Start of HDI container deletion: &1
                                                 iv_level = if_cts_hot_logger=>co_level_2
                                                 iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~flush( ).
        ENDIF.
      ENDIF.
      lr_trkorr_logger->original_logger->flush( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD log_begin_deployment.
    GET TIME STAMP FIELD DATA(lv_timestamp).

    LOOP AT mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      lr_trkorr_logger->original_logger->new_log_section( ).
      lr_trkorr_logger->original_logger->message( iv_msg_nr = '520'  "Start of deployment of HDI containers: &1
                                                  iv_level = if_cts_hot_logger=>co_level_2
                                                  iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).

      IF mv_is_update_upgrade_active = abap_true.
        DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( lr_trkorr_logger->hdi_logger->get_logfile( ) ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = '586'  "  HDI container deployment log: &1&2
                                                    iv_level = if_cts_hot_logger=>co_level_3
                                                    iv_var1 = ls_split-chunk1
                                                    iv_var2 = ls_split-chunk2 ).

        IF lr_trkorr_logger->hdi_logger IS BOUND.
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~new_log_section( ).
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~message( iv_msg_nr = '520'  "Start of deployment of HDI containers: &1
                                                 iv_level = if_cts_hot_logger=>co_level_2
                                                 iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
          lr_trkorr_logger->hdi_logger->if_cts_hot_logger~flush( ).
        ENDIF.
      ENDIF.
      lr_trkorr_logger->original_logger->flush( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD log_end_deletion.
    GET TIME STAMP FIELD DATA(lv_timestamp).

    LOOP AT mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      IF mv_is_update_upgrade_active = abap_false OR lr_trkorr_logger->hdi_logger IS NOT BOUND.
        lr_trkorr_logger->original_logger->new_log_section( ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = '539' "End of HDI container deletion: &1
                                                    iv_level = if_cts_hot_logger=>co_level_2
                                                    iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->original_logger->flush( ).
      ELSE.
        DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( lr_trkorr_logger->hdi_logger->get_logfile( ) ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = SWITCH #( lr_trkorr_logger->hdi_logger->if_cts_hot_logger~get_max_severity( )
                                                                       WHEN if_cts_hot_logger=>co_severity_info THEN '591' "  HDI containers were deleted; see &1
                                                                       WHEN if_cts_hot_logger=>co_severity_warning THEN '592' "  Warnings in deletion of HDI containers; see &1
                                                                       ELSE '593' ) "Error when deleting HDI containers; see &1 and SAP Note 2602571(?)
                                                      iv_level = if_cts_hot_logger=>co_level_2
                                                      iv_severity = SWITCH #( lr_trkorr_logger->hdi_logger->if_cts_hot_logger~get_max_severity( )
                                                                       WHEN if_cts_hot_logger=>co_severity_info THEN if_cts_hot_logger=>co_severity_info
                                                                       WHEN if_cts_hot_logger=>co_severity_warning THEN if_cts_hot_logger=>co_severity_warning
                                                                       ELSE if_cts_hot_logger=>co_severity_post_processing )
                                                      iv_var1 = |{ lr_trkorr_logger->hdi_logfile_name }|
                                                      iv_var2 = ls_split-chunk1
                                                      iv_var3 = ls_split-chunk2 ).

        lr_trkorr_logger->original_logger->message( iv_msg_nr = '539'  "End of HDI container deletion: &1
                                                    iv_level = if_cts_hot_logger=>co_level_2
                                                    iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->original_logger->flush( ).

        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~new_log_section( ).
        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~message( iv_msg_nr = '539' "End of HDI container deletion: &1
                                                                 iv_level = if_cts_hot_logger=>co_level_2
                                                                 iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~flush( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_end_deployment.
    GET TIME STAMP FIELD DATA(lv_timestamp).

    LOOP AT mt_trkorr_logger REFERENCE INTO DATA(lr_trkorr_logger).
      IF mv_is_update_upgrade_active = abap_false OR lr_trkorr_logger->hdi_logger IS NOT BOUND.
        lr_trkorr_logger->original_logger->new_log_section( ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = '521' "Ende Deployment von HDI Containern: &1
                                                    iv_level = if_cts_hot_logger=>co_level_2
                                                    iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->original_logger->flush( ).
      ELSE.
        DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( lr_trkorr_logger->hdi_logger->get_logfile( ) ).
        lr_trkorr_logger->original_logger->message( iv_msg_nr = SWITCH #( lr_trkorr_logger->hdi_logger->if_cts_hot_logger~get_max_severity( )
                                                                       WHEN if_cts_hot_logger=>co_severity_info THEN '587' "  HDI containers were deployed; see &1
                                                                       WHEN if_cts_hot_logger=>co_severity_warning THEN '588' "  Warnings in deployment of HDI containers; see &1
                                                                       ELSE '589' ) "Error when deploying HDI containers; see &1 and SAP Note 2602571(?)
                                                      iv_level = if_cts_hot_logger=>co_level_2
                                                      iv_severity = SWITCH #( lr_trkorr_logger->hdi_logger->if_cts_hot_logger~get_max_severity( )
                                                                       WHEN if_cts_hot_logger=>co_severity_info THEN if_cts_hot_logger=>co_severity_info
                                                                       WHEN if_cts_hot_logger=>co_severity_warning THEN if_cts_hot_logger=>co_severity_warning
                                                                       ELSE if_cts_hot_logger=>co_severity_post_processing )
                                                      iv_var1 = |{ lr_trkorr_logger->hdi_logfile_name }|
                                                      iv_var2 = ls_split-chunk1
                                                      iv_var3 = ls_split-chunk2 ).

        lr_trkorr_logger->original_logger->message( iv_msg_nr = '521'  "Ende Deployment von HDI Containern: &1
                                                    iv_level = if_cts_hot_logger=>co_level_2
                                                    iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->original_logger->flush( ).

        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~new_log_section( ).
        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~message( iv_msg_nr = '521' "Ende Deployment von HDI Containern: &1
                                                                 iv_level = if_cts_hot_logger=>co_level_2
                                                                 iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_timestamp ) } (UTC)| ).
        lr_trkorr_logger->hdi_logger->if_cts_hot_logger~flush( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_update_check_warning.
    LOOP AT mr_e071_amhc_refs->* REFERENCE INTO DATA(lr_e071)
                                                     GROUP BY ( logger = lr_e071->logger )
                                                     WITHOUT MEMBERS
                                                     REFERENCE INTO DATA(lr_e071_group).
      lr_e071_group->logger->warning( iv_msg_nr = '245' ). "  Fehler bei der Abfrage ob gerade ein Update oder Upgrade läuft. Weitermachen als wenn kein Upgrade läuft
      IF ir_exc IS BOUND.
        lr_e071_group->logger->warning_exception( ir_exc ).
      ELSEIF is_sy IS SUPPLIED.
        lr_e071_group->logger->message(
          EXPORTING
            iv_msg_id   = is_sy-msgid
            iv_msg_nr   = CONV msgnr( is_sy-msgno )
            iv_level    = if_cts_hot_logger=>co_level_3
            iv_severity = if_cts_hot_logger=>co_severity_warning
            iv_var1     = is_sy-msgv1
            iv_var2     = is_sy-msgv2
            iv_var3     = is_sy-msgv3
            iv_var4     = is_sy-msgv4
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_is_update_upgrade_active.
    DATA(lr_ext_hot_call_internal) = NEW cl_cts_hot_ext_call_intenal( ).

    TRY.
        IF lr_ext_hot_call_internal->if_cts_hot_ext_call_internal~is_blue_green_update_running( ) = abap_true.
          mv_is_update_upgrade_active = abap_true.
          RETURN.
        ENDIF.
      CATCH cx_cts_hta INTO DATA(lr_exc).
        me->log_update_check_warning( ir_exc = lr_exc ).
        "However, continue with check if "normal" update/upgrade (is_update_upgrade_running) is running
    ENDTRY.

    TRY.
        IF lr_ext_hot_call_internal->if_cts_hot_ext_call_internal~is_update_upgrade_running( ) = abap_true.
          mv_is_update_upgrade_active = abap_true.
          RETURN.
        ENDIF.
      CATCH cx_cts_hta INTO lr_exc.
        me->log_update_check_warning( ir_exc = lr_exc ).
        "no return by intention to check for ZDO Upgrade in next step
    ENDTRY.

    mv_is_update_upgrade_active = lr_ext_hot_call_internal->if_cts_hot_ext_call_internal~is_sum_with_zdo_running( ).
  ENDMETHOD.
ENDCLASS.


**************************************************************************
* END Local Class Definitions and Implementations - Start Report Coding                           *
**************************************************************************
DATA:
  ls_e071_hot_refs        TYPE ty_e071_hot_refs,
  "! Global table containing all E071 entries for deployment in correct order. (HOTAs are expanded to HOTP/HOTOs and inserted in this)
  gt_e071_hot_refs        TYPE tt_e071_hot_refs, "table must keep order as passed into this report!!! (last tr of an object is printed out)
  gs_e071_amhc_ref        TYPE ty_s_e071_amhc_ref,
  gt_e071_amhc_ref        TYPE ty_t_e071_amhc_ref,
  gt_e071_amhc_ref_del    TYPE ty_t_e071_amhc_ref,
  gt_e071_hdi_refs        TYPE cl_cts_hta_hdi_rdd_obj_deploy=>ty_t_e071_hotx_objects,
  gs_e071_hdi_refs        TYPE cl_cts_hta_hdi_rdd_obj_deploy=>ty_s_e071_hotx_object,
  gt_xmsg                 TYPE TABLE OF sprot_u,
  lv_dirtype              TYPE tstrf01-dirtype,
  ls_trbat                TYPE trbat,
  "!indicating whether the deployment was started by transport or by external call (Workbench activation or API call)
  gv_external_call        TYPE abap_bool,
  "! Stores max severity ('A' greater than 'E' greater than 'W' greater than ' ') for all logs created in forms protokoll...
  gv_max_severity         TYPE sprot_u-severity,
  gr_external_persistency TYPE REF TO lcl_external_persistency,
  gr_cts_hot_db_access    TYPE REF TO if_cts_hot_db_access,
  gr_hdi_object_db_access TYPE REF TO if_cts_hdi_object_db_access,
  gr_logger_list          TYPE REF TO if_cts_hot_logger.
DATA:
  gv_container_types_to_skip TYPE c LENGTH 10.

*----------------------------------------------------------------------*
SELECT-OPTIONS: so_trbat FOR ls_trbat LOWER CASE. "LOWER CASE needed because trbat contains logname with 'tmp' and this neds to be on correct case!
PARAMETERS: p_deplf TYPE char1 DEFAULT space NO-DISPLAY . "deploy failed for both, HDI and REPO='X'/only Repo='R', only HDI='H', all other=no redeployment
PARAMETERS: p_argum TYPE trbat-argument.

START-OF-SELECTION.
  FREE gt_xmsg.
  FREE gt_e071_hot_refs.
  FREE gt_e071_amhc_ref.
  FREE gt_e071_hdi_refs.
  CLEAR gv_external_call.
  CLEAR gv_max_severity.

  gr_logger_list = NEW cl_cts_hot_logger_list( ).
  gr_logger_list->set_msg_id( 'SCTS_HOT' ).

  lv_dirtype = 'T'.

  IF p_deplf CA 'XRH'.
    PERFORM deploy_all_failed USING p_deplf.
  ELSE.
    "This is the transport use case!
    gr_cts_hot_db_access = NEW cl_cts_hot_db_access( ).
    gr_hdi_object_db_access = NEW cl_cts_hdi_object_db_access( ).
    DATA(lv_broken_exists_before) = gr_cts_hot_db_access->exist_broken_object_or_package( ).
    DATA(lv_broken_hdi_exists_before) = gr_hdi_object_db_access->exists_broken_object( ).

    "read all HOTA HOTO HOTP objects for passed trkorrs from e071 and initialize log files. (ignore lockflag because we want to print also all objects with errors in previous step [lockflag <> 2 and <> 3])
    LOOP AT so_trbat REFERENCE INTO DATA(lr_trbat).
      "HANA repository objects never contain a '/' in obj_name
      SELECT @lr_trbat->low-timestmp AS trbat_order, trkorr, as4pos, pgmid, object, obj_name, lockflag, lang FROM e071 APPENDING CORRESPONDING FIELDS OF TABLE @gt_e071_hot_refs
                                     WHERE trkorr = @lr_trbat->low-trkorr
                                       AND ( object = 'HOTA' OR object = 'HOTO' OR object = 'HOTP' )
                                       AND obj_name NOT LIKE '%/%'.

      "HDI objects always contain a '/' in obj_name (either to separate namespace and container or to separate hash from first 10 readable charachtes
      SELECT @lr_trbat->low-timestmp AS trbat_order, trkorr, as4pos, pgmid, object, obj_name, objfunc, lockflag, lang FROM e071 APPENDING CORRESPONDING FIELDS OF TABLE @gt_e071_hdi_refs
                                     WHERE trkorr = @lr_trbat->low-trkorr
                                       AND ( object = 'HOTA' OR object = 'HOTO' )
                                       AND obj_name LIKE '%/%'.

      SELECT @lr_trbat->low-timestmp AS trbat_order, trkorr, as4pos, pgmid, object, obj_name, lockflag FROM e071 APPENDING CORRESPONDING FIELDS OF TABLE @gt_e071_amhc_ref
                                     WHERE trkorr = @lr_trbat->low-trkorr AND pgmid = 'R3TR' AND object = 'AMHC' AND lockflag = '2' AND objfunc NE 'D'.

      SELECT @lr_trbat->low-timestmp AS trbat_order, trkorr, as4pos, pgmid, object, obj_name, lockflag FROM e071 APPENDING CORRESPONDING FIELDS OF TABLE @gt_e071_amhc_ref_del
                                     WHERE trkorr = @lr_trbat->low-trkorr AND pgmid = 'R3TR' AND object = 'AMHC' AND lockflag = '2' AND objfunc = 'D'.

      CLEAR ls_e071_hot_refs.
      ls_e071_hot_refs-logger = cl_cts_hot_logger_tr=>create_instance( iv_trkorr = lr_trbat->low-trkorr iv_logname = lr_trbat->low-logname ).
      ls_e071_hot_refs-logfile = CAST cl_cts_hot_logger_tr( ls_e071_hot_refs-logger )->get_logfile( ).

      CAST cl_cts_hot_logger_list( gr_logger_list )->add_logger( ls_e071_hot_refs-logger ).

      "add logfile name and logger to our global hot table
      MODIFY gt_e071_hot_refs FROM ls_e071_hot_refs TRANSPORTING logfile logger WHERE trbat_order = lr_trbat->low-timestmp AND trkorr = lr_trbat->low-trkorr.

      "add logger instance to global table containing HDI objects.
      gs_e071_hdi_refs-logger = ls_e071_hot_refs-logger.
      MODIFY gt_e071_hdi_refs FROM gs_e071_hdi_refs TRANSPORTING logger WHERE trbat_order = lr_trbat->low-timestmp AND trkorr = lr_trbat->low-trkorr.

      "add logger to AMHC objects in global AMHC table with trkorr and trbat_order
      gs_e071_amhc_ref-logger = ls_e071_hot_refs-logger. " logger is same for all objects (HOTA, HOTO, HOTP or AMHC) of one tr
      MODIFY gt_e071_amhc_ref FROM gs_e071_amhc_ref TRANSPORTING logger WHERE trbat_order = lr_trbat->low-timestmp AND trkorr = lr_trbat->low-trkorr.
      MODIFY gt_e071_amhc_ref_del FROM gs_e071_amhc_ref TRANSPORTING logger WHERE trbat_order = lr_trbat->low-timestmp AND trkorr = lr_trbat->low-trkorr.

    ENDLOOP.

    "HDI deployment at all?
    IF gt_e071_amhc_ref IS NOT INITIAL OR gt_e071_hdi_refs IS NOT INITIAL OR gt_e071_amhc_ref_del IS NOT INITIAL.
      gr_logger_list->set_msg_id( 'SCTS_HDI' ).

      "check whether system is running on HANA and if HDI is enabled. If not on HANA, log info message.
      "If HDI is not enabled, log error "Fehler in der Konfiguration; HDI-Transporte nicht möglich" with RC 12
      DATA lv_hdi_enabled TYPE abap_bool.
      PERFORM is_hdi_enabled CHANGING lv_hdi_enabled. " using gt_e071_amhc_ref and gt_e071_hdi_refs
      IF lv_hdi_enabled = abap_true.
        IF p_argum(4) = 'SCT='. "skip parameter passed by TP as trbat.argument for tp parameter SKIP_HDI_DEPLOYMENT_CONTAINER_TYPES
          gv_container_types_to_skip = p_argum+4.
        ENDIF.
        PERFORM execute_container_deployment.  " using gt_e071_amhc_ref

        PERFORM execute_hdi_object_deployment. " using gt_e071_hdi_refs

        PERFORM execute_container_deletion USING gt_e071_amhc_ref_del gv_container_types_to_skip.
      ENDIF.
    ENDIF.

    gr_logger_list->set_msg_id( 'SCTS_HOT' ).

    DATA: lt_skipped_api_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list, "packages passed by API that do not exist in HTA
          lt_skipped_api_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list. "objects passed by API that do not exist in HTA
    PERFORM execute_hana_deployment USING 'A' CHANGING lt_skipped_api_packages lt_skipped_api_objects.

    PERFORM submit_job_deploy_all_failed USING lv_broken_exists_before
                                               lv_broken_hdi_exists_before.
  ENDIF.


***********************END report - start of forms

*---------------------------------------------------------------------*
*       FORM execute_hana_deployment                            *
*---------------------------------------------------------------------*
  "! Executes the HANA deployment.
  "!
  "! @parameter iv_abap_status | ABAP status to be used for reading data from HTA
  "! @parameter ct_skipped_api_packages | Packages passed by API that do not exist in HTA
  "! @parameter ct_skipped_api_objects | Objects passed by API that do not exist in HTA
FORM execute_hana_deployment USING    iv_abap_status TYPE cts_hot_abap_status
                             CHANGING ct_skipped_api_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                                      ct_skipped_api_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

  DATA: lr_e071_hot_refs               TYPE REF TO ty_e071_hot_refs,
        lt_e071_resolved_hota          TYPE tt_e071_hot_refs,
        lt_packages_to_deploy          TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
        lt_packages_to_be_deleted      TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
        lt_objects_to_deploy           TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
        "! All skipped objects plus successfully activated objects (also deletions) minus error objects during external view creation
        "! minus error objects during text deployment. Purpose is to update E071 correctly.
        lt_ok_deployed_objects_e071    TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,      "NICETODO Namen evtl. ändern
        lt_objects_for_text_deploy     TYPE cl_cts_hot_hana_connector=>ty_text_deploy_inputs,
        "! All objects with successful text deployment per lang or lang is empty table if all langs are successfully deployed
        "! (skipped is also seen as successful). Purpose is to update E071 correctly.
        lt_ok_deployed_obj_texts_e071  TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results,
        lr_object_for_text_deploy      TYPE REF TO cl_cts_hot_hana_connector=>ty_text_deploy_input,
        lv_abap_hana_package_id        TYPE cts_hot_package-abap_hana_package_id,
        lv_hot_status                  TYPE cts_hot_package-hot_status,
        hot_deployer                   TYPE REF TO cl_cts_hot_hana_connector,                     "NICETODO Namen evtl. ändern
        lo_cl_cts_hot_object           TYPE REF TO cl_cts_hot_object_v1,
        lo_cl_cts_hot_package          TYPE REF TO cl_cts_hot_package,
        lt_logfiles_packages           TYPE tt_logfile, "list of logfiles where correspinding TRs have packages assigned
        lt_logfiles_packages_to_be_del TYPE tt_logfile, "list of logfiles where correspinding TRs have packages for deletion assigned
        lt_logfiles_objects            TYPE tt_logfile, "List of logfiles for objects
        lv_enqueue_success             TYPE abap_bool.

  IF gt_e071_hot_refs IS INITIAL.
    RETURN. "nothing to do
  ENDIF.

  gr_external_persistency = NEW lcl_external_persistency( ).
  gr_cts_hot_db_access = NEW cl_cts_hot_db_access( ).

  IF gr_external_persistency->is_view_layer_system( ).
    LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs
                                               GROUP BY ( logger = lr_e071_hot_refs->logger )
                                               REFERENCE INTO DATA(lr_logger_group).
      lr_logger_group->logger->new_log_section( ).
      lr_logger_group->logger->message( iv_msg_nr = '501'
                                        iv_level = if_cts_hot_logger=>co_level_2
                                        iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( ) } (UTC)| ). "Begin deployment to HANA repository: &1
      IF gv_external_call = abap_true.
        lr_logger_group->logger->error( '632' ). "HANA Repository Content cannot be imported into view layer system
      ELSE.
        lr_logger_group->logger->warning( '632' ). "HANA Repository Content cannot be imported into view layer system
      ENDIF.
      lr_logger_group->logger->message( iv_msg_nr = '503'
                                        iv_level = if_cts_hot_logger=>co_level_2
                                        iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( ) } (UTC)| ). "End deployment to HANA repository: &1
      lr_logger_group->logger->flush( ).
    ENDLOOP.
    RETURN.
  ENDIF.

* Start of HANA Deployment (write to ALL logfiles)
*  PERFORM log_headerline_with_timestamp USING '2' '501' 'X'. "Begin deployment to HANA repository: &1
  gr_logger_list->info_level_4( iv_msg_nr = '502' iv_var1 = CONV #( sy-host ) ). "on app server: xyz
  "do not log sy-uname when deployment is triggered by import
  IF gv_external_call = abap_true.
    gr_logger_list->info_level_4( iv_msg_nr = '511' iv_var1 = CONV #( sy-uname ) ). "started by: xyz
  ENDIF.
  gr_logger_list->flush( ).

  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'HOTA' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs
                                               GROUP BY ( logger = lr_e071_hot_refs->logger )
                                               REFERENCE INTO DATA(lr_logger_group_2).
      lr_logger_group_2->logger->abnormal_termination( iv_msg_nr = '031' ). "no permission to deploy hana objects
      lr_logger_group_2->logger->flush( ).
    ENDLOOP.
    RETURN.
  ENDIF.

  PERFORM enqueue_hotas CHANGING lv_enqueue_success. "using gt_e071_hot_refs
  IF lv_enqueue_success = abap_false.
    RETURN.
  ENDIF.

***  CALL FUNCTION 'TRINT_CALL_AFTER_IMP_METHOD' " TODO: welche Parameter sollten wir zusätzlich berücksichtigen?
***    EXPORTING
***      iv_trkorr          = comfile
***      is_e070            = gs_e070
***      it_e071            = e071_tab[]
***      it_e071k           = e071k_tab[]
***      iv_datname         = gv_trbat_datname
***      iv_update_lockflag = 'X'
***      iv_ctc             = gv_ctc
***      iv_sfw_call        = space
***      iv_statlog_file    = gv_statlog_file
***      iv_trace_on        = trace_on
***    CHANGING
***      it_client          = gt_client.

* NICETODO: e071-lockflag behandeln
*  DATA: lv_object_name_suffix TYPE cts_hot_object_name_suffix. "TODONICE delete line?

* Try to get instance of hana connector to check whether we are on HANA or not
  TRY.
      hot_deployer = cl_cts_hot_hana_connector=>create_instance( ). "if this system is not on hana an exception is thrown.

* For all R3TR HOTAs compare HTA repository with HANA repository and add all objects that are in HANA but not in HTA as deletions to CTS_HOT_OBJECT
      PERFORM check_hana_for_obosolete_objs USING hot_deployer. "using gt_e071_hot_refs

* Determine HANA Repository packages/objects to deploy (lockflag = 2) and packages/objects already deployed (lockflag = 3). Ignore all other lockflags as they indicate errors in previous steps.
* First: expand R3TR HOTA to Limu HOTP and HOTO (but ignore duplicate HOTAs)
      LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE pgmid = 'R3TR' AND object = 'HOTA' AND ( lockflag = '2' OR lockflag = '3' )
                                                               GROUP BY ( obj_name = lr_e071_hot_refs->obj_name ) REFERENCE INTO DATA(lr_hota_group).
        "select only to check if package exists in HOTA repository
        SELECT SINGLE abap_hana_package_id FROM cts_hot_package INTO lv_abap_hana_package_id
                                           WHERE abap_hana_package_id = lr_hota_group->obj_name(40) AND abap_status = iv_abap_status.
        IF sy-subrc = 0.
          "expand hota in all requests where this hota exists.
          LOOP AT GROUP lr_hota_group INTO DATA(ls_hota_member).
            ls_hota_member-pgmid = 'LIMU'.
            ls_hota_member-object = 'HOTP'.
            INSERT ls_hota_member INTO TABLE lt_e071_resolved_hota.
            ls_hota_member-pgmid = 'LIMU'.
            ls_hota_member-object = 'HOTO'.
            CLEAR ls_hota_member-obj_name+40(80).
            SELECT abap_hana_object_name_suffix FROM cts_hot_object INTO ls_hota_member-obj_name+40
                                                WHERE abap_hana_package_id = ls_hota_member-obj_name(40) AND abap_status = iv_abap_status.
              INSERT ls_hota_member INTO TABLE lt_e071_resolved_hota.
              CLEAR ls_hota_member-obj_name+40(80).
            ENDSELECT.
          ENDLOOP.
        ELSE.
          " not in hot anymore - skipped
        ENDIF.
      ENDLOOP.

      INSERT LINES OF lt_e071_resolved_hota INTO TABLE gt_e071_hot_refs.
      FREE lt_e071_resolved_hota.

* Second: expand LANG HOTA to LANG HOTO (but ignore duplicate HOTAs)
      LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE pgmid = 'LANG' AND object = 'HOTA' AND ( lockflag = '2' OR lockflag = '3' )
                                                               GROUP BY ( obj_name = lr_e071_hot_refs->obj_name ) REFERENCE INTO DATA(lr_lang_hota_group).

        "check whether package exists in HTA and assign to LANG HOTA
        CLEAR lo_cl_cts_hot_package.
        lo_cl_cts_hot_package = cl_cts_hot_package=>create_instance_from_objname( iv_objname = lr_lang_hota_group->obj_name(40) iv_abap_status = iv_abap_status ).

        IF lo_cl_cts_hot_package->hana_package_id IS NOT INITIAL. "if package exists in HTA
          "expand hota in all requests where this hota exists.
          LOOP AT GROUP lr_lang_hota_group INTO DATA(ls_lang_hota_member).
            "set lr_package to all requests containing this LANG HOTA
            IF ls_lang_hota_member-cts_hot_package IS INITIAL.
              ls_lang_hota_member-cts_hot_package = lo_cl_cts_hot_package.
              MODIFY gt_e071_hot_refs FROM ls_lang_hota_member TRANSPORTING cts_hot_package WHERE pgmid = 'LANG' AND object = 'HOTA'
                                                                                              AND obj_name = ls_lang_hota_member-obj_name
                                                                                              AND ( lockflag = '2' OR lockflag = '3' ).
            ENDIF.

            ls_lang_hota_member-pgmid = 'LANG'.
            ls_lang_hota_member-object = 'HOTO'.
            CLEAR ls_lang_hota_member-obj_name+40(80).
            SELECT abap_hana_object_name_suffix FROM cts_hot_object INTO ls_lang_hota_member-obj_name+40
                                                WHERE abap_hana_package_id = ls_lang_hota_member-obj_name(40) AND abap_status = iv_abap_status.
              INSERT ls_lang_hota_member INTO TABLE lt_e071_resolved_hota.
              CLEAR ls_lang_hota_member-obj_name+40(80).
            ENDSELECT.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      INSERT LINES OF lt_e071_resolved_hota INTO TABLE gt_e071_hot_refs.
      FREE lt_e071_resolved_hota.

* Third: select all HOTPs (ignore duplicates, for this use group by)
      LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE pgmid = 'LIMU' AND object = 'HOTP' AND ( lockflag = '2' OR lockflag = '3' )
                                                               GROUP BY ( obj_name = lr_e071_hot_refs->obj_name ) REFERENCE INTO DATA(lr_hotp_group).

        CLEAR lo_cl_cts_hot_package.
        " check whether there already exists a LANG HOTA for same package (lockflag 2 or 3) and reuse instance for cts_hot_package
        LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_refs3) WHERE pgmid = 'LANG' AND object = 'HOTA'
                                                                              AND obj_name = lr_hotp_group->obj_name
                                                                              AND ( lockflag = '2' OR lockflag = '3' )
                                                                              AND cts_hot_package IS BOUND.
          lo_cl_cts_hot_package = lr_e071_hot_refs3->cts_hot_package.
          EXIT.
        ENDLOOP.

        "if no LANG HOTA exists yet, create a CTS_HOT_PACKAGE
        IF lo_cl_cts_hot_package IS NOT BOUND.
          lo_cl_cts_hot_package = cl_cts_hot_package=>create_instance_from_objname( iv_objname = lr_hotp_group->obj_name(40) iv_abap_status = iv_abap_status ).
        ENDIF.

        IF lo_cl_cts_hot_package->hana_package_id IS NOT INITIAL. "if package exists in HTA
          LOOP AT GROUP lr_hotp_group INTO DATA(ls_hotp_member).
            "set cl_cts_hot_package to all requests containing this package
            IF ls_hotp_member-cts_hot_package IS NOT BOUND. "following modify only needed once as ALL LIMU HOTPs / R3TR HOTAs are changed
              ls_hotp_member-cts_hot_package = lo_cl_cts_hot_package.
              MODIFY gt_e071_hot_refs FROM ls_hotp_member TRANSPORTING cts_hot_package WHERE ( pgmid = 'R3TR' OR pgmid = 'LIMU' ) AND ( object = 'HOTP' OR object = 'HOTA' )
                                                                                             AND obj_name = ls_hotp_member-obj_name "also HOTA to mark that this HOTA is not skipped
                                                                                             AND ( lockflag = '2' OR lockflag = '3' ).
            ENDIF.

            IF ls_hotp_member-lockflag = '2'. "we only need to work on packages with lockflag = '2'
              SELECT SINGLE hot_status FROM cts_hot_package INTO lv_hot_status WHERE abap_hana_package_id = ls_hotp_member-cts_hot_package->abap_hana_package_id AND abap_status = iv_abap_status.
              IF lv_hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                OR lv_hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
                APPEND ls_hotp_member-cts_hot_package TO lt_packages_to_be_deleted. "do not use lo_cl_cts_hot_package here because this is different instance and might be set by form deploy already if report is called via WB / API!!!
              ELSE.
                APPEND ls_hotp_member-cts_hot_package TO lt_packages_to_deploy. "do not use lo_cl_cts_hot_package here because this is different instance and might be set by form deploy already if report is called via WB / API!!!
              ENDIF.
              EXIT. "exit loop because we only need to have the package once in our worklist
            ENDIF.
          ENDLOOP.
        ELSE.
          " not in hot anymore - skipped
          "remove cts_hot_packages if there were some assigned by API users that do not exist in HTA.
          LOOP AT GROUP lr_hotp_group INTO ls_hotp_member.
            IF ls_hotp_member-cts_hot_package IS BOUND.
              APPEND ls_hotp_member-cts_hot_package TO ct_skipped_api_packages.
              CLEAR ls_hotp_member-cts_hot_package.
              MODIFY gt_e071_hot_refs FROM ls_hotp_member TRANSPORTING cts_hot_package WHERE pgmid = 'LIMU' AND object = 'HOTP'
                                                                                             AND obj_name = ls_hotp_member-obj_name
                                                                                             AND ( lockflag = '2' OR lockflag = '3' ).
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

* Fourth: select all LIMU HOTOs (ignore duplicates, for this use group by)
      LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE pgmid = 'LIMU' AND object = 'HOTO' AND ( lockflag = '2' OR lockflag = '3' )
                                                               GROUP BY ( obj_name = lr_e071_hot_refs->obj_name ) REFERENCE INTO DATA(lr_hoto_group).

        lo_cl_cts_hot_object = cl_cts_hot_object_v1=>create_instance_from_objname( iv_objname = lr_hoto_group->obj_name(110) iv_abap_status = iv_abap_status ).

        IF lo_cl_cts_hot_object->hana_object_name IS NOT INITIAL OR lo_cl_cts_hot_object->hana_object_suffix IS NOT INITIAL.
          LOOP AT GROUP lr_hoto_group INTO DATA(ls_hoto_member).
            "set cl_cts_hot_object to all requests containing this object
            IF ls_hoto_member-cts_hot_object IS NOT BOUND. "following modify only needed once as ALL hotos are changed
              ls_hoto_member-cts_hot_object = lo_cl_cts_hot_object.
              MODIFY gt_e071_hot_refs FROM ls_hoto_member TRANSPORTING cts_hot_object WHERE pgmid = 'LIMU' AND object = 'HOTO' AND obj_name = ls_hoto_member-obj_name
                                                                                            AND ( lockflag = '2' OR lockflag = '3' ).
            ENDIF.

            IF ls_hoto_member-lockflag = '2'. "we only need to work on objects with lockflag = '2'
              APPEND ls_hoto_member-cts_hot_object TO lt_objects_to_deploy. "do not use lo_cl_cts_hot_object here because this is different instance and might be set by form deploy already if report is called via WB / API!!!
              "append object also to text deploy table with language 'all' (empty abap_langs table)
              INSERT VALUE cl_cts_hot_hana_connector=>ty_text_deploy_input( cts_hot_object = ls_hoto_member-cts_hot_object ) INTO TABLE lt_objects_for_text_deploy.
              EXIT. "exit loop because we only need to have the object once in our worklist
            ENDIF.
          ENDLOOP.
        ELSE.
          " not in hot anymore - skipped
          "remove cts_hot_packages if there were some assigned by API users that do not exist in HTA.
          LOOP AT GROUP lr_hoto_group INTO ls_hoto_member.
            IF ls_hoto_member-cts_hot_object IS BOUND.
              APPEND ls_hoto_member-cts_hot_object TO ct_skipped_api_objects.
              CLEAR ls_hoto_member-cts_hot_object.
              MODIFY gt_e071_hot_refs FROM ls_hoto_member TRANSPORTING cts_hot_object WHERE pgmid = 'LIMU' AND object = 'HOTO'
                                                                                             AND obj_name = ls_hoto_member-obj_name
                                                                                             AND ( lockflag = '2' OR lockflag = '3' ).
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

* Fifth: select all LANG HOTOs per language (ignore duplicates, for this use group by)
      LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE pgmid = 'LANG' AND object = 'HOTO' AND ( lockflag = '2' OR lockflag = '3' )
                                                               GROUP BY ( obj_name = lr_e071_hot_refs->obj_name lang = lr_e071_hot_refs->lang )
                                                               REFERENCE INTO DATA(lr_lang_hoto_group).

        CLEAR lo_cl_cts_hot_object.
        " check whether there exists a LIMU or LANG HOTO for same object (lockflag 2 or 3) and reuse instance for cts_hot_object
        LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_refs2) WHERE ( pgmid = 'LIMU' OR pgmid = 'LANG' ) AND object = 'HOTO'
                                                                              AND obj_name = lr_lang_hoto_group->obj_name
                                                                              AND ( lockflag = '2' OR lockflag = '3' )
                                                                              AND cts_hot_object IS BOUND.
          lo_cl_cts_hot_object = lr_e071_hot_refs2->cts_hot_object.
          EXIT.
        ENDLOOP.

        "if no LIMU/LANG HOTO exists yet, create a CTS_HOT_OBJECT
        IF lo_cl_cts_hot_object IS NOT BOUND.
          lo_cl_cts_hot_object = cl_cts_hot_object_v1=>create_instance_from_objname( iv_objname = lr_lang_hoto_group->obj_name(110) iv_abap_status = iv_abap_status ).
        ENDIF.

        IF lo_cl_cts_hot_object->hana_object_name IS NOT INITIAL OR lo_cl_cts_hot_object->hana_object_suffix IS NOT INITIAL. "check on name or suffix because empty file names are allowed!
          LOOP AT GROUP lr_lang_hoto_group INTO DATA(ls_lang_hoto_member).
            "set cl_cts_hot_object to all requests containing this object and the current language
            IF ls_lang_hoto_member-cts_hot_object IS NOT BOUND. "following modify only needed once as ALL lang hotos are changed
              ls_lang_hoto_member-cts_hot_object = lo_cl_cts_hot_object.
              MODIFY gt_e071_hot_refs FROM ls_lang_hoto_member TRANSPORTING cts_hot_object WHERE pgmid = 'LANG' AND object = 'HOTO' AND obj_name = ls_lang_hoto_member-obj_name
                                                                                                 AND ( lockflag = '2' OR lockflag = '3' ).
            ENDIF.

            IF ls_lang_hoto_member-lockflag = '2'. "we only need to work on objects with lockflag = '2'
              lr_object_for_text_deploy = REF #( lt_objects_for_text_deploy[ cts_hot_object = ls_lang_hoto_member-cts_hot_object ] OPTIONAL ).
              "do not use lo_cl_cts_hot_object here because this is different instance and might be set by form deploy already if report is called via WB / API!!!
              IF lr_object_for_text_deploy IS BOUND.
                IF lr_object_for_text_deploy->abap_langs IS NOT INITIAL.
                  "only lang objects for this object, so add language
                  APPEND lr_lang_hoto_group->lang TO lr_object_for_text_deploy->abap_langs.
                ELSE.
                  "abap_langs initial means deploy all languages
                ENDIF.
              ELSE.
                CREATE DATA lr_object_for_text_deploy.
                lr_object_for_text_deploy->cts_hot_object = ls_lang_hoto_member-cts_hot_object. "do not use lo_cl_cts_hot_object here because this is different instance and might be set by form deploy already if report is called via WB / API!!!
                APPEND lr_lang_hoto_group->lang TO lr_object_for_text_deploy->abap_langs.
                INSERT lr_object_for_text_deploy->* INTO TABLE lt_objects_for_text_deploy.
              ENDIF.
              EXIT. "exit loop because we only need to have the language of this object once in our worklist
            ENDIF.
          ENDLOOP.
        ELSE.
          " not in hot anymore - skipped
        ENDIF.
      ENDLOOP.

* log HANA names of transport objects (LIMUs/R3TRs/LANGs) as they might have hashed values and to expand HOTAs
      PERFORM log_hana_names_of_trans_objs. "using gt_e071_hot_refs

* Log objects that were not imported by TP/R3trans and thus we do not need to work on them (lockflag <> 2 and <> 3)
      PERFORM log_not_imported_tr_objects.

* Log objects that are already imported successfully (lockflag is already 3)
      PERFORM log_already_imprted_tr_objects.

* log skipped transport objects (LIMUs/R3TRs on TR but not in HOT tables anymore) maybe already deleted in HANA and in HOT
      PERFORM log_skipped_transport_objects USING ct_skipped_api_packages ct_skipped_api_objects. "using gt_e071_hot_refs

* Start HANA deployment
      DATA(switch_framework_accessor) = NEW lcl_switch_framework_accessor( ).
      IF lt_packages_to_deploy IS NOT INITIAL.
        "sort packages ascending to create package hierarchy correctly
        SORT lt_packages_to_deploy BY table_line->hana_package_id ASCENDING.

        "Performance??? Idee: alle bisher gefundenen lt_logfiles_packages in die zweite loop where bedingung mit 'logfile not in lt_logfiles_packages' aufnehmen.
        LOOP AT lt_packages_to_deploy INTO DATA(lr_package).
          LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE cts_hot_package = lr_package.
            IF NOT line_exists( lt_logfiles_packages[ table_line = lr_e071_hot_refs->logfile ] ).
              APPEND lr_e071_hot_refs->logfile TO lt_logfiles_packages.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        PERFORM log_headerline_with_timestamp USING '2' '504' 'X'. "Start deployment of hana repository packages
        PERFORM write_prot_to_files USING lt_logfiles_packages.

        " check packages for prework done settings
        PERFORM check_and_log_prework_packags USING iv_abap_status lt_logfiles_packages CHANGING lt_packages_to_deploy gt_e071_hot_refs.

        " check packages for switch settings
        PERFORM check_and_log_switches_packags USING switch_framework_accessor lt_logfiles_packages iv_abap_status CHANGING lt_packages_to_deploy gt_e071_hot_refs.

        IF lt_packages_to_deploy IS NOT INITIAL. "might get initial during check of prework and switch settings
          hot_deployer->modify_packages_in_hana(
              EXPORTING
                  i_itab_packages = lt_packages_to_deploy " without packages to be deleted
                  i_abap_status   = iv_abap_status
              IMPORTING
                  e_created_packages = DATA(lt_created_packages)
                  e_skipped_packages = DATA(lt_skipped_packages)
                  e_skipped_packages_n = DATA(lt_skipped_packages_n)
                  e_deleted_packages = DATA(lt_deleted_packages)
                  e_updated_packages = DATA(lt_updated_packages)
                  e_failed_packages  = DATA(lt_failed_packages)
          ).

          PERFORM update_e071_hotp USING lt_created_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_skipped_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_deleted_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_updated_packages CHANGING gt_e071_hot_refs.

          "DANIEL: ##TODO: commit work? wegen e071?

          PERFORM log_package_results USING lt_logfiles_packages lt_skipped_packages lt_skipped_packages_n lt_created_packages lt_updated_packages lt_deleted_packages lt_failed_packages.
        ENDIF.

        PERFORM log_headerline_with_timestamp USING '2' '505' ' '. "End deployment of HANA repository packages" <date/time> 5x
        PERFORM write_prot_to_files USING lt_logfiles_packages.
      ENDIF.

      IF lt_objects_to_deploy IS NOT INITIAL OR lt_objects_for_text_deploy IS NOT INITIAL.
        LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE ( pgmid = 'LIMU' OR pgmid = 'LANG' ) AND object = 'HOTO' AND cts_hot_object IS BOUND
                                                                 GROUP BY ( logfile = lr_e071_hot_refs->logfile ).
          APPEND lr_e071_hot_refs->logfile TO lt_logfiles_objects.
        ENDLOOP.

        PERFORM log_headerline_with_timestamp USING '2' '508' 'X'. "Start deployment of hana repository objects
        PERFORM write_prot_to_files USING lt_logfiles_objects.

        " check objects for prework done settings
        PERFORM check_and_log_prework_objects USING iv_abap_status CHANGING lt_objects_to_deploy lt_objects_for_text_deploy gt_e071_hot_refs.

        " check objects for switch settings and removes if switch is on
        PERFORM check_and_log_switches_objects USING switch_framework_accessor iv_abap_status CHANGING lt_objects_to_deploy gt_e071_hot_refs.

        IF lt_objects_to_deploy IS NOT INITIAL. "might get initial during check of prework and switch settings
          DATA(lv_nr_of_trs) = lines( so_trbat ).
          PERFORM deploy_objects USING hot_deployer lt_objects_to_deploy lv_nr_of_trs iv_abap_status CHANGING lt_ok_deployed_objects_e071. "using gt_e071_hot_refs
        ENDIF.

        IF lt_objects_for_text_deploy IS NOT INITIAL.
          PERFORM deploy_object_texts USING hot_deployer lt_objects_for_text_deploy CHANGING lt_ok_deployed_objects_e071 lt_ok_deployed_obj_texts_e071.
        ENDIF.

        PERFORM update_e071_hoto USING lt_ok_deployed_objects_e071 CHANGING gt_e071_hot_refs."update only successfully activated/skipped objects. error objects do not need an update and are not contained in passed table
        PERFORM update_e071_lang_hoto USING lt_ok_deployed_obj_texts_e071 CHANGING gt_e071_hot_refs."update only successfully activated/skipped objects. error objects do not need an update and are not contained in passed table

        "DANIEL: todo commit work? wegen e071?

        PERFORM log_headerline_with_timestamp USING '2' '509' ' '. "End deployment of HANA repository objects" <date/time> 5x
        PERFORM write_prot_to_files USING lt_logfiles_objects.
      ENDIF.

* deploy hana packages to be deleted: lt_packages_to_be_deleted
      IF lt_packages_to_be_deleted IS NOT INITIAL.
        "Performance??? Idee: alle bisher gefundenen lt_logfiles_packages in die zweite loop where bedingung mit 'logfile not in lt_logfiles_packages' aufnehmen.
        LOOP AT lt_packages_to_be_deleted INTO DATA(lr_package_to_be_del).
          LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE cts_hot_package = lr_package_to_be_del.
            IF NOT line_exists( lt_logfiles_packages_to_be_del[ table_line = lr_e071_hot_refs->logfile ] ).
              APPEND lr_e071_hot_refs->logfile TO lt_logfiles_packages_to_be_del.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        PERFORM log_headerline_with_timestamp USING '2' '526' 'X'. "Start deletion of HANA repository packages: &1
        PERFORM write_prot_to_files USING lt_logfiles_packages_to_be_del.

        " check packages for prework done settings
        PERFORM check_and_log_prework_packags USING iv_abap_status lt_logfiles_packages_to_be_del CHANGING lt_packages_to_be_deleted gt_e071_hot_refs.

        " check packages for switch settings
        PERFORM check_and_log_switches_packags USING switch_framework_accessor lt_logfiles_packages_to_be_del iv_abap_status CHANGING lt_packages_to_be_deleted gt_e071_hot_refs.

        IF lt_packages_to_be_deleted IS NOT INITIAL. "might get initial during check of prework and switch settings
          "sort packages descending to first delete 'deepest/loewest' package in an hierarchy
          SORT lt_packages_to_be_deleted BY table_line->hana_package_id DESCENDING.

          CLEAR: lt_created_packages, lt_deleted_packages, lt_updated_packages, lt_failed_packages, lt_skipped_packages, lt_skipped_packages_n.
          hot_deployer->modify_packages_in_hana(
              EXPORTING
                  i_itab_packages = lt_packages_to_be_deleted
                  i_abap_status   = iv_abap_status
              IMPORTING
                  e_created_packages = lt_created_packages
                  e_deleted_packages = lt_deleted_packages
                  e_updated_packages = lt_updated_packages
                  e_failed_packages  = lt_failed_packages
                  e_skipped_packages = lt_skipped_packages
                  e_skipped_packages_n = lt_skipped_packages_n
          ).

          "TODO: is it OK to update E071 lcoakflag for created/updated packages if intention was to delete packages?
          PERFORM update_e071_hotp USING lt_created_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_skipped_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_deleted_packages CHANGING gt_e071_hot_refs.
          PERFORM update_e071_hotp USING lt_updated_packages CHANGING gt_e071_hot_refs.

          "DANIEL: todo commit work? wegen e071?

          PERFORM log_package_results_deletion USING lt_logfiles_packages_to_be_del lt_skipped_packages lt_skipped_packages_n lt_created_packages lt_updated_packages lt_deleted_packages lt_failed_packages.
        ENDIF.

        PERFORM log_headerline_with_timestamp USING '2' '527' ' '. "End deletion of HANA repository packages: &1
        PERFORM write_prot_to_files USING lt_logfiles_packages_to_be_del.
      ENDIF.

      PERFORM update_e071_hota CHANGING gt_e071_hot_refs.

    CATCH cx_hana_object_transport INTO DATA(lo_hot_exc).
      IF lo_hot_exc->if_t100_message~t100key = cx_hana_object_transport=>no_hana_database.
        PERFORM protokoll_with_ag USING '2' ' ' lo_hot_exc->if_t100_message~t100key-msgid lo_hot_exc->if_t100_message~t100key-msgno ' ' space  space space space.
        "in case of no HANA and SNote/SCWB, take over all objects/packages without activation
        IF iv_abap_status = 'I'.
          LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE cts_hot_package IS BOUND.
            gr_cts_hot_db_access->activate_package_cwb_snote( lr_e071_hot_refs->cts_hot_package->abap_hana_package_id ).
            lr_e071_hot_refs->lockflag = '3'. "indicate successful deployment
          ENDLOOP.

          LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_refs WHERE cts_hot_object IS BOUND.
            gr_cts_hot_db_access->activate_object_cwb_snote( lr_e071_hot_refs->cts_hot_object ).
            lr_e071_hot_refs->lockflag = '3'. "indicate successful deployment
          ENDLOOP.
        ENDIF.
      ELSE.
        PERFORM protokoll USING '2' 'A' '568' ' ' space space space space. "Error during deployment to HANA repository
        PERFORM protokoll_with_ag USING '2' 'A' lo_hot_exc->if_t100_message~t100key-msgid lo_hot_exc->if_t100_message~t100key-msgno ' ' lo_hot_exc->msgv1  lo_hot_exc->msgv2 lo_hot_exc->hana_error_code lo_hot_exc->hana_error_msg.
        IF lo_hot_exc->previous IS BOUND.
          DATA(lv_exc_text) = lo_hot_exc->previous->get_text( ).
          PERFORM protokoll_longtext USING '2' 'A' '531' ' ' lv_exc_text.
        ENDIF.
        IF lo_hot_exc->hana_error_code IS NOT INITIAL OR lo_hot_exc->hana_error_msg IS NOT INITIAL.
          lv_exc_text = | { lo_hot_exc->hana_error_code } { lo_hot_exc->hana_error_msg } |.
          PERFORM protokoll_longtext USING '2' 'A' '530' ' ' lv_exc_text.
        ENDIF.
      ENDIF.
      PERFORM write_prot_to_all_logfiles.
    CATCH cx_nhi_hana_repository INTO DATA(lo_nhi_exc).
      PERFORM protokoll USING '2' 'A' '568' ' ' space space space space. "Error during deployment to HANA repository
      PERFORM protokoll_with_ag USING '2' 'A' lo_nhi_exc->if_t100_message~t100key-msgid lo_nhi_exc->if_t100_message~t100key-msgno ' ' lo_nhi_exc->msgv1  lo_nhi_exc->msgv2 lo_nhi_exc->msgv3 lo_nhi_exc->msgv4.
      IF lo_nhi_exc->previous IS BOUND.
        DATA(lv_exc_text2) = lo_nhi_exc->previous->get_text( ).
        PERFORM protokoll_longtext USING '2' 'A' '531' ' ' lv_exc_text2.
      ENDIF.
  ENDTRY.

*  PERFORM log_headerline_with_timestamp USING '2' '503' ' '. "End deployment to HANA repository: &1
  PERFORM write_prot_to_all_logfiles.

  PERFORM dequeue_hotas.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_skipped_transport_objects                            *
*---------------------------------------------------------------------*
"! All entries in gt_e071_hot_refs that do not have a reference to either
"! cl_cts_hot_package or cl_cts_hot_object and which are not yet completely
"! processed (lockflag = 2) are skipped transport objects.
"! Log them in the log of their transport request and update lockflag in e071 to 3
FORM log_skipped_transport_objects USING it_skipped_api_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                                         it_skipped_api_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

  DATA lv_tmptext TYPE string.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_without_ref) WHERE cts_hot_object IS NOT BOUND AND cts_hot_package IS NOT BOUND AND lockflag = '2'
                                                                    GROUP BY ( tr_korr = lr_e071_without_ref->trkorr logfile = lr_e071_without_ref->logfile count = GROUP SIZE )
                                                                                                                                                                                                                REFERENCE INTO DATA(lr_e071_skipped_group).
    lv_tmptext = lr_e071_skipped_group->count. "convert to string because count is of type i with leading spaces which will be printed log in no_condense mode
    PERFORM protokoll USING '3' 'W' '523' 'X' lv_tmptext space space space. "Number of skipped transport objects
    PERFORM protokoll USING '3' 'W' '507' ' ' space  space space space. "empty line
    PERFORM protokoll USING '3' 'W' '514' ' ' space  space space space. "Following transport objects skipped

    LOOP AT GROUP lr_e071_skipped_group REFERENCE INTO DATA(lr_e071_skipped_group_member).
      lv_tmptext = |{ lr_e071_skipped_group_member->pgmid } { lr_e071_skipped_group_member->object } { lr_e071_skipped_group_member->obj_name } { lr_e071_skipped_group_member->lang }|.
      IF gv_external_call = abap_true AND lr_e071_skipped_group_member->pgmid = 'LIMU'.
        IF lr_e071_skipped_group_member->object = 'HOTP'.
          DATA(lr_package) = VALUE #( it_skipped_api_packages[ table_line->abap_hana_package_id = lr_e071_skipped_group_member->obj_name ] OPTIONAL ).
          IF lr_package IS BOUND.
            lv_tmptext = |{ lv_tmptext } --> { lr_package->hana_package_id }|.
          ENDIF.
        ELSEIF lr_e071_skipped_group_member->object = 'HOTO'.
          DATA(lr_object) = VALUE #( it_skipped_api_objects[ table_line->abap_hana_package_id = lr_e071_skipped_group_member->obj_name(40)
                                                             table_line->abap_hana_object_name_suffix = lr_e071_skipped_group_member->obj_name+40 ] OPTIONAL ).
          IF lr_object IS BOUND.
            lv_tmptext = |{ lv_tmptext } --> { lr_object->hana_object_name }.{ lr_object->hana_object_suffix } ({ lr_object->hana_package_id })|.
          ENDIF.
        ENDIF.
      ENDIF.
      PERFORM protokoll_longtext USING '3' 'W' '579' ' ' lv_tmptext. "  &1&2&3&4

      lr_e071_skipped_group_member->lockflag = '3'.
      IF gv_external_call = abap_false. "if not in API mode
        UPDATE e071 SET lockflag = '3' WHERE trkorr = lr_e071_skipped_group_member->trkorr AND as4pos = lr_e071_skipped_group_member->as4pos.
      ENDIF.
    ENDLOOP.

    PERFORM write_prot_no_condense_to_file USING lr_e071_skipped_group->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_hana_names_of_trans_objs                            *
*---------------------------------------------------------------------*
"! All entries in gt_e071_hot_refs that have a reference to either
"! cl_cts_hot_package or cl_cts_hot_object are logged with their
"! corresponding HANA name.
FORM log_hana_names_of_trans_objs.
  TYPES: BEGIN OF ty_trkorr_obj_name,
           trkorr     TYPE trkorr,
           trobj_name TYPE trobj_name,
         END OF ty_trkorr_obj_name,
         ty_trkorr_obj_names TYPE SORTED TABLE OF ty_trkorr_obj_name WITH UNIQUE KEY table_line. "to prevent duplicate logging of LANG HOTOs/HOTAs

  DATA: lv_tmptext              TYPE string,
        lt_header_logged        TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line,
        lt_lang_obj_name_logged TYPE ty_trkorr_obj_names. "to prevent duplicate logging of LANG HOTOs/HOTAs

*1. log all R3TRs with their HOTP and then their HOTOs
  "loop over content for each TR to log per log file but only objects that were imported successfully in previous step (lockflag = 2) or that were already deployed successfully (lockflag = 3).
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE ( pgmid = 'R3TR' OR pgmid = 'LANG' )
                                                                  AND object = 'HOTA' AND cts_hot_package IS BOUND
                                                                  AND ( lockflag = '2' OR lockflag = '3' )
                                                                GROUP BY ( tr_korr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile )
                                                                REFERENCE INTO DATA(lr_e071_hot_ref_group).
    " Use two loop at groups to get the R3TRs and LANGs ordered by hana_package_id
    LOOP AT GROUP lr_e071_hot_ref_group REFERENCE INTO DATA(lr_e071_hot_ref_group_ordered) GROUP BY ( hana_package_id = lr_e071_hot_ref_group_ordered->cts_hot_package->hana_package_id ) ASCENDING.
      LOOP AT GROUP lr_e071_hot_ref_group_ordered REFERENCE INTO DATA(lr_e071_hot_ref_group_member).
        IF ( lr_e071_hot_ref_group_member->pgmid = 'R3TR' OR lr_e071_hot_ref_group_member->pgmid = 'LANG' ) AND lr_e071_hot_ref_group_member->object = 'HOTA'.
          IF NOT line_exists( lt_header_logged[ table_line = lr_e071_hot_ref_group_member->trkorr ] ).
            PERFORM protokoll USING '4' ' ' '545' 'X' space space space space. "Mapping of transport object names ot HANA names
            INSERT lr_e071_hot_ref_group_member->trkorr INTO TABLE lt_header_logged.
          ENDIF.

          IF lr_e071_hot_ref_group_member->pgmid = 'LANG'.
            IF line_exists( lt_lang_obj_name_logged[ trkorr = lr_e071_hot_ref_group_member->trkorr trobj_name = lr_e071_hot_ref_group_member->obj_name ] ).
              CONTINUE. "continue loop with next HOTA because current LANG HOTA already logged for other language
            ELSE.
              INSERT VALUE #( trkorr = lr_e071_hot_ref_group_member->trkorr trobj_name = lr_e071_hot_ref_group_member->obj_name ) INTO TABLE lt_lang_obj_name_logged.
            ENDIF.
          ENDIF.

          lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }|.

          PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4

          "log all packages of R3TR HOTA (LANG HOTA has no HOTPs)
          IF lr_e071_hot_ref_group_member->pgmid = 'R3TR'.
            LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref2) WHERE trbat_order = lr_e071_hot_ref_group_member->trbat_order
                                                                             AND trkorr = lr_e071_hot_ref_group_member->trkorr
                                                                             AND pgmid = 'LIMU' AND object = 'HOTP' AND obj_name(40) = lr_e071_hot_ref_group_member->obj_name.
              lv_tmptext = |{ lr_e071_hot_ref2->pgmid } { lr_e071_hot_ref2->object } { lr_e071_hot_ref2->obj_name }| &
                           | --> { lr_e071_hot_ref2->cts_hot_package->hana_package_id }|.
              PERFORM protokoll_longtext USING '4' ' ' '531' ' ' lv_tmptext. "  &1&2&3&4
            ENDLOOP.
          ENDIF.

          "log all objects of R3TR/LANG HOTA
          LOOP AT gt_e071_hot_refs REFERENCE INTO lr_e071_hot_ref2 WHERE trbat_order = lr_e071_hot_ref_group_member->trbat_order
                                                                     AND trkorr = lr_e071_hot_ref_group_member->trkorr
                                                                     AND as4pos = lr_e071_hot_ref_group_member->as4pos
                                                                     AND ( pgmid = 'LIMU' OR pgmid = 'LANG' )
                                                                     AND object = 'HOTO' AND cts_hot_object IS BOUND
                                                                     AND obj_name(40) = lr_e071_hot_ref_group_member->obj_name.
            lv_tmptext = |{ lr_e071_hot_ref2->pgmid } { lr_e071_hot_ref2->object } { lr_e071_hot_ref2->obj_name }| &
                         | --> { lr_e071_hot_ref2->cts_hot_object->hana_object_name }.{ lr_e071_hot_ref2->cts_hot_object->hana_object_suffix } ({ lr_e071_hot_ref2->cts_hot_object->hana_package_id })|.
            PERFORM protokoll_longtext USING '4' ' ' '531' ' ' lv_tmptext. "  &1&2&3&4
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.

*2. log all HOTPs that are not part of a HOTA (LANG HOTP not needed as no LANG HOTP supported)
  "loop over content for each TR to log per log file but only objects that were imported successfully in previous step (lockflag = 2) or that were already deployed successfully (lockflag = 3).
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref_hotp) WHERE pgmid = 'LIMU' AND object = 'HOTP' AND cts_hot_package IS BOUND AND ( lockflag = '2' OR lockflag = '3' )
                                                                     GROUP BY ( tr_korr = lr_e071_hot_ref_hotp->trkorr logfile = lr_e071_hot_ref_hotp->logfile )
                                                                     REFERENCE INTO DATA(lr_e071_hot_ref_group_hotp).

    LOOP AT GROUP lr_e071_hot_ref_group_hotp REFERENCE INTO lr_e071_hot_ref_group_member.
      "Skip log of LIMU HOTPs if R3TR of these LIMUs is part of transport request as well (expanded R3TR)
      IF line_exists( gt_e071_hot_refs[ trbat_order = lr_e071_hot_ref_group_member->trbat_order trkorr = lr_e071_hot_ref_group_member->trkorr pgmid = 'R3TR' object = 'HOTA' obj_name = lr_e071_hot_ref_group_member->obj_name(40) ] ).
        CONTINUE.
      ENDIF.

      IF NOT line_exists( lt_header_logged[ table_line = lr_e071_hot_ref_group_member->trkorr ] ).
        PERFORM protokoll USING '4' ' ' '545' 'X' space space space space.
        INSERT lr_e071_hot_ref_group_member->trkorr INTO TABLE lt_header_logged.
      ENDIF.

      lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }| &
                   | --> { lr_e071_hot_ref_group_member->cts_hot_package->hana_package_id }|.
      PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
    ENDLOOP.
    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group_hotp->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.

*3. log all HOTOs that are not part of a HOTA
  "loop over content for each TR to log per log file but only objects that were imported successfully in previous step (lockflag = 2) or that were already deployed successfully (lockflag = 3).
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref_hoto) WHERE ( pgmid = 'LIMU' OR pgmid = 'LANG' )
                                                                           AND object = 'HOTO' AND cts_hot_object IS BOUND
                                                                           AND ( lockflag = '2' OR lockflag = '3' )
                                                                     GROUP BY ( tr_korr = lr_e071_hot_ref_hoto->trkorr logfile = lr_e071_hot_ref_hoto->logfile )
                                                                     REFERENCE INTO DATA(lr_e071_hot_ref_group_hoto).

    LOOP AT GROUP lr_e071_hot_ref_group_hoto REFERENCE INTO lr_e071_hot_ref_group_member.
      "Skip log of LIMU HOTOs if R3TR of these LIMUs is part of transport request as well (expanded R3TR/LANG)
      IF lr_e071_hot_ref_group_member->pgmid = 'LIMU' AND
          line_exists( gt_e071_hot_refs[ trbat_order = lr_e071_hot_ref_group_member->trbat_order trkorr = lr_e071_hot_ref_group_member->trkorr pgmid = 'R3TR' object = 'HOTA' obj_name = lr_e071_hot_ref_group_member->obj_name(40) ] ).
        CONTINUE.
      ELSEIF lr_e071_hot_ref_group_member->pgmid = 'LANG' AND
        line_exists( gt_e071_hot_refs[ trbat_order = lr_e071_hot_ref_group_member->trbat_order trkorr = lr_e071_hot_ref_group_member->trkorr pgmid = 'LANG' object = 'HOTA' obj_name = lr_e071_hot_ref_group_member->obj_name(40) ] )..
        CONTINUE.
      ENDIF.

      IF NOT line_exists( lt_header_logged[ table_line = lr_e071_hot_ref_group_member->trkorr ] ).
        PERFORM protokoll USING '4' ' ' '545' 'X' space space space space.
        INSERT lr_e071_hot_ref_group_member->trkorr INTO TABLE lt_header_logged.
      ENDIF.

      IF lr_e071_hot_ref_group_member->pgmid = 'LANG'.
        IF line_exists( lt_lang_obj_name_logged[ trkorr = lr_e071_hot_ref_group_member->trkorr trobj_name = lr_e071_hot_ref_group_member->obj_name ] ).
          CONTINUE. "continue loop with next HOTA because current LANG HOTA already logged for other language
        ELSE.
          INSERT VALUE #( trkorr = lr_e071_hot_ref_group_member->trkorr trobj_name = lr_e071_hot_ref_group_member->obj_name ) INTO TABLE lt_lang_obj_name_logged.
        ENDIF.
      ENDIF.

      lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }| &
                   | --> { lr_e071_hot_ref_group_member->cts_hot_object->hana_object_name }.{ lr_e071_hot_ref_group_member->cts_hot_object->hana_object_suffix } ({ lr_e071_hot_ref_group_member->cts_hot_object->hana_package_id })|.
      PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4

    ENDLOOP.
    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group_hoto->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.

ENDFORM.

"! Logs the results of package deployment. Filters for packages and their requests and only logs them
"! in requests where they are contained.
FORM log_package_results USING it_logfiles_packages TYPE tt_logfile
                               it_skipped_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                               it_skipped_packages_n TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                               it_created_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                               it_updated_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                               it_deleted_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                               it_failed_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages.

  DATA: lt_skipped_filtered   TYPE tt_dref_log_message_package,
        lt_skipped_filtered_n TYPE tt_dref_log_message_package,
        lt_created_filtered   TYPE tt_dref_log_message_package,
        lt_updated_filtered   TYPE tt_dref_log_message_package,
        lt_deleted_filtered   TYPE tt_dref_log_message_package,
        lt_failed_filtered    TYPE tt_dref_log_message_package.

  "create all package log lines per request
  LOOP AT it_logfiles_packages INTO DATA(lv_logfile).
    FREE: lt_skipped_filtered, lt_skipped_filtered_n, lt_created_filtered, lt_updated_filtered, lt_deleted_filtered, lt_failed_filtered.

    PERFORM filter_packages_for_logfile USING it_skipped_packages lv_logfile CHANGING lt_skipped_filtered.
    PERFORM filter_packages_for_logfile USING it_skipped_packages_n lv_logfile CHANGING lt_skipped_filtered_n.
    PERFORM filter_packages_for_logfile USING it_created_packages lv_logfile CHANGING lt_created_filtered.
    PERFORM filter_packages_for_logfile USING it_updated_packages lv_logfile CHANGING lt_updated_filtered.
    PERFORM filter_packages_for_logfile USING it_deleted_packages lv_logfile CHANGING lt_deleted_filtered.
    PERFORM filter_packages_for_logfile USING it_failed_packages lv_logfile CHANGING lt_failed_filtered.

**  Log summary of processed packages
    IF lt_skipped_filtered IS NOT INITIAL.
      PERFORM log_summary_line USING '3' ' ' '528' ' ' lt_skipped_filtered. "Number of skipped packages that are already active
      PERFORM log_processed_packages USING '4' space '529' lt_skipped_filtered lv_logfile. "Following packages were skipped because alrwady active
    ENDIF.

    IF lt_skipped_filtered_n IS NOT INITIAL.
      PERFORM log_summary_line USING '3' 'W' '625' ' ' lt_skipped_filtered_n. "Number of skipped packages that were skipped due to HOT_STATUS = 'N'
      PERFORM log_processed_packages USING '3' 'W' '627' lt_skipped_filtered_n lv_logfile. "Following packages were skipped due to HOT_STATUS = 'N'
    ENDIF.

    "Skip logging if all filtered tables are empty, meaning that no package was created/updated for the current request to be logged.
    IF lt_created_filtered IS INITIAL
          AND lt_updated_filtered IS INITIAL
          AND lt_deleted_filtered IS INITIAL
          AND lt_failed_filtered IS INITIAL.
      PERFORM write_prot_no_condense_to_file USING lv_logfile.
      CONTINUE.
    ENDIF.

    PERFORM log_summary_line USING '3' ' ' '520' ' ' lt_created_filtered. "Number of successfully created packages
    PERFORM log_summary_line USING '3' ' ' '521' ' ' lt_updated_filtered. "Number of successfully updated packages
    IF it_deleted_packages IS NOT INITIAL.   "Only print if there were deletions
      PERFORM log_summary_line USING '3' ' ' '522' ' ' lt_deleted_filtered. ""Number of successfully deleted packages
    ENDIF.
    IF lt_failed_filtered IS INITIAL.
      PERFORM log_summary_line USING '3' ' ' '524' ' ' lt_failed_filtered. "Number of failed packages
    ELSE.
      PERFORM log_summary_line USING '2' 'E' '524' ' ' lt_failed_filtered. "Number of failed packages
    ENDIF.

**  Log package details (names) for each category
    PERFORM log_processed_packages USING '4' space '506' lt_created_filtered lv_logfile. "Following packages were created successfully
    PERFORM log_processed_packages USING '4' space '512' lt_updated_filtered lv_logfile. "Following packages were updated successfully
    PERFORM log_processed_packages USING '4' space '513' lt_deleted_filtered lv_logfile. "Following packages were deleted successfully
    PERFORM log_processed_packages USING '2' 'E' '515' lt_failed_filtered lv_logfile. "Following packages had failures

    PERFORM write_prot_no_condense_to_file USING lv_logfile.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_package_results_deletion                             *
*---------------------------------------------------------------------*
"! Logs the results of package deletion. Filters for packages and their requests and only logs them
"! in requests where they are contained.
"! Seperate form in parallel to log_package_results because of special handling in case packages
"! got updated but only deletion should have happened...
FORM log_package_results_deletion USING it_logfiles_packages_to_be_del TYPE tt_logfile
                                        it_skipped_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                        it_skipped_packages_n TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                        it_created_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                        it_updated_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                        it_deleted_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                        it_failed_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages.

  DATA: lt_skipped_filtered   TYPE tt_dref_log_message_package,
        lt_skipped_filtered_n TYPE tt_dref_log_message_package,
        lt_created_filtered   TYPE tt_dref_log_message_package,
        lt_updated_filtered   TYPE tt_dref_log_message_package,
        lt_deleted_filtered   TYPE tt_dref_log_message_package,
        lt_failed_filtered    TYPE tt_dref_log_message_package.

  "create all package log lines per request
  LOOP AT it_logfiles_packages_to_be_del INTO DATA(lv_logfile).
    FREE: lt_skipped_filtered, lt_skipped_filtered_n, lt_created_filtered, lt_updated_filtered, lt_deleted_filtered, lt_failed_filtered.

    PERFORM filter_packages_for_logfile USING it_skipped_packages lv_logfile CHANGING lt_skipped_filtered.
    PERFORM filter_packages_for_logfile USING it_skipped_packages_n lv_logfile CHANGING lt_skipped_filtered_n.
    PERFORM filter_packages_for_logfile USING it_created_packages lv_logfile CHANGING lt_created_filtered.
    PERFORM filter_packages_for_logfile USING it_updated_packages lv_logfile CHANGING lt_updated_filtered.
    PERFORM filter_packages_for_logfile USING it_deleted_packages lv_logfile CHANGING lt_deleted_filtered.
    PERFORM filter_packages_for_logfile USING it_failed_packages lv_logfile CHANGING lt_failed_filtered.

    "Skip logging if all filtered tables are empty, meaning that no package was deleted for the current request to be logged.
    IF lt_skipped_filtered IS INITIAL
          AND lt_skipped_filtered_n IS INITIAL
          AND lt_created_filtered IS INITIAL
          AND lt_updated_filtered IS INITIAL
          AND lt_deleted_filtered IS INITIAL
          AND lt_failed_filtered IS INITIAL.
      PERFORM write_prot_no_condense_to_file USING lv_logfile.
      CONTINUE.
    ENDIF.

**  Log summary of processed packages
    PERFORM log_summary_line USING '3' ' ' '522' ' ' lt_deleted_filtered. ""Number of successfully deleted packages
    IF lt_failed_filtered IS INITIAL.
      PERFORM log_summary_line USING '3' ' ' '524' ' ' lt_failed_filtered. "Number of failed packages
    ELSE.
      PERFORM log_summary_line USING '2' 'E' '524' ' ' lt_failed_filtered. "Number of failed packages
    ENDIF.

    "Special handling if packages that were intended for deletion but finally were skipped/created/updated
    IF lt_skipped_filtered IS NOT INITIAL. "usually there should be no skipped packages during deletion. only in case of parallel imports?
      PERFORM log_summary_line USING '2' 'E' '519' ' ' lt_skipped_filtered. "Number of unexpected skipped packages
    ENDIF.

    IF lt_skipped_filtered_n IS NOT INITIAL. "usually there should be no skipped packages during deletion. only in case of parallel imports?
      PERFORM log_summary_line USING '2' 'E' '519' ' ' lt_skipped_filtered_n. "Number of unexpected skipped packages
    ENDIF.

    IF lt_created_filtered IS NOT INITIAL. "usually there should be no created packages during deletion. only in case of parallel imports?
      PERFORM log_summary_line USING '2' 'E' '517' ' ' lt_created_filtered. "Number of unexpected created packages


    ENDIF.
    IF lt_updated_filtered IS NOT INITIAL. "usually there should be no updated packages during deletion. only in case of parallel imports?
      PERFORM log_summary_line USING '2' 'E' '518' ' ' lt_updated_filtered. "Number of unexpected updated packages

    ENDIF.

**  Log packages for each category
    PERFORM log_processed_packages USING '4' space '513' lt_deleted_filtered lv_logfile. "Following packages were skipped unexpectedly
    PERFORM log_processed_packages USING '2' 'E' '515' lt_failed_filtered lv_logfile. "Following packages had failures

    IF lt_skipped_filtered IS NOT INITIAL.
      PERFORM log_processed_packages USING '2' 'E' '534' lt_skipped_filtered lv_logfile. "Following packages were skipped unexpectedly
    ENDIF.
    IF lt_skipped_filtered_n IS NOT INITIAL.
      PERFORM log_processed_packages USING '2' 'E' '534' lt_skipped_filtered_n lv_logfile. "Following packages were skipped unexpectedly
    ENDIF.
    IF lt_created_filtered IS NOT INITIAL.
      PERFORM log_processed_packages USING '2' 'E' '535' lt_created_filtered lv_logfile. "Following packages were created unexpectedly
    ENDIF.
    IF lt_updated_filtered IS NOT INITIAL.
      PERFORM log_processed_packages USING '2' 'E' '536' lt_updated_filtered lv_logfile. "Following packages were updated unexpectedly
    ENDIF.

    PERFORM write_prot_no_condense_to_file USING lv_logfile.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM filter_packages_for_logfile                              *
*---------------------------------------------------------------------*
"! Returns in ct_packages only those packages of it_packages that should be logged to logfile in iv_logfile.
FORM filter_packages_for_logfile USING    it_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                                          iv_logfile  TYPE trfile
                                 CHANGING ct_packages TYPE tt_dref_log_message_package.

  LOOP AT it_packages REFERENCE INTO DATA(lr_package).
    "Performance??? Idee: secondary key über logfile, cts_hot_package?
    IF line_exists( gt_e071_hot_refs[ logfile = iv_logfile pgmid = 'LIMU' object = 'HOTP' obj_name = lr_package->cts_hot_package->abap_hana_package_id ] ).
      APPEND lr_package TO ct_packages.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM filter_objects_for_logfile                               *
*---------------------------------------------------------------------*
"! Inserts into ct_objects only those objects of it_objects that should
"! be logged to logfile of tr in iv_trkorr.
"! iv_pgmid either LANG or LIMU
FORM filter_objects_for_logfile USING    iv_pgmid   TYPE pgmid
                                         it_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                         iv_trkorr  TYPE trkorr
                                CHANGING ct_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.
  "ct_objects must not be cleared here...
  LOOP AT it_objects INTO DATA(lr_object).
    "Performance??? Idee: secondary key über logfile, cts_hot_package?
    IF line_exists( gt_e071_hot_refs[ trkorr = iv_trkorr pgmid = iv_pgmid object = 'HOTO' obj_name = lr_object->transport_object_name ] ).
      INSERT lr_object INTO TABLE ct_objects.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_processed_packages                                   *
*---------------------------------------------------------------------*
"! Logs the passed packages in it_packages on level 4.<br/>
"! First a line as new object (uline) with passed i_level_i_severity and
"! i_msgnr, is written and then all the HANA package names are written
"! in single lines (if hana package name is too long it is printed in
"! 2 or more log lines)
"! If a package is contained in several requests than the request last in
"! the queue is added as info to see which version was deployed.
FORM log_processed_packages USING i_level
                                  i_severity
                                  i_msgnr
                                  it_log_message_packages TYPE tt_dref_log_message_package
                                  iv_logfile  TYPE trfile.

  DATA: lv_tmptext TYPE string,
        lv_cnt     TYPE i,
        lv_msg_nr  TYPE string VALUE '531',
        lv_akh     TYPE ufps_posid.

  IF it_log_message_packages IS NOT INITIAL.
    PERFORM protokoll USING i_level i_severity '507' ' ' space space space space. "empty line
    PERFORM protokoll USING i_level i_severity i_msgnr ' ' space space space space.

    LOOP AT it_log_message_packages INTO DATA(lr_log_message_package).
      lv_tmptext = lr_log_message_package->cts_hot_package->hana_package_id. "by default only package name is logged
      LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE pgmid = 'LIMU' AND object = 'HOTP' AND cts_hot_package = lr_log_message_package->cts_hot_package GROUP BY ( cts_hot_package = lr_e071_hot_ref->cts_hot_package count = GROUP SIZE )
                                                                                                                                                                                                                  REFERENCE INTO DATA(lr_e071_hot_ref_group).
        "overwrite tmptext in case several requests contain the same package and then use last request in queue and add to log
        IF lr_e071_hot_ref_group->count > 1.
          lv_cnt = 0.
          LOOP AT GROUP lr_e071_hot_ref_group INTO DATA(ls_e071_hot_ref_group_member).
            lv_cnt = lv_cnt + 1.
            "only print last group member (last trkorr with this package) maybe there is a better way to access last group member? but print only when TR is different than current TR (current logfile)
            IF lv_cnt = lr_e071_hot_ref_group->count AND ls_e071_hot_ref_group_member-logfile <> iv_logfile.
              lv_msg_nr = '537'.
              lv_tmptext = |{ lv_tmptext } - version of { ls_e071_hot_ref_group_member-trkorr }|.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      IF i_level = '4' AND i_severity = space.
        PERFORM protokoll_longtext USING '4' space lv_msg_nr ' ' lv_tmptext. "package name in tmptext could be 256 chars

        "print also details if available
        IF lr_log_message_package->message IS NOT INITIAL AND lr_log_message_package->message <> 'No error'.
          IF lr_log_message_package->is_hana_message = 'X'.
            lv_tmptext = |{ lr_log_message_package->error_code }: { lr_log_message_package->message }|.
            PERFORM protokoll_longtext USING '4' ' ' '532' ' ' lv_tmptext.
          ELSE.
            PERFORM protokoll_longtext USING '4' ' ' '533' ' ' lr_log_message_package->message.
          ENDIF.
        ENDIF.
      ELSE.
        IF i_severity = 'E'. "add AKH if package is printed as error
          lv_akh = gr_external_persistency->get_akh_for_package( lr_log_message_package->cts_hot_package->abap_hana_package_id ).
          IF lv_akh IS NOT INITIAL.
            lv_tmptext = |{ lv_tmptext } [{ lv_akh }]|.
          ENDIF.
        ENDIF.

        PERFORM protokoll_longtext USING i_level i_severity lv_msg_nr ' ' lv_tmptext. "package name in tmptext could be 256 chars

        "in case of errors also print the message, either with hana error_code or without
        IF lr_log_message_package->message IS NOT INITIAL AND lr_log_message_package->message <> 'No error'.
          IF lr_log_message_package->is_hana_message = 'X'.
            lv_tmptext = |{ lr_log_message_package->error_code }: { lr_log_message_package->message }|.
            PERFORM protokoll_longtext USING i_level i_severity '532' ' ' lv_tmptext.
          ELSE.
            PERFORM protokoll_longtext USING i_level i_severity '533' ' ' lr_log_message_package->message.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_summary_line                                         *
*---------------------------------------------------------------------*
"! Logs a summary line for passed values.<br/>
"! E.g.: 'Updated packages &1'
FORM log_summary_line USING i_level
                            i_severity
                            i_msgnr
                            i_newobj
                            it_sometable TYPE ANY TABLE.

  DATA lv_tmptext TYPE string. "must be string casted because if integer is used the number is printed with leading spaces when writing log with no_condense
  lv_tmptext = |{ lines( it_sometable ) }|.
  PERFORM protokoll USING i_level i_severity i_msgnr i_newobj lv_tmptext space space space.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_headerline_with_timestamp                            *
*---------------------------------------------------------------------*
"! Writes a log entry on passed i_level for passed i_msgnr and uses currrent timestamp as variable 1 in i_msgnr.
"! If i_newobj is set, it also gets a new section in the log (uline before this line)
FORM log_headerline_with_timestamp USING i_level
                                         i_msgnr
                                         i_newobj.

  DATA(lv_timestamp) = |{ cl_cts_hot_utility=>get_formatted_timestamp( ) } (UTC)|.
  PERFORM protokoll USING i_level ' ' i_msgnr i_newobj lv_timestamp space space space.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_processed_objects                                    *
*---------------------------------------------------------------------*
"! Logs the passed objects in it_objects on level 4.<br/>
"! First a line as new object (uline) with passed i_msgnr is written and
"! then all the HANA object names (format: name.suffix (package)) are
"! written in single lines (if hana object name is too long it is printed
"! in 2 or more log lines)
"! iv_current_trkorr indicates the trkorr of current process log
FORM log_processed_objects USING i_msgnr
                                 iv_current_trkorr TYPE trkorr
                                 it_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

  DATA: lv_tmptext               TYPE string,
        lv_last_trkorr_of_object TYPE trkorr.
  IF lines( it_objects ) > 0.
    IF i_msgnr = '628'. "Following objects not deployed because synchronized needs empty line on level 3 as warning
      PERFORM protokoll USING '3' 'W' '507' ' '  space space space space. "empty line
      PERFORM protokoll USING '3' 'W' i_msgnr ' '  space space space space.
    ELSE.
      PERFORM protokoll USING '4' ' ' '507' ' '  space space space space. "empty line
      PERFORM protokoll USING '4' ' ' i_msgnr ' '  space space space space.
    ENDIF.
    LOOP AT it_objects INTO DATA(lo_current_cts_hot_object).
      lv_tmptext = |{ lo_current_cts_hot_object->hana_object_name }.{ lo_current_cts_hot_object->hana_object_suffix } ({ lo_current_cts_hot_object->hana_package_id })|.
      IF i_msgnr = '556'. "Following objects not deployed because already active (needs other indent then all other outputs) and must not show any version as we do not know which one was deployed
        PERFORM protokoll_longtext USING '4' ' ' '531' ' ' lv_tmptext.
      ELSEIF i_msgnr = '628'. "Following objects not deployed because synchronized (needs other indent then all other outputs) and should be printed as warning on level 3
        PERFORM protokoll_longtext USING '3' 'W' '531' ' ' lv_tmptext.
      ELSE.
        PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_trkorr_of_object.
        IF lv_last_trkorr_of_object <> iv_current_trkorr.
          lv_tmptext = |  { lv_tmptext } - version of { lv_last_trkorr_of_object }|. "other indent for 537
          PERFORM protokoll_longtext USING '4' ' ' '537' ' ' lv_tmptext.
        ELSE.
          PERFORM protokoll_longtext USING '4' ' ' '533' ' ' lv_tmptext.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_cancelled_objects                                    *
*---------------------------------------------------------------------*
"! Logs the passed objects in it_all_cancelled_objects if they are
"! part of current request (it_objects_ot_request) on level 2 as error
"! as cancelled object.<br/>
"! All logged objects are also inserted to ct_already_logged.<br/>
"! HANA object names (format: name.suffix (package)) are
"! written in single lines (if hana object name is too long it is printed
"! in 2 or more log lines)
FORM log_cancelled_objects USING    it_all_cancelled_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                    it_objects_ot_request TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                    iv_current_trkorr TYPE trkorr
                           CHANGING ct_already_logged TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                    ct_failed_ordered TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.
  DATA: lv_tmptext               TYPE string,
        lv_last_trkorr_of_object TYPE trkorr,
        lv_akh                   TYPE ufps_posid.

  LOOP AT it_all_cancelled_objects INTO DATA(lr_hot_object).
    IF line_exists( it_objects_ot_request[ table_line = lr_hot_object ] ).
      lv_tmptext = |{ lr_hot_object->hana_object_name }.{ lr_hot_object->hana_object_suffix } ({ lr_hot_object->hana_package_id }) cancelled|.

      PERFORM find_last_request_for_object USING lr_hot_object CHANGING lv_last_trkorr_of_object.
      IF lv_last_trkorr_of_object <> iv_current_trkorr.
        lv_tmptext = |{ lv_tmptext } - version of { lv_last_trkorr_of_object }|.
      ENDIF.
      lv_akh = gr_external_persistency->get_akh_for_package( lr_hot_object->abap_hana_package_id ).
      IF lv_akh IS NOT INITIAL.
        lv_tmptext = |{ lv_tmptext } [{ lv_akh }]|.
      ENDIF.

      PERFORM protokoll_longtext USING '2' 'E' '533' ' ' lv_tmptext.

      INSERT lr_hot_object INTO TABLE ct_already_logged.
      APPEND lr_hot_object TO ct_failed_ordered.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_failed_object_summary                                        *
*---------------------------------------------------------------------*
"! Logs summary section for failed objects.
"! If it_failed_objects has entries an introduction line is printed
"! with iv_msgnr as new section.
"! Then all objects of it_failed_objects are printed with or without
"! hint to another version of the object if another version of the
"! object caused the error.
"! iv_trkorr is the current trkorr the log is written for.
FORM log_failed_object_summary USING iv_msgnr
                                     it_failed_objects TYPE ANY TABLE
                                     iv_trkorr TYPE trkorr.

  DATA: lo_failed_cts_hot_object TYPE REF TO cl_cts_hot_object_v1,
        lv_log_text              TYPE string,
        lv_last_tr_of_object     TYPE trkorr,
        lv_akh                   TYPE ufps_posid.

  IF lines( it_failed_objects ) > 0.
    PERFORM protokoll USING '2' 'E ' iv_msgnr 'X' space space space space. "summary of activation errors or import errors

    LOOP AT it_failed_objects INTO lo_failed_cts_hot_object.
      PERFORM find_last_request_for_object USING lo_failed_cts_hot_object CHANGING lv_last_tr_of_object.

      lv_log_text = |{ lo_failed_cts_hot_object->hana_object_name }.{ lo_failed_cts_hot_object->hana_object_suffix } ({ lo_failed_cts_hot_object->hana_package_id })|.
      IF lv_last_tr_of_object <> iv_trkorr.
        lv_log_text = |{ lv_log_text } - version of { lv_last_tr_of_object }|.
      ENDIF.
      lv_akh = gr_external_persistency->get_akh_for_package( lo_failed_cts_hot_object->abap_hana_package_id ).
      IF lv_akh IS NOT INITIAL.
        lv_log_text = |{ lv_log_text } [{ lv_akh }]|.
      ENDIF.
      PERFORM protokoll_longtext USING '2' 'E' '531' ' ' lv_log_text.
    ENDLOOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_object_header                                        *
*---------------------------------------------------------------------*
"! Logs object header with passed values in message 531 or 537<br/>
"! HANA object names format: 'name.suffix (package) &lt;text&gt;' is splitted
"! into several loglines if necessary (if hana object name is too long
"! it is printed in 2 or more log lines)<br/>
"! i_object_part_of_log indicates whether this object is part of this log(trkorr) or not.<br/>
"! i_last_tr_of_object indicates the last trkorr of the object (queue order) or
"!                     initial if object is unknown or if external deployment (e.g. by API/SNOTE/SCWB)<br/>
"! i_trkorr is the trkorr of current logfile or initial if external deployment (e.g. by API/SNOTE/SCWB)<br/>
"! i_last_attempt_of_object indicates whether this object was last processed by
"!                          current logged activation attempt or whether it was
"!                          tried again later. If it is tried again in another
"!                          activation call, print this info as different message<br/>
"! i_object_success  indicates whether object was a successful object or a failed object in current activation attempt<br/>
"! i_object_deletion indicates whether object was an object that was deleted (tried to be deleted)
FORM log_object_header USING i_cts_hot_object TYPE REF TO cl_cts_hot_object_v1
                             i_object_part_of_log TYPE abap_bool
                             i_last_tr_of_object TYPE trkorr
                             i_trkorr TYPE trkorr
                             i_last_attempt_of_obj TYPE abap_bool
                             i_object_success TYPE abap_bool
                             i_object_deletion TYPE abap_bool.

  DATA: lv_log_text TYPE string,
        lv_akh      TYPE ufps_posid.

  lv_log_text = |{ i_cts_hot_object->hana_object_name }.{ i_cts_hot_object->hana_object_suffix } ({ i_cts_hot_object->hana_package_id })|.

  IF i_object_part_of_log = abap_true.
    IF i_last_attempt_of_obj = abap_true.
      IF i_object_success = abap_true.
        IF i_object_deletion = abap_true.
          lv_log_text = |{ lv_log_text } deleted successfully|.
        ELSE.
          lv_log_text = |{ lv_log_text } activated successfully|.
        ENDIF.
        IF i_last_tr_of_object <> i_trkorr.
          lv_log_text = |{ lv_log_text } - version of { i_last_tr_of_object }|.
          PERFORM protokoll_longtext USING '3' ' ' '537' ' ' lv_log_text. "newobj=' ' because no details are logged for these objects
        ELSE.
          PERFORM protokoll_longtext USING '3' ' ' '531' 'X' lv_log_text.
        ENDIF.
      ELSE. "failed object
        IF i_object_deletion = abap_true.
          lv_log_text = |{ lv_log_text } deletion failed|.
        ELSE.
          lv_log_text = |{ lv_log_text } activation failed|.
        ENDIF.

        lv_akh = gr_external_persistency->get_akh_for_package( i_cts_hot_object->abap_hana_package_id ).
        IF lv_akh IS NOT INITIAL.
          lv_log_text = |{ lv_log_text } [{ lv_akh }]|.
        ENDIF.
        IF i_last_tr_of_object <> i_trkorr.
          lv_log_text = |{ lv_log_text } - version of { i_last_tr_of_object }|.
          PERFORM protokoll_longtext USING '2' 'E' '537' 'X' lv_log_text.
        ELSE.
          PERFORM protokoll_longtext USING '2' 'E' '546' 'X' lv_log_text.
        ENDIF.
      ENDIF.
    ELSE. "not last attempt of object
      IF i_object_success = abap_false. "e.g. in case of ok_objects_only all objects are retried in next attempt, even failed objects
        lv_log_text = |{ lv_log_text } failed, will be retried later|.
        PERFORM protokoll_longtext USING '4' 'W' '549' ' ' lv_log_text.
      ELSE.
        lv_log_text = |{ lv_log_text } would be OK, see next attempt|.
        PERFORM protokoll_longtext USING '4' ' ' '540' ' ' lv_log_text.
      ENDIF.
    ENDIF.
  ELSE. "object is not part of this log/tr
    IF i_last_tr_of_object IS INITIAL. "initial means, object was not part of import at all
      "could be due to implicitly added to activation list because of dependencies (A->B->C) and A and C are to be activated
      "or in case an activation plugin failed to activate content and was returned as error itself.
      IF i_object_success = abap_true.
        lv_log_text = |{ lv_log_text } activated successfully - implicitly added object (see longtext)|.
        PERFORM protokoll_longtext USING '4' ' ' '547' ' ' lv_log_text. "only log as level 4 to keep HANA order
      ELSE.
        lv_log_text = |{ lv_log_text } activation failed - implicitly added object (see longtext)|.
        PERFORM protokoll_longtext USING '3' 'W' '547' ' ' lv_log_text.
      ENDIF.
    ELSE. "if ok or failed object from other tr, only log as level 4 to keep HANA order
      lv_log_text = |{ lv_log_text } - from request { i_last_tr_of_object }|.
      PERFORM protokoll_longtext USING '4' ' ' '538' ' ' lv_log_text.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_object_header_regeneration                           *
*---------------------------------------------------------------------*
"! Logs object header with passed values in message 543 or 544<br/>
"! HANA object names format: 'name.suffix (package) &lt;text&gt;' is splitted
"! into several loglines if necessary (if hana object name is too long
"! it is printed in 2 or more log lines)<br/>
"! i_cts_hot_object the object to be logged as header<br/>
"! i_object_part_of_import indicates whether this object is part of the overall import or not<br/>
"! i_last_tr_of_object If object is part of import, then the last request of the object but only if not current request. There more logging is available<br/>
"! i_activated_successfully If object is part of import, it's status:
"!                           <ul><li>-1 indicates that the object was activated successfully BEFORE the activation leading to the logging of this regeneration</li>
"!                           <li>0 indicates that the object was NOT activated successfully during overall deployment</li>
"!                           <li>1 indicates that the object was activated successfully AFTER the activation leading to the logging of this regeneration</li></ul>
FORM log_object_header_regeneration USING i_cts_hot_object TYPE REF TO cl_cts_hot_object_v1
                                          i_object_part_of_import TYPE abap_bool
                                          i_last_tr_of_object TYPE trkorr
                                          i_activated_successfully TYPE i
                                          i_hana_severity TYPE string.

  DATA(lv_log_text) = |{ i_cts_hot_object->hana_object_name }.{ i_cts_hot_object->hana_object_suffix } ({ i_cts_hot_object->hana_package_id })|.

  IF i_hana_severity = '2' OR i_hana_severity = '3'.
    lv_log_text = |{ lv_log_text } regeneration failed - see longtext|.

    IF i_object_part_of_import = abap_true.
      IF i_last_tr_of_object IS NOT INITIAL.
        lv_log_text = |{ lv_log_text } - version of { i_last_tr_of_object }|.
      ENDIF.
      CASE i_activated_successfully.
        WHEN -1.
          PERFORM protokoll_longtext USING '3' 'W' '615' 'X' lv_log_text. "&1&2&3&4
        WHEN 0.
          PERFORM protokoll_longtext USING '3' 'W' '543' 'X' lv_log_text. "&1&2&3&4
        WHEN 1.
          PERFORM protokoll_longtext USING '4' ' ' '616' 'X' lv_log_text. "&1&2&3&4
        WHEN OTHERS.
          PERFORM protokoll_longtext USING '3' 'W' '543' 'X' lv_log_text. "&1&2&3&4
      ENDCASE.
    ELSE.
      PERFORM protokoll_longtext USING '3' 'W' '544' 'X' lv_log_text. "&1&2&3&4
    ENDIF.
  ELSE.
    lv_log_text = |{ lv_log_text } regenerated successfully|.
    PERFORM protokoll_longtext USING '4' ' ' '531' ' ' lv_log_text. "&1&2&3&4
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM update_e071_hotp                                         *
*---------------------------------------------------------------------*
"! Updates lockflag to '3' in e071 for passed packages part of passed transport requests (but only LIMU HOTP) <br/>
"! Changes it_e071_resolved_hota and sets lockflag for successfull hotp entries.
FORM update_e071_hotp USING    it_packages      TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages
                      CHANGING ct_e071_hot_refs TYPE tt_e071_hot_refs.

  "TRINT_E071_LOCKFLAG_WRITE?
  LOOP AT it_packages REFERENCE INTO DATA(dr_package).
    LOOP AT ct_e071_hot_refs REFERENCE INTO DATA(dr_e071_hot_ref) WHERE obj_name = dr_package->cts_hot_package->abap_hana_package_id AND pgmid = 'LIMU' AND object = 'HOTP' AND lockflag = '2'.
      dr_e071_hot_ref->lockflag = '3'.

      "update lockflag in table e071 only if it is an import/SP update/upgrade and if it is NOT a resolved limu hotp out of r3tr hota
      IF gv_external_call = abap_false AND NOT line_exists( ct_e071_hot_refs[ trkorr = dr_e071_hot_ref->trkorr as4pos = dr_e071_hot_ref->as4pos pgmid = 'R3TR' object = 'HOTA' ] ).
        UPDATE e071 SET lockflag = '3' WHERE trkorr = dr_e071_hot_ref->trkorr AND as4pos = dr_e071_hot_ref->as4pos.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM update_e071_hoto                                         *
*---------------------------------------------------------------------*
"! Updates lockflag to '3' in e071 for passed objects part of passed transport requests (but only LIMU HOTO)
FORM update_e071_hoto USING    it_objects       TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                      CHANGING ct_e071_hot_refs TYPE tt_e071_hot_refs.

  "TRINT_E071_LOCKFLAG_WRITE?
  DATA obj_name TYPE e071-obj_name.
  LOOP AT it_objects INTO DATA(lo_object).
    obj_name = lo_object->abap_hana_package_id.
    obj_name+40 = lo_object->abap_hana_object_name_suffix.
    LOOP AT ct_e071_hot_refs REFERENCE INTO DATA(dr_e071_hot_ref) WHERE obj_name = obj_name AND pgmid = 'LIMU' AND object = 'HOTO' AND lockflag = '2'.
      dr_e071_hot_ref->lockflag = '3'.

      "update lockflag in table e071 only if it is an import/SP update/upgrade and if it is NOT a resolved limu hoto out of r3tr hota
      IF gv_external_call = abap_false AND NOT line_exists( ct_e071_hot_refs[ trkorr = dr_e071_hot_ref->trkorr as4pos = dr_e071_hot_ref->as4pos pgmid = 'R3TR' object = 'HOTA' ] ).
        UPDATE e071 SET lockflag = '3' WHERE trkorr = dr_e071_hot_ref->trkorr AND as4pos = dr_e071_hot_ref->as4pos.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM update_e071_lang_hoto                                         *
*---------------------------------------------------------------------*
"! Updates lockflag to '3' in e071 for passed objects part of passed transport requests (but only LANG HOTO)
FORM update_e071_lang_hoto USING    it_objects       TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results
                           CHANGING ct_e071_hot_refs TYPE tt_e071_hot_refs.

  "TRINT_E071_LOCKFLAG_WRITE?
  DATA obj_name TYPE e071-obj_name.
  LOOP AT it_objects REFERENCE INTO DATA(lr_object).
    obj_name = lr_object->cts_hot_object->abap_hana_package_id.
    obj_name+40 = lr_object->cts_hot_object->abap_hana_object_name_suffix.

    "if abap_lang is inital all languages were deployed successfully
    IF lr_object->abap_lang IS INITIAL.
      "update all languages of object with lockflag 3.
      LOOP AT ct_e071_hot_refs REFERENCE INTO DATA(dr_e071_hot_ref) WHERE obj_name = obj_name AND pgmid = 'LANG' AND object = 'HOTO' AND lockflag = '2'.
        dr_e071_hot_ref->lockflag = '3'.

        "update lockflag in table e071 only if it is an import/SP update/upgrade and if it is NOT a resolved lang hoto out of r3tr hota
        IF gv_external_call = abap_false AND NOT line_exists( ct_e071_hot_refs[ trkorr = dr_e071_hot_ref->trkorr as4pos = dr_e071_hot_ref->as4pos pgmid = 'LANG' object = 'HOTA' ] ).
          UPDATE e071 SET lockflag = '3' WHERE trkorr = dr_e071_hot_ref->trkorr AND as4pos = dr_e071_hot_ref->as4pos.
        ENDIF.
      ENDLOOP.
    ELSE.
      "update only successfully deployed languages of object with lockflag 3.
      LOOP AT ct_e071_hot_refs REFERENCE INTO dr_e071_hot_ref WHERE obj_name = obj_name AND pgmid = 'LANG' AND object = 'HOTO' AND lockflag = '2' AND lang = lr_object->abap_lang.
        dr_e071_hot_ref->lockflag = '3'.

        "update lockflag in table e071 only if it is an import/SP update/upgrade and if it is NOT a resolved lang hoto out of lang hota
        IF gv_external_call = abap_false AND NOT line_exists( ct_e071_hot_refs[ trkorr = dr_e071_hot_ref->trkorr as4pos = dr_e071_hot_ref->as4pos pgmid = 'LANG' object = 'HOTA' lang = dr_e071_hot_ref->lang ] ).
          UPDATE e071 SET lockflag = '3' WHERE trkorr = dr_e071_hot_ref->trkorr AND as4pos = dr_e071_hot_ref->as4pos.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM update_e071_hota                                         *
*---------------------------------------------------------------------*
"! Updates lockflag to '3' in e071 for all successfull hota objects.<br/>
"! (LIMU HOTP and all LIMU HOTOs for a request have lockflag = '3' in
"! passed table ct_e071_hot_refs.<br/>
FORM update_e071_hota CHANGING ct_e071_hot_refs TYPE tt_e071_hot_refs.

  DATA(successfull_hota) = abap_true.
  LOOP AT ct_e071_hot_refs REFERENCE INTO DATA(dr_e071_hot_ref_hota) WHERE ( pgmid = 'R3TR' OR pgmid = 'LANG' ) AND object = 'HOTA' AND lockflag = '2'.
    successfull_hota = abap_true.
    LOOP AT ct_e071_hot_refs REFERENCE INTO DATA(dr_e071_hot_ref_entry) WHERE trkorr = dr_e071_hot_ref_hota->trkorr AND as4pos = dr_e071_hot_ref_hota->as4pos
                                                                            AND ( pgmid = 'LIMU' OR pgmid = 'LANG' ) AND object <> 'HOTA'
                                                                            AND obj_name(40) = dr_e071_hot_ref_hota->obj_name.
      IF dr_e071_hot_ref_entry->lockflag <> '3'.
        successfull_hota = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF successfull_hota = abap_true.
      dr_e071_hot_ref_hota->lockflag = '3'.
      "update lockflag in table e071 only if it is an import/SP update/upgrade
      IF gv_external_call = abap_false.
        UPDATE e071 SET lockflag = '3' WHERE trkorr = dr_e071_hot_ref_hota->trkorr AND as4pos = dr_e071_hot_ref_hota->as4pos.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PROTOKOLL                                                *
*---------------------------------------------------------------------*
"X bei newobj - heßt oben drüber gibt es einen schwarzen Strich
FORM protokoll USING p_level
                     p_severity
                     p_msgnr
                     p_newobj
                     p_var1
                     p_var2
                     p_var3
                     p_var4.
  PERFORM protokoll_with_ag USING
                     p_level
                     p_severity
                     gc_ag
                     p_msgnr
                     p_newobj
                     p_var1
                     p_var2
                     p_var3
                     p_var4.
ENDFORM.                    "protokoll

*---------------------------------------------------------------------*
*       FORM PROTOKOLL_WITH_AG                                        *
*---------------------------------------------------------------------*
FORM protokoll_with_ag USING
                     p_level
                     p_severity
                     p_ag
                     p_msgnr
                     p_newobj
                     p_var1
                     p_var2
                     p_var3
                     p_var4.
  DATA: ls_xmsg  TYPE sprot_u.
  ls_xmsg-level       = p_level     .
  ls_xmsg-severity    = p_severity  .
  ls_xmsg-langu       = gc_langu       .
  ls_xmsg-ag          = p_ag        .
  ls_xmsg-msgnr       = p_msgnr     .
  ls_xmsg-newobj      = p_newobj    .
  ls_xmsg-var1        = p_var1      .
  ls_xmsg-var2        = p_var2      .
  ls_xmsg-var3        = p_var3      .
  ls_xmsg-var4        = p_var4      .
  APPEND ls_xmsg TO gt_xmsg.


  "Sets value gv_max_severity of i_severity is higher than gv_max_severity.
  "'A' greater than 'E' greater than 'W' greater than space

  "better algorithm?
  IF gv_max_severity = 'A'.
    "nothing to do, max already reached
  ELSEIF gv_max_severity = 'E'.
    IF p_severity = 'A'.
      gv_max_severity = 'A'.
    ENDIF.
  ELSEIF gv_max_severity = 'W'.
    IF p_severity = 'A' OR p_severity = 'E'.
      gv_max_severity = p_severity.
    ENDIF.
  ELSEIF gv_max_severity = space OR gv_max_severity = 'I'.
    IF p_severity <> space AND p_severity <> 'I'.
      gv_max_severity = p_severity.
    ENDIF.
  ENDIF.
ENDFORM.                    "protokoll_with_ag

*-----------------------------------------------------------------------*
*       FORM PROTOKOLL_LONGTEXT                                         *
*-----------------------------------------------------------------------*
"! Form to print long text into protokoll.<br/>
"! Passing a long text as p_var1 to be splitted into parts that fit
"! into protokoll even spanning several lines.<br/>
"! BUT: all 4 variables will be filled with data from this long text!<br/>
"! So ONLY to be used for texts with &1&2&3&4
FORM protokoll_longtext USING
                     p_level
                     p_severity
                     p_msgnr
                     p_newobj
                     p_var1.

  lcl_rddhanadeployment_helper=>split_message(
                  EXPORTING message = p_var1
                  IMPORTING et_split_message = DATA(lt_split_message)
  ).

  LOOP AT lt_split_message REFERENCE INTO DATA(lr_split_message).
    "create new line seperator only for first line
    IF sy-tabix = 1.
      PERFORM protokoll USING p_level p_severity p_msgnr p_newobj
                              lr_split_message->var1 lr_split_message->var2
                              lr_split_message->var3 lr_split_message->var4.
    ELSE.
      PERFORM protokoll USING p_level p_severity p_msgnr space
                              lr_split_message->var1 lr_split_message->var2
                              lr_split_message->var3 lr_split_message->var4.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "protokoll_with_ag


*---------------------------------------------------------------------*
*       FORM WRITE_PROT                                               *
*---------------------------------------------------------------------*
"! writes protocol to ALL log files!
FORM write_prot_to_all_logfiles.

  IF gv_external_call = abap_true OR gt_xmsg IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) GROUP BY ( key1 = lr_e071_hot_ref->trkorr ).
    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'FILE'     "Protokolltyp 'FILE', 'DB', 'MEMORY'
        iv_logname_file   = lr_e071_hot_ref->logfile     "Name des Protokolls (File)
*       lv_logname_db     =      "Name des Protokolls (Datenbank)
*       iv_logname_memory =      "Name des Protokolls (Memory)
*       iv_append_mode    = ' '     "Memory-Protokoll Anfügen der Meldungen
*       iv_condense       = 'X'     "Zeilen werden zusammengeschoben
      TABLES
        it_msgs           = gt_xmsg    "Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.
  ENDLOOP.

  CLEAR gt_xmsg.

ENDFORM.                    "write_prot

*---------------------------------------------------------------------*
*       FORM write_prot_to_files                                      *
*---------------------------------------------------------------------*
FORM write_prot_to_files USING it_logfiles TYPE tt_logfile.

  IF gv_external_call = abap_true OR gt_xmsg IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT it_logfiles INTO DATA(lv_logfile).
    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'FILE'     "Protokolltyp 'FILE', 'DB', 'MEMORY'
        iv_logname_file   = lv_logfile     "Name des Protokolls (File)
      TABLES
        it_msgs           = gt_xmsg    "Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.
  ENDLOOP.

  CLEAR gt_xmsg.

ENDFORM.                    "write_prot_to_files

*---------------------------------------------------------------------*
*       FORM write_prot_no_condense_to_file                           *
*                                                                     *
* Writes protocol to specified file but does not condense spaces      *
*---------------------------------------------------------------------*
"! Writes protocol to specified file and does not condense spaces.
FORM write_prot_no_condense_to_file USING iv_logfile TYPE trfile.

  IF gv_external_call = abap_true OR gt_xmsg IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'TR_WRITE_LOG'
    EXPORTING
      iv_log_type       = 'FILE'     "Protokolltyp 'FILE', 'DB', 'MEMORY'
      iv_logname_file   = iv_logfile     "Name des Protokolls (File)
      iv_condense       = ' '     "Zeilen werden zusammengeschoben
    TABLES
      it_msgs           = gt_xmsg    "Tabelle mit den Meldungen
    EXCEPTIONS
      invalid_input     = 1
      file_access_error = 2
      db_access_error   = 3
      OTHERS            = 4.

  CLEAR gt_xmsg.

ENDFORM.                    "write_prot

*---------------------------------------------------------------------*
*       FORM write_prot_to_file                                       *
*                                                                     *
* Writes protocol to specified file with condense                     *
*---------------------------------------------------------------------*
"! Writes protocol to specified file and does condense spaces.
FORM write_prot_to_file USING iv_logfile TYPE trfile.

  IF gv_external_call = abap_true OR gt_xmsg IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'TR_WRITE_LOG'
    EXPORTING
      iv_log_type       = 'FILE'     "Protokolltyp 'FILE', 'DB', 'MEMORY'
      iv_logname_file   = iv_logfile     "Name des Protokolls (File)
      iv_condense       = 'X'     "Zeilen werden zusammengeschoben
    TABLES
      it_msgs           = gt_xmsg    "Tabelle mit den Meldungen
    EXCEPTIONS
      invalid_input     = 1
      file_access_error = 2
      db_access_error   = 3
      OTHERS            = 4.

  CLEAR gt_xmsg.

ENDFORM.                    "write_prot

*---------------------------------------------------------------------*
*       FORM deploy_objects                                           *
*---------------------------------------------------------------------*
"! Form that deploys the objects to HANA, triggers external view creation
"! and does logging.
"!
"! @parameter ct_successful_deployed_objects | All skipped objects plus successfully activated objects (also deletions and reverted objects)
"!                                             minus error objects during external view creation
FORM deploy_objects USING   io_hana_connector           TYPE REF TO cl_cts_hot_hana_connector
                            it_objects                  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
                            iv_nr_of_transport_requests TYPE i
                            iv_abap_status              TYPE cts_hot_abap_status
                    CHANGING ct_successful_deployed_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                    RAISING cx_hana_object_transport
                            cx_nhi_hana_repository.

  DATA: log_activation_result          TYPE abap_bool,
        lt_skipped_filtered            TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that were skipped due to HOT_STATUS = 'N' per request
        lt_skipped_filtered_n          TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All successfully activated objects (not successfully deleted objects, see lt_successful_deleted)
        lt_successful_objects          TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All successfully activated objects per request (not successfully deleted objects, see lt_successful_deleted)
        lt_successful_filtered         TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that were deleted successfully (deleted from inactive AND active)
        lt_successful_deleted          TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that were deleted successfully per request (deleted from inactive AND active)
        lt_successful_deleted_filtered TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All successfully imported objects, per request (does not contain inactively deleted or reverted objects)
        lt_imported_filtered           TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that were inactively deleted and require activation, per request
        lt_inactive_deleted_filtered   TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        lt_import_error_objects        TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        lt_import_error_filtered       TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! objects that were only available as inactive and thus only reverted without an activation of the deletion afterwards, per request
        lt_reverted_filtered           TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that failed during activation (not objects with import errors)
        lt_failed_objects              TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All objects that failed during activation per request
        lt_failed_filtered             TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        "! All activation failures in order as they happened for summary
        lt_failed_filtered_ordered     TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
        lo_current_cts_hot_object      TYPE REF TO cl_cts_hot_object_v1,
        lv_tmptext                     TYPE string,
        lv_tmptext2                    TYPE string,
        lv_severity                    TYPE c,
        lv_level                       TYPE c,
        lt_already_logged_cts_hot_objs TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects, "remeber which objects have alrwady been logged
        dr_previous_log_message        TYPE REF TO cl_cts_hot_hana_connector=>ty_log_message, "to know whether to begin new section or not in case log lines do not have objects.
        lv_new_log_section             TYPE string VALUE ' ',
        lv_current_object_part_of_log  TYPE abap_bool, "indicates whether current object in lo_current_cts_hot_object is part of TR of cutrrent log file
        lv_last_attempt_of_current_obj TYPE abap_bool, "indicates whether current activation result is last deployment for current object in lo_current_cts_hot_object
        lv_current_obj_successfull     TYPE abap_bool, "indicates whether current object in lo_current_cts_hot_object was successfull or failed
        lv_regeneration_phase          TYPE abap_bool, "indicates whether current logging is already about regeneration phase
        lv_last_tr_of_current_object   TYPE trkorr, "last trkorr (queue order) of lo_current_cts_hot_object, initial if lo_current_cts_hot_object is unknown (some depending object returned by HANA - usually failed at regeneration)
        lr_external_view_handler       TYPE REF TO lcl_external_viewhandler,
        lt_ext_view_results            TYPE lcl_external_viewhandler=>ty_t_ext_view_result_with_hoto,
        lv_activation_mode             TYPE char1,
        lv_max_nr_activation_attempts  TYPE i,
        lv_activate_with_hints         TYPE abap_bool.

  IF p_deplf = 'X'.
    lv_activation_mode = gr_cts_hot_db_access->read_activation_mode_rf( ).
    lv_max_nr_activation_attempts = gr_cts_hot_db_access->read_max_nr_act_attempts_rf( ).
  ELSE.
    lv_activation_mode = gr_cts_hot_db_access->read_activation_mode( ).
    lv_max_nr_activation_attempts = gr_cts_hot_db_access->read_max_nr_activation_atempts( ).
  ENDIF.
  lv_activate_with_hints = gr_cts_hot_db_access->read_activate_with_hints( ).

  io_hana_connector->deploy_objects_to_hana(
    EXPORTING
      i_objects                    = it_objects
      i_abap_status                = iv_abap_status
      i_activation_mode            = lv_activation_mode
      i_max_nr_activation_attempts = lv_max_nr_activation_attempts
      i_activate_with_hints        = lv_activate_with_hints
    IMPORTING
      e_skipped_objects        = DATA(lt_skipped_objects) "skip returns objects with hot_status A(already active) and unkown objects. As we handle unknown before in this report skipped contains only objects with status A
      e_skipped_objects_n      = DATA(lt_skipped_objects_n) "skip returns objects with hot_status N(synchronized).
      e_successfull_objects    = DATA(lt_all_successful_objects) "contains all successful objects (successful activation, successful deletion and successful revert)
      e_failed_objects         = DATA(lt_all_failed_objects) "contains all failed objects (failure during import or failure during activation or due to wrong hot status)
      e_deploy_result          = DATA(lo_deploy_result)
  ).

  "remember all successful objects for changing parameter
  ct_successful_deployed_objects = lt_all_successful_objects.
  INSERT LINES OF lt_skipped_objects INTO TABLE ct_successful_deployed_objects.

  lt_successful_objects = lt_all_successful_objects.
  lt_failed_objects = lt_all_failed_objects.

  "Find out all objects that failed during import and objects that were reverted only (no activation afterwards).
  LOOP AT lo_deploy_result-import_result-objects_with_last_action INTO lo_current_cts_hot_object.
    "Not all objects with last action at import are error objects at import because reverted objects have by default last action at import but they do not have a problem
    IF line_exists( lt_failed_objects[ table_line = lo_current_cts_hot_object ] ).
      INSERT lo_current_cts_hot_object INTO TABLE lt_import_error_objects.
      DELETE TABLE lt_failed_objects FROM lo_current_cts_hot_object. "lt_failed_objects should only contain objects with error during activation
    ELSE.
      DELETE TABLE lt_successful_objects FROM lo_current_cts_hot_object. "lt_successfull_objects should only contain successfully activated objects
    ENDIF.
  ENDLOOP.

  "use only successful activated objects but not successful deleted objects for external view creation
  "but remember successfully deleted objects to be logged as deleted and not as activated objects (delete from lt_successfull_objects and add to lt_successful_deleted)
  LOOP AT lo_deploy_result-import_result-successfully_deleted_objects INTO lo_current_cts_hot_object.
    "if deletion of object was successfully activated, then delete it from lt_successful so that no create alias is triggered
    IF line_exists( lt_successful_objects[ table_line = lo_current_cts_hot_object ] ).
      INSERT lo_current_cts_hot_object INTO TABLE lt_successful_deleted.
      DELETE TABLE lt_successful_objects FROM lo_current_cts_hot_object.
    ENDIF.
  ENDLOOP.

  "trigger external view creation for all successful objects. We do not know whether there are external views defined at all...
  lr_external_view_handler = NEW lcl_external_viewhandler( ).
  lr_external_view_handler->create_aliases_for_ext_views(
                EXPORTING i_hoto_objects = lt_successful_objects
                IMPORTING e_ext_views_results = lt_ext_view_results ).

  "Remove objects with errors in external view creation from ct_successful_deployed_objects. Only if external view creation was also successful, object should be updated in E071
  "Logging for external view creation is done later during logging per request
  LOOP AT lt_ext_view_results ASSIGNING FIELD-SYMBOL(<ext_view_result>).
    IF <ext_view_result>-hot_object IS BOUND AND <ext_view_result>-max_severity <> 'I' AND <ext_view_result>-max_severity <> 'W'.
      DELETE TABLE ct_successful_deployed_objects FROM <ext_view_result>-hot_object.
    ENDIF.
  ENDLOOP.

  "loop over all log files for which HOTO was processed
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE pgmid = 'LIMU' AND object = 'HOTO' AND cts_hot_object IS BOUND
                                                                GROUP BY ( trkorr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile )
                                                                REFERENCE INTO DATA(lr_e071_hot_ref_group).
    FREE: lt_already_logged_cts_hot_objs, lt_skipped_filtered, lt_successful_filtered, lt_imported_filtered, lt_import_error_filtered, lt_inactive_deleted_filtered,
          lt_failed_filtered, lt_failed_filtered_ordered, lt_reverted_filtered, lt_successful_deleted_filtered, lt_skipped_filtered_n.

    PERFORM filter_objects_for_logfile USING 'LIMU' lt_skipped_objects lr_e071_hot_ref_group->trkorr CHANGING lt_skipped_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_skipped_objects_n lr_e071_hot_ref_group->trkorr CHANGING lt_skipped_filtered_n.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_successful_objects lr_e071_hot_ref_group->trkorr CHANGING lt_successful_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lo_deploy_result-import_result-successfully_imported_objects lr_e071_hot_ref_group->trkorr CHANGING lt_imported_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lo_deploy_result-import_result-successfully_deleted_objects lr_e071_hot_ref_group->trkorr CHANGING lt_inactive_deleted_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lo_deploy_result-import_result-successfully_reverted_objects lr_e071_hot_ref_group->trkorr CHANGING lt_reverted_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_import_error_objects lr_e071_hot_ref_group->trkorr CHANGING lt_import_error_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_failed_objects lr_e071_hot_ref_group->trkorr CHANGING lt_failed_filtered.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_successful_deleted lr_e071_hot_ref_group->trkorr CHANGING lt_successful_deleted_filtered.

    IF lt_skipped_filtered IS NOT INITIAL.
      PERFORM log_summary_line USING '3' ' ' '550' ' ' lt_skipped_filtered. "Number of skipped objects because already active
      PERFORM log_processed_objects USING '556' lr_e071_hot_ref_group->trkorr lt_skipped_filtered. " Following Objects were skipped because already active
    ENDIF.

    IF lt_skipped_filtered_n IS NOT INITIAL.
      PERFORM log_summary_line USING '3' 'W' '626' ' ' lt_skipped_filtered_n. "Number of skipped objects because they were synchronized
      PERFORM log_processed_objects USING '628' lr_e071_hot_ref_group->trkorr lt_skipped_filtered_n. " Following Objects were skipped because they were synchronized
    ENDIF.

    "Skip logging if all filtered tables apart from skipped are empty, meaning that no object was imported/activated/deleted for the current request to be logged.
    IF lt_successful_filtered IS INITIAL
          AND lt_imported_filtered IS INITIAL
          AND lt_inactive_deleted_filtered IS INITIAL
          AND lt_import_error_filtered IS INITIAL
          AND lt_failed_filtered IS INITIAL
          AND lt_reverted_filtered IS INITIAL
          AND lt_successful_deleted_filtered IS INITIAL.
      PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
      CONTINUE.
    ENDIF.

    PERFORM protokoll USING '3' ' ' '572' 'X' space space space space. "Begin import of objects

* log summary of import
    PERFORM log_summary_line USING '3' ' ' '551' ' ' lt_imported_filtered. "Number of inactive imported objects
    lv_tmptext = |{ lines( lt_inactive_deleted_filtered ) + lines( lt_reverted_filtered ) }|.
    PERFORM protokoll USING '3' ' ' '554' ' ' lv_tmptext space space space. "Number of inactive deleted objects
    IF lines( lt_import_error_filtered ) > 0.
      PERFORM log_summary_line USING '2' 'E' '552' ' ' lt_import_error_filtered. "Number of objects with import error
    ELSE.
      PERFORM log_summary_line USING '3' ' ' '552' ' ' lt_import_error_filtered. "Number of objects with import error
    ENDIF.

* log details of import
    PERFORM log_processed_objects USING '557' lr_e071_hot_ref_group->trkorr lt_imported_filtered. "Following objects were successfully imported to inactive
    PERFORM log_processed_objects USING '566' lr_e071_hot_ref_group->trkorr lt_inactive_deleted_filtered. "Following objects were successfully deleted in inactive

    "log all reverted objects as successfully deleted because no activation required
    IF lines( lt_reverted_filtered ) > 0.
      PERFORM protokoll USING '3' ' ' '507' ' '  space space space space. "empty line
      lv_tmptext = |{ lines( lt_reverted_filtered ) }|.

      IF lt_inactive_deleted_filtered IS INITIAL.
        PERFORM protokoll USING '3' ' ' '607' ' '  lv_tmptext space space space. "Following objects were deleted inactive without need for activation
      ELSE.
        lv_tmptext2 = |{ lines( lt_inactive_deleted_filtered ) }|.
        PERFORM protokoll USING '3' ' ' '608' ' '  lv_tmptext lv_tmptext2 space space. "Following objects were deleted inactive without need for activation
      ENDIF.

      LOOP AT lt_reverted_filtered INTO lo_current_cts_hot_object.
        "similar to form log_object_header
        lv_tmptext = |  { lo_current_cts_hot_object->hana_object_name }.{ lo_current_cts_hot_object->hana_object_suffix } ({ lo_current_cts_hot_object->hana_package_id }) deleted successfully|.
        PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.
        IF lv_last_tr_of_current_object <> lr_e071_hot_ref_group->trkorr.
          lv_tmptext = |{ lv_tmptext } - version of { lv_last_tr_of_current_object }|.
          PERFORM protokoll_longtext USING '3' ' ' '537' ' ' lv_tmptext.
        ELSE.
          PERFORM protokoll_longtext USING '3' ' ' '531' ' ' lv_tmptext.
        ENDIF.

        INSERT lo_current_cts_hot_object INTO TABLE lt_already_logged_cts_hot_objs.
      ENDLOOP.
    ENDIF.

    "log objects with error during import
    IF lines( lt_import_error_filtered ) > 0.
      PERFORM protokoll USING '2' 'E' '558' 'X' space space space space. "objects with error during import
      DATA: lv_last_trkorr_of_object TYPE trkorr,
            lv_akh                   TYPE ufps_posid.
      LOOP AT lt_import_error_filtered INTO lo_current_cts_hot_object.
        PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_trkorr_of_object.
        IF lv_last_trkorr_of_object <> lr_e071_hot_ref_group->trkorr.
          lv_tmptext = |{ lo_current_cts_hot_object->hana_object_name }.{ lo_current_cts_hot_object->hana_object_suffix } ({ lo_current_cts_hot_object->hana_package_id }) import failed - version of { lv_last_trkorr_of_object }|.
        ELSE.
          lv_tmptext = |{ lo_current_cts_hot_object->hana_object_name }.{ lo_current_cts_hot_object->hana_object_suffix } ({ lo_current_cts_hot_object->hana_package_id }) import failed|.
        ENDIF.
        lv_akh = gr_external_persistency->get_akh_for_package( lo_current_cts_hot_object->abap_hana_package_id ).
        IF lv_akh IS NOT INITIAL.
          lv_tmptext = |{ lv_tmptext } [{ lv_akh }]|.
        ENDIF.

        PERFORM protokoll_longtext USING '2' 'E' '531' ' ' lv_tmptext.

        INSERT lo_current_cts_hot_object INTO TABLE lt_already_logged_cts_hot_objs.

        "log all log lines of this object
        LOOP AT lo_deploy_result-import_result-log_messages REFERENCE INTO DATA(dr_imp_log_message) WHERE cts_hot_object = lo_current_cts_hot_object.
          PERFORM protokoll_log_message_532_533 USING dr_imp_log_message '2' 'E' ' ' io_hana_connector->g_hana_timezone_string.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    PERFORM protokoll USING '3' ' ' '573' ' ' space space space space. "End import of objects
    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.

    "Skip logging if all filtered tables containing activation results empty, meaning that no object was activated/deleted for the current request to be logged.
    IF lt_successful_filtered IS INITIAL
          AND lt_failed_filtered IS INITIAL
          AND lt_successful_deleted_filtered IS INITIAL
          AND lt_reverted_filtered IS INITIAL
          AND lt_import_error_filtered IS INITIAL.
      PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
      CONTINUE.
    ENDIF.

* log summary of activation
    PERFORM protokoll USING '3' ' ' '559' 'X' space space space space. "Begin activation of objects
    PERFORM log_summary_line USING '3' ' ' '553' ' ' lt_successful_filtered. "Number of successfully activated objects
    PERFORM log_summary_line USING '3' ' ' '609' ' ' lt_successful_deleted_filtered. "Number of successfully deleted objects

    IF lines( lt_reverted_filtered ) > 0.
      PERFORM log_summary_line USING '3' ' ' '611' ' ' lt_reverted_filtered. "Number of objects without activation due to revert only
    ELSE.
      PERFORM log_summary_line USING '4' ' ' '611' ' ' lt_reverted_filtered. "Number of objects without activation due to revert only
    ENDIF.

    IF lines( lt_import_error_filtered ) > 0.
      PERFORM log_summary_line USING '3' ' ' '610' ' ' lt_import_error_filtered. "Number of objects without activation due to import error
    ELSE.
      PERFORM log_summary_line USING '4' ' ' '610' ' ' lt_import_error_filtered. "Number of objects without activation due to import error
    ENDIF.

    IF lines( lt_failed_filtered ) > 0.
      PERFORM log_summary_line USING '2' 'E' '555' ' ' lt_failed_filtered. "Number of objects with activation error
    ELSE.
      PERFORM log_summary_line USING '3' ' ' '555' ' ' lt_failed_filtered. "Number of objects with activation error
    ENDIF.

    PERFORM log_processed_objects USING '612' lr_e071_hot_ref_group->trkorr lt_import_error_filtered. " Following objects not activated due to import errors

    "Skip logging if all filtered tables containing activation results empty, meaning that no object was activated/deleted for the current request to be logged.
    IF lt_successful_filtered IS NOT INITIAL
          OR lt_failed_filtered IS NOT INITIAL
          OR lt_successful_deleted_filtered IS NOT INITIAL.

* log details of activation
      PERFORM protokoll USING '3' ' ' '507' ' ' space space space space. "empty line
      IF gv_external_call = abap_false AND p_deplf IS INITIAL.
        lv_tmptext = |{ iv_nr_of_transport_requests }|.
        PERFORM protokoll USING '3' ' ' '569' ' ' lv_tmptext space space space. "Die SAP-HANA-Repo-Objekte der &1 importierten Aufträge werden aktiviert:
      ENDIF.
      IF p_deplf = 'X'.
        lv_tmptext = |{ if_cts_hot_db_access=>co_name_hot_activation_mode_rf }={ lv_activation_mode }| &&
                     | { if_cts_hot_db_access=>co_name_max_no_act_attempts_rf }={ lv_max_nr_activation_attempts }| &&
                     | { if_cts_hot_db_access=>co_name_hana_log_with_hints }={ lv_activate_with_hints }|.
        PERFORM protokoll_longtext USING '4' ' ' '622' ' ' lv_tmptext. "&1&2&3&4
      ELSE.
        lv_tmptext = |{ if_cts_hot_db_access=>co_name_hot_activation_mode }={ lv_activation_mode }| &&
                     | { if_cts_hot_db_access=>co_name_max_no_act_attempts }={ lv_max_nr_activation_attempts }| &&
                     | { if_cts_hot_db_access=>co_name_hana_log_with_hints }={ lv_activate_with_hints }|.
        PERFORM protokoll_longtext USING '4' ' ' '614' ' ' lv_tmptext. "&1&2&3&4
      ENDIF.

* log all activation results if applicable for current logfile
      LOOP AT lo_deploy_result-activation_results REFERENCE INTO DATA(dr_act_result).
        lv_tmptext = dr_act_result->activation_counter.
        lv_tmptext2 = dr_act_result->nr_of_attempted_objects.
        "tmptext must be used because of leading spaces when using i
        IF dr_act_result->ok_objects_only = abap_false.
          PERFORM protokoll USING '4' ' ' '539' 'X' lv_tmptext lv_tmptext2 space space. " Attempt &1: Activation of &2 object(s).
        ELSE.
          PERFORM protokoll USING '4' ' ' '613' 'X' lv_tmptext lv_tmptext2 space space. " Attempt &1: Activation of &2 successful object(s) of last attempt.
        ENDIF.

        "find out if current activation_result contains objects relevant for current logfile
        IF gv_external_call = abap_true.
          log_activation_result = abap_true.
        ELSE.
          log_activation_result = abap_false.
        ENDIF.

        IF log_activation_result = abap_false.
          IF lines( lo_deploy_result-activation_results ) = 2 AND lo_deploy_result-activation_results[ 2 ]-hana_error_code = '618'.
            "in case deployment was cancelled already after first round with "Activation canceled. Object set to activate remains constant.", there must be a generic HANA error that should be logged to all log files
            log_activation_result = abap_true.
          ELSEIF dr_act_result->activation_success = abap_true OR dr_act_result->ok_objects_only = abap_false.
            "in case of activation_success all OK objects are contained in objects_with_last_action.
            "In case of no success, failed objects are only contained if not in "nachbrenner" round (ok_objects_only = abap_false)
            LOOP AT dr_act_result->objects_with_last_action INTO DATA(lr_obj_with_last_action).
              IF line_exists( gt_e071_hot_refs[ trkorr = lr_e071_hot_ref_group->trkorr cts_hot_object = lr_obj_with_last_action ] ).
                log_activation_result = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            "Following loop finds out if any cts_hot_object of the log messages is part of this request
            LOOP AT dr_act_result->log_messages REFERENCE INTO DATA(lr_log_message) GROUP BY ( cts_hot_object = lr_log_message->cts_hot_object ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_log_message_group).
              IF lr_log_message_group->cts_hot_object IS BOUND
                  AND line_exists( gt_e071_hot_refs[ trkorr = lr_e071_hot_ref_group->trkorr cts_hot_object = lr_log_message_group->cts_hot_object ] ).
                log_activation_result = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF log_activation_result = abap_false.
          PERFORM protokoll USING '4' ' ' '542' ' ' space space space space. "No details in this log
          CONTINUE. "continue with next activation result
        ENDIF.

        IF dr_act_result->hana_activation_id = 'SCTS_HOT'. "SCTS_HOT if cancellation of activation due to list not decreasing or max attempts reached
          PERFORM protokoll USING '2' 'E' dr_act_result->hana_error_code 'X' space space space space. "Cancel Activation due to list not decreasing or max attempts reached (617, 618)
          PERFORM log_cancelled_objects USING dr_act_result->failed_objects lt_failed_filtered lr_e071_hot_ref_group->trkorr
                                        CHANGING lt_already_logged_cts_hot_objs lt_failed_filtered_ordered.
          PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
          CONTINUE. "continue with next activation result because cancellation does not contain any log messages. But usually cancellation should be last activation result...
        ENDIF.

        "log overall activation result (as error on level 2 or as info on level 4)
        lv_level = '4'.
        lv_severity = ' '.
        IF lines( lo_deploy_result-activation_results ) = 2 AND lo_deploy_result-activation_results[ 2 ]-hana_error_code = '618'.
          "in case deployment was cancelled already after first round with "Activation canceled. Object set to activate remains constant.", there must be a generic HANA error that should be logged to all log files
          lv_level = '2'.
          lv_severity = 'E'.
        ELSEIF dr_act_result->activation_success = abap_false
          AND dr_act_result->ok_objects_only = abap_false "errors in OK objects rounds are not logged as error
          AND lt_failed_filtered IS NOT INITIAL. "only if failed objects of current request contained, log as error
          "but only log if failed objects failed in current attempt.
          LOOP AT lt_failed_filtered INTO lo_current_cts_hot_object.
            IF line_exists( dr_act_result->objects_with_last_action[ table_line = lo_current_cts_hot_object ] ).
              lv_level = '2'.
              lv_severity = 'E'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        PERFORM protokoll USING lv_level lv_severity '563' ' ' dr_act_result->hana_activation_id dr_act_result->hana_error_code space space. "Overall Activation ID and Return Code"

        IF dr_act_result->hana_error_msg IS NOT INITIAL.
          lv_tmptext = |HANA Message: { dr_act_result->hana_error_msg }|.
          PERFORM protokoll_longtext USING lv_level lv_severity '532' ' ' lv_tmptext.
        ENDIF.
        IF dr_act_result->hana_error_arg IS NOT INITIAL.
          lv_tmptext = |HANA Arguments: { dr_act_result->hana_error_arg }|.
          PERFORM protokoll_longtext USING lv_level lv_severity '532' ' ' lv_tmptext.
        ENDIF.

        PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.

        CLEAR lo_current_cts_hot_object.
        CLEAR lv_regeneration_phase.
        LOOP AT dr_act_result->log_messages REFERENCE INTO DATA(dr_log_message).
          "print header line of (Object) but only print if object changes.
          IF lo_current_cts_hot_object IS NOT BOUND OR lo_current_cts_hot_object <> dr_log_message->cts_hot_object.
            lo_current_cts_hot_object = dr_log_message->cts_hot_object.

            IF lo_current_cts_hot_object IS BOUND.
              IF lv_regeneration_phase = abap_false.
                "for regeneration phase error logging we do not need last TR and all this information below
                PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.

                IF lv_last_tr_of_current_object IS INITIAL "this is the case if HANA returns objects for activation result that were added implicitly by activation order calculation or in case some activation plugin ended with error
                      AND gv_external_call = abap_false. "for external calls (API calls) we do not have requests

                  lv_current_object_part_of_log = abap_false.
                  lv_last_attempt_of_current_obj = abap_false."not required for unknown objects

                  "check whether unknown object failed with error or warning in this activation round
                  IF line_exists( dr_act_result->log_messages[ cts_hot_object = lo_current_cts_hot_object severity = '2' ] )
                    OR line_exists( dr_act_result->log_messages[ cts_hot_object = lo_current_cts_hot_object severity = '3' ] ).
                    lv_current_obj_successfull = abap_false.
                  ELSE.
                    lv_current_obj_successfull = abap_true.
                  ENDIF.
                ELSE.
                  IF line_exists( gt_e071_hot_refs[ trkorr = lr_e071_hot_ref_group->trkorr cts_hot_object = lo_current_cts_hot_object ] ).
                    lv_current_object_part_of_log = abap_true.
                  ELSE.
                    lv_current_object_part_of_log = abap_false.
                  ENDIF.

                  IF line_exists( dr_act_result->objects_with_last_action[ table_line = lo_current_cts_hot_object ] ).
                    lv_last_attempt_of_current_obj = abap_true.
                  ELSE.
                    lv_last_attempt_of_current_obj = abap_false.
                  ENDIF.

                  IF line_exists( dr_act_result->failed_objects[ table_line = lo_current_cts_hot_object ] ).
                    lv_current_obj_successfull = abap_false.

                    IF lv_last_attempt_of_current_obj = abap_true
                        AND line_exists( lt_failed_filtered[ table_line = lo_current_cts_hot_object ] )
                        AND NOT line_exists( lt_failed_filtered_ordered[ table_line = lo_current_cts_hot_object ] ).
                      APPEND lo_current_cts_hot_object TO lt_failed_filtered_ordered.
                    ENDIF.
                  ELSE.
                    lv_current_obj_successfull = abap_true.
                  ENDIF.
                ENDIF.

                PERFORM log_object_header USING lo_current_cts_hot_object lv_current_object_part_of_log lv_last_tr_of_current_object lr_e071_hot_ref_group->trkorr lv_last_attempt_of_current_obj lv_current_obj_successfull abap_false.

                IF lv_last_attempt_of_current_obj = abap_true.
                  INSERT lo_current_cts_hot_object INTO TABLE lt_already_logged_cts_hot_objs.
                ENDIF.
              ELSE. "Generation Phase
                CLEAR: lv_current_object_part_of_log, lv_last_tr_of_current_object, lv_last_attempt_of_current_obj, lv_current_obj_successfull.
                "for regeneration phase we print the objects that were regenerated only in logs that have at least 1 object with successful
                "activation because only these successful activated objects might be reason for regeneration
                IF lt_successful_filtered IS NOT INITIAL OR lt_successful_deleted_filtered IS NOT INITIAL.
                  IF line_exists( it_objects[ table_line = lo_current_cts_hot_object ] ).
                    PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.
                    IF lv_last_tr_of_current_object = lr_e071_hot_ref_group->trkorr.
                      CLEAR lv_last_tr_of_current_object. "do not print TR if current TR is same as last TR of this object
                    ENDIF.
                    IF line_exists( lt_failed_objects[ table_line = lo_current_cts_hot_object ] ).
                      PERFORM log_object_header_regeneration USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object 0 dr_log_message->severity.
                    ELSE. "object was successfully activated/deleted in another attempt before or after current successful attempt leading to regeneration.
                      LOOP AT lo_deploy_result-activation_results REFERENCE INTO DATA(dr_act_result_regen) WHERE activation_success = abap_true
                                                                                                             AND activation_counter > dr_act_result->activation_counter.
                        lv_current_obj_successfull = abap_true. "successfully activated/delete after current attempt, so finally OK
                        EXIT.
                      ENDLOOP.
                      IF lv_current_obj_successfull = abap_true.
                        PERFORM log_object_header_regeneration USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object 1 dr_log_message->severity.
                      ELSE.
                        PERFORM log_object_header_regeneration USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object -1 dr_log_message->severity.
                      ENDIF.
                    ENDIF.
                  ELSE.
                    PERFORM log_object_header_regeneration USING lo_current_cts_hot_object abap_false lv_last_tr_of_current_object 0 dr_log_message->severity.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              "lo_current_cts_hot_object not bound, no header logging
            ENDIF.
          ENDIF.

          "print logline of object
          "make sure to handle severity = '3' correct and map to 8 only if object (dr_log_message->cts_hot_object) is part of the TR. If obj is not part of TR then only log as level 4 to document correct hana order.
          IF lo_current_cts_hot_object IS BOUND. "log with more spaces otherwise object independent message
            IF lv_regeneration_phase = abap_true AND ( lt_successful_filtered IS NOT INITIAL OR lt_successful_deleted_filtered IS NOT INITIAL ).
              "in regeneration phase always print as warning but only in logs that have at least one successful activated objet.
              IF dr_log_message->severity = '2' OR dr_log_message->severity = '3'.
                IF lv_current_obj_successfull = abap_true. "if object was part of import and successful in the end, only log as level 4
                  PERFORM protokoll_log_message_532_533 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
                ELSE.
                  "object was either not part of deployment or object was successful before this activation attempt but finally is failed.
                  PERFORM protokoll_log_message_532_533 USING dr_log_message '3' 'W' ' ' io_hana_connector->g_hana_timezone_string.
                ENDIF.
              ELSE.
                " 11.05.2015 - skip output of details for regenerated objects
                "PERFORM protokoll_log_message_532_533 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
              ENDIF.
            ELSE.
              "only log details for objects of this request.
              IF lv_current_object_part_of_log = abap_true.
                "only log details if it was last attempt
                IF lv_last_attempt_of_current_obj = abap_true AND lv_last_tr_of_current_object = lr_e071_hot_ref_group->trkorr.
                  IF lv_current_obj_successfull = abap_true.
                    PERFORM protokoll_log_message_532_533 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
                  ELSE.
                    "For error objects only log messages of severity 3 as error
                    IF dr_log_message->severity = '3'.
                      PERFORM protokoll_log_message_532_533 USING dr_log_message '2' 'E' ' ' io_hana_connector->g_hana_timezone_string.
                    ELSE.
                      PERFORM protokoll_log_message_532_533 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
                    ENDIF.
                  ENDIF.
                ELSEIF lv_last_attempt_of_current_obj = abap_false AND lv_last_tr_of_current_object = lr_e071_hot_ref_group->trkorr
                        AND dr_act_result->ok_objects_only = abap_true AND dr_log_message->severity = '3'.
                  "if not last attempt but error during activation of OK objects only, log the error as waring
                  PERFORM protokoll_log_message_532_533 USING dr_log_message '4' 'W' ' ' io_hana_connector->g_hana_timezone_string.
                ELSE.
                  "if not last attempt and not the request of the deployed version, do not log details
                ENDIF.
              ELSE.
                "object not part of this request, log only on level 3 with details for unknown objects (no tr found for this object)
                IF lv_last_tr_of_current_object IS INITIAL.
                  "object not part of any request, so automatically added by repository to activation list
                  IF dr_log_message->severity = '2' OR dr_log_message->severity = '3'.
                    PERFORM protokoll_log_message_532_533 USING dr_log_message '3' 'W' ' ' io_hana_connector->g_hana_timezone_string.
                  ELSE.
                    "Do not print details for automatically added objects that were successful
                    "PERFORM protokoll_log_message_532_533 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
                  ENDIF.
                ELSE.
                  "For known objects the log will be contained in the corresponding log file.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE. " log line without object
            IF dr_previous_log_message IS NOT INITIAL AND ( dr_previous_log_message->cts_hot_object IS BOUND
                                                            OR ( dr_previous_log_message->severity <> dr_log_message->severity
                                                                 AND dr_log_message->severity CA '23' ) ).
              lv_new_log_section = 'X'.
            ELSE.
              lv_new_log_section = ' '.
            ENDIF.
            IF dr_log_message->severity = '3'.
              IF lv_new_log_section = 'X'.
                IF lv_regeneration_phase = abap_true.
                  PERFORM protokoll USING '3' ' ' '541' 'X' space space space space. "only write 541 here and not for hana messages "start activation" and "finished activation"
                ELSE.
                  PERFORM protokoll USING '2' ' ' '541' 'X' space space space space. "only write 541 here and not for hana messages "start activation" and "finished activation"
                ENDIF.
              ENDIF.
              IF lv_regeneration_phase = abap_true.
                PERFORM protokoll_log_message_530_531 USING dr_log_message '3' 'W' space io_hana_connector->g_hana_timezone_string.
              ELSE.
                PERFORM protokoll_log_message_530_531 USING dr_log_message '2' 'E' space io_hana_connector->g_hana_timezone_string.
              ENDIF.
            ELSE.
              " check for end of activation log line, to print all not yet logged successful objects before that line
              IF dr_log_message->severity = '4' AND dr_log_message->error_code = '40137'.
                FIND FIRST OCCURRENCE OF 'Finished activation phase.' IN dr_log_message->message.
                IF sy-subrc = 0.
                  lv_regeneration_phase = abap_true.

                  "print all successful objects not yet logged before this "activation finished" line but only for "normal" successful activation
                  IF dr_act_result->ok_objects_only = abap_false.
                    PERFORM log_not_yet_logged_succes_objs USING lt_successful_objects lr_e071_hot_ref_group->trkorr abap_false CHANGING lt_already_logged_cts_hot_objs.

                    "log all successfully deleted objects
                    PERFORM log_not_yet_logged_succes_objs USING lt_successful_deleted lr_e071_hot_ref_group->trkorr abap_true CHANGING lt_already_logged_cts_hot_objs.
                  ELSE.
                    "in case ok_objects_only = abap_true the objects_with_last_action contains all successful objects
                    PERFORM log_not_yet_logged_succes_objs USING dr_act_result->objects_with_last_action lr_e071_hot_ref_group->trkorr abap_false CHANGING lt_already_logged_cts_hot_objs.
                  ENDIF.
                  CLEAR: lv_last_tr_of_current_object, lv_last_attempt_of_current_obj, lv_current_object_part_of_log.
                ENDIF.
                PERFORM protokoll_log_message_530_531 USING dr_log_message '4' ' ' ' ' io_hana_connector->g_hana_timezone_string.
              ELSE.
                PERFORM protokoll_log_message_530_531 USING dr_log_message '4' ' ' 'X' io_hana_connector->g_hana_timezone_string.
              ENDIF.
            ENDIF.
          ENDIF.
          dr_previous_log_message = dr_log_message.
        ENDLOOP. "loop over all log messages of an activation result
        PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
      ENDLOOP. "loop over all activation results

* log objects that were not yet logged if any... (usually all should have been logged before)
      "log successful objects of current TR / log
      LOOP AT lt_successful_filtered INTO lo_current_cts_hot_object.
        IF NOT line_exists( lt_already_logged_cts_hot_objs[ table_line = lo_current_cts_hot_object ] ).
          PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.

          PERFORM log_object_header USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object lr_e071_hot_ref_group->trkorr abap_true abap_true abap_false.
        ENDIF.
      ENDLOOP.

      "log deleted objects of current TR / log
      LOOP AT lt_successful_deleted_filtered INTO lo_current_cts_hot_object.
        IF NOT line_exists( lt_already_logged_cts_hot_objs[ table_line = lo_current_cts_hot_object ] ).
          PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.

          PERFORM log_object_header USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object lr_e071_hot_ref_group->trkorr abap_true abap_true abap_true.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_failed_filtered INTO lo_current_cts_hot_object.
        IF NOT line_exists( lt_already_logged_cts_hot_objs[ table_line = lo_current_cts_hot_object ] ).
          PERFORM find_last_request_for_object USING lo_current_cts_hot_object CHANGING lv_last_tr_of_current_object.

          PERFORM log_object_header USING lo_current_cts_hot_object abap_true lv_last_tr_of_current_object lr_e071_hot_ref_group->trkorr abap_true abap_false abap_false.
          IF NOT line_exists( lt_failed_filtered_ordered[ table_line = lo_current_cts_hot_object ] ).
            APPEND lo_current_cts_hot_object TO lt_failed_filtered_ordered.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    PERFORM protokoll USING '3' ' ' '560' ' ' space space space space. "end of object activation
    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.

* log external view creation result if any
    PERFORM log_ext_view_creation_result USING lt_successful_filtered lt_ext_view_results.

* log summary of all objects having errors
    PERFORM log_failed_object_summary USING '561' lt_import_error_filtered lr_e071_hot_ref_group->trkorr. "Summary of all objects having import errors

    PERFORM log_failed_object_summary USING '562' lt_failed_filtered_ordered lr_e071_hot_ref_group->trkorr. "Summary of all objects having activation errors

    IF lt_import_error_filtered IS NOT INITIAL OR lt_failed_filtered_ordered IS NOT INITIAL.
      PERFORM protokoll USING '2' 'E' '619' 'X' space space space space. "For analyzing the import/activation errors see SAP Note 2109690.
    ENDIF.

*      PERFORM protokoll USING '3' 'W' '507' 'X' 'Zusammenfassung der Objekte mit Genierungsfehler:' space space space.

    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
  ENDLOOP. "loop over all logfiles
ENDFORM.

*---------------------------------------------------------------------*
*       FORM deploy_object_texts                                           *
*---------------------------------------------------------------------*
"! Form to deploy texts for all objects of all imported requests. Failed objects are also considered but text deployment will not happen
"! because objects are not active in HTA.
"!
"! @parameter ct_ok_deployed_objects_e071   | List of all objects deployed successfully during deployment. Will be reduced by those objects for which
"!                                            text deployment failed. Purpose to be used for E071 update.
"! @parameter ct_ok_deployed_obj_texts_e071 | List of objects with lang (abap_lang) where text deployment was successful or skipped. If all languages
"!                                            were successfully deployed, abap_lang is empty table. Purpose to be sued for E071 update.
FORM deploy_object_texts USING io_hana_connector          TYPE REF TO cl_cts_hot_hana_connector
                               it_objects_for_text_deploy TYPE cl_cts_hot_hana_connector=>ty_text_deploy_inputs
                         CHANGING ct_ok_deployed_objects_e071 TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                  ct_ok_deployed_obj_texts_e071 TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results
                         RAISING cx_hana_object_transport
                                 cx_nhi_hana_repository.

  DATA: lr_hot_object TYPE REF TO cl_cts_hot_object_v1,
        lt_loggers    TYPE STANDARD TABLE OF REF TO lcl_text_deploy_result_logger WITH DEFAULT KEY.

  " create deploy result logger, one per trkorr.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e017_hot_ref) GROUP BY ( trkorr = lr_e017_hot_ref->trkorr ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_e071_hot_ref_group).
    DATA(lr_logger) = NEW lcl_text_deploy_result_logger( i_e071_entries = gt_e071_hot_refs i_trkorr = lr_e071_hot_ref_group->trkorr ).
    lr_logger->write_header( ).
    APPEND lr_logger TO lt_loggers.
  ENDLOOP.

  io_hana_connector->deploy_object_texts(
    EXPORTING
      i_hot_objects_with_lang = it_objects_for_text_deploy
    IMPORTING
      e_not_active_objects         = DATA(lt_not_active_objects)
      e_unknown_objects            = DATA(lt_unknown_objects)
      e_skipped_objects            = DATA(lt_skipped_objects)
      e_failed_text_deploy_result  = DATA(lt_failed_text_deploy_result)
      e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
  ).

  " provide text deploy results to logger and write log and footer
  LOOP AT lt_loggers INTO lr_logger.
    lr_logger->set_not_active_objects( lt_not_active_objects ).
    lr_logger->set_unknown_objects( lt_unknown_objects ).
    lr_logger->set_skipped_objects( lt_skipped_objects ).
    lr_logger->set_failed_text_deploy_results( lt_failed_text_deploy_result ).
    lr_logger->set_ok_text_deploy_results( lt_ok_text_deploy_result ).

    lr_logger->write_log( ).
    lr_logger->write_footer( ).
  ENDLOOP.

  " All objects with text deployment error to be removed from successfully deployed objects table
  LOOP AT lt_failed_text_deploy_result REFERENCE INTO DATA(lr_failed_result) GROUP BY ( cts_hot_object = lr_failed_result->cts_hot_object )
                                                                             WITHOUT MEMBERS
                                                                             REFERENCE INTO DATA(lr_failed_result_group).
    DELETE TABLE ct_ok_deployed_objects_e071 FROM lr_failed_result_group->cts_hot_object.
  ENDLOOP.

  " all unknown objects are handled as successfully deployed with regards to e071
  LOOP AT lt_unknown_objects INTO lr_hot_object.
    INSERT VALUE #( cts_hot_object = lr_hot_object ) INTO TABLE ct_ok_deployed_obj_texts_e071.
  ENDLOOP.

  " all skipped objects are handled as successfully deployed with regards to e071
  LOOP AT lt_skipped_objects INTO lr_hot_object.
    INSERT VALUE #( cts_hot_object = lr_hot_object ) INTO TABLE ct_ok_deployed_obj_texts_e071.
  ENDLOOP.

  " all OK objects are handled as successfully deployed with regards to e071.
  " if all languages were OK, then abap_lang is not set
  " if at least 1 language failed during deployment, abap_lang is filled with successfully deployed languages only
  LOOP AT lt_ok_text_deploy_result REFERENCE INTO DATA(lr_ok_result) GROUP BY ( cts_hot_object = lr_ok_result->cts_hot_object )
                                                                     REFERENCE INTO DATA(lr_ok_result_group).
    IF NOT line_exists( lt_failed_text_deploy_result[ cts_hot_object = lr_ok_result_group->cts_hot_object ] ).
      "all languages deployed successfully.
      INSERT VALUE #( cts_hot_object = lr_ok_result_group->cts_hot_object ) INTO TABLE ct_ok_deployed_obj_texts_e071.
    ELSE.
      LOOP AT GROUP lr_ok_result_group REFERENCE INTO DATA(lr_ok_group_member).
        "only a few languages deployed successfully.
        INSERT VALUE #( cts_hot_object = lr_ok_group_member->cts_hot_object abap_lang = lr_ok_group_member->abap_lang )  INTO TABLE ct_ok_deployed_obj_texts_e071.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM protokoll_log_message_530_531s                           *
*---------------------------------------------------------------------*
"! Logs messages 530 or 531 (&1&2&3&4) and if HANA Message, then details of HANA message.
FORM protokoll_log_message_530_531 USING ir_log_message TYPE REF TO cl_cts_hot_hana_connector=>ty_log_message
                                         iv_level
                                         iv_severity
                                         iv_new_object
                                         iv_time_zone.

  IF ir_log_message->is_hana_message = abap_true.
    PERFORM protokoll_longtext USING iv_level iv_severity '530' iv_new_object ir_log_message->message. "&1&2&3&4 - HANA
    PERFORM protokoll USING iv_level iv_severity '564' ' ' ir_log_message->timestamp ir_log_message->severity ir_log_message->error_code iv_time_zone. "Timestamp(&4) &1 Severity &2 Return Code &3
    IF ir_log_message->location IS NOT INITIAL.
      PERFORM protokoll USING iv_level iv_severity '570' ' ' ir_log_message->location space space space. "Location &1
    ENDIF.
  ELSE.
    PERFORM protokoll_longtext USING iv_level iv_severity '531' iv_new_object ir_log_message->message. "&1&2&3&4
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM protokoll_log_message_532_533                            *
*---------------------------------------------------------------------*
"! Logs messages 532 (&1&2&3&4 - message from HANA) or 533 (&1&2&3&4 - no HANA message)
"! and if HANA Message, then details of HANA message but only for iv_severity not ' '.
FORM protokoll_log_message_532_533 USING ir_log_message TYPE REF TO cl_cts_hot_hana_connector=>ty_log_message
                                         iv_level
                                         iv_severity
                                         iv_new_object
                                         iv_time_zone.

  IF ir_log_message->is_hana_message = abap_true.
    PERFORM protokoll_longtext USING iv_level iv_severity '532' iv_new_object ir_log_message->message. "&1&2&3&4 - HANA
    IF iv_severity <> ' '.
      PERFORM protokoll USING iv_level iv_severity '567' ' ' ir_log_message->timestamp ir_log_message->severity ir_log_message->error_code iv_time_zone. "Timestamp(&4) &1 Severity &2 Return Code &3
      IF ir_log_message->location IS NOT INITIAL.
        PERFORM protokoll USING iv_level iv_severity '571' ' ' ir_log_message->location space space space. "Location &1
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM protokoll_longtext USING iv_level iv_severity '533' iv_new_object ir_log_message->message. "&1&2&3&4
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM find_last_request_for_object                             *
*---------------------------------------------------------------------*
"! Finds the last request in which io_cts_hot_object is contained. (order from import queue!!!)
"! Result will be returned in cv_trkorr and might be initial if object is not part of any request.
FORM find_last_request_for_object USING io_cts_hot_object TYPE REF TO cl_cts_hot_object_v1
                                  CHANGING cv_trkorr TYPE trkorr.

  DATA cnt TYPE i.
  CLEAR cv_trkorr.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref_obj) WHERE pgmid = 'LIMU' AND object = 'HOTO' AND cts_hot_object = io_cts_hot_object GROUP BY ( cts_hot_object = lr_e071_hot_ref_obj->cts_hot_object count = GROUP SIZE )
                                                                                                                                                                                                 REFERENCE INTO DATA(lr_e071_hot_ref_obj_group).
    cnt = 0.
    LOOP AT GROUP lr_e071_hot_ref_obj_group INTO DATA(ls_e071_hot_ref_obj_group_memb).
      cnt = cnt + 1.
      "last trkorr with this object. maybe there is a better way to access last group member?
      IF cnt = lr_e071_hot_ref_obj_group->count.
        cv_trkorr = ls_e071_hot_ref_obj_group_memb-trkorr.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

"!it_successful_objects used to determine whether logging should be done for this logfile or not.
FORM log_ext_view_creation_result USING it_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                        it_ext_view_results   TYPE lcl_external_viewhandler=>ty_t_ext_view_result_with_hoto.

  DATA: lv_header_written             TYPE abap_bool.

  "First print successful generated views
  PERFORM log_ext_view_result_by_severit USING it_successful_objects it_ext_view_results 'I' '3' '574'.

  "Second print external views where generation ended with warning
  PERFORM log_ext_view_result_by_severit USING it_successful_objects it_ext_view_results 'W' '3' '591'.

  "Second print external views where generation failed
  PERFORM log_ext_view_result_by_severit USING it_successful_objects it_ext_view_results 'E' '2' '575'.

  "Second print external views where generation was aborted
  PERFORM log_ext_view_result_by_severit USING it_successful_objects it_ext_view_results 'A' '2' '575'.

  "Check for unknown severity results! - Should usually not occur
  LOOP AT it_ext_view_results ASSIGNING FIELD-SYMBOL(<ext_view_result>) WHERE hot_object IS BOUND AND max_severity <> 'I' AND max_severity <> 'W'  AND max_severity <> 'E' AND max_severity <> 'A'.
    "if hot_object of external view is part of successful activated objects (filtered perlogfile) print result of external view generation
    IF line_exists( it_successful_objects[ table_line = <ext_view_result>-hot_object ] ).
      CLEAR lv_header_written.
      LOOP AT <ext_view_result>-view_results ASSIGNING FIELD-SYMBOL(<view_result>).
        IF lv_header_written = abap_false.
          PERFORM protokoll USING '2' 'E' '576' space <ext_view_result>-max_severity <view_result>-viewname <view_result>-dbviewname space. " Unbekannter Schweregrad &1 für Alias Ex. View &2 auf DB View &3
          lv_header_written = abap_true.
        ENDIF.
        IF <view_result>-arbgb IS NOT INITIAL.
          PERFORM protokoll_with_ag USING '2' 'E' <view_result>-arbgb <view_result>-msgnr space <view_result>-par1 <view_result>-par2 <view_result>-par3 <view_result>-par4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  "Following view results do not have an object assigned - should usually not occur
  LOOP AT it_ext_view_results ASSIGNING <ext_view_result> WHERE hot_object IS NOT BOUND.
    LOOP AT <ext_view_result>-view_results ASSIGNING <view_result>.
      PERFORM protokoll USING '2' 'E' '590' space <view_result>-viewname <view_result>-dbviewname space space. "   Unbekannter View wurde erzeugt für Synonym Ex. View &1 auf DB View &2
      IF <view_result>-arbgb IS NOT INITIAL.
        PERFORM protokoll_with_ag USING '2' 'E' <view_result>-arbgb <view_result>-msgnr space <view_result>-par1 <view_result>-par2 <view_result>-par3 <view_result>-par4.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

FORM log_ext_view_result_by_severit USING it_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects
                                          it_ext_view_results TYPE lcl_external_viewhandler=>ty_t_ext_view_result_with_hoto
                                          iv_severity TYPE errortyp
                                          iv_level
                                          iv_msgnr.

  DATA: lv_header_written TYPE abap_bool,
        lv_severity       TYPE errortyp,
        lv_longtext       TYPE string.

  "In case of severity = 'I', use space for logging as we always use space
  IF iv_severity = 'I'.
    lv_severity = space.
  ELSE.
    lv_severity = iv_severity.
  ENDIF.

  LOOP AT it_ext_view_results ASSIGNING FIELD-SYMBOL(<ext_view_result>) WHERE hot_object IS BOUND AND max_severity = iv_severity.
    IF lv_header_written = abap_false.
      PERFORM protokoll USING iv_level lv_severity iv_msgnr 'X' space space space space. " Folgende Ex. Views wurden erfolgreich erzeugt(Alias Ex. View -> DBView)
      lv_header_written = abap_true.
    ENDIF.

    "if hot_object of external view is part of successful activated objects (filtered perlogfile) print result of external view generation
    IF line_exists( it_successful_objects[ table_line = <ext_view_result>-hot_object ] ).
      "each HOT object might have several external views and for each of these mappings one or more <view_result> might be returned(therefore group by)
      LOOP AT <ext_view_result>-view_results ASSIGNING FIELD-SYMBOL(<view_result>) GROUP BY ( viewname = <view_result>-viewname dbviewname = <view_result>-dbviewname ).
        "Write header per ABAP-VIEW --> HANA View dependency
        lv_longtext = |{ <view_result>-viewname } -> { <view_result>-dbviewname }|.
        PERFORM protokoll_longtext USING iv_level lv_severity '531' ' ' lv_longtext. " &1&2&3&4

        "Write all messages returned by dictionary if there are any for the current ABAP-view --> HANA-view mapping
        LOOP AT GROUP <view_result> ASSIGNING FIELD-SYMBOL(<view_result_memeber>) WHERE arbgb <> space.
          PERFORM protokoll_with_ag USING iv_level <view_result_memeber>-severity <view_result_memeber>-arbgb <view_result_memeber>-msgnr space
                                          <view_result_memeber>-par1 <view_result_memeber>-par2 <view_result_memeber>-par3 <view_result_memeber>-par4.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

"! Log objects that were not imported by TP/R3trans and thus we do not need to work on them (lockflag NE 2 and NE 3)
"! Works on gt_e071_hot_refs.
FORM log_not_imported_tr_objects.

  DATA: lv_tmptext       TYPE string,
        lv_header_logged TYPE abap_bool.

  "loop over content for each TR to log per log file.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE lockflag <> '2' AND lockflag <> '3' GROUP BY ( tr_korr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile ) REFERENCE INTO DATA(lr_e071_hot_ref_group).
    CLEAR lv_header_logged.

    LOOP AT GROUP lr_e071_hot_ref_group REFERENCE INTO DATA(lr_e071_hot_ref_group_member).
      IF lr_e071_hot_ref_group_member->pgmid = 'R3TR' AND lr_e071_hot_ref_group_member->object = 'HOTA'.
        IF lv_header_logged = abap_false.
          PERFORM protokoll USING '4' ' ' '578' 'X' space space space space. "  Folgende Transportobj. wurden nicht importiert, daher nicht bearbeitet:
          lv_header_logged = abap_true.
        ENDIF.

        lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }|.
        PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
        CONTINUE.
      ENDIF.

      "Skip log of LIMU HOTPs / HOTOs if R3TR of these LIMUs is part of transport request as well (expanded R3TR)
      IF lr_e071_hot_ref_group_member->pgmid = 'LIMU'
          AND line_exists( gt_e071_hot_refs[ trbat_order = lr_e071_hot_ref_group_member->trbat_order trkorr = lr_e071_hot_ref_group_member->trkorr pgmid = 'R3TR' object = 'HOTA' obj_name = lr_e071_hot_ref_group_member->obj_name(40) ] ).
        CONTINUE.
      ENDIF.

      IF lr_e071_hot_ref_group_member->pgmid = 'LIMU' AND ( lr_e071_hot_ref_group_member->object = 'HOTP' OR lr_e071_hot_ref_group_member->object = 'HOTO' ).
        IF lv_header_logged = abap_false.
          PERFORM protokoll USING '4' ' ' '578' 'X' space space space space. "  Folgende Transportobj. wurden nicht importiert, daher nicht bearbeitet:
          lv_header_logged = abap_true.
        ENDIF.

        lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }|.
        PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
      ENDIF.
    ENDLOOP.

    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.

ENDFORM.

"! Log objects that are already imported successfully (lockflag is already 3)
"! Works on gt_e071_hot_refs.
FORM log_already_imprted_tr_objects.

  DATA: lv_tmptext       TYPE string,
        lv_header_logged TYPE abap_bool.

  "loop over content for each TR to log per log file.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE lockflag = '3' GROUP BY ( tr_korr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile ) REFERENCE INTO DATA(lr_e071_hot_ref_group).
    CLEAR lv_header_logged.

    LOOP AT GROUP lr_e071_hot_ref_group REFERENCE INTO DATA(lr_e071_hot_ref_group_member).
      IF lr_e071_hot_ref_group_member->pgmid = 'R3TR' AND lr_e071_hot_ref_group_member->object = 'HOTA'.
        IF lv_header_logged = abap_false.
          PERFORM protokoll USING '4' ' ' '577' 'X' space space space space. "  Folgende Transportobjekte wurden bereits früher erfolgreich bearbeitet:
          lv_header_logged = abap_true.
        ENDIF.

        lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }|.
        PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
        CONTINUE.
      ENDIF.

      "Skip log of LIMU HOTPs / HOTOs if R3TR of these LIMUs is part of transport request as well (expanded R3TR)
      IF lr_e071_hot_ref_group_member->pgmid = 'LIMU'
          AND line_exists( gt_e071_hot_refs[ trbat_order = lr_e071_hot_ref_group_member->trbat_order trkorr = lr_e071_hot_ref_group_member->trkorr pgmid = 'R3TR' object = 'HOTA' obj_name = lr_e071_hot_ref_group_member->obj_name(40) ] ).
        CONTINUE.
      ENDIF.

      "check if it is HOTO/HOTP and check if the same HOTO/HOTP is not part of in another request with lockflag = 2 and thus will be processed by hot_deployer
      IF lr_e071_hot_ref_group_member->pgmid = 'LIMU' AND ( lr_e071_hot_ref_group_member->object = 'HOTP' OR lr_e071_hot_ref_group_member->object = 'HOTO' )
            AND NOT line_exists( gt_e071_hot_refs[ pgmid = lr_e071_hot_ref_group_member->pgmid object = lr_e071_hot_ref_group_member->object obj_name = lr_e071_hot_ref_group_member->obj_name lockflag = '2' ] ).
        IF lv_header_logged = abap_false.
          PERFORM protokoll USING '4' ' ' '577' 'X' space space space space. "  Folgende Transportobjekte wurden bereits früher erfolgreich bearbeitet:
          lv_header_logged = abap_true.
        ENDIF.

        lv_tmptext = |{ lr_e071_hot_ref_group_member->pgmid } { lr_e071_hot_ref_group_member->object } { lr_e071_hot_ref_group_member->obj_name }|.
        PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
      ENDIF.
    ENDLOOP.

    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.
ENDFORM.

"! Check the passed packages for switch settings.<br/>
"! In case a switch is off, the package is logged, it is removed from ct_packages and set to successfully deployed in e071.
FORM check_and_log_switches_packags USING i_switch_framework TYPE REF TO lcl_switch_framework_accessor
                                          it_logfiles_packages TYPE tt_logfile
                                          iv_abap_status       TYPE cts_hot_abap_status
                                    CHANGING ct_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                                             ct_e071_hot_refs TYPE tt_e071_hot_refs.

  DATA: lt_switch_result        TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package,
        lr_switch_result        TYPE REF TO lcl_switch_framework_accessor=>ty_switch_id_devc_hot_package,
        lv_tmptext              TYPE string,  "must be string casted because if integer is used the number is printed with leading spaces when writing log with no_condense
        lt_log_mess_packages    TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages, "a bit strange to use this data type, but with this we can reuse forms update_e071_hotp and filter_packages_for_logfile
        ls_log_mess_package     TYPE cl_cts_hot_hana_connector=>ty_log_message_package, "a bit strange to use this data type, but with this we can reuse form filter_packages_for_logfile
        lt_filtered_packages    TYPE tt_dref_log_message_package,
        lv_switchheader_written TYPE abap_bool. "a bit strange to use this data type, but with this we can reuse form filter_packages_for_logfile

  "get all switches that are OFF
  i_switch_framework->get_off_switches_for_packages( EXPORTING i_hot_packages = ct_packages
                                                     IMPORTING e_switch_id_hot_packages = lt_switch_result ).

  IF lt_switch_result IS INITIAL.
    RETURN.
  ENDIF.

* update E071 and log packages with OFF switches in correct logfile
  "1. build table with all package references
  LOOP AT lt_switch_result REFERENCE INTO lr_switch_result GROUP BY lr_switch_result->hot_package.
    ls_log_mess_package-cts_hot_package = lr_switch_result->hot_package.
    APPEND ls_log_mess_package TO lt_log_mess_packages.

    IF iv_abap_status = 'I'.
      gr_cts_hot_db_access->activate_package_cwb_snote( lr_switch_result->hot_package->abap_hana_package_id ).
    ENDIF.

    "remove package with OFF switch from ct_packages
    DELETE TABLE ct_packages FROM lr_switch_result->hot_package.
  ENDLOOP.

  " 2. update e071
  PERFORM update_e071_hotp USING lt_log_mess_packages CHANGING ct_e071_hot_refs.

  "3. log per logfile
  LOOP AT it_logfiles_packages INTO DATA(lv_logfile).
    FREE lt_filtered_packages.
    PERFORM filter_packages_for_logfile USING lt_log_mess_packages lv_logfile CHANGING lt_filtered_packages.

    IF lt_filtered_packages IS INITIAL.
      CONTINUE.
    ENDIF.

    "print header line: 'Nicht deployte Pakete, da per Schalter ausgeschaltet: &1'
    lv_tmptext = |{ lines( lt_filtered_packages ) }|.
    PERFORM protokoll USING '3' 'W' '580' space lv_tmptext space space space.

    "print switches(with devclass) and packages not deployed because of switch. (1 switch can have n devclasses and 1 devclass can have n packages)
    LOOP AT lt_switch_result REFERENCE INTO lr_switch_result GROUP BY ( switch_id = lr_switch_result->switch_id devclass = lr_switch_result->devclass ) REFERENCE INTO DATA(lr_switch_group).
      CLEAR lv_switchheader_written.
      LOOP AT GROUP lr_switch_group REFERENCE INTO DATA(lr_switch_group_member).
        IF line_exists( lt_filtered_packages[ table_line->cts_hot_package = lr_switch_group_member->hot_package ] ).
          IF lv_switchheader_written = abap_false.
            PERFORM protokoll USING '4' 'W' '581' space lr_switch_group->switch_id lr_switch_group->devclass space space. " Folgende Pakete wurden nicht deployt, da Schalter &1(&2) ausgeschaltet ist:
            lv_switchheader_written = abap_true.
          ENDIF.
          PERFORM protokoll_longtext USING '4' 'W' '531' space lr_switch_group_member->hot_package->hana_package_id. "&1&2&3&4
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Leerzeile als Abschluss
    PERFORM protokoll USING '4' space '507' ' ' space  space space space. "empty line

    PERFORM write_prot_no_condense_to_file USING lv_logfile.
  ENDLOOP.
ENDFORM.

"! Check the passed objects for switch settings.<br/>
"! In case a switch is off, the object is logged, it is removed from ct_objects and set to successfully deployed in e071
FORM check_and_log_switches_objects USING i_switch_framework TYPE REF TO lcl_switch_framework_accessor
                                          iv_abap_status       TYPE cts_hot_abap_status
                                    CHANGING ct_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
                                             ct_e071_hot_refs TYPE tt_e071_hot_refs.

  DATA: lt_switch_result        TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object,
        lr_switch_result        TYPE REF TO lcl_switch_framework_accessor=>ty_switch_id_devc_hot_object,
        lv_tmptext              TYPE string,  "must be string casted because if integer is used the number is printed with leading spaces when writing log with no_condense
        lt_objects_switch_off   TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects, "a bit strange to use this data type, but with this we can reuse forms update_e071_hoto and filter_objects_for_logfile
        lt_filtered_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        lv_switchheader_written TYPE abap_bool. "a bit strange to use this data type, but with this we can reuse form filter_objects_for_logfile

  i_switch_framework->get_off_switches_for_objects( EXPORTING i_hot_objects = ct_objects
                                                    IMPORTING e_switch_id_hot_objects = lt_switch_result ).

  IF lt_switch_result IS INITIAL.
    RETURN.
  ENDIF.

* update E071 and log packages with OFF switches in correct logfile
  "1. build table with all object references
  LOOP AT lt_switch_result REFERENCE INTO lr_switch_result GROUP BY lr_switch_result->hot_object.
    INSERT lr_switch_result->hot_object INTO TABLE lt_objects_switch_off.

    IF iv_abap_status = 'I'.
      gr_cts_hot_db_access->activate_object_cwb_snote( lr_switch_result->hot_object ).
    ENDIF.

    "remove object with OFF switch from ct_objects
    DELETE TABLE ct_objects FROM lr_switch_result->hot_object.
  ENDLOOP.

  " 2. update e071
  PERFORM update_e071_hoto USING lt_objects_switch_off CHANGING ct_e071_hot_refs.

  "3. loop over all log files for which HOTO was processed and log if prework is missing for some objects of this request
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE pgmid = 'LIMU' AND object = 'HOTO' AND cts_hot_object IS BOUND GROUP BY ( trkorr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile )
                                                                                                                                     REFERENCE INTO DATA(lr_e071_hot_ref_group).
    FREE: lt_filtered_objects.
    PERFORM filter_objects_for_logfile USING 'LIMU' lt_objects_switch_off lr_e071_hot_ref_group->trkorr CHANGING lt_filtered_objects.

    IF lt_filtered_objects IS INITIAL.
      CONTINUE.
    ENDIF.

    "print header line: 'Nicht deployte Objekte, da per Schalter ausgeschaltet: &1'
    lv_tmptext = |{ lines( lt_filtered_objects ) }|.
    PERFORM protokoll USING '3' 'W' '582' space lv_tmptext space space space.

    "print switches(with devclass) and objects not deployed because of switch. (1 switch can have n devclasses and 1 devclass can have n objects)
    LOOP AT lt_switch_result REFERENCE INTO lr_switch_result GROUP BY ( switch_id = lr_switch_result->switch_id devclass = lr_switch_result->devclass ) REFERENCE INTO DATA(lr_switch_group).
      CLEAR lv_switchheader_written.
      LOOP AT GROUP lr_switch_group REFERENCE INTO DATA(lr_switch_group_member).
        IF line_exists( lt_filtered_objects[ table_line = lr_switch_group_member->hot_object ] ).
          IF lv_switchheader_written = abap_false.
            PERFORM protokoll USING '4' 'W' '583' space lr_switch_group->switch_id lr_switch_group->devclass space space. " Folgende Objekte wurden nicht deployt, da Schalter &1(&2) ausgeschaltet ist:
            lv_switchheader_written = abap_true.
          ENDIF.
          lv_tmptext = |{ lr_switch_group_member->hot_object->hana_object_name }.{ lr_switch_group_member->hot_object->hana_object_suffix } ({ lr_switch_group_member->hot_object->hana_package_id })|.
          PERFORM protokoll_longtext USING '4' 'W' '531' space lv_tmptext. "&1&2&3&4
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Leerzeile als Abschluss
    PERFORM protokoll USING '4' space '507' ' ' space  space space space. "empty line

    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
  ENDLOOP.
ENDFORM.

"! Check the passed packages for prework done settings.<br/>
"! In case a prework is not done, the package is logged, it is removed from ct_packages and set to successfully deployed in e071.
FORM check_and_log_prework_packags USING    iv_abap_status       TYPE cts_hot_abap_status
                                            it_logfiles_packages TYPE tt_logfile
                                   CHANGING ct_packages          TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                                            ct_e071_hot_refs     TYPE tt_e071_hot_refs.

  DATA: lt_package_ids       TYPE STANDARD TABLE OF cts_hot_package_id,
        lv_package_id        TYPE cts_hot_package_id,
        lt_log_mess_packages TYPE cl_cts_hot_hana_connector=>ty_log_messages_packages, "a bit strange to use this data type, but with this we can reuse forms update_e071_hotp and filter_packages_for_logfile
        ls_log_mess_package  TYPE cl_cts_hot_hana_connector=>ty_log_message_package, "a bit strange to use this data type, but with this we can reuse form filter_packages_for_logfile
        lt_filtered_packages TYPE tt_dref_log_message_package, "a bit strange to use this data type, but with this we can reuse form filter_packages_for_logfile
        lr_filtered_package  TYPE ty_dref_log_message_package, "a bit strange to use this data type, but with this we can reuse form filter_packages_for_logfile
        lv_tmptext           TYPE string,  "must be string casted because if integer is used the number is printed with leading spaces when writing log with no_condense
        lr_hot_package       TYPE REF TO cl_cts_hot_package.

  IF ct_packages IS INITIAL.
    RETURN.
  ENDIF.

  "collect all package names for SQL statement
  LOOP AT ct_packages INTO lr_hot_package.
    APPEND lr_hot_package->abap_hana_package_id TO lt_package_ids.
  ENDLOOP.

  "check prework done settings in DB (select returns all packages for which prework is missing!)
  SELECT a~abap_hana_package_id FROM cts_hot_package AS a LEFT OUTER JOIN cts_hot_prework AS b ON a~abap_hana_package_id = b~abap_hana_package_id
                                FOR ALL ENTRIES IN @lt_package_ids WHERE a~abap_hana_package_id = @lt_package_ids-table_line
                                                                         AND a~abap_status = @iv_abap_status AND a~hot_activation_mode = 'P'
                                                                         AND a~hot_status NOT IN ( 'D', 'Z' )
                                                                         AND ( b~prework_done <> 'X' OR b~prework_done IS NULL )
                                                                         INTO TABLE @lt_package_ids.

  IF lt_package_ids IS INITIAL. "no entries found for which prework done is missing, all can be (un)deployed.
    RETURN.
  ENDIF.

* update E071 and log packages with missing prework in correct logfile
  "1. build table with all package references
  LOOP AT lt_package_ids INTO lv_package_id.
    lr_hot_package = ct_packages[ table_line->abap_hana_package_id = lv_package_id ].

    IF iv_abap_status = 'I'.
      gr_cts_hot_db_access->activate_package_cwb_snote( lv_package_id ).
    ENDIF.

    ls_log_mess_package-cts_hot_package = lr_hot_package.
    APPEND ls_log_mess_package TO lt_log_mess_packages.

    "remove package with missing prework from ct_packages
    DELETE TABLE ct_packages FROM lr_hot_package.
  ENDLOOP.

  "2. update e071
  PERFORM update_e071_hotp USING lt_log_mess_packages CHANGING ct_e071_hot_refs.

  "3. log per logfile
  LOOP AT it_logfiles_packages INTO DATA(lv_logfile).
    FREE lt_filtered_packages.
    PERFORM filter_packages_for_logfile USING lt_log_mess_packages lv_logfile CHANGING lt_filtered_packages.

    IF lt_filtered_packages IS INITIAL.
      CONTINUE.
    ENDIF.

    "print header line: '  Nicht deployte Pakete, da Vorarbeit noch nicht erledigt: &1'
    lv_tmptext = |{ lines( lt_filtered_packages ) }|.
    PERFORM protokoll USING '3' 'W' '584' space lv_tmptext space space space.

    "print packages not deployed because prework is missing:
    PERFORM protokoll USING '4' 'W' '585' space space space space space. "Folgende Pakete wurden nicht deployt, da Vorarbeit fehlt:

    LOOP AT lt_filtered_packages INTO lr_filtered_package.
      PERFORM protokoll_longtext USING '4' 'W' '531' space lr_filtered_package->cts_hot_package->hana_package_id. "&1&2&3&4
    ENDLOOP.

    "Leerzeile als Abschluss
    PERFORM protokoll USING '4' space '507' ' ' space  space space space. "empty line

    PERFORM write_prot_no_condense_to_file USING lv_logfile.
  ENDLOOP.
ENDFORM.

"! Check the passed objects prework done settings.<br/>
"! In case a prework is not done, the object is logged, it is removed from ct_objects,
"! removed from ct_lang_objects and set to successfully deployed in e071
FORM check_and_log_prework_objects  USING    iv_abap_status   TYPE cts_hot_abap_status
                                    CHANGING ct_objects       TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
                                             ct_lang_objects  TYPE cl_cts_hot_hana_connector=>ty_text_deploy_inputs
                                             ct_e071_hot_refs TYPE tt_e071_hot_refs.

  DATA: lt_package_ids             TYPE SORTED TABLE OF cts_hot_package_id WITH UNIQUE DEFAULT KEY,
        lv_package_id              TYPE cts_hot_package_id,
        lv_tmptext                 TYPE string,  "must be string casted because if integer is used the number is printed with leading spaces when writing log with no_condense
        lr_hot_object              TYPE REF TO cl_cts_hot_object_v1,
        lr_lang_object             TYPE REF TO cl_cts_hot_hana_connector=>ty_text_deploy_input,
        lt_hot_objects             TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
        lt_objects_missing_prework TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects, "a bit strange to use this data type, but with this we can reuse forms update_e071_hoto and filter_objects_for_logfile
        lt_filtered_objects        TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects, "a bit strange to use this data type, but with this we can reuse form filter_objects_for_logfile,
        lt_text_deploy_results     TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results. "a bit strange to use this data type, but with this we can reuse some existing forms

  IF ct_objects IS INITIAL AND ct_lang_objects IS INITIAL.
    RETURN.
  ENDIF.

  "create local table with all hot objects (LIMU and LANG HOTO)
  lt_hot_objects = ct_objects.
  LOOP AT ct_lang_objects REFERENCE INTO lr_lang_object.
    INSERT lr_lang_object->cts_hot_object INTO TABLE lt_hot_objects.
  ENDLOOP.

  "collect all package names for SQL statement
  LOOP AT lt_hot_objects INTO lr_hot_object.
    INSERT lr_hot_object->abap_hana_package_id INTO TABLE lt_package_ids.
  ENDLOOP.

  "check prework done settings in DB (select returns all packages for which prework is missing!)
  SELECT a~abap_hana_package_id FROM cts_hot_package AS a LEFT OUTER JOIN cts_hot_prework AS b ON a~abap_hana_package_id = b~abap_hana_package_id
                                FOR ALL ENTRIES IN @lt_package_ids WHERE a~abap_hana_package_id = @lt_package_ids-table_line
                                                                         AND a~abap_status = 'A' AND a~hot_activation_mode = 'P'
                                                                         AND a~hot_status NOT IN ( 'D', 'Z' )
                                                                         AND ( b~prework_done <> 'X'  OR b~prework_done IS NULL )
                                                                         INTO TABLE @lt_package_ids.

  IF lt_package_ids IS INITIAL. "no entries for which prework done is missing found.
    RETURN.
  ENDIF.

* update E071 and log packages with missing prework in correct logfile
  "1. build up required tables with all objects for which packages have no prework yet
  LOOP AT lt_package_ids INTO lv_package_id. "##todo performance???
    LOOP AT lt_hot_objects INTO lr_hot_object WHERE table_line->abap_hana_package_id = lv_package_id.

      IF iv_abap_status = 'I'.
        gr_cts_hot_db_access->activate_object_cwb_snote( lr_hot_object ).
      ENDIF.

      INSERT lr_hot_object INTO TABLE lt_objects_missing_prework.

      "remove object with prework missing from ct_objects
      DELETE TABLE ct_objects FROM lr_hot_object.

      "remove object with prework missing from ct_lang_objects
      DELETE TABLE ct_lang_objects WITH TABLE KEY cts_hot_object = lr_hot_object.
    ENDLOOP.
  ENDLOOP.

  "2. update e071 for LIMU HOTO and LANG HOTO
  PERFORM update_e071_hoto USING lt_objects_missing_prework CHANGING ct_e071_hot_refs.
  lt_text_deploy_results = VALUE #( FOR line IN lt_objects_missing_prework ( cts_hot_object = line ) ).
  PERFORM update_e071_lang_hoto USING lt_text_deploy_results CHANGING ct_e071_hot_refs.

  "3. loop over all log files for which HOTO was processed and log if prework is missing for some objects of this request
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE ( pgmid = 'LIMU' OR pgmid = 'LANG' ) AND object = 'HOTO' AND cts_hot_object IS BOUND
                                                                GROUP BY ( trkorr = lr_e071_hot_ref->trkorr logfile = lr_e071_hot_ref->logfile )
                                                                REFERENCE INTO DATA(lr_e071_hot_ref_group).
    FREE: lt_filtered_objects.

    PERFORM filter_objects_for_logfile USING 'LIMU' lt_objects_missing_prework lr_e071_hot_ref_group->trkorr CHANGING lt_filtered_objects.
    PERFORM filter_objects_for_logfile USING 'LANG' lt_objects_missing_prework lr_e071_hot_ref_group->trkorr CHANGING lt_filtered_objects.

    IF lt_filtered_objects IS INITIAL.
      CONTINUE. "nothing to log in this file
    ENDIF.

    "print header line: 'Nicht deployte Objekte, da Vorarbeit noch nicht erledigt: &1'
    lv_tmptext = |{ lines( lt_filtered_objects ) }|.
    PERFORM protokoll USING '3' 'W' '586' space lv_tmptext space space space.

    "print objects not deployed because of prework missing on package level:
    PERFORM protokoll USING '4' 'W' '587' space space space space space. "Folgende Objekte wurden nicht deployt, da Vorarbeit fehlt:

    SORT lt_filtered_objects BY table_line->hana_package_id table_line->hana_object_name table_line->hana_object_suffix ASCENDING.

    LOOP AT lt_filtered_objects INTO lr_hot_object.
      lv_tmptext = |{ lr_hot_object->hana_object_name }.{ lr_hot_object->hana_object_suffix } ({ lr_hot_object->hana_package_id })|.
      PERFORM protokoll_longtext USING '4' 'W' '531' space lv_tmptext. "&1&2&3&4
    ENDLOOP.

    PERFORM write_prot_no_condense_to_file USING lr_e071_hot_ref_group->logfile.
  ENDLOOP.
ENDFORM.

"! *** EXTERNALLY USED FORM ***<br/><br/>
"!
"! Triggers deployment for passed packages and objects taking abap_status into account for SNote/CWB use case as well as API case.
"!
"! @parameter it_packages            | List of packages to be deployed (can also contain to be deleted packages)
"! @parameter it_objects             | List of objects to be deployed
"! @parameter iv_abap_status         | abap_status to use when reading packages/objects data from HTA repository.
"! @parameter ct_successful_packages | Returning the packages that have been deployed successfully<br/>
"!                                     (successful means here either really deployed or nothing done or skipped because of switches or prework missing)
"! @parameter ct_successful_objects  | Returning the packages that have been deployed successfully<br/>
"!                                     (successful means here either really deployed or nothing done or skipped because of switches or prework missing)
"! @parameter ct_log                 | Log messages created during deployment
"! @parameter cv_max_severity        | max severity reached during deployment: ' ' for success, 'W' for warning, 'E' for Error, 'A' for Abnormal termination
FORM deploy USING     it_packages            TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                      it_objects             TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
                      iv_abap_status         TYPE cts_hot_abap_status
            CHANGING  ct_successful_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
                      ct_successful_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
                      ct_log                 TYPE tt_sprot_u
                      cv_max_severity        TYPE sprot_u-severity.

  DATA: ls_e071_hot_ref TYPE ty_e071_hot_refs,
        lr_hot_package  TYPE REF TO cl_cts_hot_package,
        lr_hot_object   TYPE REF TO cl_cts_hot_object_v1.

  gv_external_call = abap_true.
  "in API mode we do only have one request and thus need only one "memory" logger and no list of logger...
  "Do not use memory logger because we need special flush in here
  gr_logger_list = NEW lcl_cts_hot_logger_rdd( ).
  gr_logger_list->set_msg_id( 'SCTS_HOT' ).

  FREE gt_e071_hot_refs.
  FREE gt_xmsg.
  CLEAR gv_max_severity.

* prepare global table gt_e071_hot_refs (currently needed for all logging...) :-(
  LOOP AT it_packages INTO lr_hot_package.
    CLEAR ls_e071_hot_ref.
    ls_e071_hot_ref-pgmid = 'LIMU'.
    ls_e071_hot_ref-object = 'HOTP'.
    ls_e071_hot_ref-obj_name = lr_hot_package->abap_hana_package_id.
    ls_e071_hot_ref-lockflag = '2'. "simulate successful main import
    ls_e071_hot_ref-logger = gr_logger_list.
    ls_e071_hot_ref-logfile = 'local'. "required for HOTPs because of current implementation of form log_package_results
    ls_e071_hot_ref-cts_hot_package = lr_hot_package.
    INSERT ls_e071_hot_ref INTO TABLE gt_e071_hot_refs.
  ENDLOOP.

  LOOP AT it_objects INTO lr_hot_object.
    CLEAR ls_e071_hot_ref.
    ls_e071_hot_ref-pgmid = 'LIMU'.
    ls_e071_hot_ref-object = 'HOTO'.
    ls_e071_hot_ref-obj_name = lr_hot_object->transport_object_name.
    ls_e071_hot_ref-lockflag = '2'. "simulate successful main import
    ls_e071_hot_ref-logger = gr_logger_list.
    ls_e071_hot_ref-logfile = 'local'. "must be same as for HOTPs because of some logging
    ls_e071_hot_ref-cts_hot_object = lr_hot_object.
    INSERT ls_e071_hot_ref INTO TABLE gt_e071_hot_refs.
  ENDLOOP.

*prepare log header for external activation
  PERFORM protokoll USING '1' space '507' 'X' space space space space. "empty line
  PERFORM log_headerline_with_timestamp USING '1' '501' space. "Begin deployment to HANA repository: &1
  PERFORM protokoll USING '1' space '507' space space space space space. "empty line

  PERFORM execute_hana_deployment USING iv_abap_status
                                  CHANGING ct_successful_packages ct_successful_objects.

  PERFORM protokoll USING '1' space '507' 'X' space space space space. "empty line
  PERFORM log_headerline_with_timestamp USING '1' '503' ' '. "End deployment to HANA repository: &1
  PERFORM protokoll USING '1' space '507' space space space space space. "empty line

* map all successful packages/objects to changing tables.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE lockflag = '3'.
    IF lr_e071_hot_ref->cts_hot_object IS BOUND.
      APPEND lr_e071_hot_ref->cts_hot_object TO ct_successful_objects.
    ENDIF.
    IF lr_e071_hot_ref->cts_hot_package IS BOUND.
      APPEND lr_e071_hot_ref->cts_hot_package TO ct_successful_packages.
    ENDIF.
  ENDLOOP.

  ct_log = gt_xmsg.
  cv_max_severity = gv_max_severity.
ENDFORM.


FORM deploy_all_failed USING iv_deploy_mode TYPE char1.
  DATA: lr_hot_logger         TYPE REF TO if_cts_hot_logger,
        lv_logfile            TYPE trfile,
        lv_tp_supports_step_5 TYPE abap_bool.

  PERFORM create_redeploy_failed_log CHANGING lr_hot_logger
                                              lv_logfile
                                              lv_tp_supports_step_5.

  DATA(lv_repo) = COND #( WHEN p_deplf = 'X' OR p_deplf = 'R' THEN 'X' ELSE '-' ).
  DATA(lv_hdi) = COND #( WHEN p_deplf = 'X' OR p_deplf = 'H' THEN 'X' ELSE '-' ).
  lr_hot_logger->message( iv_msg_nr = '621'
                          iv_level = if_cts_hot_logger=>co_level_2
                          iv_var1 = CONV #( lv_hdi )
                          iv_var2 = CONV #( lv_repo ) ). "Redeployment fehlerhafter Objekte gestartet für HDI=&1,HANA-Repository=&2
  lr_hot_logger->flush( ).

  IF p_deplf = 'X'.
    "This is deploy all failed use case (triggered after each transport)
    PERFORM deploy_all_failed_hdi USING lr_hot_logger.
    PERFORM deploy_all_failed_repo USING lv_logfile
                                         lv_tp_supports_step_5.
  ELSEIF p_deplf = 'R'.
    PERFORM deploy_all_failed_repo USING lv_logfile
                                         lv_tp_supports_step_5.
  ELSEIF p_deplf = 'H'.
    PERFORM deploy_all_failed_hdi USING lr_hot_logger.
  ENDIF.

  PERFORM close_redeploy_failed_log USING lv_logfile
                                          lv_tp_supports_step_5.
ENDFORM.

FORM create_redeploy_failed_log CHANGING cr_hot_logger TYPE REF TO if_cts_hot_logger
                                         cv_logfile TYPE trfile
                                         cv_tp_supports_step_5 TYPE abap_bool.
  .
  DATA: lv_logname   TYPE ctslogname,
        lt_tp_params TYPE STANDARD TABLE OF tpparams,
        lv_tp_hota   TYPE trtppvalue.

  lv_logname = |:D:T:S:log:F:5{ sy-datum+2 }.{ sy-sysid }|.

  "check if tp knows step 5 or not and can copy files from tmp to log
  CALL FUNCTION 'TRINT_TP_INTERFACE'
    EXPORTING
      iv_tp_command  = 'GETCAPABILITIES'
      iv_system_name = CONV sysname( sy-sysid )
    TABLES
      tt_params      = lt_tp_params
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0.
    "ignore error and directly write to log directory
    PERFORM protokoll_with_ag USING '3' 'W' sy-msgid sy-msgno space sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    "check tp version if tmp dir can be used
    lv_tp_hota = VALUE #( lt_tp_params[ name = 'TP_HANA_OBJECT_TRANSPORT' ]-value DEFAULT lv_tp_hota ).
    IF lv_tp_hota IS NOT INITIAL AND ( CONV i( lv_tp_hota ) >= 2 ).
      cv_tp_supports_step_5 = abap_true.
      lv_logname = |:D:T:S:tmp:F:5{ sy-datum+2 }.{ sy-sysid }|.
    ELSE.
      cv_tp_supports_step_5 = abap_false.
    ENDIF.
  ENDIF.

  DATA(lr_hot_logger) = cl_cts_hot_logger_tr=>create_instance( iv_trkorr = 'UNKNOWN'
                                                               iv_logname = lv_logname ).

  cv_logfile = lr_hot_logger->get_logfile( ).
  cr_hot_logger = lr_hot_logger.
ENDFORM.

FORM close_redeploy_failed_log USING iv_logfile TYPE trfile
                                     iv_tp_supports_step_5 TYPE abap_bool.

  PERFORM write_prot_no_condense_to_file USING iv_logfile.

* 5. call tp to copy file from tmp to log directory.
  IF iv_tp_supports_step_5 = abap_true.
    ls_trbat-trkorr = 'HEADER'.
    ls_trbat-function = '5'.
    ls_trbat-retcode = 'F'.
    ls_trbat-timestmp = sy-datum && sy-uzeit.
    MODIFY trbat FROM ls_trbat.

    CALL FUNCTION 'TRINT_TP_INTERFACE'
      EXPORTING
        iv_tp_command  = 'GETPROTS'     " tp Kommando (SHOWBUFFER,IMPORT,...)
        iv_system_name = CONV sysname( sy-sysid )
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      PERFORM protokoll_with_ag USING '3' 'W' sy-msgid sy-msgno space sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM write_prot_no_condense_to_file USING iv_logfile.

      DELETE trbat FROM ls_trbat. "usually tp would do, but in error csae we better delete
    ENDIF.
  ENDIF.
ENDFORM.

"! Deploys all objects that are currently marked as failed (HOT_STATUS = 'E' or 'Z').<br/>
"! Log is either written directly to log directory with file name in the format 5&lt;yyyyMMdd&gt;.&lt;sid&gt;
"! or to tmp directory if TP is new enough and knows step 5 to copy log file
FORM deploy_all_failed_repo USING iv_logfile TYPE trfile
                                  iv_tp_supports_step_5 TYPE abap_bool.
  DATA: lv_objname      TYPE cts_hot_object_name,
        ls_e071_hot_ref TYPE ty_e071_hot_refs,
        lr_hot_package  TYPE REF TO cl_cts_hot_package,
        lr_hot_object   TYPE REF TO cl_cts_hot_object_v1.

* 0. Do not execute redeploy failed for HANA Repo objects in View Layer systems
  DATA(lr_external_persistency) = NEW lcl_external_persistency( ).

  IF lr_external_persistency->is_view_layer_system( ).
    PERFORM log_headerline_with_timestamp USING '2' '501' 'X'. "Begin deployment to HANA repository: &1
    IF gv_external_call = abap_true.
      PERFORM protokoll USING '3' 'E' '632' space space space space space. "HANA Repository Content cannot be imported into view layer system
    ELSE.
      PERFORM protokoll USING '3' 'W' '632' space space space space space. "HANA Repository Content cannot be imported into view layer system
    ENDIF.
    PERFORM log_headerline_with_timestamp USING '2' '503' ' '. "End deployment to HANA repository: &1
    PERFORM write_prot_no_condense_to_file USING iv_logfile.

    RETURN.
  ENDIF.

* 1. find all broken objects
  FREE gt_e071_hot_refs.
  FREE gt_xmsg.
  CLEAR gv_max_severity.

  SELECT abap_hana_package_id FROM cts_hot_package INTO TABLE @DATA(lt_broken_packages)
      WHERE ( hot_status = @if_cts_hot_db_access=>co_hot_status_deploy_error OR hot_status = @if_cts_hot_db_access=>co_hot_status_delete_error )
            AND abap_status = 'A'.

  SELECT abap_hana_package_id, abap_hana_object_name_suffix FROM cts_hot_object INTO TABLE @DATA(lt_broken_objects)
      WHERE ( hot_status = @if_cts_hot_db_access=>co_hot_status_deploy_error OR hot_status = @if_cts_hot_db_access=>co_hot_status_delete_error )
            AND abap_status = 'A'.                      "#EC CI_NOFIRST

  IF lt_broken_objects IS INITIAL AND lt_broken_packages IS INITIAL.
    PERFORM protokoll USING '3' space '629' 'X' space space space space. "Keine fehlerhaften HANA-Repository-Objekte für Redeployment vorhanden
    RETURN. "nothing to do.
  ENDIF.

* 3. Fill global table with packages/objects for deployment
  LOOP AT lt_broken_packages REFERENCE INTO DATA(lr_package).
    lr_hot_package = cl_cts_hot_package=>create_instance_from_objname( lr_package->abap_hana_package_id ).
    CLEAR ls_e071_hot_ref.
    ls_e071_hot_ref-trkorr = 'UNKNOWN'.
    ls_e071_hot_ref-pgmid = 'LIMU'.
    ls_e071_hot_ref-object = 'HOTP'.
    ls_e071_hot_ref-obj_name = lr_hot_package->abap_hana_package_id.
    ls_e071_hot_ref-lockflag = '2'. "simulate successful main import
    ls_e071_hot_ref-logfile = iv_logfile.
    ls_e071_hot_ref-cts_hot_package = lr_hot_package.
    INSERT ls_e071_hot_ref INTO TABLE gt_e071_hot_refs.
  ENDLOOP.

  LOOP AT lt_broken_objects REFERENCE INTO DATA(lr_object).
    lv_objname = lr_object->abap_hana_package_id.
    lv_objname+40(70) = lr_object->abap_hana_object_name_suffix.

    lr_hot_object = cl_cts_hot_object_v1=>create_instance_from_objname( lv_objname ).
    CLEAR ls_e071_hot_ref.
    ls_e071_hot_ref-trkorr = 'UNKNOWN'.
    ls_e071_hot_ref-pgmid = 'LIMU'.
    ls_e071_hot_ref-object = 'HOTO'.
    ls_e071_hot_ref-obj_name = lr_hot_object->transport_object_name.
    ls_e071_hot_ref-lockflag = '2'. "simulate successful main import
    ls_e071_hot_ref-logfile = iv_logfile.
    ls_e071_hot_ref-cts_hot_object = lr_hot_object.
    INSERT ls_e071_hot_ref INTO TABLE gt_e071_hot_refs.
  ENDLOOP.

  IF iv_tp_supports_step_5 = abap_false.
    " prepare log header because not started by TP
    PERFORM protokoll USING '1' space '507' space space space space space. "empty line
    PERFORM log_headerline_with_timestamp USING '1' '501' space. "Begin deployment to HANA repository: &1
    PERFORM protokoll USING '1' space '507' space space space space space. "empty line
    PERFORM write_prot_no_condense_to_file USING iv_logfile.
  ENDIF.

* 4. Execute deployment
  DATA: lt_skipped_api_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list, "packages passed by API that do not exist in HTA
        lt_skipped_api_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

  PERFORM execute_hana_deployment USING 'A'
                                  CHANGING lt_skipped_api_packages lt_skipped_api_objects.

  IF iv_tp_supports_step_5 = abap_false.
    " finish log header because not started by TP (TODO remove if copy is done by TP)
    PERFORM protokoll USING '1' space '507' 'X' space space space space. "empty line
    PERFORM log_headerline_with_timestamp USING '1' '503' ' '. "End deployment to HANA repository: &1
    PERFORM protokoll USING '1' space '507' space space space space space. "empty line
    PERFORM write_prot_no_condense_to_file USING iv_logfile.
  ENDIF.
ENDFORM.


FORM deploy_all_failed_hdi USING ir_hot_logger TYPE REF TO if_cts_hot_logger.
  DATA: lt_broken_objects TYPE cl_cts_hdi_object=>ty_t_hdi_objects.

  lt_broken_objects = cl_cts_hdi_object=>load_broken_objects( ).

  ir_hot_logger->set_msg_id( 'SCTS_HDI' ).
  IF lt_broken_objects IS INITIAL.
    ir_hot_logger->new_log_section( ).
    ir_hot_logger->info_level_3( '630' ). "Keine fehlerhaften HDI-Objetke für Redeployment vorhanden
    RETURN. "nothing to do.
  ENDIF.

  DATA(lr_hdi_deployer) = NEW cl_cts_hta_hdi_rdd_obj_deploy( ).
  "here we use deploy_objects and not deploy because there is no need to do transport related actions,
  "e.g.: expand hotas and update e071 is not needed
  lr_hdi_deployer->deploy_objects( it_objects = lt_broken_objects
                                   ir_hot_logger = ir_hot_logger ).
  ir_hot_logger->flush( ).
  ir_hot_logger->set_msg_id( 'SCTS_HOT' ).
ENDFORM.

"! Creates job REDEPLOY_FAILED_HANA_OBJECTS for deployment of all failed objects/packages/HDI objects
"! if at least one broken object/package/HDI object exists but only if system is currently not running
"! in upgrade mode.
FORM submit_job_deploy_all_failed USING iv_broken_repo_exists_before TYPE abap_bool
                                        iv_broken_hdi_exists_before TYPE abap_bool.

  NEW cl_cts_hot_aftrburnr_scheduler(
    iv_broken_repo_exists_before = iv_broken_repo_exists_before
    iv_broken_hdi_exists_before  = iv_broken_hdi_exists_before )->schedule_job(
    EXPORTING
      ir_logger               = gr_logger_list
      iv_transport_logs       = abap_true
  ).

  gr_logger_list->flush( ).


ENDFORM.

"! "check if already a job is scheduled but not yet started.<br/>
"! If c_job_exists is already abap_true, then nothing is done.<br/>
"! i_job_state could be P or S or Y or Z (taken from FM show_jobstate)
FORM is_job_already_planned USING i_job_state TYPE btcstatus
                            CHANGING c_job_exists TYPE abap_bool.
  IF c_job_exists = abap_true.
    RETURN.
  ENDIF.

  "check if already a job exists for passed state
  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      abap_program_name = 'SCTS_HTA_REDEPLOY_FAILED'    " ABAP Programmname
      status            = i_job_state    " Jobstatus
    EXCEPTIONS
      no_jobs_found     = 1
      OTHERS            = 2.

  IF sy-subrc = 0. "if jobs were found we do not want to schedule job
    c_job_exists = abap_true.
  ENDIF.
  IF sy-subrc = 2. "in other error cases than the expected error (sy-subrc=1), we do not want to schedule job, so also setting c_job_exists to abap_true
    PERFORM protokoll_with_ag USING '3' 'W' sy-msgid sy-msgno space sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    PERFORM write_prot_to_all_logfiles.
    c_job_exists = abap_true.
  ENDIF.
ENDFORM.

"! For all R3TR HOTAs with lockflag = '2', compare HTA repository with HANA repository and add all objects that are in HANA but not in HTA as deletions to CTS_HOT_OBJECT
FORM check_hana_for_obosolete_objs USING i_hot_connector TYPE REF TO cl_cts_hot_hana_connector
                                   RAISING cx_hana_object_transport.
  DATA: lt_packages      TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
        lr_package       TYPE REF TO cl_cts_hot_package,
        lv_tmptext       TYPE string,
        lv_header_logged TYPE abap_bool.

* 1. find all HOTAs to execute check for obsolete objects
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref) WHERE pgmid = 'R3TR' AND object = 'HOTA' AND lockflag = '2'
                                                                GROUP BY ( obj_name = lr_e071_hot_ref->obj_name )
                                                                WITHOUT MEMBERS
                                                                REFERENCE INTO DATA(lr_e071_hot_ref_group).

    lr_package = cl_cts_hot_package=>create_instance_from_objname( CONV #( lr_e071_hot_ref_group->obj_name ) ).
    IF lr_package->hana_package_id IS NOT INITIAL. "If it is initial, the hana package does not exist in HTA (anymore) and thus we do not know the package name for checking objects in HANA
      APPEND lr_package TO lt_packages.
    ENDIF.
  ENDLOOP.

* 2. find obsolete objects and mark as deleted
  DATA(lt_objects) = i_hot_connector->check_hana_for_obosolete_objs( lt_packages ).
  IF lt_objects IS INITIAL.
    RETURN.
  ENDIF.

  SORT lt_objects BY table_line->abap_hana_package_id ASCENDING.

* 3. loop over content for each TR to log per log file
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_e071_hot_ref2) WHERE pgmid = 'R3TR' AND object = 'HOTA' AND lockflag = '2'
                                                                 GROUP BY ( tr_korr = lr_e071_hot_ref2->trkorr logfile = lr_e071_hot_ref2->logfile )
                                                                 REFERENCE INTO DATA(lr_e071_hot_ref_group2).
    CLEAR lv_header_logged.
    LOOP AT GROUP lr_e071_hot_ref_group2 REFERENCE INTO DATA(lr_e071_hot_ref_group_member).
      LOOP AT lt_objects INTO DATA(lr_object) WHERE table_line->abap_hana_package_id = lr_e071_hot_ref_group_member->obj_name.
        IF lv_header_logged = abap_false.
          PERFORM protokoll USING '4' ' ' '595' 'X' space space space space. "Following objects exist in HANA but not in HTA and were added as deletions
          lv_header_logged = abap_true.
        ENDIF.

        lv_tmptext = |{ lr_object->hana_object_name }.{ lr_object->hana_object_suffix } ({ lr_object->hana_package_id })|.
        PERFORM protokoll_longtext USING '4' ' ' '579' ' ' lv_tmptext. "  &1&2&3&4
      ENDLOOP.
    ENDLOOP.

    PERFORM write_prot_to_file USING lr_e071_hot_ref_group2->logfile. "no_condense of spaces because of spaces between package and objname in e071 entry, e.g. 'com.pack           obj1.attributeview'
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_not_yet_logged_succes_objs                                        *
*---------------------------------------------------------------------*
"! Logs object header for passed objects if not yet logged.<br/>
"! HANA object names format: 'name.suffix (package) &lt;text&gt;' is splitted
"! into several loglines if necessary (if hana object name is too long
"! it is printed in 2 or more log lines)<br/>
"! i_objects the objects to log a header for if not yet logged<br/>
"! i_trkorr trkorr currently processed<br/>
"! i_deletions objects in i_objects are deletions<br/>
"! c_already_logged table containing all already logged objects. Objects logged within this form will also be added to this changing parameter
FORM log_not_yet_logged_succes_objs USING    i_objects        TYPE if_cts_hot_hana_conn_internal=>ty_cts_hot_objects
                                             i_trkorr         TYPE trkorr
                                             i_deletions      TYPE abap_bool
                                    CHANGING c_already_logged TYPE if_cts_hot_hana_conn_internal=>ty_cts_hot_objects.
  DATA: lv_last_tr_of_current_object  TYPE trkorr,
        lv_current_object_part_of_log TYPE abap_bool.

  LOOP AT i_objects INTO DATA(lr_object).
    IF NOT line_exists( c_already_logged[ table_line = lr_object ] ).
      PERFORM find_last_request_for_object USING lr_object CHANGING lv_last_tr_of_current_object.
      IF line_exists( gt_e071_hot_refs[ trkorr = i_trkorr cts_hot_object = lr_object ] ).
        lv_current_object_part_of_log = abap_true.
      ELSE.
        lv_current_object_part_of_log = abap_false.
      ENDIF.

      PERFORM log_object_header USING lr_object lv_current_object_part_of_log lv_last_tr_of_current_object i_trkorr abap_true abap_true i_deletions.

      INSERT lr_object INTO TABLE c_already_logged.
    ENDIF.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM enqueue_hotas                                            *
*---------------------------------------------------------------------*
"! Tries to enqueue all packages/objects to be deployed on HOTA level.<br/>
"! If the call comes from API/SNote and at least 1 object or package is already
"! in process, an error is written to log and no deployment will be triggered.<br/>
"! If the call comes from transport or job 'deploy_all_failed' then we wait actively
"! until previous deployment finishes and continue afterwards
FORM enqueue_hotas CHANGING e_success TYPE abap_bool.
  CONSTANTS: lc_enqueue_access_key TYPE cts_hot_package_id VALUE ';HOTA;ENQUEUE;ACCESS;'.
  DATA: lv_has_enqueue_access TYPE abap_bool,
        lv_starttime          TYPE timestamp,
        lv_endtime            TYPE timestamp,
        lv_duration           TYPE tzntstmpl,
        lv_tmptext            TYPE string.

  GET TIME STAMP FIELD lv_starttime.
  lv_endtime = lv_starttime.

  "Because we need to lock several objects we first check whether another process currently creates enqueues to prevent deadlocks
  WHILE cl_abap_tstmp=>subtract( tstmp1 = lv_endtime tstmp2 = lv_starttime ) < 600. "Giving the other process at least 10 minutes to finish enqueue of HOTAS.
    TRY.
        CALL FUNCTION 'ENQUEUE_ENQ_HOTA'
          EXPORTING
            abap_hana_pkg_id = lc_enqueue_access_key
            _wait            = 'X'  "wait as defined in profile enque/delay_max, at least 1 sec, kernel default 5
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
        IF sy-subrc = 0.
          lv_has_enqueue_access = abap_true.
          EXIT.
        ENDIF.
      CATCH cx_sy_dyn_call_illegal_func.
        "in case enqueue object ENQ_HOTA is not yet active we do not enqueue the objects at all as in previous releases.
        "We saw this exception when HOTA objects where activated during initial import of enqueue object R3TR ENQ ENQ_HOTA.
        RETURN.
    ENDTRY.
    GET TIME STAMP FIELD lv_endtime.
  ENDWHILE.

  IF lv_has_enqueue_access = abap_false.
    PERFORM protokoll_with_ag USING '2' 'E' sy-msgid sy-msgno space sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. "log latest enqueue error
    PERFORM protokoll USING '2' 'E' '620' space space space space space. "deployment already running. try again later
    PERFORM write_prot_to_all_logfiles.
    e_success = abap_false.
    RETURN.
  ENDIF.

  e_success = abap_true.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_hot_ref) WHERE object = 'HOTA' OR object = 'HOTP' OR object = 'HOTO'
                                                           GROUP BY ( obj_name = lr_hot_ref->obj_name(40) )
                                                           REFERENCE INTO DATA(lr_hot_ref_group).
    WHILE 1 = 1. "try to lock HOTA until previous process released the lock
      CALL FUNCTION 'ENQUEUE_ENQ_HOTA'
        EXPORTING
          abap_hana_pkg_id = lr_hot_ref_group->obj_name
          _wait            = 'X' "wait as defined in profile enque/delay_max, at least 1 sec, kernel default 5
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          OTHERS           = 3.

      IF sy-subrc = 0.
        EXIT.
      ELSEIF gv_external_call = abap_true. "in case of SNote/CWB/SCTS_HTA_DEPLOY UI/HTA Deploy API stop if any HOTA is already locked
        PERFORM protokoll_with_ag USING '2' 'E' sy-msgid sy-msgno space sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        PERFORM protokoll USING '2' 'E' '620' space lr_hot_ref_group->obj_name space space space. "deployment already running. try again later
        PERFORM write_prot_to_all_logfiles.
        e_success = abap_false.
        EXIT.
      ENDIF.
    ENDWHILE.
    IF e_success = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF e_success = abap_false.
    PERFORM dequeue_hotas.
  ENDIF.

  "Free enqueue access so that other processes can try to enqueue objects
  CALL FUNCTION 'DEQUEUE_ENQ_HOTA'
    EXPORTING
      abap_hana_pkg_id = lc_enqueue_access_key
      _synchron        = 'X'.

  GET TIME STAMP FIELD lv_endtime.

  lv_duration = cl_abap_tstmp=>subtract( tstmp1 = lv_endtime tstmp2 = lv_starttime ).
  IF lv_duration > 60. "only log enqueue time if it took more than 1 minute overall
    lv_tmptext = lv_duration.
    " delete trailing '0' and '.', if possible
    SHIFT lv_tmptext RIGHT DELETING TRAILING '0'.
    SHIFT lv_tmptext RIGHT DELETING TRAILING '.'.
    CONDENSE lv_tmptext NO-GAPS.

    PERFORM protokoll USING '4' ' ' '623' space lv_tmptext space space space. "enqueue took &1 seconds
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM dequeue_hotas                                            *
*---------------------------------------------------------------------*
"! Tries to dequeue all packages/objects to be deployed on HOTA level.<br/>
FORM dequeue_hotas.
  LOOP AT gt_e071_hot_refs REFERENCE INTO DATA(lr_hot_ref) WHERE object = 'HOTA' OR object = 'HOTP' OR object = 'HOTO'
                                                           GROUP BY ( obj_name = lr_hot_ref->obj_name(40) )
                                                           REFERENCE INTO DATA(lr_hot_ref_group).
    TRY.
        CALL FUNCTION 'DEQUEUE_ENQ_HOTA'
          EXPORTING
            abap_hana_pkg_id = lr_hot_ref_group->obj_name
            _synchron        = 'X'.
      CATCH cx_sy_dyn_call_illegal_func.
        "in case enqueue object ENQ_HOTA is not yet active we do not dequeue the objects at all as in previous releases.
        "We saw this exception when HOTA objects where activated during initial import of enqueue object R3TR ENQ ENQ_HOTA.
        RETURN.
    ENDTRY.
  ENDLOOP.
ENDFORM.


FORM execute_container_deletion USING it_e071_amhc_ref_del TYPE ty_t_e071_amhc_ref
                                      iv_container_types_to_skip LIKE gv_container_types_to_skip.
  IF it_e071_amhc_ref_del IS INITIAL.
    RETURN.
  ENDIF.

* 0. Authority check to be logged in each TR that contains AMHCs for deletion
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'AMHC' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    LOOP AT it_e071_amhc_ref_del REFERENCE INTO DATA(lr_e071_amhcs_del)
                                               GROUP BY ( logger = lr_e071_amhcs_del->logger )
                                               REFERENCE INTO DATA(lr_logger_group).
      lr_logger_group->logger->abnormal_termination( iv_msg_id = 'SCTS_HOT' iv_msg_nr = '032' ). "no permission to deploy hdi container
      lr_logger_group->logger->flush( ).
    ENDLOOP.
    RETURN.
  ENDIF.

* 1. Create logger lists per container so that all TRs where a hdi container (AMHC) is part of gets the logs of container deletion
  TYPES:
    BEGIN OF ty_s_cont_name_w_logger_list,
      container_name TYPE trobj_name,
      logger_list    TYPE REF TO cl_cts_hot_logger_list, " logger_list => logger list for transport protocols
    END OF ty_s_cont_name_w_logger_list,
    ty_t_cont_name_w_logger_list TYPE STANDARD TABLE OF ty_s_cont_name_w_logger_list.

  DATA:
    lt_container_with_logger_list TYPE ty_t_cont_name_w_logger_list,
    ls_container_with_logger_list TYPE ty_s_cont_name_w_logger_list,
    lv_deletion_ok                TYPE abap_bool,
    lr_container                  TYPE REF TO if_cts_hdi_container.

  "replaces the logger instances in gt_e071_amhc_refs with hdi log files if this is an update/upgrade
  DATA(lr_update_upgrade_handler) = NEW lcl_amhc_log_helper( REF #( it_e071_amhc_ref_del ) ).

*   2. Write a header entry ("Beginn Löschen von HDI Containern: &1) to each tr protocol containing at least one AMHC
  lr_update_upgrade_handler->log_begin_deletion( ).

  LOOP AT it_e071_amhc_ref_del REFERENCE INTO DATA(lr_e071_amhc_ref) GROUP BY ( obj_name = lr_e071_amhc_ref->obj_name ) REFERENCE INTO DATA(lr_amhc_group).
    ls_container_with_logger_list-container_name = lr_amhc_group->obj_name.
    ls_container_with_logger_list-logger_list = NEW cl_cts_hot_logger_list( ).

    LOOP AT GROUP lr_amhc_group REFERENCE INTO DATA(lr_amhc_group_member).
      ls_container_with_logger_list-logger_list->add_logger( lr_amhc_group_member->logger ). " add logger for each tr, that contains this hdi container
    ENDLOOP.
    ls_container_with_logger_list-logger_list->if_cts_hot_logger~set_msg_id( 'SCTS_HDI' ).
    APPEND ls_container_with_logger_list TO lt_container_with_logger_list.
  ENDLOOP.

*   3. Sort containers in lt_container_with_logger_list.
  DATA: ls_container_to_sort           TYPE cl_cts_hdi_container_sort=>ty_s_logical_cont_with_vers,
        lt_container_to_sort           TYPE cl_cts_hdi_container_sort=>ty_t_logical_cont_with_vers,
        lt_sorted_cont_with_loggerlist LIKE lt_container_with_logger_list.

  ls_container_to_sort-version = 'A'.  "sets default version for sorting
  LOOP AT lt_container_with_logger_list INTO ls_container_with_logger_list.
    ls_container_to_sort-logical_container = ls_container_with_logger_list-container_name.
    APPEND ls_container_to_sort TO lt_container_to_sort.
  ENDLOOP.

  TRY.
      cl_cts_hdi_container_sort=>sort_containers_desc(
                                      EXPORTING it_container              = lt_container_to_sort
                                      IMPORTING et_sorted_container       = DATA(lt_sorted_container)
                                                et_not_existing_container = DATA(lt_not_existing_container) ).
    CATCH  cx_cts_hdi_container.
      lt_sorted_container = lt_container_to_sort.
  ENDTRY.

  APPEND LINES OF lt_not_existing_container TO lt_sorted_container.

  LOOP AT lt_sorted_container INTO DATA(ls_sorted_container).
    READ TABLE lt_container_with_logger_list INTO ls_container_with_logger_list WITH KEY container_name = ls_sorted_container-logical_container.
    APPEND ls_container_with_logger_list TO lt_sorted_cont_with_loggerlist.
  ENDLOOP.

*   4. Delete containers
  LOOP AT lt_sorted_cont_with_loggerlist INTO ls_container_with_logger_list.
    lr_container = cl_cts_hdi_container=>create_instance( iv_logical_name = CONV #( ls_container_with_logger_list-container_name ) ).
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid           = 'R3TR'  " Eingabe zum TADIR-Feld PGMID
        wi_tadir_object          = 'AMHC'  " Eingabe zum TADIR-Feld OBJECT
        wi_tadir_obj_name        = CONV sobj_name( ls_container_with_logger_list-container_name ) " Eingabe zum TADIR-Feld OBJ_NAME
        wi_read_only             = 'X'
      EXCEPTIONS
        tadir_entry_not_existing = 1
        OTHERS                   = 2.
    IF sy-subrc <> 1.
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~info_level_3( iv_msg_nr = '534'  "Container &1 wird nicht gelöscht, da Objektkatalog-Eintrag existiert
                                              iv_var1 =  CONV #( ls_container_with_logger_list-container_name ) ).
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
      lv_deletion_ok = abap_true.
    ELSE.
      lr_container->read_data( ).
      lr_container->get_data( IMPORTING es_container_data = DATA(ls_container_data) ).

*   5. Handle container with inactive data
      IF ls_container_data-data-inactive_data IS NOT INITIAL.
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~abnormal_termination( iv_msg_nr = '549'
                                                   iv_var1 = CONV #( ls_container_data-logical_container ) ). "  Interner Fehler: zu löschender AMHC &1 hat eine inaktive Version
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
        CONTINUE.
      ENDIF.

      IF ls_container_data-data-active_data IS NOT INITIAL.
        " Handle container types to be skipped (used in Upgrades)
        IF ls_container_data-data-active_data-container_type_ref->get_type( ) CA iv_container_types_to_skip.
          ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
          ls_container_with_logger_list-logger_list->if_cts_hot_logger~warning(
            EXPORTING
              iv_msg_nr = '585' "    Skipping deletion for HDI container &1
              iv_var1   = CONV #( ls_container_data-logical_container )
              iv_var2   = CONV #( iv_container_types_to_skip )
          ).
          ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
          CONTINUE.
        ENDIF.

        lr_container->delete(
          EXPORTING
            ir_hot_logger  = ls_container_with_logger_list-logger_list
          IMPORTING
            ev_deletion_ok = lv_deletion_ok ).
      ELSE.
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~message( iv_msg_nr = '557'  "     HDI-Container &1 muss nicht gelöscht werden, da er nicht existiert
          iv_level = if_cts_hot_logger=>co_level_2 iv_var1 = CONV #( ls_container_data-logical_container ) ).
        ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
        lv_deletion_ok = abap_true.
      ENDIF.
    ENDIF.

* update table e071 after container deletion
    IF lv_deletion_ok EQ abap_true .
      LOOP AT it_e071_amhc_ref_del INTO DATA(ls_e071_amhc_ref) WHERE obj_name = lr_container->mv_container_name.
        NEW cl_cts_hdi_container_db_access( iv_logical_name = CONV #( ls_e071_amhc_ref-obj_name ) )->if_cts_hdi_container_db_access~update_e071_after_amhc_drop(
          EXPORTING
            iv_trkorr = ls_e071_amhc_ref-trkorr
            iv_as4pos = ls_e071_amhc_ref-as4pos  ).
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*   6. Write a header entry ("Ende von Löschen von HDI Containern: &1) to each tr log containing at least one AMHC
  lr_update_upgrade_handler->log_end_deletion( ).

  lr_update_upgrade_handler->reset_logger( ).

ENDFORM.

*---------------------------------------------------------------------*
*       FORM execute_container_deployment                            *
*---------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="de">
"! Führt das HDI Container Deployment aus
"! </p>
"! Executes the HDI container deployment.
FORM execute_container_deployment.
  IF gt_e071_amhc_ref IS INITIAL.
    RETURN.
  ENDIF.

* 0. Authority check to be logged in each TR that contains HDI container
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'AMHC' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    LOOP AT gt_e071_amhc_ref REFERENCE INTO DATA(lr_e071_amhcs)
                                               GROUP BY ( logger = lr_e071_amhcs->logger )
                                               REFERENCE INTO DATA(lr_logger_group).
      lr_logger_group->logger->abnormal_termination( iv_msg_id = 'SCTS_HOT' iv_msg_nr = '032' ). "no permission to deploy hdi container
      lr_logger_group->logger->flush( ).
    ENDLOOP.
    RETURN.
  ENDIF.

* 1. Create logger lists per container so that all TRs where a hdi container (AMHC) is part of gets the logs of container deployment
  TYPES:
    BEGIN OF ty_s_cont_name_w_logger_list,
      container_name TYPE trobj_name,
      logger_list    TYPE REF TO cl_cts_hot_logger_list, " logger_list => logger list for transport protocols
    END OF ty_s_cont_name_w_logger_list,
    ty_t_cont_name_w_logger_list TYPE STANDARD TABLE OF ty_s_cont_name_w_logger_list.

  DATA:
    lt_container_with_logger_list TYPE ty_t_cont_name_w_logger_list,
    ls_container_with_logger_list TYPE ty_s_cont_name_w_logger_list.

  "replaces the logger instances in gt_e071_amhc_refs with hdi log files if this is an update/upgrade
  DATA(lr_update_upgrade_handler) = NEW lcl_amhc_log_helper( REF #( gt_e071_amhc_ref ) ).

* 2. Write a header entry ("Beginn Deployment von HDI Containern: &1) to each tr protocol containing at least one AMHC
  lr_update_upgrade_handler->log_begin_deployment( ).

  LOOP AT gt_e071_amhc_ref REFERENCE INTO DATA(lr_e071_amhc_ref) GROUP BY ( obj_name = lr_e071_amhc_ref->obj_name ) REFERENCE INTO DATA(lr_amhc_group).
    ls_container_with_logger_list-container_name = lr_amhc_group->obj_name. " change obj_name => cont_name?
    ls_container_with_logger_list-logger_list = NEW cl_cts_hot_logger_list( ).

    LOOP AT GROUP lr_amhc_group REFERENCE INTO DATA(lr_amhc_group_member).
      ls_container_with_logger_list-logger_list->add_logger( lr_amhc_group_member->logger ). " add logger for each tr, that contains this hdi container
    ENDLOOP.

    ls_container_with_logger_list-logger_list->if_cts_hot_logger~set_msg_id( 'SCTS_HDI' ).
    APPEND ls_container_with_logger_list TO lt_container_with_logger_list.
  ENDLOOP.

*   3. Sort containers in lt_container_with_logger_list.
  DATA: ls_container_to_sort           TYPE cl_cts_hdi_container_sort=>ty_s_logical_cont_with_vers,
        lt_container_to_sort           TYPE cl_cts_hdi_container_sort=>ty_t_logical_cont_with_vers,
        lt_sorted_cont_with_loggerlist LIKE lt_container_with_logger_list.

  ls_container_to_sort-version = 'I'.
  LOOP AT lt_container_with_logger_list INTO ls_container_with_logger_list.
    ls_container_to_sort-logical_container = ls_container_with_logger_list-container_name.
    APPEND ls_container_to_sort TO lt_container_to_sort.
  ENDLOOP.

  TRY.
      cl_cts_hdi_container_sort=>sort_containers(
                                      EXPORTING it_container              = lt_container_to_sort
                                      IMPORTING et_sorted_container       = DATA(lt_sorted_container)
                                                et_not_existing_container = DATA(lt_container_withno_inact_data) ).
    CATCH  cx_cts_hdi_container INTO DATA(lr_exc).
*     Exception is handled later in " 3. Write a header entry ..." ==> "IF lr_exc IS BOUND."
  ENDTRY.

  LOOP AT lt_sorted_container INTO DATA(ls_sorted_container).
    READ TABLE lt_container_with_logger_list INTO ls_container_with_logger_list WITH KEY container_name = ls_sorted_container-logical_container.
    APPEND ls_container_with_logger_list TO lt_sorted_cont_with_loggerlist.
  ENDLOOP.

  LOOP AT gt_e071_amhc_ref REFERENCE INTO DATA(lr_e071_amhc_ref2) GROUP BY ( logger = lr_e071_amhc_ref2->logger ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_amhc_group2).
    lr_amhc_group2->logger->info_level_3( iv_msg_nr = '530' ). "Sortieren für Container-Deployment-Reihenfolge gestartet
    IF lr_exc IS BOUND.
      lr_amhc_group2->logger->error_exception( lr_exc ).
    ENDIF.
    lr_amhc_group2->logger->info_level_3( iv_msg_nr = '531' ). "Sortieren für Container-Deployment-Reihenfolge beendet
    lr_amhc_group2->logger->flush( ).
  ENDLOOP.

*   3.5 Handle container with no inactive data
  LOOP AT lt_container_withno_inact_data INTO DATA(ls_container_withno_inact_data).
    READ TABLE lt_container_with_logger_list INTO ls_container_with_logger_list WITH KEY container_name = ls_container_withno_inact_data-logical_container.
    ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
    ls_container_with_logger_list-logger_list->if_cts_hot_logger~warning(
      EXPORTING
        iv_msg_nr = '527' "  Keine inaktiven Daten für Container &1 vorhanden
        iv_var1   = CONV #( ls_container_withno_inact_data-logical_container )
    ).
    ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
*     CONTINUE.
  ENDLOOP.

*   4. Deploy containers by loop over sorted container list!
  LOOP AT lt_sorted_cont_with_loggerlist INTO ls_container_with_logger_list.

    DATA(lr_container) = cl_cts_hdi_container=>create_instance( iv_logical_name = CONV #( ls_container_with_logger_list-container_name ) ).
    lr_container->read_data( ).
    lr_container->get_data( IMPORTING es_container_data = DATA(ls_container) ).

    "skip containers if skip was set by tp
    IF ( ls_container-data-inactive_data IS NOT INITIAL AND ls_container-data-inactive_data-container_type_ref->get_type( ) CA gv_container_types_to_skip )
           OR ( ls_container-data-active_data IS NOT INITIAL AND ls_container-data-active_data-container_type_ref->get_type( ) CA gv_container_types_to_skip ).
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~new_log_section( ).
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~warning(
        EXPORTING
          iv_msg_nr = '532' "  Überspringe Deployment für HDI Container &1
          iv_var1   = CONV #( ls_container-logical_container )
          iv_var2   = CONV #( gv_container_types_to_skip )
      ).
      ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ).
      CONTINUE.
    ENDIF.

    lr_container->activate( EXPORTING ir_hot_logger          = ls_container_with_logger_list-logger_list
                            IMPORTING ev_activation_ok       = DATA(lv_activation_ok)
                                      ev_nothing_to_activate = DATA(lv_nothing_to_activate) ).
    IF  lv_activation_ok = 'X' OR lv_nothing_to_activate = 'X'.
      LOOP AT gt_e071_amhc_ref INTO DATA(ls_e071_amhc_ref) WHERE pgmid = 'R3TR' AND object = 'AMHC' AND obj_name = lr_container->mv_container_name.
        UPDATE e071 SET lockflag = '3' WHERE trkorr = ls_e071_amhc_ref-trkorr AND as4pos = ls_e071_amhc_ref-as4pos.
      ENDLOOP.
    ENDIF.

    ls_container_with_logger_list-logger_list->if_cts_hot_logger~flush( ). "this will write the logs to all logs of all TRs of this logger_list

  ENDLOOP.

*   5. Write a header entry ("Ende Deployment von HDI Containern: &1) to each tr log containing at least one AMHC
  lr_update_upgrade_handler->log_end_deployment( ).

  lr_update_upgrade_handler->reset_logger( ).

ENDFORM.

*---------------------------------------------------------------------*
*       FORM execute_hdi_object_deployment                            *
*---------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="de">
"! Führt das HDI Object Deployment aus
"! </p>
"! Executes the HDI object deployment.
FORM execute_hdi_object_deployment.
* Authority check to be logged in each TR that contains HDI objects
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'HOTA' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    LOOP AT gt_e071_hdi_refs REFERENCE INTO DATA(lr_e071_hdi_objects)
                                               GROUP BY ( logger = lr_e071_hdi_objects->logger )
                                               REFERENCE INTO DATA(lr_logger_group).
      lr_logger_group->logger->abnormal_termination( iv_msg_id = 'SCTS_HOT' iv_msg_nr = '031' ). "no permission to deploy hana objects
      lr_logger_group->logger->flush( ).
    ENDLOOP.
    RETURN.
  ENDIF.

  IF gt_e071_hdi_refs IS NOT INITIAL.
    DATA(lr_hdi_deployer) = NEW cl_cts_hta_hdi_rdd_obj_deploy( ).
    lr_hdi_deployer->deploy( it_e071_hdi_objects = gt_e071_hdi_refs
                             iv_container_types_to_skip = gv_container_types_to_skip ).
  ENDIF.
ENDFORM.

FORM is_hdi_enabled CHANGING cv_hdi_enabled TYPE abap_bool.
  TRY.
      cl_cts_hdi_container_group_api=>create_instance( ).
      cv_hdi_enabled = abap_true.
    CATCH cx_cts_hdi_api_no_hdb INTO DATA(lr_exc_hdi_api_no_hdb).
      gr_logger_list->message( iv_msg_id   = 'SCTS_HOT'
                               iv_msg_nr   = '004'
                               iv_level    = if_cts_hot_logger=>co_level_2
      ).
      gr_logger_list->flush( ).
      cv_hdi_enabled = abap_false.
    CATCH cx_cts_hdi_api INTO DATA(lr_exc_hdi_api).
      LOOP AT gt_e071_hdi_refs REFERENCE INTO DATA(lr_e071) GROUP BY ( logger = lr_e071->logger ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_group).
        lr_group->logger->abnormal_termination_exception( lr_exc_hdi_api ).
      ENDLOOP.
      LOOP AT gt_e071_amhc_ref REFERENCE INTO DATA(lr_e071_amhc) GROUP BY ( logger = lr_e071_amhc->logger ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_amhc_group).
        lr_amhc_group->logger->abnormal_termination_exception( lr_exc_hdi_api ).
      ENDLOOP.
      LOOP AT gt_e071_amhc_ref_del REFERENCE INTO DATA(lr_e071_amhc_del) GROUP BY ( logger = lr_e071_amhc_del->logger ) WITHOUT MEMBERS REFERENCE INTO DATA(lr_amhc_del_group).
        lr_amhc_del_group->logger->abnormal_termination_exception( lr_exc_hdi_api ).
      ENDLOOP.
      cv_hdi_enabled = abap_false.
  ENDTRY.
ENDFORM.


**************************************************************************
* END Report Coding, Start of test classes definition and implmentation  *
**************************************************************************

CLASS ltcl_rddhanadeployment_helper DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Tests the split message method for a fixed bug
      "! "MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) - version of AT3K900022" was handled wrong
      split_message_test1 FOR TESTING RAISING cx_static_check,
      "! Tests the split messages after change to have two spaces in front as of second line with real life example data
      split_message_test2 FOR TESTING RAISING cx_static_check,
      "! another test with real string data
      split_message_test3 FOR TESTING RAISING cx_static_check,
      "! another test with real string data
      split_message_test4 FOR TESTING RAISING cx_static_check,
      "! test split message with alternating char and space (a b c d ...) to verify space handling
      split_message_char_space_alter FOR TESTING RAISING cx_static_check,
      "! test split message with alternating 2 char and 2 space (aa  bb  cc  ...) to verify space handling
      split_message_2c_2spaces_alter FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_rddhanadeployment_helper IMPLEMENTATION.

  METHOD split_message_test1.
    lcl_rddhanadeployment_helper=>split_message(
      EXPORTING
        message          = 'MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) - version of AT3K900022'
      IMPORTING
        et_split_message = DATA(lt_split_message)
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'MERGEPROCEDURE_1_4_0.procedure' act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = ' (tester1.merge1) - version of A' act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'T3K900022' act = lt_split_message[ 1 ]-var3 ).

  ENDMETHOD.

  METHOD split_message_test2.
    lcl_rddhanadeployment_helper=>split_message(
      EXPORTING
        message          = |create olap scenario '<?xml version='1.0' encoding='utf-8'?><cubeSchema defaultLanguage='EN' defaultSchema='_SYS_BIC' operation='createHanaCube' version=''><sqlScriptView name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' | &&
                           |schema='_SYS_BIC' sqlScriptF...3369 chars skipped by HTA...ensionAttribute></dimensionAttributes><measures><measure column='CLV' '| &&
                           |name='CLV' aggregationType='sum'/><measure column='CHRN_RATE' name='CHURN_RATE' aggregationType='sum'/></measures></sqlScriptView></cubeSchema>'|
      IMPORTING
        et_split_message = DATA(lt_split_message)
    ).

    cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |create olap scenario '<?xml ver| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |sion='1.0' encoding='utf-8'?><c| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |ubeSchema defaultLanguage='EN'| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = | defaultSchema='_SYS_BIC' operat| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  ion='createHanaCube' version=| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |''><sqlScriptView name='sap.han| act = lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |a-app.cuan.ai/CA_AI_CUSTOMER_CC| act = lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |R' schema='_SYS_BIC' sqlScriptF| act = lt_split_message[ 2 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  ...3369 chars skipped by HTA.| act = lt_split_message[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |..ensionAttribute></dimensionAt| act = lt_split_message[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |tributes><measures><measure col| act = lt_split_message[ 3 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |umn='CLV' 'name='CLV' aggregati| act = lt_split_message[ 3 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  onType='sum'/><measure column| act = lt_split_message[ 4 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |='CHRN_RATE' name='CHURN_RATE'| act = lt_split_message[ 4 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | aggregationType='sum'/></measur| act = lt_split_message[ 4 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |es></sqlScriptView></cubeSchema| act = lt_split_message[ 4 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  >'| act = lt_split_message[ 5 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 5 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 5 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 5 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_test3.

    lcl_rddhanadeployment_helper=>split_message(
       EXPORTING
         message          = |AT_CPRED_DEMO_INSURANCE_JS_IC.attributeview (sap.hana-app.cuan.cpred.demo.insurance_ic.datafoundation.internal) would be OK, see next attempt|
       IMPORTING
         et_split_message = DATA(lt_split_message)
     ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |AT_CPRED_DEMO_INSURANCE_JS_IC.a| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ttributeview (sap.hana-app.cuan| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |.cpred.demo.insurance_ic.datafo| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |undation.internal) would be OK,| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |   see next attempt| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_test4.

    lcl_rddhanadeployment_helper=>split_message(
       EXPORTING
         message          = |PR_T100_1.procedure (tmp.hta.aunit) regeneration failed - see longtext|
       IMPORTING
         et_split_message = DATA(lt_split_message)
     ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |PR_T100_1.procedure (tmp.hta.au| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |nit) regeneration failed - see| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | longtext| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = || act = lt_split_message[ 1 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_char_space_alter.

    lcl_rddhanadeployment_helper=>split_message(
       EXPORTING
         message          = |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                            |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                            |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9|
       IMPORTING
         et_split_message = DATA(lt_split_message)
     ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |a b c d e f g h i j k l m n o p| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | q r s t u v w x y z A B C D E| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | F G H I J K L M N O P R S T U V| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = | W X Y Z 0 1 2 3 4 5 6 7 8 9 a| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  b c d e f g h i j k l m n o p| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | q r s t u v w x y z A B C D E| act = lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | F G H I J K L M N O P R S T U V| act = lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = | W X Y Z 0 1 2 3 4 5 6 7 8 9 a| act = lt_split_message[ 2 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  b c d e f g h i j k l m n o p| act = lt_split_message[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | q r s t u v w x y z A B C D E| act = lt_split_message[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | F G H I J K L M N O P R S T U V| act = lt_split_message[ 3 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = | W X Y Z 0 1 2 3 4 5 6 7 8 9| act = lt_split_message[ 3 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_2c_2spaces_alter.

    lcl_rddhanadeployment_helper=>split_message(
       EXPORTING
         message          = |aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll mm nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy  zz  AA  BB  CC  DD  EE  FF  GG  HH  II  JJ|
       IMPORTING
         et_split_message = DATA(lt_split_message)
     ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |aa  bb  cc  dd  ee  ff  gg  hh| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |  ii  jj  kk  ll mm nn  oo  pp| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | qq  rr  ss  tt  uu  vv  ww  xx| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  yy  zz  AA  BB  CC  DD  EE  FF| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |    GG  HH  II  JJ| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = lt_split_message[ 2 ]-var4 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltd_external_persistency DEFINITION FINAL FOR TESTING INHERITING FROM lcl_external_persistency.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_package_devclass,
        package  TYPE cts_hot_hana_package_id,
        devclass TYPE devclass,
      END OF ty_package_devclass,
      tt_package_devclass TYPE SORTED TABLE OF ty_package_devclass WITH UNIQUE KEY package,

      BEGIN OF ty_switch_switch_state,
        switch_id    TYPE sfw_switch_id,
        switch_state TYPE sfw_switchpos,
      END OF ty_switch_switch_state,
      tt_switch_id_switch_state TYPE SORTED TABLE OF ty_switch_switch_state WITH UNIQUE KEY switch_id.

    DATA:
      mocked_package_devclasses   TYPE tt_package_devclass,
      mocked_devclass_switches    TYPE lcl_switch_framework_accessor=>tt_devclass_switch_id,
      mocked_switch_switch_states TYPE tt_switch_id_switch_state.

    METHODS:
      get_devclass_for_package REDEFINITION,
      get_switch_id_for_devclass REDEFINITION,
      is_switch_switched_off REDEFINITION.

  PRIVATE SECTION.
    DATA:
      "! Cache for packages used for get_devclass_for_package to prevent multiple calls with
      "! same package, as this should be cached in lcl_switch_framework_accessor (performance)
      m_package_cache   TYPE SORTED TABLE OF cts_hot_package_id WITH UNIQUE DEFAULT KEY,
      "! Cache for devclasses used for get_switch_id_for_devclass to prevent multiple calls with
      "! same devclass, as this should be cached in lcl_switch_framework_accessor (performance)
      m_devclass_cache  TYPE SORTED TABLE OF devclass WITH UNIQUE DEFAULT KEY,
      "! Cache for switch_ids used for is_switch_switched_off to prevent multiple calls with
      "! same switch_id as this should be cached in lcl_switch_framework_accessor (performance)
      m_switch_id_cache TYPE SORTED TABLE OF sfw_switch_id WITH UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS ltd_external_persistency IMPLEMENTATION.

  METHOD get_devclass_for_package.
    DATA: ls_package_devclass TYPE ty_package_devclass.

    IF line_exists( m_package_cache[ table_line = i_package ] ).
      cl_abap_unit_assert=>fail( msg = 'Performance: get_devclass_for_package was already called for package ' && i_package ).
    ENDIF.
    INSERT i_package INTO TABLE m_package_cache.

    READ TABLE mocked_package_devclasses WITH KEY package = i_package INTO ls_package_devclass.
    IF sy-subrc = 0.
      r_result = ls_package_devclass-devclass.
    ENDIF.
  ENDMETHOD.

  METHOD get_switch_id_for_devclass.
    DATA: ls_devclass_switch_id TYPE lcl_switch_framework_accessor=>ty_devclass_switch_id.

    IF line_exists( m_devclass_cache[ table_line = i_devclass ] ).
      cl_abap_unit_assert=>fail( msg = 'Performance: get_switch_id_for_devclass was already called for devclass ' && i_devclass ).
    ENDIF.
    INSERT i_devclass INTO TABLE m_devclass_cache.

    READ TABLE mocked_devclass_switches WITH KEY devclass = i_devclass INTO ls_devclass_switch_id.
    IF sy-subrc = 0.
      r_result = ls_devclass_switch_id-switch_id.
    ENDIF.
  ENDMETHOD.

  METHOD is_switch_switched_off.
    DATA: ls_switch_switch_state TYPE ltd_external_persistency=>ty_switch_switch_state.

    IF line_exists( m_switch_id_cache[ table_line = i_switch_id ] ).
      cl_abap_unit_assert=>fail( msg = 'Performance: is_switch_switched_off was already called for switch ' && i_switch_id ).
    ENDIF.
    INSERT i_switch_id INTO TABLE m_switch_id_cache.

    READ TABLE mocked_switch_switch_states WITH KEY switch_id = i_switch_id INTO ls_switch_switch_state.
    IF sy-subrc = 0.
      IF ls_switch_switch_state-switch_state = cl_abap_switch=>c_off.
        r_result = abap_true.
      ELSE.
        r_result = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_switch_framework_accessor DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      "! setup class under test and set default mock data valid for all tests
      setup,
      "! Test switch settings for a single package for which the switch is off
      switch_off_single_package FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single package for which the switch is on
      switch_on_single_package FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single package for which the devclass does not exists (=assume switch is on)
      single_package_no_devclass FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single package for which the switch does not exists (=assume switch is on)
      single_package_no_switch FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single package for which the switch is on standby state (=assume switch is on as we test for switch off state)
      single_package_switch_standby FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 packages, each a separate switch and both switches off
      two_packages_2_switches_off FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 packages, each a separate switch and 1 switch off
      two_packages_1_switch_off FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 packages, each a separate switch and no switch off
      two_packages_0_switch_off FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 different packages that have same devclass and therefore same switch, switch on
      two_packages_same_devclass_on FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 different packages that have same devclass and therefore same switch, switch off
      two_packages_same_devclass_off FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 packages in 2 devclass but belonging to 1 switch, sitch off
      two_packages_2_devc_1_sw_off FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 packages in 2 devclass but belonging to 1 switch, sitch on
      two_packages_2_devc_1_sw_on FOR TESTING RAISING cx_static_check,
      "! Test switch settings complex case combining all other package tests in one.
      complex_case_packages FOR TESTING RAISING cx_static_check,
      "! Test performance, read switch for same package twice.
      read_same_package_twice FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single object for which the switch (on package level) is off
      switch_off_single_object FOR TESTING RAISING cx_static_check,
      "! Test switch settings for a single object for which the switch (on package level) is on
      switch_on_single_object FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 objects of same package for which the switch (on package level) is off
      switch_off_2_objects_same_pack FOR TESTING RAISING cx_static_check,
      "! Test switch settings for 2 objects of different packages but same devclass for which the switch (on package level) is off
      switch_off_2_objects_diff_pack FOR TESTING RAISING cx_static_check,
      "! Test switch settings complex case for several objects including all packages of the package tests.
      complex_case_objects FOR TESTING RAISING cx_static_check.

    DATA:
      m_cut                TYPE REF TO lcl_switch_framework_accessor,
      m_persistency_access TYPE REF TO ltd_external_persistency.
ENDCLASS.

CLASS ltcl_switch_framework_accessor IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT m_cut.
    CREATE OBJECT m_persistency_access.
    m_cut->m_persistency_access = m_persistency_access.

    "set mock data
    m_persistency_access->mocked_package_devclasses = VALUE ltd_external_persistency=>tt_package_devclass(
                                        ( package = 'PACK1.DEVCLASS1.SWITCH1.OFF' devclass = 'DEVCLASS1_SWITCH1_OFF' )
                                        ( package = 'PACK1.DEVCLASS1.SWITCH1.OFF.2' devclass = 'DEVCLASS1_SWITCH1_OFF' )
                                        ( package = 'PACK1.DEVCLASS1_1.SWITCH1.OFF.3' devclass = 'DEVCLASS1_1_SWITCH1_OFF' )
                                        ( package = 'PACK2.DEVCLASS2.SWITCH2.ON' devclass = 'DEVCLASS2_SWITCH2_ON' )
                                        ( package = 'PACK2.DEVCLASS2.SWITCH2.ON.2' devclass = 'DEVCLASS2_SWITCH2_ON' )
                                        ( package = 'PACK2.DEVCLASS2_1.SWITCH2.ON.2' devclass = 'DEVCLASS2_1_SWITCH2_ON' )
                                        ( package = 'PACK3.DEVCLASS3.NO.SWITCH' devclass = 'DEVCLASS3_NO_SWITCH' )
                                        ( package = 'PACK5.DEVCLASS5.SWITCH5.STANDBY' devclass = 'DEVCLASS5_SWITCH5_STANDBY' )
                                        ( package = 'PACK6.DEVCLASS6.SWITCH6.ON' devclass = 'DEVCLASS6_SWITCH6_ON' )
                                        ( package = 'PACK7.DEVCLASS7.SWITCH7.OFF' devclass = 'DEVCLASS7_SWITCH7_OFF' ) ).

    m_persistency_access->mocked_devclass_switches = VALUE lcl_switch_framework_accessor=>tt_devclass_switch_id(
                                        ( devclass = 'DEVCLASS1_SWITCH1_OFF' switch_id = 'SWITCH1_OFF' )
                                        ( devclass = 'DEVCLASS1_1_SWITCH1_OFF' switch_id = 'SWITCH1_OFF' )
                                        ( devclass = 'DEVCLASS2_SWITCH2_ON' switch_id = 'SWITCH2_ON' )
                                        ( devclass = 'DEVCLASS2_1_SWITCH2_ON' switch_id = 'SWITCH2_ON' )
                                        ( devclass = 'DEVCLASS5_SWITCH5_STANDBY' switch_id = 'SWITCH5_STANDBY' )
                                        ( devclass = 'DEVCLASS6_SWITCH6_ON' switch_id = 'SWITCH6_ON' )
                                        ( devclass = 'DEVCLASS7_SWITCH7_OFF' switch_id = 'SWITCH7_OFF' ) ).

    m_persistency_access->mocked_switch_switch_states = VALUE ltd_external_persistency=>tt_switch_id_switch_state(
                                        ( switch_id = 'SWITCH1_OFF' switch_state = cl_abap_switch=>c_off )
                                        ( switch_id = 'SWITCH2_ON' switch_state = cl_abap_switch=>c_on )
                                        ( switch_id = 'SWITCH5_STANDBY' switch_state = cl_abap_switch=>c_stand_by )
                                        ( switch_id = 'SWITCH6_ON' switch_state = cl_abap_switch=>c_on )
                                        ( switch_id = 'SWITCH7_OFF' switch_state = cl_abap_switch=>c_off ) ).

  ENDMETHOD.

  METHOD switch_off_single_package.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.switch1.off' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_switch_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_packages[ 1 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_package->hana_package_id ).

  ENDMETHOD.

  METHOD switch_on_single_package.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD single_package_no_switch.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'PACK3.DEVCLASS3.NO.SWITCH' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD single_package_no_devclass.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack4.no.devclass' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD single_package_switch_standby.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack5.devclass5.switch5.standby' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD two_packages_0_switch_off.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack6.devclass6.Switch6.on' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD two_packages_1_switch_off.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_switch_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lt_switch_packages[ 1 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).
  ENDMETHOD.

  METHOD two_packages_2_switches_off.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack7.devclass7.switch7.off' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH7_OFF' act = lt_switch_packages[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS7_SWITCH7_OFF' act = lt_switch_packages[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lt_switch_packages[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lt_switch_packages[ 2 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).
    lr_hot_package = lt_switch_packages[ 2 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lr_hot_package->hana_package_id ).
  ENDMETHOD.

  METHOD two_packages_same_devclass_off.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off.2' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lt_switch_packages[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off.2' act = lt_switch_packages[ 2 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).
    lr_hot_package = lt_switch_packages[ 2 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off.2' act = lr_hot_package->hana_package_id ).
  ENDMETHOD.

  METHOD two_packages_same_devclass_on.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on.2' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD complex_case_packages.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off.2' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1_1.Switch1.off.3' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on.2' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2_1.Switch2.on.2' ) )
                                ( cl_cts_hot_package=>create_instance( 'PACK3.DEVCLASS3.NO.SWITCH' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack4.no.devclass' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack5.devclass5.switch5.standby' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack6.devclass6.Switch6.on' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack7.devclass7.switch7.off' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_switch_packages ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.Switch1.off.3' act = lt_switch_packages[ 1 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.Switch1.off.3' act = lr_hot_package->hana_package_id ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lt_switch_packages[ 2 ]-hana_package_id ).
    lr_hot_package = lt_switch_packages[ 2 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 3 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 3 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off.2' act = lt_switch_packages[ 3 ]-hana_package_id ).
    lr_hot_package = lt_switch_packages[ 3 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off.2' act = lr_hot_package->hana_package_id ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH7_OFF' act = lt_switch_packages[ 4 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS7_SWITCH7_OFF' act = lt_switch_packages[ 4 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lt_switch_packages[ 4 ]-hana_package_id ).
    lr_hot_package = lt_switch_packages[ 4 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lr_hot_package->hana_package_id ).


*    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_packages[ 4 ]-hot_packages ) ).
*    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_packages[ 1 ].
*    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).
*    lr_hot_package = lt_switch_packages[ 1 ]-hot_packages[ 2 ].
*    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off.2' act = lr_hot_package->hana_package_id ).
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_switch_packages[ 2 ]-hot_packages ) ).
*    lr_hot_package = lt_switch_packages[ 2 ]-hot_packages[ 1 ].
*    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lr_hot_package->hana_package_id ).
  ENDMETHOD.

  METHOD read_same_package_twice.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

  METHOD switch_off_single_object.
    DATA: lt_switch_id_hot_objects TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object.

    DATA(lr_hot_object) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object1'
                       iv_hana_object_suffix = 'suffix1' ).

    m_cut->get_off_switches_for_objects(
      EXPORTING i_hot_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( lr_hot_object ) )
      IMPORTING e_switch_id_hot_objects = lt_switch_id_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_switch_id_hot_objects ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 1 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lr_hot_object->hana_object_suffix ).
  ENDMETHOD.

  METHOD switch_on_single_object.
    DATA: lt_switch_id_hot_objects TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object.

    DATA(lr_hot_object) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack2.devclass2.switch2.on'
                       iv_hana_object_name   = 'object2'
                       iv_hana_object_suffix = 'suffix2' ).

    m_cut->get_off_switches_for_objects(
      EXPORTING i_hot_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( lr_hot_object ) )
      IMPORTING e_switch_id_hot_objects = lt_switch_id_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_id_hot_objects ) ).
  ENDMETHOD.

  METHOD complex_case_objects.
    DATA: lt_switch_id_hot_objects TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object.

    DATA(lr_hot_object) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object1'
                       iv_hana_object_suffix = 'suffix1' ).
    DATA(lr_hot_object2) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object2'
                       iv_hana_object_suffix = 'suffix2' ).
    DATA(lr_hot_object3) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1_1.switch1.off.3'
                       iv_hana_object_name   = 'object3'
                       iv_hana_object_suffix = 'suffix3' ).
    DATA(lr_hot_object4) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off.2'
                       iv_hana_object_name   = 'object4'
                       iv_hana_object_suffix = 'suffix4' ).
    DATA(lr_hot_object5) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack2.devclass2.Switch2.on'
                       iv_hana_object_name   = 'object5'
                       iv_hana_object_suffix = 'suffix5' ).
    DATA(lr_hot_object6) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack2.devclass2.Switch2.on.2'
                       iv_hana_object_name   = 'object6'
                       iv_hana_object_suffix = 'suffix6' ).
    DATA(lr_hot_object7) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'PACK3.DEVCLASS3.NO.SWITCH'
                       iv_hana_object_name   = 'object7'
                       iv_hana_object_suffix = 'suffix7' ).
    DATA(lr_hot_object8) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack4.no.devclass'
                       iv_hana_object_name   = 'object8'
                       iv_hana_object_suffix = 'suffix8' ).
    DATA(lr_hot_object9) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack5.devclass5.switch5.standby'
                       iv_hana_object_name   = 'object9'
                       iv_hana_object_suffix = 'suffix9' ).
    DATA(lr_hot_object10) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack6.devclass6.Switch6.on'
                       iv_hana_object_name   = 'object10'
                       iv_hana_object_suffix = 'suffix10' ).
    DATA(lr_hot_object11) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack7.devclass7.switch7.off'
                       iv_hana_object_name   = 'object11'
                       iv_hana_object_suffix = 'suffix11' ).

    m_cut->get_off_switches_for_objects(
      EXPORTING i_hot_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( lr_hot_object ) ( lr_hot_object2 )
                                                                                       ( lr_hot_object3 ) ( lr_hot_object4 )
                                                                                       ( lr_hot_object5 ) ( lr_hot_object6 )
                                                                                       ( lr_hot_object7 ) ( lr_hot_object8 )
                                                                                       ( lr_hot_object9 ) ( lr_hot_object10 )
                                                                                       ( lr_hot_object11 ) )
      IMPORTING e_switch_id_hot_objects = lt_switch_id_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( lt_switch_id_hot_objects ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.switch1.off.3' act = lt_switch_id_hot_objects[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object3' act = lt_switch_id_hot_objects[ 1 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix3' act = lt_switch_id_hot_objects[ 1 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 1 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.switch1.off.3' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object3' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix3' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 2 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lt_switch_id_hot_objects[ 2 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lt_switch_id_hot_objects[ 2 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 2 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 3 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 3 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 3 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lt_switch_id_hot_objects[ 3 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lt_switch_id_hot_objects[ 3 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 3 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 4 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 4 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off.2' act = lt_switch_id_hot_objects[ 4 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object4' act = lt_switch_id_hot_objects[ 4 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix4' act = lt_switch_id_hot_objects[ 4 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 4 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off.2' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object4' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix4' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH7_OFF' act = lt_switch_id_hot_objects[ 5 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS7_SWITCH7_OFF' act = lt_switch_id_hot_objects[ 5 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lt_switch_id_hot_objects[ 5 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object11' act = lt_switch_id_hot_objects[ 5 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix11' act = lt_switch_id_hot_objects[ 5 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 5 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack7.devclass7.switch7.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object11' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix11' act = lr_hot_object->hana_object_suffix ).
  ENDMETHOD.

  METHOD switch_off_2_objects_diff_pack.
    DATA: lt_switch_id_hot_objects TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object.

    DATA(lr_hot_object) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object1'
                       iv_hana_object_suffix = 'suffix1' ).
    DATA(lr_hot_object2) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off.2'
                       iv_hana_object_name   = 'object2'
                       iv_hana_object_suffix = 'suffix2' ).

    m_cut->get_off_switches_for_objects(
      EXPORTING i_hot_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( lr_hot_object )
                                                                                       ( lr_hot_object2 ) )
      IMPORTING e_switch_id_hot_objects = lt_switch_id_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_id_hot_objects ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 1 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off.2' act = lt_switch_id_hot_objects[ 2 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lt_switch_id_hot_objects[ 2 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lt_switch_id_hot_objects[ 2 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 2 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off.2' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lr_hot_object->hana_object_suffix ).
  ENDMETHOD.

  METHOD switch_off_2_objects_same_pack.
    DATA: lt_switch_id_hot_objects TYPE lcl_switch_framework_accessor=>tt_switch_id_devc_hot_object.

    DATA(lr_hot_object) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object1'
                       iv_hana_object_suffix = 'suffix1' ).
    DATA(lr_hot_object2) = cl_cts_hot_object_v1=>create_instance(
                       iv_hana_package_id    = 'pack1.devclass1.switch1.off'
                       iv_hana_object_name   = 'object2'
                       iv_hana_object_suffix = 'suffix2' ).

    m_cut->get_off_switches_for_objects(
      EXPORTING i_hot_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( lr_hot_object )
                                                                                       ( lr_hot_object2 ) )
      IMPORTING e_switch_id_hot_objects = lt_switch_id_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_id_hot_objects ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lt_switch_id_hot_objects[ 1 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 1 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object1' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix1' act = lr_hot_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_id_hot_objects[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lt_switch_id_hot_objects[ 2 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lt_switch_id_hot_objects[ 2 ]-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lt_switch_id_hot_objects[ 2 ]-hana_object_suffix ).
    lr_hot_object = lt_switch_id_hot_objects[ 2 ]-hot_object.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.switch1.off' act = lr_hot_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'object2' act = lr_hot_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 'suffix2' act = lr_hot_object->hana_object_suffix ).
  ENDMETHOD.

  METHOD two_packages_2_devc_1_sw_off.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1.Switch1.off' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack1.devclass1_1.Switch1.off.3' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_switch_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 1 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'SWITCH1_OFF' act = lt_switch_packages[ 2 ]-switch_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_1_SWITCH1_OFF' act = lt_switch_packages[ 1 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'DEVCLASS1_SWITCH1_OFF' act = lt_switch_packages[ 2 ]-devclass ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.Switch1.off.3' act = lt_switch_packages[ 1 ]-hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lt_switch_packages[ 2 ]-hana_package_id ).
    DATA(lr_hot_package) = lt_switch_packages[ 1 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1_1.Switch1.off.3' act = lr_hot_package->hana_package_id ).
    lr_hot_package = lt_switch_packages[ 2 ]-hot_package.
    cl_abap_unit_assert=>assert_equals( exp = 'pack1.devclass1.Switch1.off' act = lr_hot_package->hana_package_id ).
  ENDMETHOD.

  METHOD two_packages_2_devc_1_sw_on.
    DATA: lt_switch_packages TYPE lcl_switch_framework_accessor=>tt_switch_id_hot_package.

    m_cut->get_off_switches_for_packages(
      EXPORTING i_hot_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2.Switch2.on' ) )
                                ( cl_cts_hot_package=>create_instance( 'pack2.devclass2_1.Switch2.on.3' ) ) )
      IMPORTING e_switch_id_hot_packages = lt_switch_packages ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_switch_packages ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_external_viewhandler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Test set_max_severity with i_severity = any severity and c_severity is already 'A'.<br/>
      "! c_severity should stay a for any severity input.
      set_max_severity_a FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with i_severity = any severity and c_severity is 'E'.<br/>
      "! c_severity should stay 'E' for any severity but change to 'A' if i_severity = 'A'.
      set_max_severity_e FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with i_severity = any severity and c_severity is 'W'.<br/>
      "! c_severity should stay 'W' for any severity but change to 'A' or 'E' if i_severity = 'A' or 'E'.
      set_max_severity_w FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with i_severity = any severity and c_severity is 'I'.<br/>
      "! c_severity should change to 'W' or 'E' or 'A' if i_severity = 'W' or 'E' or 'A'.
      set_max_severity_i FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with i_severity = any severity and c_severity is space.<br/>
      "! c_severity should change to 'W' or 'E' or 'A' if i_severity = 'W' or 'E' or 'A'.
      set_max_severity_space FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_external_viewhandler IMPLEMENTATION.
  METHOD set_max_severity_a.
    DATA(view_handler) = NEW lcl_external_viewhandler( ).

    DATA(lv_severity) = 'A'.
    view_handler->set_max_severity( EXPORTING i_severity = space
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'I'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'W'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'E'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'A'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_e.
    DATA(view_handler) = NEW lcl_external_viewhandler( ).

    DATA(lv_severity) = 'E'.
    view_handler->set_max_severity( EXPORTING i_severity = space
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'I'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'W'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'E'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'A'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_w.
    DATA(view_handler) = NEW lcl_external_viewhandler( ).

    DATA(lv_severity) = 'W'.
    view_handler->set_max_severity( EXPORTING i_severity = space
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'I'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'W'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'E'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = 'W'. "reset previous test
    view_handler->set_max_severity( EXPORTING i_severity = 'A'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_i.
    DATA(view_handler) = NEW lcl_external_viewhandler( ).

    DATA(lv_severity) = 'I'.
    view_handler->set_max_severity( EXPORTING i_severity = space
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'I' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'I'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'I' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'W'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    lv_severity = 'I'. "reset previous test
    view_handler->set_max_severity( EXPORTING i_severity = 'E'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = 'I'. "reset previous test
    view_handler->set_max_severity( EXPORTING i_severity = 'A'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_space.
    DATA(view_handler) = NEW lcl_external_viewhandler( ).

    DATA(lv_severity) = space.
    view_handler->set_max_severity( EXPORTING i_severity = space
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'I' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'I'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'I' ).

    view_handler->set_max_severity( EXPORTING i_severity = 'W'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    lv_severity = space. "reset previous test
    view_handler->set_max_severity( EXPORTING i_severity = 'E'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = space. "reset previous test
    view_handler->set_max_severity( EXPORTING i_severity = 'A'
                                    CHANGING  c_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

ENDCLASS.
CLASS ltd_logger DEFINITION INHERITING FROM lcl_logger FOR TESTING.
  PUBLIC SECTION.
    METHODS:
      "Redefine write log, so that no real logfile gets written
      write_log REDEFINITION,

      get_log_messages
        RETURNING
          VALUE(r_messages) TYPE tt_sprot_u.
ENDCLASS.

CLASS ltd_logger IMPLEMENTATION.
  METHOD write_log.
    "do nothing
  ENDMETHOD.

  METHOD get_log_messages.
    r_messages = mt_messages.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_text_deploy_result_logger DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      "! setup class under test and set default mock data valid for all tests
      setup RAISING cx_static_check,
      "! Verify the logs, compares tables i_act_messages and i_exp_messages
      verify_log
        IMPORTING
          i_act_messages TYPE tt_sprot_u
          i_exp_messages TYPE tt_sprot_u,

      "! Test write header if at least one HOTO (LIMU or LANG) item was part of TR
      write_header FOR TESTING RAISING cx_static_check,
      "! Test write footer if at least one HOTO (LIMU or LANG) item was part of TR
      write_footer FOR TESTING RAISING cx_static_check,
      "! Test that there are no log entries if no items of this TR.
      write_no_log FOR TESTING RAISING cx_static_check,
      "! Test Write for not active object
      write_not_active FOR TESTING RAISING cx_static_check,
      "! Test Write for unknown object
      write_unknown FOR TESTING RAISING cx_static_check,
      "! Test Write for skipped object
      write_skipped FOR TESTING RAISING cx_static_check,
      "! Test Write for 1 failed object and 1 lang
      write_failed_1obj_1lang FOR TESTING RAISING cx_static_check,
      "! Test Write for 1 OK object and 1 lang
      write_ok_1obj_1lang FOR TESTING RAISING cx_static_check,
      "! Test Write for 1 object for which 1 language works and 1 fails.
      write_ok_and_failed_1obj_2lang FOR TESTING RAISING cx_static_check,
      "! Test a more complex scenario: 1 object with 2 languages in 3 transport requests. all text deployed OK.<br/>
      "! TR1 contains 1 entry, LIMU HOTO (= all languages= D and E)<br/>
      "! TR2 contains 1 entry, LANG HOTO with language D<br/>
      "! TR3 contains 1 entry, LANG HOTO with language F which was not deployed at all<br/>
      write_complex_3trs_1obj_2lang FOR TESTING RAISING cx_static_check,
      "! Some complex test as tested manually in STMS
      write_complex_test FOR TESTING RAISING cx_static_check.

    DATA:
      "! 1 LIMU HOTO part of request with translation
      m_cut_limu     TYPE REF TO lcl_text_deploy_result_logger,
      "! 1 LANG HOTO part of request
      m_cut_lang     TYPE REF TO lcl_text_deploy_result_logger,
      "! No object with translation part of request
      m_cut_none     TYPE REF TO lcl_text_deploy_result_logger,
      m_hot_object_1 TYPE REF TO cl_cts_hot_object_v1,
      m_hot_object_2 TYPE REF TO cl_cts_hot_object_v1,
      m_hot_object_3 TYPE REF TO cl_cts_hot_object_v1,
      m_hot_object_4 TYPE REF TO cl_cts_hot_object_v1,
      m_hot_object_5 TYPE REF TO cl_cts_hot_object_v1.

ENDCLASS.

CLASS ltcl_text_deploy_result_logger IMPLEMENTATION.
  METHOD setup.
    "Generate Test data
    "test data (objects, db entries, ...)
    m_hot_object_1 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT1'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_2 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test.with.some.long.package.name'
                     iv_hana_object_name      = 'OBJECT1_WTIH_SOME_LONG_NAME'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_3 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT3'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_4 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'some'
                     iv_hana_object_suffix    = 'hdbtextbundle'
                 ).

    m_hot_object_5 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT5'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    "1 LIMU test data
    DATA(lt_e071) = VALUE tt_e071_hot_refs( ( trkorr = 'ABCK000001' pgmid = 'LIMU' object = 'HOTO' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_1 ) ).
    m_cut_limu = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000001' ).
    m_cut_limu->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000001.tmp' ).

    "1 LANG test data
    lt_e071 = VALUE tt_e071_hot_refs( ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_1 lang = 'D' ) ).
    m_cut_lang = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000002' ).
    m_cut_lang->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000002.tmp' ).

    "no valid object test data (use a request that has no HOTO object at all)
    m_cut_none = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'UNK000001' ).

  ENDMETHOD.

  METHOD write_header.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u( ( msgnr = '593' level = '3' newobj = 'X' ag = gc_ag langu = gc_langu ) ). "Begin Textdeployment

    " execute business method for LIMU
    m_cut_limu->write_header( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

    " execute business method for LANG
    m_cut_lang->write_header( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_footer.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u( ( msgnr = '594' level = '3' ag = gc_ag langu = gc_langu ) ). "End Textdeployment

    " execute business method for LIMU
    m_cut_limu->write_footer( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

    " execute business method for LANG
    m_cut_lang->write_footer( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_no_log.
    " verfiy that logger is not for m_cut_none.
    cl_abap_unit_assert=>assert_initial( m_cut_none->mr_logger ).

    " execute business method
    m_cut_none->write_header( ).

    "verify
    cl_abap_unit_assert=>assert_initial( m_cut_none->mr_logger ).

    " execute business method
    m_cut_none->write_footer( ).

    "verify
    cl_abap_unit_assert=>assert_initial( m_cut_none->mr_logger ).

    " execute business method
    m_cut_none->write_log( ).

    "verify
    cl_abap_unit_assert=>assert_initial( m_cut_none->mr_logger ).
  ENDMETHOD.

  METHOD write_not_active.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '3' severity = 'W' var1 = '1' ag = gc_ag langu = gc_langu ) "Skipped objects due to import/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '3' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '606' level = '3' severity = 'W' ag = gc_ag langu = gc_langu ) "Following objects skipped due to errors:
          ( msgnr = '533' level = '3' severity = 'W' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't)' ag = gc_ag langu = gc_langu ) "&1&2&3&4
     ).

*1. Test LIMU HOTO
    m_cut_limu->set_not_active_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    m_cut_lang->set_not_active_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_unknown.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '605' level = '4' ag = gc_ag langu = gc_langu ) "Following objects not found in HTA:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't)' ag = gc_ag langu = gc_langu ) "&1&2&3&4
     ).

*1. Test LIMU HOTO
    m_cut_limu->set_unknown_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    m_cut_lang->set_unknown_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_skipped.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '602' level = '4' ag = gc_ag langu = gc_langu ) "No texts found for following objects:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't)' ag = gc_ag langu = gc_langu ) "&1&2&3&4
     ).

*1. Test LIMU HOTO
    m_cut_limu->set_skipped_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    m_cut_lang->set_skipped_objects( VALUE #( ( m_hot_object_1 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_failed_1obj_1lang.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '2' severity = 'E' var1 = '1' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '604' level = '2' severity = 'E' newobj = 'X' ag = gc_ag langu = gc_langu ) "Following objects had errors during text deployment:
          ( msgnr = '533' level = '2' severity = 'E' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) language: D/de' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '2' severity = 'E' var1 = '  1234: some error' ag = gc_ag langu = gc_langu ) "&1&2&3&4
     ).

*1. Test LIMU HOTO
    m_cut_limu->set_failed_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' hana_error_code = '1234' hana_error_message = 'some error' imported_text_count = 0 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    m_cut_lang->set_failed_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' hana_error_code = '1234' hana_error_message = 'some error' imported_text_count = 0 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_ok_1obj_1lang.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '8' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - D/de:8' ag = gc_ag langu = gc_langu ) "&1&2&3&4
     ).

*1. Test LIMU HOTO
    m_cut_limu->set_ok_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' imported_text_count = 8 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    m_cut_lang->set_ok_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' imported_text_count = 8 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.


  METHOD verify_log.
    cl_abap_unit_assert=>assert_equals( act = lines( i_act_messages ) exp = lines( i_exp_messages ) ).

    LOOP AT i_exp_messages INTO DATA(ls_exp_log).
      cl_abap_unit_assert=>assert_equals( act = i_act_messages[ sy-tabix ] exp = ls_exp_log ).
    ENDLOOP.
  ENDMETHOD.

  METHOD write_ok_and_failed_1obj_2lang.
    "Created expected log messages:
    DATA(lt_expected_log) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '8' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '2' severity = 'E' var1 = '1' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '604' level = '2' severity = 'E' newobj = 'X' ag = gc_ag langu = gc_langu ) "Following objects had errors during text deployment:
          ( msgnr = '533' level = '2' severity = 'E' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) language: E/en' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '2' severity = 'E' var1 = '  1234: some error' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - D/de:8' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).

*1. Test LIMU HOTO
    m_cut_limu->set_ok_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' imported_text_count = 8 ) ) ).
    m_cut_limu->set_failed_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'E' hana_lang = 'en' hana_error_code = '1234' hana_error_message = 'some error' imported_text_count = 8 ) ) ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).

*2. Test LANG HOTO
    "create cut
    DATA(lt_e071) = VALUE tt_e071_hot_refs( ( trkorr = 'ABCK000001' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_1 )
                                            ( trkorr = 'ABCK000001' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_1 ) ).
    m_cut_lang = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000001' ).
    m_cut_lang->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000001.tmp' ).

    "execute business methods
    m_cut_lang->set_ok_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' imported_text_count = 8 ) ) ).
    m_cut_lang->set_failed_text_deploy_results( VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'E' hana_lang = 'en' hana_error_code = '1234' hana_error_message = 'some error' imported_text_count = 8 ) ) ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log ).
  ENDMETHOD.

  METHOD write_complex_3trs_1obj_2lang.
    "created test data input (E071 entries)
    DATA(lt_e071) = VALUE tt_e071_hot_refs( ( trkorr = 'ABCK000001' pgmid = 'LIMU' object = 'HOTO' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_1 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_1 )
                                            ( trkorr = 'ABCK000003' pgmid = 'LANG' object = 'HOTO' lang = 'F' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000003.tmp' cts_hot_object = m_hot_object_1 ) ).

    "create text deploy result
    DATA lt_ok_deploy_results TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results.
    lt_ok_deploy_results = VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'D' hana_lang = 'de' imported_text_count = 8 )
                                    ( cts_hot_object = m_hot_object_1 abap_lang = 'E' hana_lang = 'en' imported_text_count = 8 ) ).

    "Created expected log messages
    "For TR1 with LIMU: D and E are expected as languages because LIMU object is on request
    DATA(lt_expected_log_limu) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '16' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de E/en' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - D/de:8 E/en:8' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).
    "For TR2 with LANG: only D with 8 deployed texts is expected in log because only LANG HOTO with language D is on the request
    DATA(lt_expected_log_lang) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '8' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - D/de:8' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).
    "For TR3 with LANG: lang F not known in HTA, so skipped in log
    DATA(lt_expected_log_lang2) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '602' level = '4' ag = gc_ag langu = gc_langu ) "No texts found for following objects:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - F' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).

*1. Test LIMU HOTO (all languages)
    "create cut
    m_cut_limu = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000001' ).
    m_cut_limu->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000001.tmp' ).

    m_cut_limu->set_ok_text_deploy_results( lt_ok_deploy_results ).
    m_cut_limu->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_limu->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log_limu ).

*2. Test LANG HOTO (language D)
    "create cut
    m_cut_lang = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000002' ).
    m_cut_lang->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000002.tmp' ).

    "execute business methods
    m_cut_lang->set_ok_text_deploy_results( lt_ok_deploy_results ).
    m_cut_lang->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log_lang ).

*3. Test LANG HOTO (language F)
    "create cut
    DATA(m_cut_lang2) = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000003' ).
    m_cut_lang2->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000003.tmp' ).

    "execute business methods
    m_cut_lang2->set_ok_text_deploy_results( lt_ok_deploy_results ).
    m_cut_lang2->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_lang2->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log_lang2 ).
  ENDMETHOD.

  METHOD write_complex_test.
    "Further test objects compared to setup
    DATA(lr_hot_object_6) = cl_cts_hot_object_v1=>create_instance(
                            iv_hana_package_id       = m_hot_object_1->hana_package_id
                            iv_hana_object_name      = 'OBJECT6'
                            iv_hana_object_suffix    = m_hot_object_1->hana_object_suffix ).
    DATA(lr_hot_object_7) = cl_cts_hot_object_v1=>create_instance(
                            iv_hana_package_id       = m_hot_object_1->hana_package_id
                            iv_hana_object_name      = 'OBJECT7'
                            iv_hana_object_suffix    = m_hot_object_1->hana_object_suffix ).
    DATA(lr_hot_object_8) = cl_cts_hot_object_v1=>create_instance(
                            iv_hana_package_id       = m_hot_object_1->hana_package_id
                            iv_hana_object_name      = 'OBJECT8'
                            iv_hana_object_suffix    = m_hot_object_1->hana_object_suffix ).

    "created test data input (E071 entries)
    DATA(lt_e071) = VALUE tt_e071_hot_refs( ( trkorr = 'ABCK000001' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_1 )
                                            ( trkorr = 'ABCK000001' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = m_hot_object_3->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_3 )
                                            ( trkorr = 'ABCK000001' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_4->transport_object_name lockflag = '2' logfile = 'ABCK000001.tmp' cts_hot_object = m_hot_object_4 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = m_hot_object_1->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_1 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_3->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_3 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = m_hot_object_3->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_3 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = m_hot_object_4->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_4 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = m_hot_object_4->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = m_hot_object_4 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = lr_hot_object_6->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = lr_hot_object_6 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'D' obj_name = lr_hot_object_7->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = lr_hot_object_7 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = lr_hot_object_7->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = lr_hot_object_7 )
                                            ( trkorr = 'ABCK000002' pgmid = 'LANG' object = 'HOTO' lang = 'E' obj_name = lr_hot_object_8->transport_object_name lockflag = '2' logfile = 'ABCK000002.tmp' cts_hot_object = lr_hot_object_8 )
    ).

    "create text deploy result
    DATA lt_ok_deploy_results TYPE cl_cts_hot_hana_connector=>ty_text_deploy_results.
    lt_ok_deploy_results = VALUE #( ( cts_hot_object = m_hot_object_1 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
                                    ( cts_hot_object = m_hot_object_3 abap_lang = 'D' hana_lang = 'de' imported_text_count = 11 )
                                    ( cts_hot_object = m_hot_object_3 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
                                    ( cts_hot_object = m_hot_object_4 abap_lang = 'D' hana_lang = 'de' imported_text_count = 1 )
                                    ( cts_hot_object = m_hot_object_4 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
                                    ( cts_hot_object = lr_hot_object_6 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
                                    ( cts_hot_object = lr_hot_object_7 abap_lang = 'D' hana_lang = 'de' imported_text_count = 11 )
                                    ( cts_hot_object = lr_hot_object_7 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
                                    ( cts_hot_object = lr_hot_object_8 abap_lang = 'E' hana_lang = 'en' imported_text_count = 11 )
    ).

    "Created expected log messages
    DATA(lt_expected_log_tr1) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '2' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '12' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de E/en' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '1' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT3.suffix (com.package.tes' var2 = 't) - E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'some.hdbtextbundle (com.package' var2 = '.test) - D/de:1' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '602' level = '4' ag = gc_ag langu = gc_langu ) "No texts found for following objects:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - D' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).

    "For TR2 all objects as part of LANG HOTA are OK deployed with texts in D and E
    DATA(lt_expected_log_tr2) = VALUE tt_sprot_u(
          ( msgnr = '596' level = '3' var1 = '6' ag = gc_ag langu = gc_langu ) "Objects for which text was deployed: &1
          ( msgnr = '597' level = '3' var1 = '89' ag = gc_ag langu = gc_langu ) "Deployed texts: &1
          ( msgnr = '598' level = '3' var1 = 'D/de E/en' ag = gc_ag langu = gc_langu )  "Deployed languages: &1
          ( msgnr = '600' level = '4' var1 = '0' ag = gc_ag langu = gc_langu )  "Objects without textdeployment due to import-/activation errors: &1
          ( msgnr = '601' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Objects without textdeployment: &1
          ( msgnr = '603' level = '3' var1 = '0' ag = gc_ag langu = gc_langu ) "Object for which text deployment failed: &1
          ( msgnr = '507' level = '4' ag = gc_ag langu = gc_langu ) "empty line
          ( msgnr = '599' level = '4' ag = gc_ag langu = gc_langu ) "Following objects with text deployment:
          ( msgnr = '533' level = '4' var1 = 'OBJECT1.suffix (com.package.tes' var2 = 't) - E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'OBJECT3.suffix (com.package.tes' var2 = 't) - D/de:11 E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'OBJECT6.suffix (com.package.tes' var2 = 't) - E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'OBJECT7.suffix (com.package.tes' var2 = 't) - D/de:11 E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'OBJECT8.suffix (com.package.tes' var2 = 't) - E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
          ( msgnr = '533' level = '4' var1 = 'some.hdbtextbundle (com.package' var2 = '.test) - D/de:1 E/en:11' ag = gc_ag langu = gc_langu ) "&1&2&3&4
    ).

*1. Test TR1
    "create cut
    DATA(m_cut_tr1) = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000001' ).
    m_cut_tr1->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000001.tmp' ).

    m_cut_tr1->set_ok_text_deploy_results( lt_ok_deploy_results ).
    m_cut_tr1->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_tr1->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log_tr1 ).

*2. Test TR2
    "create cut
    DATA(m_cut_tr2) = NEW lcl_text_deploy_result_logger( i_e071_entries = lt_e071 i_trkorr = 'ABCK000002' ).
    m_cut_tr2->mr_logger =  NEW ltd_logger( i_logfile = 'ABCK000002.tmp' ).

    m_cut_tr2->set_ok_text_deploy_results( lt_ok_deploy_results ).
    m_cut_tr2->write_log( ).

    "verify
    verify_log( i_act_messages = CAST ltd_logger( m_cut_tr2->mr_logger )->get_log_messages( ) i_exp_messages = lt_expected_log_tr2 ).

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class ltcl_amhc_deletion
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS ltcl_amhc_deletion DEFINITION FOR TESTING
 RISK LEVEL HARMLESS   DURATION SHORT. .
  PRIVATE SECTION.
    DATA:
      mt_test_e071       TYPE STANDARD TABLE OF e071 WITH EMPTY KEY,
      mt_test_amhc_def   TYPE STANDARD TABLE OF cts_amhc_def WITH EMPTY KEY,
      mt_test_amhc_map   TYPE STANDARD TABLE OF cts_amhc_map WITH EMPTY KEY,
      mt_test_hdi_object TYPE STANDARD TABLE OF cts_hdi_object WITH EMPTY KEY,
      mv_trkorr          TYPE e070-trkorr VALUE 'TSTK999999'.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    CLASS-DATA:
      mr_environment TYPE REF TO if_osql_test_environment.

    METHODS:
      setup RAISING cx_static_check,
      test_delete FOR TESTING RAISING cx_static_check,
      test_delete_type_skipped FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_amhc_deletion IMPLEMENTATION.
  METHOD class_setup.
    mr_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'E071' ) ( 'TADIR' )
    ( 'CTS_AMHC_DEF' ) ( 'CTS_AMHC_MAP' ) ( 'CTS_HDI_OBJECT' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mr_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mr_environment->clear_doubles( ).
    mt_test_e071 = VALUE #( ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC' as4pos = 1 trkorr = mv_trkorr objfunc = 'D' lockflag = 2 )
    ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC_A' as4pos = 2 trkorr = mv_trkorr objfunc = 'D' lockflag = 2 )
    ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC_SKIP_A' as4pos = 3 trkorr = mv_trkorr objfunc = 'D' lockflag = 2 )  ).
    mt_test_amhc_def = VALUE #( ( logical_container_name = 'TEST_AMHC' version = 'A' container_type = 'R' )
    ( logical_container_name = 'TEST_AMHC' version = 'I' container_type = 'R' )
    ( logical_container_name = 'TEST_AMHC_A' version = 'A' container_type = 'R' )
    ( logical_container_name = 'TEST_AMHC_SKIP_A' version = 'A' container_type = 'A' ) ).
    mt_test_amhc_map = VALUE #( ( logical_container_name = 'TEST_AMHC' physical_container_name = 'TEST_AMHC_0001' )
    ( logical_container_name = 'TEST_AMHC_A' physical_container_name = 'TEST_AMHC_A_0001' )
    ( logical_container_name = 'TEST_AMHC_SKIP_A' physical_container_name = 'TEST_AMHC_SKIP_A_0001' ) ).
    mt_test_hdi_object = VALUE #( (  abap_hdi_cont_namespace = 'TEST.AMHC/TEST_AMHC' abap_hdi_obj_path_name_suffix = 'CFG/TEST.AMHC/.HDINAMESPACE'
    abap_status = 'A'     hot_status = 'A'  hdi_container =  'TEST_AMHC'  hdi_namespace = 'TEST.AMHC' hdi_object_path = 'cfg/TEST.AMHC'
    hdi_object_suffix = '.hdinamespace' )
    (  abap_hdi_cont_namespace = 'TEST.AMHC/TEST_AMHC' abap_hdi_obj_path_name_suffix = 'SRC/TEST.AMHC/.HDINAMESPACE'
    abap_status = 'A'     hot_status = 'A'  hdi_container =  'TEST_AMHC'  hdi_namespace = 'TEST.AMHC' hdi_object_path = 'src/TEST.AMHC'
    hdi_object_suffix = '.hdinamespace' ) ).
    mr_environment->insert_test_data( mt_test_e071 ).
    mr_environment->insert_test_data( mt_test_amhc_def ).
    mr_environment->insert_test_data( mt_test_amhc_map ).
    mr_environment->insert_test_data( mt_test_hdi_object ).
  ENDMETHOD.

  METHOD test_delete.
    DATA: lt_e071_amhc_ref_del TYPE ty_t_e071_amhc_ref.
    DATA(lr_logger_memory) = cl_cts_hot_logger_memory=>create_instance( 'SCTS_HDI' ).
    lt_e071_amhc_ref_del = VALUE #(
    ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC' as4pos = 1 trkorr = mv_trkorr lockflag = 2  logger = lr_logger_memory )
    ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC_A' as4pos = 2 trkorr = mv_trkorr lockflag = 2 logger = lr_logger_memory ) ).

    PERFORM execute_container_deletion IN PROGRAM rddhanadeployment USING lt_e071_amhc_ref_del ''.
    " the def table will not be cleared as none of containers was deleted because they did not exist in HDI and thus HDI returns RC=-1
    " and therefore in AMHC tables nothing is deleted. One container has version I
    SELECT * FROM cts_amhc_def INTO TABLE @DATA(lt_mahc_def_1). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  =             4                        " Datenobjekt mit erwartetem Wert
        act                  =    lines( lt_mahc_def_1 )       ).                          " Datenobjekt mit akutellem Wert
    " the map table will not be cleared as none of containers was deleted because they did not exist in HDI and thus HDI returns RC=-1
    " and therefore in AMHC tables nothing is deleted. One container has version I
    SELECT  * FROM cts_amhc_map INTO TABLE @DATA(lt_amhc_map_1). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  =             3                        " Datenobjekt mit erwartetem Wert
        act                  =    lines( lt_amhc_map_1 )       ).


    DATA(lt_log_messages) = CAST cl_cts_hot_logger_memory( lr_logger_memory )->get_log_messages( ).
    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = lt_log_messages    ).
    " now the entry with I version is deleted
    DELETE FROM cts_amhc_def WHERE version = 'I'.       "#EC CI_NOFIRST

    PERFORM execute_container_deletion IN PROGRAM rddhanadeployment USING lt_e071_amhc_ref_del space.
    " the def table will not be cleared as none of containers exist on DB level
    SELECT * FROM cts_amhc_def INTO TABLE @DATA(lt_mahc_def_2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  =             3                        " Datenobjekt mit erwartetem Wert
        act                  =    lines( lt_mahc_def_2 )       ).                          " Datenobjekt mit akutellem Wert
    " the map table will not be cleared as none of containers exist on DB level
    SELECT  * FROM cts_amhc_map INTO TABLE @DATA(lt_amhc_map_2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  =             3                        " Datenobjekt mit erwartetem Wert
        act                  =    lines( lt_amhc_map_2 )       ).
    " the e071 table will not be cleared as none of containers exist on DB level
    SELECT  * FROM e071 INTO TABLE @DATA(lt_e071) WHERE obj_name = 'TEST_AMHC'. "#EC CI_NOFIRST
    IF lt_e071 IS NOT INITIAL.
      cl_abap_unit_assert=>assert_equals(
    EXPORTING
      exp                  =             2                        " Datenobjekt mit erwartetem Wert
      act                  =    lt_e071[ 1 ]-lockflag       ).
    ENDIF.
    CLEAR: lt_log_messages.
    lt_log_messages = CAST cl_cts_hot_logger_memory( lr_logger_memory )->get_log_messages( ).
    IF  line_exists( lt_log_messages[ msgnr = 559 ] ).  "HDI-Container &1 (&2) konnte nicht gelöscht werden.
      DATA(lv_line_exists) = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = lv_line_exists  ).
  ENDMETHOD.

  METHOD test_delete_type_skipped.
    DATA: lt_e071_amhc_ref_del TYPE ty_t_e071_amhc_ref.

    DATA(lr_logger_memory) = cl_cts_hot_logger_memory=>create_instance( 'SCTS_HDI' ).
    lt_e071_amhc_ref_del = VALUE #(
        ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC_SKIP_A' as4pos = 1 trkorr = mv_trkorr lockflag = 2  logger = lr_logger_memory )
        ( pgmid = 'R3TR' object = 'AMHC' obj_name = 'TEST_AMHC_A' as4pos = 2 trkorr = mv_trkorr lockflag = 2 logger = lr_logger_memory ) ).

    PERFORM execute_container_deletion IN PROGRAM rddhanadeployment USING lt_e071_amhc_ref_del 'A'. "skip API container

    DATA(lt_log_messages) = CAST cl_cts_hot_logger_memory( lr_logger_memory )->get_log_messages( ).
    "expect log message that TEST_AMHC_SKIP_A was skipped
    cl_abap_unit_assert=>assert_table_contains( table = lt_log_messages
                                                line = VALUE sprot_u( ag = 'SCTS_HDI' langu = 'E' msgnr = 585 level = 3 newobj = 'X' severity = 'W' var1 = 'TEST_AMHC_SKIP_A' var2 = 'A' )
                                                msg = |Following logline is missing: 'Skipping deletion for HDI container TEST_AMHC_SKIP_A'| ).

    "do not expect log message "  Deletion of container &1 (&2) started: &3" for TEST_AMHC_SKIP_A
    IF line_exists( lt_log_messages[ msgnr = 536 var1 = 'TEST_AMHC_SKIP_A' ] ).
      cl_abap_unit_assert=>fail( |Following logline was not expected: 'Deletion of container TEST_AMHC_SKIP_A (TEST_AMHC_SKIP_A_0001) started: &3'| ).
    ENDIF.

    "expect log message "  Deletion of container &1 (&2) started: &3" for TEST_AMHC_A
    IF NOT line_exists( lt_log_messages[ msgnr = 536 var1 = 'TEST_AMHC_A' var2 = 'TEST_AMHC_A_0001' ] ).
      cl_abap_unit_assert=>fail( |Following logline is missing: 'Deletion of container &1 (&2) started: &3' with var1 = TEST_AMHC_A and var2 = TEST_AMHC_A_0001| ).
    ENDIF.

    " the map table will not be cleared as none of containers exist on DB level
    SELECT * FROM cts_amhc_map INTO TABLE @DATA(lt_amhc_map). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_amhc_map ) ).
  ENDMETHOD.

ENDCLASS.