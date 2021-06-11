*"* use this source file for your ABAP unit test classes
CLASS ltd_nhi_object DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    "not inheriting class cl_nhi_api as calls to unimplemented methods should and will fail and cl_nhi_object is not published in SNHI_API-SNHI_REPOSITORY_API
    INTERFACES if_nhi_object PARTIALLY IMPLEMENTED.

    TYPES:
      BEGIN OF ty_response_obj,
        cts_hot_object      TYPE REF TO cl_cts_hot_object_v1,
        nhi_read_object_res TYPE REF TO cl_nhi_read_object_res,
      END OF ty_response_obj,
      ty_read_responses_list TYPE STANDARD TABLE OF ty_response_obj WITH DEFAULT KEY.

    DATA:
      read_return_object  TYPE REF TO cl_nhi_read_object_res,
      read_return_objects TYPE ty_read_responses_list,
      read_exception      TYPE REF TO cx_nhi_hana_repository.

    CLASS-METHODS:
      create_stub_instance RETURNING VALUE(r_instance) TYPE REF TO ltd_nhi_object.

ENDCLASS.

CLASS ltd_nhi_object IMPLEMENTATION.

  METHOD create_stub_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.

  METHOD if_nhi_object~create_read_object_req.
    request = cl_nhi_read_object_req=>create_read_object_req(
              winformat            = winformat
              lang                 = lang
              getoutgoingrefs      = getoutgoingrefs
              getincomingrefs      = getincomingrefs
              getreferencedobjects = getreferencedobjects
              object               = object
              session              = session
              version              = version
          ).
  ENDMETHOD.

  METHOD if_nhi_object~read.
    IF read_exception IS BOUND.
      RAISE EXCEPTION read_exception.
    ENDIF.

    "return object as requested
    IF read_return_object IS BOUND.
      "if single return was set by test, return single return object
      response = read_return_object.
    ELSEIF read_return_objects IS NOT INITIAL.
      "if test has set return list then return the fitting object.
      response = VALUE #( ).
      LOOP AT read_return_objects INTO DATA(return_obj).
        IF return_obj IS NOT INITIAL
          AND return_obj-cts_hot_object->hana_package_id = request->object->package
          AND return_obj-cts_hot_object->hana_object_name = request->object->name
          AND return_obj-cts_hot_object->hana_object_suffix = request->object->suffix.
          response = return_obj-nhi_read_object_res.
        ENDIF.
      ENDLOOP.
      "if no result object found, fail
      cl_abap_unit_assert=>assert_bound( act = response msg = 'Internal Error in Unit Test. Mock data provided not the same as used during test.' ).
    ELSE.
      response = VALUE #( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_nhi_package DEFINITION INHERITING FROM cl_nhi_package FINAL FOR TESTING.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_pack_response_obj,
        package              TYPE string,
        nhi_read_package_res TYPE REF TO cl_nhi_read_package_res,
      END OF ty_pack_response_obj,
      ty_read_pack_responses_list TYPE STANDARD TABLE OF ty_pack_response_obj WITH DEFAULT KEY.

    DATA:
      read_pack_return_object  TYPE REF TO cl_nhi_read_package_res,
      read_pack_return_objects TYPE ty_read_pack_responses_list.

    METHODS:
      constructor,

      "redefine all methods that would connect to HANA. By default all below method calls will fail as long as no return object is set.
      if_nhi_package~create REDEFINITION,
      if_nhi_package~delete REDEFINITION,
      if_nhi_package~exists REDEFINITION,
      if_nhi_package~get_editing_state REDEFINITION,
      if_nhi_package~get_sub_packages REDEFINITION,
      if_nhi_package~list REDEFINITION,
      if_nhi_package~list_delivery_units REDEFINITION,
      if_nhi_package~list_root_packages REDEFINITION,
      if_nhi_package~read REDEFINITION,
      if_nhi_package~update REDEFINITION.

    CLASS-METHODS:
      create_stub_instance RETURNING VALUE(r_instance) TYPE REF TO ltd_nhi_package.

ENDCLASS.

CLASS ltd_nhi_package IMPLEMENTATION.

  METHOD constructor.
    "call super class with not existing DBCONNECTION so that in case there is a db call in the tests the call will fail.
    super->constructor( db_conn_name = 'NOTEXISTINGDBCON' ).
  ENDMETHOD.

  METHOD create_stub_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.

  METHOD if_nhi_package~create.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~delete.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~exists.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~get_editing_state.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~get_sub_packages.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~list.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~list_delivery_units.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~list_root_packages.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_nhi_package~read.
    "return object as requested
    IF read_pack_return_object IS BOUND.
      "if single return was set by test, return single return object
      response = read_pack_return_object.
    ELSEIF read_pack_return_objects IS NOT INITIAL.
      "if test has set return list then return the fitting object.
      response = VALUE #( ).
      LOOP AT read_pack_return_objects INTO DATA(return_obj).
        IF return_obj IS NOT INITIAL
          AND return_obj-nhi_read_package_res->package = request->package.
          response = return_obj-nhi_read_package_res.
        ENDIF.
      ENDLOOP.
      "if no result object found, fail
      cl_abap_unit_assert=>assert_bound( act = response msg = 'Internal Error in Unit Test. Mock data provided not the same as used during test.' ).
    ELSE.
      cl_aunit_assert=>fail( msg = 'Test did not set expected return object' ).
    ENDIF.
  ENDMETHOD.

  METHOD if_nhi_package~update.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_nhi_delivery_unit DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    "not inheriting class cl_nhi_api as calls to unimplemented methods should and will fail and cl_nhi_object is not published in SNHI_API-SNHI_REPOSITORY_API
    INTERFACES if_nhi_delivery_unit PARTIALLY IMPLEMENTED.

    CLASS-METHODS:
      create_stub_instance RETURNING VALUE(r_instance) TYPE REF TO ltd_nhi_delivery_unit.

ENDCLASS.

CLASS ltd_nhi_delivery_unit IMPLEMENTATION.

  METHOD create_stub_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_nhi_text DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    "not inheriting class cl_nhi_api as calls to unimplemented methods should and will fail and cl_nhi_object is not published in SNHI_API-SNHI_REPOSITORY_API
    INTERFACES if_nhi_text PARTIALLY IMPLEMENTED.

    CLASS-METHODS:
      create_stub_instance RETURNING VALUE(r_instance) TYPE REF TO ltd_nhi_text.

ENDCLASS.

CLASS ltd_nhi_text IMPLEMENTATION.

  METHOD create_stub_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_nhi_api DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    "not inheriting class cl_nhi_api as calls to unimplemented methods should and will fail
    INTERFACES if_nhi_api PARTIALLY IMPLEMENTED.

    METHODS:
      set_object_api_stub IMPORTING i_object_api_stub TYPE REF TO ltd_nhi_object,
      set_package_api_stub IMPORTING i_package_api_stub TYPE REF TO ltd_nhi_package,
      set_delivery_unit_api_stub IMPORTING i_delivery_unit_api_stub TYPE REF TO ltd_nhi_delivery_unit,
      set_text_api_stub IMPORTING i_text_api_stub TYPE REF TO ltd_nhi_text.

  PRIVATE SECTION.
    DATA:
      m_object_api_stub        TYPE REF TO ltd_nhi_object,
      m_package_api_stub       TYPE REF TO ltd_nhi_package,
      m_delivery_unit_api_stub TYPE REF TO ltd_nhi_delivery_unit,
      m_text_api_stub          TYPE REF TO ltd_nhi_text.

ENDCLASS.

CLASS ltd_nhi_api IMPLEMENTATION.

  METHOD set_object_api_stub.
    m_object_api_stub = i_object_api_stub.
  ENDMETHOD.

  METHOD if_nhi_api~get_object.
    IF m_object_api_stub IS BOUND.
      object = m_object_api_stub.
    ELSE.
      cl_aunit_assert=>fail( msg = 'Object API Stub not set in class ltd_nhi_api' ).
    ENDIF.
  ENDMETHOD.

  METHOD set_package_api_stub.
    m_package_api_stub = i_package_api_stub.
  ENDMETHOD.

  METHOD if_nhi_api~get_package.
    IF m_package_api_stub IS BOUND.
      package = m_package_api_stub.
    ELSE.
      cl_aunit_assert=>fail( msg = 'Package API Stub not set in class ltd_nhi_api' ).
    ENDIF.
  ENDMETHOD.

  METHOD set_delivery_unit_api_stub.
    m_delivery_unit_api_stub = i_delivery_unit_api_stub.
  ENDMETHOD.

  METHOD if_nhi_api~get_delivery_unit.
    IF m_delivery_unit_api_stub IS BOUND.
      delivery_unit = m_delivery_unit_api_stub.
    ELSE.
      cl_aunit_assert=>fail( msg = 'DeliveryUnit API Stub not set in class ltd_nhi_api' ).
    ENDIF.
  ENDMETHOD.

  METHOD set_text_api_stub.
    m_text_api_stub = i_text_api_stub.
  ENDMETHOD.

  METHOD if_nhi_api~get_text.
    IF m_text_api_stub IS BOUND.
      text = m_text_api_stub.
    ELSE.
      cl_aunit_assert=>fail( msg = 'DeliveryUnit API Stub not set in class ltd_nhi_api' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_cts_hot_db_access DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES if_cts_hot_db_access PARTIALLY IMPLEMENTED.

    METHODS:
      read_cts_hot_object
        IMPORTING i_cts_hot_object    TYPE REF TO cl_cts_hot_object_v1
        RETURNING VALUE(r_hot_object) TYPE cts_hot_object,
      get_nr_of_cts_hot_objects
        RETURNING VALUE(r_nr_of_cts_hot_objects) TYPE i,
      read_cts_hot_package
        IMPORTING i_package            TYPE string
        RETURNING VALUE(r_hot_package) TYPE cts_hot_package.
    "##TODO enable again for otext
*      read_cts_hot_object_texts
*        IMPORTING i_cts_hot_object          TYPE REF TO cl_cts_hot_object_v1
*        RETURNING VALUE(r_hot_object_texts) TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts.

    DATA: was_delete_cts_hot_objct_calld TYPE abap_bool VALUE abap_false.

  PRIVATE SECTION.
    "! Internal table playing the role of the DB table CTS_HOT_OBJECT
    DATA m_table_stub_cts_hot_object TYPE HASHED TABLE OF cts_hot_object WITH UNIQUE KEY abap_hana_package_id abap_hana_object_name_suffix.
    "! Internal table playing the role of the DB table CTS_HOT_PACKAGE
    DATA m_table_stub_cts_hot_package TYPE HASHED TABLE OF cts_hot_package WITH UNIQUE KEY abap_hana_package_id.
    "##TODO enable again for otext
*    "! Internal table playing the role of the DB table CTS_HOT_OTEXTS
*    DATA m_table_stub_cts_hot_otexts TYPE HASHED TABLE OF cts_hot_otexts WITH UNIQUE KEY abap_hana_package_id abap_hana_object_name_suffix language text_type hana_text_id.
ENDCLASS.

CLASS ltd_cts_hot_db_access IMPLEMENTATION.
  METHOD if_cts_hot_db_access~modify_cts_hot_object.
    READ TABLE m_table_stub_cts_hot_object FROM i_cts_hot_object TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MODIFY TABLE m_table_stub_cts_hot_object FROM i_cts_hot_object.
    ELSE.
      INSERT i_cts_hot_object INTO TABLE m_table_stub_cts_hot_object.
    ENDIF.
  ENDMETHOD.

  METHOD if_cts_hot_db_access~modify_cts_hot_package.
    READ TABLE m_table_stub_cts_hot_package FROM i_cts_hot_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MODIFY TABLE m_table_stub_cts_hot_package FROM i_cts_hot_package.
    ELSE.
      INSERT i_cts_hot_package INTO TABLE m_table_stub_cts_hot_package.
    ENDIF.
  ENDMETHOD.

  "##TODO enable again for otext
*  METHOD if_cts_hot_db_access~modify_cts_hot_otexts.
*    LOOP AT i_cts_hot_otexts INTO DATA(ls_cts_hot_otext).
*      READ TABLE m_table_stub_cts_hot_otexts FROM ls_cts_hot_otext TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        MODIFY TABLE m_table_stub_cts_hot_otexts FROM ls_cts_hot_otext.
*      ELSE.
*        INSERT ls_cts_hot_otext INTO TABLE m_table_stub_cts_hot_otexts.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD read_cts_hot_object.
    READ TABLE m_table_stub_cts_hot_object
        WITH KEY
            hana_package_id = i_cts_hot_object->hana_package_id
            hana_object_name = i_cts_hot_object->hana_object_name
            hana_object_suffix = i_cts_hot_object->hana_object_suffix
        INTO r_hot_object.
  ENDMETHOD.

  METHOD get_nr_of_cts_hot_objects.
    r_nr_of_cts_hot_objects = lines( m_table_stub_cts_hot_object ).
  ENDMETHOD.

  METHOD read_cts_hot_package.
    READ TABLE m_table_stub_cts_hot_package
        WITH KEY
            hana_package_id = i_package
        INTO r_hot_package.
  ENDMETHOD.

  "##TODO enable again for otext
*  METHOD read_cts_hot_object_texts.
*    LOOP AT m_table_stub_cts_hot_otexts INTO DATA(ls_hot_otext).
*      IF ls_hot_otext-abap_hana_package_id = i_cts_hot_object->abap_hana_package_id
*                  AND ls_hot_otext-abap_hana_object_name_suffix = i_cts_hot_object->abap_hana_object_name_suffix.
*        APPEND ls_hot_otext TO r_hot_object_texts.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD if_cts_hot_db_access~delete_cts_hot_object.
    me->was_delete_cts_hot_objct_calld = abap_true.
  ENDMETHOD.

  METHOD if_cts_hot_db_access~read_cts_hot_package.
    "##TODO better package mapping here...
    r_result-abap_hana_package_id = i_abap_hana_package_id.
    r_result-hana_pack_orig_lang = 'en_US'.

    "cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_cts_hot_db_access~delete_cts_hot_package.
    cl_aunit_assert=>fail( msg = 'Unexpected method call' ).
  ENDMETHOD.

  METHOD if_cts_hot_db_access~delete_smodi_entries.
  ENDMETHOD.

  METHOD if_cts_hot_db_access~update_smodi_entries.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hot_converter_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! test whether conv_hana_actvted_at_to_timest converts the time activated_at string correctly
      conv_hana_actvted_at_to_timest FOR TESTING RAISING cx_static_check,
      "! test whether conv_hana_actvted_at_to_timest converts the time activated_at string correctly using UTC-5 as timezone
      conv_hana_actvted_at_2_ts_utc5 FOR TESTING RAISING cx_static_check,
      "! test convert int to bool
      conv_int_to_bool_ok FOR TESTING RAISING cx_static_check,
      "! test convert bool to int
      conv_bool_to_int_ok FOR TESTING RAISING cx_static_check,
      "! test convert int to bool with wrong input
      conv_int_to_bool_error FOR TESTING RAISING cx_static_check,
      "! test convert bool to int with wrong input
      conv_bool_to_int_error FOR TESTING RAISING cx_static_check.
ENDCLASS.

"make friednship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_converter_methods.
CLASS ltcl_cts_hot_converter_methods IMPLEMENTATION.

  METHOD conv_hana_actvted_at_to_timest.
    DATA: act_timest TYPE timestampl,
          exp_timest TYPE timestampl VALUE '20140415132533.1230000'.

    cl_cts_hot_hana_connector=>g_hana_timezone_string = 'UTC'. "set timezone to UTC so that actual and expected can be compared
    act_timest = cl_cts_hot_hana_connector=>conv_hana_actvted_at_to_timest( '2014-04-15 13:25:33.1230000' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_timest act = act_timest ).

  ENDMETHOD.

  METHOD conv_hana_actvted_at_2_ts_utc5.
    DATA: act_timest TYPE timestampl,
          exp_timest TYPE timestampl VALUE '20140415082533.1230000'.

    cl_cts_hot_hana_connector=>g_hana_timezone_string = 'UTC+5'. "set timezone to UTC+5 so that actual and expected can be compared
    act_timest = cl_cts_hot_hana_connector=>conv_hana_actvted_at_to_timest( '2014-04-15 13:25:33.1230000' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_timest act = act_timest ).

  ENDMETHOD.

  METHOD conv_int_to_bool_ok.
    cl_abap_unit_assert=>assert_equals(
                          exp = abap_false
                          act = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( 0 ) ).
    cl_abap_unit_assert=>assert_equals(
                          exp = abap_true
                          act = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( 1 ) ).
  ENDMETHOD.

  METHOD conv_bool_to_int_ok.
    cl_abap_unit_assert=>assert_equals(
                          exp = 0
                          act = cl_cts_hot_hana_connector=>conv_abap_bool_2_is_structural( abap_false ) ).
    cl_abap_unit_assert=>assert_equals(
                          exp = 1
                          act = cl_cts_hot_hana_connector=>conv_abap_bool_2_is_structural( abap_true ) ).
  ENDMETHOD.

  METHOD conv_int_to_bool_error.

*    try.
*      cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( -1 ). "endet in Überlauf bei Konvertierung -1
*      cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of -1' ).
*    catch cx_cts_hot_invalid_input into data(e1) ##NO_HANDLER.
*      "expected exception
*    endtry.

*    try.
*      cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( -1234 ).
*      cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of -1234' ). "endet in Überlauf bei Konvertierung -1234
*    catch cx_cts_hot_invalid_input into data(e2) ##NO_HANDLER.
*      "expected exception
*    endtry.

    TRY.
        cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( 2 ).
        cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of 2' ).
      CATCH cx_cts_hot_invalid_input INTO DATA(e3) ##NO_HANDLER.
        "expected exception
    ENDTRY.

    TRY.
        cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( 45 ).
        cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of 345' ).
      CATCH cx_cts_hot_invalid_input INTO DATA(e4) ##NO_HANDLER.
        "expected exception
    ENDTRY.

  ENDMETHOD.

  METHOD conv_bool_to_int_error.

    TRY.
        cl_cts_hot_hana_connector=>conv_abap_bool_2_is_structural( abap_undefined ).
        cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of 345' ).
      CATCH cx_cts_hot_invalid_input INTO DATA(e1) ##NO_HANDLER.
        "expected exception
    ENDTRY.

    TRY.
        cl_cts_hot_hana_connector=>conv_abap_bool_2_is_structural( 'c' ).
        cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of 345' ).
      CATCH cx_cts_hot_invalid_input INTO DATA(e2) ##NO_HANDLER.
        "expected exception
    ENDTRY.

    TRY.
        cl_cts_hot_hana_connector=>conv_abap_bool_2_is_structural( '6' ).
        cl_abap_unit_assert=>fail( msg = 'exception expected for invalid conversion of 345' ).
      CATCH cx_cts_hot_invalid_input INTO DATA(e3) ##NO_HANDLER.
        "expected exception
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hot_hana_connector DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: m_cut                        TYPE REF TO cl_cts_hot_hana_connector,
          m_db_access_stub             TYPE REF TO ltd_cts_hot_db_access,
          m_nhi_api_stub               TYPE REF TO ltd_nhi_api,
          m_nhi_object_api_stub        TYPE REF TO ltd_nhi_object,
          m_nhi_package_api_stub       TYPE REF TO ltd_nhi_package,
          m_nhi_delivery_unit_api_stub TYPE REF TO ltd_nhi_delivery_unit,
          m_nhi_text_api_stub          TYPE REF TO ltd_nhi_text.

    METHODS:
      setup RAISING cx_hana_object_transport,

      "********** Verification helper methods *************
      "! verifies whether the read was successfull and the objects are in internal table of db access stub as stub to table CTS_HOT_OBJECT
      check_read_object_in_db_stub
        IMPORTING
          it_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
      "! verifies whether the read was successfull and the packages are in internal table of db access stub as stub to table CTS_HOT_PAKKAGE
      check_read_packages_in_db_stub
        IMPORTING
          it_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,

      "*********** Testing methods **************
      "! tries store an oject with cdata in hot
      read_obj_and_store_ok_cdata FOR TESTING RAISING cx_static_check,
      "! tries store 3 ojects with cdata in hot
      read_3_obj_and_store_ok_cdata FOR TESTING RAISING cx_static_check,
      "! tries store an oject with bdata in hot
      read_obj_and_store_ok_bdata FOR TESTING RAISING cx_static_check,
      "! tries store an oject with cdata and texts in hot
      read_obj_and_store_ok_wtext FOR TESTING RAISING cx_static_check,
      "! tries to store an object in HOT which does not exist in HANA. Should be deleted in HOT
      read_obj_and_store_not_exis FOR TESTING RAISING cx_static_check,
      "! tries to read an object from HANA to store in HOT but HANA returns with specific error_code
      read_obj_with_error_code FOR TESTING RAISING cx_static_check,
      "! tries to read an object from HANA to store in HOT but call to HANA fails with cx_nhi_repository_exception
      read_obj_with_nhi_exception FOR TESTING RAISING cx_static_check,
      "! tries to read an object from HANA to store in HOT but nhi call ends with null response
      read_obj_with_null_response FOR TESTING RAISING cx_static_check.

ENDCLASS.

"make friednship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_hana_connector.
CLASS ltcl_cts_hot_hana_connector IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT m_nhi_api_stub.

    " prepare NHI API stub objects and set to CUT
    m_nhi_object_api_stub = ltd_nhi_object=>create_stub_instance( ).
    m_nhi_api_stub->set_object_api_stub( m_nhi_object_api_stub ).
    m_nhi_package_api_stub = ltd_nhi_package=>create_stub_instance( ).
    m_nhi_api_stub->set_package_api_stub( m_nhi_package_api_stub ).
    m_nhi_delivery_unit_api_stub = ltd_nhi_delivery_unit=>create_stub_instance( ).
    m_nhi_api_stub->set_delivery_unit_api_stub( m_nhi_delivery_unit_api_stub ).
    m_nhi_text_api_stub = ltd_nhi_text=>create_stub_instance( ).
    m_nhi_api_stub->set_text_api_stub( m_nhi_text_api_stub ).

    " class under test -- CL_CTS_HOT_HANA_CONNECTOR
    m_cut = cl_cts_hot_hana_connector=>create_instance(
            i_nhi_api      = m_nhi_api_stub
            i_nhi_api_user = 'test_user_not_existing'
        ).

    " preapre DB Access stub and set to CUT
    CREATE OBJECT m_db_access_stub.
    m_cut->m_cts_hot_db_access = m_db_access_stub.

    " set global data usually read from HANA directly during create instance
    m_cut->g_hana_sid = 'HAN'.
    m_cut->g_hana_timezone_offset = 7200.
    m_cut->g_hana_build_version = '1.00.14.00.123456'.
  ENDMETHOD.

  METHOD read_obj_and_store_ok_cdata.
    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

    "prepare mock data in test double of NHI API
    m_nhi_object_api_stub->read_return_object = cl_nhi_read_object_res=>create_read_object_res(
        EXPORTING
          error_code        = VALUE #( )
          error_msg         = VALUE #( )
          error_arg         = VALUE #( )
          texts             = VALUE #( )
          content_texts     = VALUE #( )
          outgoingrefs      = VALUE #( )
          incomingrefs      = VALUE #( )
          referencedobjects = VALUE #( )
          version           = cl_nhi_active_version=>create_active_version(  )
          metadata          = cl_nhi_metadata_active_ver=>create_metadata(
                            version_id   = '111'
                            activated_at = '2014-02-15 15:00:00.1230000'
                            activated_by = 'activate_user'
                            edit         = '0'
                        )
          cdata             = 'some cdata content'
          bdata             = VALUE #( )
      ).

    "prepare input - nhi_api_stub will return default object for this object ID
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id'
                        iv_hana_object_name   = 'obj_name'
                        iv_hana_object_suffix = 'ob_suffix'
                    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "call business method
    m_cut->read_objects_from_hana_to_hot(
      EXPORTING
        i_objects = lt_cts_hot_objects
    ).

    check_read_object_in_db_stub( lt_cts_hot_objects ).
  ENDMETHOD.

  METHOD read_obj_and_store_ok_wtext.
*    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.
*
*    "prepare mock data in test double of NHI API
*    DATA(lo_text1) = cl_nhi_text_with_language=>create_text_with_language( lang = '' text_id = 'TEXT_ID_1' text_type = 'XCOL' max_length = '120' content = 'This is some XCOL text for translation with length = 120' ).
*    DATA(lo_text2) = cl_nhi_text_with_language=>create_text_with_language( lang = '' text_id = 'TEXT_ID_2' text_type = 'XMSG' max_length = '0' content = 'Some XMSG text for translation with length = 0' ).
*    DATA(lo_text3) = cl_nhi_text_with_language=>create_text_with_language( lang = '' text_id = 'TEXT_ID_3' text_type = 'XTXT' max_length = '0' content = 'Some XTXT with length = 0' ).
*    DATA(lo_text4) = cl_nhi_text_with_language=>create_text_with_language( lang = '' text_id = 'TEXT_ID_4' text_type = 'XTBS' max_length = '0' content = 'Some XTBS text with length = 0' ).
*
*    m_nhi_object_api_stub->read_return_object = cl_nhi_read_object_res=>create_read_object_res(
*        EXPORTING
*          error_code        = VALUE #( )
*          error_msg         = VALUE #( )
*          error_arg         = VALUE #( )
*          texts             = VALUE cl_nhi_text_with_language=>ty_text_with_languages( ( lo_text1 ) ( lo_text2 ) )
*          content_texts     = VALUE cl_nhi_text_with_language=>ty_text_with_languages( ( lo_text3 ) ( lo_text4 ) )
*          outgoingrefs      = VALUE #( )
*          incomingrefs      = VALUE #( )
*          referencedobjects = VALUE #( )
*          version           = cl_nhi_active_version=>create_active_version(  )
*          metadata          = cl_nhi_metadata_active_ver=>create_metadata(
*                            version_id   = '111'
*                            activated_at = '2014-02-15 15:00:00.1230000'
*                            activated_by = 'activate_user'
*                            edit         = '0'
*                        )
*          cdata             = 'some cdata content'
*          bdata             = VALUE #( )
*      ).
*
*    "prepare input - nhi_api_stub will return default object for this object ID
*    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
*                        iv_hana_package_id    = 'pack_id'
*                        iv_hana_object_name   = 'obj_name'
*                        iv_hana_object_suffix = 'ob_suffix'
*                    ).
*    APPEND lo_cts_hot_object TO lt_cts_hot_objects.
*
*    "call business method
*    m_cut->read_objects_from_hana_to_hot(
*      EXPORTING
*        i_objects = lt_cts_hot_objects
*    ).
*
*    check_read_object_in_db_stub( lt_cts_hot_objects ).
  ENDMETHOD.


  METHOD read_3_obj_and_store_ok_cdata.
    DATA: lt_cts_hot_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lt_response_objects TYPE m_nhi_object_api_stub->ty_read_responses_list,
          ls_response_object  LIKE LINE OF lt_response_objects.

    "prepare input and expected reposnse of NHI API
    DO 3 TIMES.
      DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack' && sy-index
                        iv_hana_object_name   = 'obj_nam' && sy-index
                        iv_hana_object_suffix ='obj_suff' && sy-index
                    ).

      APPEND lo_cts_hot_object TO lt_cts_hot_objects.

      "prepare mock data for test double
      ls_response_object-cts_hot_object = lo_cts_hot_object.
      ls_response_object-nhi_read_object_res = cl_nhi_read_object_res=>create_read_object_res(
        EXPORTING
          error_code        = VALUE #( )
          error_msg         = VALUE #( )
          error_arg         = VALUE #( )
          texts             = VALUE #( )
          content_texts     = VALUE #( )
          outgoingrefs      = VALUE #( )
          incomingrefs      = VALUE #( )
          referencedobjects = VALUE #( )
          version           = cl_nhi_active_version=>create_active_version(  )
          metadata          = cl_nhi_metadata_active_ver=>create_metadata(
                            version_id   = '111' && sy-index
                            activated_at = '2014-02-15 15:00:00.12' && sy-index && '0000' "set last ms to sy-index
                            activated_by = 'activate_user' && sy-index
                            edit         = '0'
                        )
          cdata             = 'CONTENT cdata ' && sy-index
          bdata             = VALUE #( )
      ).

      APPEND ls_response_object TO lt_response_objects.
    ENDDO.

    m_nhi_object_api_stub->read_return_objects = lt_response_objects.

    "call business method
    m_cut->read_objects_from_hana_to_hot( lt_cts_hot_objects ).

    check_read_object_in_db_stub( lt_cts_hot_objects ).
  ENDMETHOD.

  METHOD read_obj_and_store_ok_bdata.
    DATA: lv_bdata           TYPE xstring VALUE '1234',
          lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

    "prepare input
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id'
                        iv_hana_object_name   = 'obj_name'
                        iv_hana_object_suffix = 'ob_suffix'
                    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "prepare mock data in test double of NHI API
    m_nhi_object_api_stub->read_return_object = cl_nhi_read_object_res=>create_read_object_res(
        EXPORTING
          error_code        = VALUE #( )
          error_msg         = VALUE #( )
          error_arg         = VALUE #( )
          texts             = VALUE #( )
          content_texts     = VALUE #( )
          outgoingrefs      = VALUE #( )
          incomingrefs      = VALUE #( )
          referencedobjects = VALUE #( )
          version           = cl_nhi_active_version=>create_active_version(  )
          metadata          = cl_nhi_metadata_active_ver=>create_metadata(
                            version_id   = '111'
                            activated_at = '2014-02-15 15:00:00.1230000'
                            activated_by = 'activate_user'
                            edit         = '0'
                        )
          cdata             = VALUE #( )
          bdata             = lv_bdata
      ).

    "call business method
    m_cut->read_objects_from_hana_to_hot( i_objects = lt_cts_hot_objects ).

    check_read_object_in_db_stub( lt_cts_hot_objects ).
  ENDMETHOD.

  METHOD read_obj_and_store_not_exis.
    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.

    "prepare input
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id_ne'
                        iv_hana_object_name   = 'obj_name_ne'
                        iv_hana_object_suffix = 'ob_suffix_ne'
    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "prepare mock data in test double
    m_nhi_object_api_stub->read_return_object = cl_nhi_read_object_res=>create_read_object_res(
        EXPORTING
          error_code        = '40112'
          error_msg         = 'AUNIT: Obj does not exist'
          error_arg         = 'AUINT: error_arg'
          texts             = VALUE #( )
          content_texts     = VALUE #( )
          outgoingrefs      = VALUE #( )
          incomingrefs      = VALUE #( )
          referencedobjects = VALUE #( )
          version           = VALUE #( )
          metadata          = VALUE #( )
          cdata             = VALUE #( )
          bdata             = VALUE #( )
      ).

    "call business method
    m_cut->read_objects_from_hana_to_hot( i_objects = lt_cts_hot_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = m_db_access_stub->get_nr_of_cts_hot_objects( ) ).
    cl_abap_unit_assert=>assert_true( m_db_access_stub->was_delete_cts_hot_objct_calld ).

  ENDMETHOD.

  METHOD read_obj_with_error_code.
    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lx_hot_exc         TYPE REF TO cx_hana_object_transport.

    "prepare mock data in test double
    m_nhi_object_api_stub->read_return_object = cl_nhi_read_object_res=>create_read_object_res(
        EXPORTING
          error_code        = '40158'
          error_msg         = 'AUNIT: Some Error'
          error_arg         = VALUE #( )
          texts             = VALUE #( )
          content_texts     = VALUE #( )
          outgoingrefs      = VALUE #( )
          incomingrefs      = VALUE #( )
          referencedobjects = VALUE #( )
          version           = VALUE #( )
          metadata          = VALUE #( )
          cdata             = VALUE #( )
          bdata             = VALUE #( )
      ).

    "prepare input
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id'
                        iv_hana_object_name   = 'obj_name'
                        iv_hana_object_suffix = 'ob_suffix'
    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "call business method
    TRY.
        m_cut->read_objects_from_hana_to_hot( i_objects = lt_cts_hot_objects ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>read_object_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '40158' act = lx_hot_exc->hana_error_code ).
        cl_abap_unit_assert=>assert_equals( exp = 'AUNIT: Some Error' act = lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_not_bound( lx_hot_exc->previous ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 0 act = m_db_access_stub->get_nr_of_cts_hot_objects( ) ).

  ENDMETHOD.

  METHOD read_obj_with_nhi_exception.
    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lx_nhi_exc         TYPE REF TO cx_nhi_hana_repository,
          lx_hot_exc         TYPE REF TO cx_hana_object_transport.

    "prepare mock data in test double
    CREATE OBJECT lx_nhi_exc EXPORTING textid = cx_nhi_hana_repository=>error_remote_hana_call msgv1 = 'Exception error_remote_hana_call'.
    m_nhi_object_api_stub->read_exception = lx_nhi_exc.

    "prepare input
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id'
                        iv_hana_object_name   = 'obj_name'
                        iv_hana_object_suffix = 'ob_suffix'
    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "call business method
    TRY.
        m_cut->read_objects_from_hana_to_hot( i_objects = lt_cts_hot_objects ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>cx_nhi_hana_repository_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_code ). "because call failed before HANA returned valid response
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_bound( lx_hot_exc->previous ).
        "##TODO check that previous is of type cx_nhi_hana_repository
        DATA(l_cx_nhi) = CAST cx_nhi_hana_repository( lx_hot_exc->previous ).
        cl_abap_unit_assert=>assert_equals( exp = cx_nhi_hana_repository=>error_remote_hana_call act = l_cx_nhi->if_t100_message~t100key ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 0 act = m_db_access_stub->get_nr_of_cts_hot_objects( ) ).

  ENDMETHOD.

  METHOD read_obj_with_null_response.
    DATA: lt_cts_hot_objects TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lx_hot_exc         TYPE REF TO cx_hana_object_transport.

    "prepare mock data in test double
    m_nhi_object_api_stub->read_return_object = VALUE #( ).

    "prepare input
    DATA(lo_cts_hot_object) = cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id    = 'pack_id'
                        iv_hana_object_name   = 'obj_name'
                        iv_hana_object_suffix = 'ob_suffix'
    ).
    APPEND lo_cts_hot_object TO lt_cts_hot_objects.

    "call business method
    TRY.
        m_cut->read_objects_from_hana_to_hot( i_objects = lt_cts_hot_objects ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>response_is_null_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_code ). "because call failed before HANA returned valid response
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_not_bound( lx_hot_exc->previous ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 0 act = m_db_access_stub->get_nr_of_cts_hot_objects( ) ).

  ENDMETHOD.

*  METHOD read_package_metadata.
*    "prepare test data - test package
*    DATA lv_package TYPE string VALUE 'package.test'.
*
*    "set expected responses to NHI stubs stub
*    m_nhi_package_api_stub->read_pack_return_object = cl_nhi_read_package_res=>create_read_package_res(
*                         error_code              = VALUE #( )
*                         error_msg               = VALUE #( )
*                         error_arg               = VALUE #( )
*                         tenant                  = ''
*                         package                 = lv_package
*                         src_system              = 'SRC'
*                         src_tenant              = ''
*                         description             = 'Descriptin of hana package'
*                         responsible             = 'srsponsible'
*                         orig_lang               = 'DE'
*                         structural              = abap_false
*                         delivery_unit           = 'MYDUNAME'
*                         du_vendor               = 'DUVENDOR'
*                         transportable           = abap_true
*                         content                 = ''
*                         text_collection         = 'Text Coll'
*                         text_status             = 'Text Status'
*                         text_terminology_domain = 'Terminology Domain'
*                         hints_for_translation   = 'Hints for Translation'
*                         texts                   = VALUE #( )
*                     ).
*
*    "prepare input data
*    DATA(lt_packages_to_read) = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( (  cl_cts_hot_package=>create_instance( lv_package ) ) ).
*
*    "execute business method
*    m_cut->read_packages_from_hana_to_hot( lt_packages_to_read ).
*
*    "verify whether apcakges were added to table stub
*    check_read_packages_in_db_stub( it_packages = lt_packages_to_read ).
*
*  ENDMETHOD.

  METHOD check_read_object_in_db_stub.
    "verify read was performed and correct data is in DB
    DATA: ls_cts_hot_object  TYPE cts_hot_object,
          "##TODO enable again for otext
*          lt_cts_hot_object_texts TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,
*          ls_cts_hot_otext        TYPE cts_hot_otexts,
          lo_cts_hot_object  TYPE REF TO cl_cts_hot_object_v1,
          lo_read_return_obj TYPE REF TO cl_nhi_read_object_res.

    LOOP AT it_cts_hot_objects INTO lo_cts_hot_object.
      "get response object which was returned by test double
      IF m_nhi_object_api_stub->read_return_object IS BOUND.
        "if test has set 1 response object, use this for verification
        lo_read_return_obj = m_nhi_object_api_stub->read_return_object.
      ELSEIF m_nhi_object_api_stub->read_return_objects IS NOT INITIAL.
        "if test has set multiple response objects, find the correct one
        lo_read_return_obj = VALUE #( ).
        LOOP AT m_nhi_object_api_stub->read_return_objects INTO DATA(return_obj).
          IF return_obj IS NOT INITIAL
            AND return_obj-cts_hot_object->hana_package_id = lo_cts_hot_object->hana_package_id
            AND return_obj-cts_hot_object->hana_object_name = lo_cts_hot_object->hana_object_name
            AND return_obj-cts_hot_object->hana_object_suffix = lo_cts_hot_object->hana_object_suffix.
            lo_read_return_obj = return_obj-nhi_read_object_res.
          ENDIF.
        ENDLOOP.
        ASSERT lo_read_return_obj IS BOUND.
      ELSE.
        "if test has set nothing, test case is wrong
        cl_abap_unit_assert=>fail( 'Test did not specify read return object but tries to compare result.' ).
      ENDIF.

      ls_cts_hot_object = m_db_access_stub->read_cts_hot_object( lo_cts_hot_object ).

      cl_abap_unit_assert=>assert_equals( exp = lo_cts_hot_object->hana_package_id act = ls_cts_hot_object-hana_package_id ).
      cl_abap_unit_assert=>assert_equals( exp = lo_cts_hot_object->hana_object_name act = ls_cts_hot_object-hana_object_name ).
      cl_abap_unit_assert=>assert_equals( exp = lo_cts_hot_object->hana_object_suffix act = ls_cts_hot_object-hana_object_suffix ).

      DATA lo_active_metadata_exp TYPE REF TO cl_nhi_metadata_active_ver.
      lo_active_metadata_exp ?= lo_read_return_obj->metadata.
      cl_abap_unit_assert=>assert_equals( exp = lo_active_metadata_exp->version_id act = ls_cts_hot_object-hana_object_version ).
      cl_abap_unit_assert=>assert_equals( exp = lo_active_metadata_exp->version_id act = ls_cts_hot_object-hana_source_object_version ).
      "##TODO correct date verification
      "cl_abap_unit_assert=>assert_equals( exp = lo_active_metadata_exp->activated_at act = ls_cts_hot_object-hana_activated_at ).
      cl_abap_unit_assert=>assert_equals( exp = lo_active_metadata_exp->activated_by act = ls_cts_hot_object-hana_activated_by ).

      cl_abap_unit_assert=>assert_equals( exp = lo_cts_hot_object->abap_hana_package_id act = ls_cts_hot_object-abap_hana_package_id ).
      cl_abap_unit_assert=>assert_equals( exp = lo_cts_hot_object->abap_hana_object_name_suffix act = ls_cts_hot_object-abap_hana_object_name_suffix ).
      cl_abap_unit_assert=>assert_equals( exp = sy-sysid act = ls_cts_hot_object-abap_sync_system ).
      cl_abap_unit_assert=>assert_equals( exp = m_cut->g_hana_sid act = ls_cts_hot_object-hana_read_system ).
      cl_abap_unit_assert=>assert_equals( exp = m_cut->g_hana_build_version act = ls_cts_hot_object-hana_source_build_version ).

      cl_abap_unit_assert=>assert_equals( exp = lo_read_return_obj->bdata act = ls_cts_hot_object-hana_content_bdata ).
      cl_abap_unit_assert=>assert_equals( exp = lo_read_return_obj->cdata act = ls_cts_hot_object-hana_content_cdata ).

      cl_abap_unit_assert=>assert_equals( exp = 'A' act = ls_cts_hot_object-abap_status ).
      cl_abap_unit_assert=>assert_equals( exp = if_cts_hot_db_access=>co_hot_status_new act = ls_cts_hot_object-hot_status ).
      cl_abap_unit_assert=>assert_not_initial( act = ls_cts_hot_object-abap_synced_at ). "how to test transferred data? enough to check that it is set
      cl_abap_unit_assert=>assert_initial( act = ls_cts_hot_object-abap_import_timestamp ).
      cl_abap_unit_assert=>assert_equals( exp = sy-uname act = ls_cts_hot_object-abap_synced_by ).

      "##TODO enable again for otext
*      "check whether texts where added to DB as well.
*      lt_cts_hot_object_texts = m_db_access_stub->read_cts_hot_object_texts( lo_cts_hot_object ).
*      DATA(lt_input_texts) = lo_read_return_obj->texts.
*      DATA(lt_input_content_texts) = lo_read_return_obj->content_texts.
*
*      cl_abap_unit_assert=>assert_equals( exp = lines( lt_input_texts ) + lines( lt_input_content_texts ) act = lines( lt_cts_hot_object_texts ) ).
*
*      LOOP AT lt_input_texts INTO DATA(lo_text).
*        "create data to test whether text was passed to DB
*        CLEAR ls_cts_hot_otext.
*
*        READ TABLE lt_cts_hot_object_texts WITH KEY abap_hana_package_id = lo_cts_hot_object->abap_hana_package_id
*                                                          abap_hana_object_name_suffix = lo_cts_hot_object->abap_hana_object_name_suffix
*                                                          language = 'E'
*                                                          isolanguage = 'en_US'
*                                                          text_type = if_cts_hot_db_access=>co_cts_hot_otexts_type_text
*                                                          hana_text_id = lo_text->text_id
*                                           INTO ls_cts_hot_otext.
*        IF sy-subrc <> 0.
*          cl_abap_unit_assert=>fail( msg = 'Text missing in DB Stub for ID ' && lo_text->text_id ).
*        ENDIF.
*
*        cl_abap_unit_assert=>assert_equals( exp = lo_text->content act = ls_cts_hot_otext-hana_text_content ).
*        cl_abap_unit_assert=>assert_equals( exp = lo_text->max_length act = ls_cts_hot_otext-hana_text_max_length ).
*        cl_abap_unit_assert=>assert_equals( exp = lo_text->text_type act = ls_cts_hot_otext-hana_text_type ).
*
*        cl_abap_unit_assert=>assert_equals( exp = 0 act = ls_cts_hot_otext-abap_import_timestamp ).
*        cl_abap_unit_assert=>assert_not_initial( ls_cts_hot_otext-abap_synced_at ).
*        cl_abap_unit_assert=>assert_equals( exp = sy-uname act = ls_cts_hot_otext-abap_synced_by ).
*        cl_abap_unit_assert=>assert_equals( exp = sy-sysid act = ls_cts_hot_otext-abap_sync_system ).
*        cl_abap_unit_assert=>assert_equals( exp = m_cut->g_hana_sid act = ls_cts_hot_otext-hana_read_system ).
*        cl_abap_unit_assert=>assert_equals( exp = lo_read_return_obj->metadata->version_id act = ls_cts_hot_otext-hana_source_object_version ).
*      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_read_packages_in_db_stub.
    "verify read was performed and correct data is in DB
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          l_read_return_obj  TYPE REF TO cl_nhi_read_package_res.

    LOOP AT it_packages INTO DATA(l_package).
      DATA(ls_hot_package) = m_db_access_stub->read_cts_hot_package( l_package->hana_package_id ).

      "get response package which was returned by test double
      IF m_nhi_package_api_stub->read_pack_return_object IS BOUND.
        "if test has set 1 response object, use this for verification
        l_read_return_obj = m_nhi_package_api_stub->read_pack_return_object.
      ELSEIF m_nhi_package_api_stub->read_pack_return_objects IS NOT INITIAL.
        "if test has set multiple response objects, find the correct one
        l_read_return_obj = VALUE #( ).
        LOOP AT m_nhi_package_api_stub->read_pack_return_objects INTO DATA(return_obj).
          IF return_obj IS NOT INITIAL
            AND return_obj-package = l_package->hana_package_id.
            l_read_return_obj = return_obj-nhi_read_package_res.
          ENDIF.
        ENDLOOP.
        ASSERT l_read_return_obj IS BOUND.
      ELSE.
        "if test has set nothing, use default return object
        cl_aunit_assert=>fail( 'No Return Object found in stub' ).
      ENDIF.

      ls_cts_hot_package = m_db_access_stub->read_cts_hot_package( l_package->hana_package_id ).

      cl_abap_unit_assert=>assert_equals( exp = l_package->hana_package_id act = ls_cts_hot_package-hana_package_id ).
      cl_abap_unit_assert=>assert_equals( exp = l_package->abap_hana_package_id act = ls_cts_hot_package-abap_hana_package_id ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->delivery_unit act = ls_cts_hot_package-hana_pack_delivery_unit ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->description act = ls_cts_hot_package-hana_pack_description ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->du_vendor act = ls_cts_hot_package-hana_pack_du_vendor ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->hints_for_translation act = ls_cts_hot_package-hana_pack_hints_for_transl ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->orig_lang act = ls_cts_hot_package-hana_pack_orig_lang ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->responsible act = ls_cts_hot_package-hana_pack_responsible ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->src_system act = ls_cts_hot_package-hana_pack_src_system ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->src_tenant act = ls_cts_hot_package-hana_pack_src_tenant ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->structural act = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( ls_cts_hot_package-hana_pack_is_structural ) ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->text_collection act = ls_cts_hot_package-hana_pack_text_collection ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->text_status act = ls_cts_hot_package-hana_pack_text_status ).
      cl_abap_unit_assert=>assert_equals( exp = l_read_return_obj->text_terminology_domain act = ls_cts_hot_package-hana_pack_text_term_domain ).

      cl_abap_unit_assert=>assert_equals( exp = sy-sysid act = ls_cts_hot_package-abap_sync_system ).
      cl_abap_unit_assert=>assert_equals( exp = m_cut->g_hana_sid act = ls_cts_hot_package-hana_read_system ).

      cl_abap_unit_assert=>assert_equals( exp = 'A' act = ls_cts_hot_package-abap_status ).
      cl_abap_unit_assert=>assert_equals( exp = if_cts_hot_db_access=>co_hot_status_new act = ls_cts_hot_package-hot_status ).
      cl_abap_unit_assert=>assert_initial( act = ls_cts_hot_package-abap_deployed_at ).
      cl_abap_unit_assert=>assert_initial( act = ls_cts_hot_package-abap_deployed_by ).
      cl_abap_unit_assert=>assert_initial( act = ls_cts_hot_package-abap_import_timestamp ).

      cl_abap_unit_assert=>assert_not_initial( act = ls_cts_hot_package-abap_synced_at ). "how to test transferred data? enough to check that it is set
      cl_abap_unit_assert=>assert_equals( exp = sy-uname act = ls_cts_hot_package-abap_synced_by ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_timestamp_provider DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      lif_timestamp_provider.

    DATA:
      gv_timestamp_to_return TYPE timestampl.
ENDCLASS.

CLASS ltd_timestamp_provider IMPLEMENTATION.

  METHOD lif_timestamp_provider~get_timestamp.
    r_timestamp = gv_timestamp_to_return.
  ENDMETHOD.

ENDCLASS.


"! Test class using test double framwork
CLASS ltcl_cts_hot_hana_conector_tdf DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "********** Setup and helper Methods **********
      "! setup the test case by creating m_cut and m_package_api_double
      setup RAISING cx_hana_object_transport,
      "! Configure NHI Object API call for activation of objects.<br/>
      "! Activation will fail and checkresults with error for all passed objects will be returned.
      conf_activate_objects_failed IMPORTING i_nhi_objects TYPE cl_nhi_object_id=>ty_objlist RAISING cx_nhi_hana_repository,
      "! Configure db access double for read call and set expected return
      conf_db_access_double_read IMPORTING i_cts_hot_package TYPE cts_hot_package,
      "! Configure db access double for modify call and set expected return
      "! I_TIMES defines how often we expect the call. "##TODO currently needed as we do not set return value and ignore all parameters because of timestamp
      conf_db_access_double_modify IMPORTING i_cts_hot_package TYPE cts_hot_package i_times TYPE i DEFAULT 1,
      "! Configure db access double for update call of hot_status, deployed_at and deployed_by and set expected return
      conf_db_access_double_update
        IMPORTING
          i_old_package     TYPE cts_hot_package
          i_new_status      TYPE cts_hot_object_status DEFAULT if_cts_hot_db_access=>co_hot_status_active
          i_new_deployed_at TYPE timestampl OPTIONAL
          i_new_deployed_by TYPE cts_hot_abap_deployed_by OPTIONAL,
      "! Configure db access double for delete call
      conf_db_access_double_delete IMPORTING i_abap_hana_package_id TYPE cts_hot_package_id,
      "! Configuration of NHI Object API to return OK for delete object request.
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should be deleted successfully
      "! @parameter i_revert | set to abap_true if revert (inactive deletion) should be prepared
      "! @raising cx_nhi_hana_repository |
      conf_delete_object IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "! Configuration of NHI Object API to return Error for delete object request.
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should be deleted successfully
      "! @raising cx_nhi_hana_repository |
      conf_delete_object_failed IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "! Configuration of NHI Object API to return abap_true for exists object request.
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should be seen as existing
      conf_exists_object_inactive IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "! Configure nhi_object_api double to expect no call to activate
      conf_no_call_activate RAISING cx_nhi_hana_repository,
      "! Configure package api double for exist call
      conf_package_api_double_exist IMPORTING i_cts_hot_package TYPE cts_hot_package i_exists TYPE abap_bool RAISING cx_nhi_hana_repository,
      "! Configure package api double for create call
      conf_package_api_double_create IMPORTING i_cts_hot_package TYPE cts_hot_package RAISING cx_nhi_hana_repository,
      "! Configure package api double for read call
      "! @parameter i_error_code    | If i_error_code is NOT set i_cts_hot_package will be used for read return value (package found). Otherwise read will return passed i_error_code and i_error_msg (some error, e.g. package not found or other)
      "! @parameter i_error_msg     | Passed value is returned in case i_error_code is set to some value
      "! @parameter i_exception     | If abap_true, testdouble will throw exception for read method
      "! @parameter i_null_response | If abap_true, the read call is configured to return a null response.
      conf_package_api_double_read IMPORTING i_cts_hot_package TYPE cts_hot_package
                                             i_error_code      TYPE string OPTIONAL
                                             i_error_msg       TYPE string OPTIONAL
                                             i_exception       TYPE abap_bool DEFAULT abap_false
                                             i_null_response   TYPE abap_bool DEFAULT abap_false
                                   RAISING   cx_nhi_hana_repository,
      "! Configure package api double for update call
      conf_package_api_double_update IMPORTING i_cts_hot_package TYPE cts_hot_package RAISING cx_nhi_hana_repository,
      "! Configure package api double for delete call
      conf_package_api_double_delete IMPORTING i_cts_hot_package TYPE cts_hot_package RAISING cx_nhi_hana_repository,
      "! @parameter i_cts_hot_object | HOT object for which hot_status should be updated with 'E' or 'Z' but only if no parallel import happened. <br/>
      "!                               Only following fields are used abap_hana_package_id, abap_hana_object_name_suffix, abap_status, hot_status, hana_source_object_version.
      conf_update_obj_after_failed
        IMPORTING
          i_cts_hot_object TYPE cts_hot_object
          i_new_status     TYPE cts_hot_object_status DEFAULT if_cts_hot_db_access=>co_hot_status_deploy_error,
      "! Configuration of NHI Object API to return metadata when reading objects metadata
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should get this result
      conf_read_metadata_existing IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "! Configuration of NHI Object API to return "Not existing" when reading object metadata (simulating new object)
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should get this result
      conf_read_metadata_not_existng IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "! Configuration of db access double for expected call read_cts_hot_object (expecting 2 times, 1 for read in the beginning and 1 for read for modify on DB).
      "! @parameter i_cts_hot_object | HOT object data to be returned by read_cts_hot_object
      conf_read_cts_hot_object IMPORTING i_cts_hot_object TYPE cts_hot_object,
      "! Configuration of NHI Object API to return OK for write object request.
      "! @parameter i_times | Number of times this call should be prepared = number of objects that should be imported successfully
      conf_write_object IMPORTING i_times TYPE i DEFAULT 1 RAISING cx_nhi_hana_repository,
      "!
      create_dummy_test_package
        IMPORTING i_hana_package_id        TYPE cts_hot_package-hana_package_id
                  i_hot_status             TYPE cts_hot_package-hot_status DEFAULT if_cts_hot_db_access=>co_hot_status_new
        RETURNING VALUE(r_cts_hot_package) TYPE cts_hot_package
        RAISING   cx_hana_object_transport,


      "********** Test Methods ************
      "! Creates package from HOT repository in HANA.
      "! Package does not exist in HANA before.
      modify_package_not_existing FOR TESTING RAISING cx_static_check,
      "! Creates package from HOT repository in HANA with only package id known, rest of all attributes is ''
      "! Package does not exist in HANA before.
      modify_package_not_existing2 FOR TESTING RAISING cx_static_check,
      "! Modifies package from HOT repository in HANA.
      "! Package does exist in HANA before.
      modify_package_existing FOR TESTING RAISING cx_static_check,
      "! Creates structural package from HOT repository in HANA.
      "! Package does not exist in HANA before.
      modify_package_not_existing_st FOR TESTING RAISING cx_static_check,
      "! Creates, updates and deletes packages from HOT repository in HANA.
      "! Packages do exist and do not exist in HANA before.
      modify_multiple_packages FOR TESTING RAISING cx_static_check,
      "! tries to read package metadata and store it in hot
      read_package FOR TESTING RAISING cx_static_check,
      "! Reads a package that does not exist in HANA (anymore) - so should be deleted in HOT
      read_package_not_existing FOR TESTING RAISING cx_static_check,
      "! tries to read multiple packages metadata and store it in hot (some packages are still in HANA, 1 is not there anymore)
      read_multiple_packages FOR TESTING RAISING cx_static_check,
      "! tries to read package metadata but gets cx_nhi_hana_repository while reading which should be mapped to cx_hana_object_transport
      read_package_with_nhi_exceptn FOR TESTING RAISING cx_static_check,
      "! tries to read package metadata but gets a null response
      read_package_with_null_respons FOR TESTING RAISING cx_static_check,
      "! tries to read package metadata but gets an error code returned from HANA
      read_package_with_error_code FOR TESTING RAISING cx_static_check,
      "! Test tries to deploy 2 objects but both are failing at import<br/>
      "! Object 1 initially has hot_status = 'I'<br/>
      "! Object 2 initially has hot_status = 'E'<br/>
      deploy_objects_import_error FOR TESTING RAISING cx_static_check,
      "! Test tries to deploy 2 objects but both are failing during deployment.<br/>
      "! Object 1 initially has hot_status = 'I'<br/>
      "! Object 2 initially has hot_status = 'E'<br/>
      deploy_objects_activation_err FOR TESTING RAISING cx_static_check,
      "! Test tries to revert 2 inactive objects but revert is failing.<br/>
      "! Object 1 initially has hot_status = 'D'<br/>
      "! Object 2 initially has hot_status = 'Z'<br/>
      deploy_objects_revert_error FOR TESTING RAISING cx_static_check,
      "! Test tries to delete 2 active objects but activation of deletion is failing.<br/>
      "! Object 1 initially has hot_status = 'D'<br/>
      "! Object 2 initially has hot_status = 'Z'<br/>
      deploy_objects_delete_error FOR TESTING RAISING cx_static_check,
      "! Test calls append_log_message with a successful CheckResult and a short message (short means < 310 chars)
      append_log_message_ok_short FOR TESTING RAISING cx_static_check,
      "! Test calls append_log_message with a successful CheckResult and a long message (long means > 310 chars)
      append_log_message_ok_long FOR TESTING RAISING cx_static_check,
      "! Test calls append_log_message with a not successful  CheckResult and a short message (short means < 10000 chars)
      append_log_message_not_ok_s FOR TESTING RAISING cx_static_check,
      "! Test calls append_log_message with a not successful  CheckResult and a long message (short means > 10000 chars)
      append_log_message_not_ok_l FOR TESTING RAISING cx_static_check,
      "! Test calls append_log_message with a not successful  CheckResult and a medium long message and with a customer defined value of max_chars_for_logging
      append_log_message_not_ok_c FOR TESTING RAISING cx_static_check.

    DATA:
      "! Test Object 1
      m_hot_object_1              TYPE REF TO cl_cts_hot_object_v1,
      "! Test Object 1 as type cts_hot_object
      m_cts_hot_object_1          TYPE cts_hot_object,
      "! Test Object 1 as cl_nhi_object_id
      m_nhi_object_1              TYPE REF TO cl_nhi_object_id,
      "! Test Object 2
      m_hot_object_2              TYPE REF TO cl_cts_hot_object_v1,
      "! Test Object 2 as type cts_hot_object
      m_cts_hot_object_2          TYPE cts_hot_object,
      "! Test Object 2 as cl_nhi_object_id
      m_nhi_object_2              TYPE REF TO cl_nhi_object_id,
      m_cut                       TYPE REF TO cl_cts_hot_hana_connector,
      m_package_api_double        TYPE REF TO if_nhi_package,
      m_object_api_double         TYPE REF TO if_nhi_object,
      m_db_access_double          TYPE REF TO if_cts_hot_db_access,
      m_timestamp_provider_double TYPE REF TO ltd_timestamp_provider.

ENDCLASS.

"make friendship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_hana_conector_tdf.
CLASS ltcl_cts_hot_hana_conector_tdf IMPLEMENTATION.

  METHOD setup.
    DATA: it_packages       TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          nhi_api_double    TYPE REF TO if_nhi_api,
          object_api_double TYPE REF TO if_nhi_object.

    m_cts_hot_object_1-abap_hana_package_id = 'TMP.HTA.AUNIT.DEPLOY'.
    m_cts_hot_object_1-abap_hana_object_name_suffix = 'OBJECT_1.SUFFIX_1'.
    m_cts_hot_object_1-hana_package_id = 'tmp.hta.aunit.deploy'.
    m_cts_hot_object_1-hana_object_name = 'OBJECT_1'.
    m_cts_hot_object_1-hana_object_suffix = 'SUFFIX_1'.
    m_cts_hot_object_1-abap_status = 'A'.
    m_cts_hot_object_1-hot_status = 'I'.
    m_cts_hot_object_1-hana_source_object_version = 1.

    m_cts_hot_object_2-abap_hana_package_id = 'TMP.HTA.AUNIT.DEPLOY.2'.
    m_cts_hot_object_2-abap_hana_object_name_suffix = 'OBJECT_2.SUFFIX_2'.
    m_cts_hot_object_2-hana_package_id = 'tmp.hta.aunit.deploy.2'.
    m_cts_hot_object_2-hana_object_name = 'OBJECT_2'.
    m_cts_hot_object_2-hana_object_suffix = 'SUFFIX_2'.
    m_cts_hot_object_2-abap_status = 'A'.
    m_cts_hot_object_2-hot_status = 'E'.
    m_cts_hot_object_2-hana_source_object_version = 2.

    m_hot_object_1 = cl_cts_hot_object_v1=>create_instance(
                 iv_hana_package_id       = m_cts_hot_object_1-hana_package_id
                 iv_hana_object_name      = m_cts_hot_object_1-hana_object_name
                 iv_hana_object_suffix    = m_cts_hot_object_1-hana_object_suffix
             ).

    m_hot_object_2 = cl_cts_hot_object_v1=>create_instance(
                 iv_hana_package_id       = m_cts_hot_object_2-hana_package_id
                 iv_hana_object_name      = m_cts_hot_object_2-hana_object_name
                 iv_hana_object_suffix    = m_cts_hot_object_2-hana_object_suffix
             ).

    m_nhi_object_1 = cl_nhi_object_id=>create_object_id(
                 tenant   = ''
                 package  = m_cts_hot_object_1-hana_package_id
                 name     = m_cts_hot_object_1-hana_object_name
                 suffix   = m_cts_hot_object_1-hana_object_suffix
             ).

    m_nhi_object_2 = cl_nhi_object_id=>create_object_id(
                 tenant   = ''
                 package  = m_cts_hot_object_2-hana_package_id
                 name     = m_cts_hot_object_2-hana_object_name
                 suffix   = m_cts_hot_object_2-hana_object_suffix
             ).

    "create test double objects
    nhi_api_double ?= cl_abap_testdouble=>create( 'if_nhi_api' ).
    m_package_api_double ?= cl_abap_testdouble=>create( 'if_nhi_package' ).
    m_object_api_double ?= cl_abap_testdouble=>create( 'if_nhi_object' ).
    m_db_access_double ?= cl_abap_testdouble=>create( 'if_cts_hot_db_access' ).

    "configuration of test double
    cl_abap_testdouble=>configure_call( nhi_api_double )->returning( m_package_api_double )->and_expect( )->is_called_once( ).
    nhi_api_double->get_package( ).

    cl_abap_testdouble=>configure_call( nhi_api_double )->returning( m_object_api_double )->and_expect( )->is_called_once( ).
    nhi_api_double->get_object( ).

    "injecting the test doubles into the object being tested
    m_cut = cl_cts_hot_hana_connector=>create_instance( i_nhi_api = nhi_api_double i_nhi_api_user = 'dummy' ).
    m_cut->m_cts_hot_db_access = m_db_access_double.

    m_timestamp_provider_double = NEW ltd_timestamp_provider( ).
    GET TIME STAMP FIELD m_timestamp_provider_double->gv_timestamp_to_return.
    m_cut->m_timestamp_provider = m_timestamp_provider_double.

    m_cut->mv_max_no_of_chars_for_log = 10000. "default value from cl_cts_hot_db_access
  ENDMETHOD.

  METHOD modify_package_not_existing.
    DATA: it_packages        TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          ls_cts_hot_package TYPE cts_hot_package.

    ls_cts_hot_package = create_dummy_test_package( i_hana_package_id = 'test.package.1' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package ).
    conf_db_access_double_update( i_old_package = ls_cts_hot_package ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package i_exists = abap_false ).
    conf_package_api_double_create( i_cts_hot_package = ls_cts_hot_package ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package ).

    " prepare input for business method
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ) TO it_packages.

    "actual/business method call
    m_cut->modify_packages_in_hana(
        EXPORTING
            i_itab_packages = it_packages
        IMPORTING
            e_created_packages = DATA(created_packages)
            e_updated_packages = DATA(updated_packages)
            e_deleted_packages = DATA(deleted_packages)
            e_skipped_packages = DATA(skipped_packages)
            e_failed_packages = DATA(failed_packages)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( created_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( updated_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( deleted_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( skipped_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( failed_packages ) ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD modify_package_not_existing2.
    DATA: it_packages        TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          ls_cts_hot_package TYPE cts_hot_package.

    ls_cts_hot_package = create_dummy_test_package( i_hana_package_id = 'test.package.1' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    ls_cts_hot_package-hana_pack_delivery_unit = ''.
    ls_cts_hot_package-hana_pack_description = ''.
    ls_cts_hot_package-hana_pack_du_vendor = ''.
    ls_cts_hot_package-hana_pack_hints_for_transl = ''.
    ls_cts_hot_package-hana_pack_is_structural = 1.
    ls_cts_hot_package-hana_pack_orig_lang = ''.
    ls_cts_hot_package-hana_pack_responsible = ''.
    ls_cts_hot_package-hana_pack_src_system = ''.
    ls_cts_hot_package-hana_pack_src_tenant = ''.
    ls_cts_hot_package-hana_pack_text_collection = ''.
    ls_cts_hot_package-hana_pack_text_status = ''.
    ls_cts_hot_package-hana_pack_text_term_domain = ''.

    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package ).
    conf_db_access_double_update( i_old_package = ls_cts_hot_package ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package i_exists = abap_false ).
    conf_package_api_double_create( i_cts_hot_package = ls_cts_hot_package ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package ).

    " prepare input for business method
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ) TO it_packages.

    "actual/business method call
    m_cut->modify_packages_in_hana(
        EXPORTING
            i_itab_packages = it_packages
        IMPORTING
            e_created_packages = DATA(created_packages)
            e_updated_packages = DATA(updated_packages)
            e_deleted_packages = DATA(deleted_packages)
            e_skipped_packages = DATA(skipped_packages)
            e_failed_packages = DATA(failed_packages)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( created_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( updated_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( deleted_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( skipped_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( failed_packages ) ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD modify_package_existing.
    DATA: it_packages        TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          ls_cts_hot_package TYPE cts_hot_package.

    ls_cts_hot_package = create_dummy_test_package( i_hana_package_id = 'test.package.1' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).

    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package ).
    conf_db_access_double_update( i_old_package = ls_cts_hot_package ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package i_exists = abap_true ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package ).

    " prepare input for business method
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ) TO it_packages.

    "actual/business method call
    m_cut->modify_packages_in_hana(
        EXPORTING
            i_itab_packages = it_packages
        IMPORTING
            e_created_packages = DATA(created_packages)
            e_updated_packages = DATA(updated_packages)
            e_deleted_packages = DATA(deleted_packages)
            e_skipped_packages = DATA(skipped_packages)
            e_failed_packages = DATA(failed_packages)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( created_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( updated_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( deleted_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( skipped_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( failed_packages ) ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD modify_package_not_existing_st.
    DATA: it_packages        TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          ls_cts_hot_package TYPE cts_hot_package.

    ls_cts_hot_package = create_dummy_test_package( i_hana_package_id = 'test.package.1' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).

    ls_cts_hot_package-hana_pack_is_structural = 1.
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package ).
    conf_db_access_double_update( i_old_package = ls_cts_hot_package ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package i_exists = abap_false ).
    conf_package_api_double_create( i_cts_hot_package = ls_cts_hot_package ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package ).

    " prepare input for business method
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ) TO it_packages.

    "actual/business method call
    m_cut->modify_packages_in_hana(
       EXPORTING
           i_itab_packages = it_packages
       IMPORTING
           e_created_packages = DATA(created_packages)
           e_updated_packages = DATA(updated_packages)
           e_deleted_packages = DATA(deleted_packages)
           e_skipped_packages = DATA(skipped_packages)
           e_failed_packages = DATA(failed_packages)
       ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( created_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( updated_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( deleted_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( skipped_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( failed_packages ) ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD modify_multiple_packages.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          ls_cts_hot_package1 TYPE cts_hot_package,
          ls_cts_hot_package2 TYPE cts_hot_package,
          ls_cts_hot_package3 TYPE cts_hot_package,
          ls_cts_hot_package4 TYPE cts_hot_package,
          ls_cts_hot_package5 TYPE cts_hot_package.

    ls_cts_hot_package1 = create_dummy_test_package( i_hana_package_id = 'test.package.1' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package1 ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package1 i_exists = abap_false ).
    conf_package_api_double_create( i_cts_hot_package = ls_cts_hot_package1 ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package1 ).

    ls_cts_hot_package2 = create_dummy_test_package( i_hana_package_id = 'test.package.2' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package2 ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package2 i_exists = abap_true ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package2 ).

    ls_cts_hot_package3 = create_dummy_test_package( i_hana_package_id = 'test.package.3' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    ls_cts_hot_package3-hana_pack_is_structural = 1.
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package3 ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package3 i_exists = abap_false ).
    conf_package_api_double_create( i_cts_hot_package = ls_cts_hot_package3 ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package3 ).

    " test with package without values apart from package id
    ls_cts_hot_package4 = create_dummy_test_package( i_hana_package_id = 'test.package.4' i_hot_status = if_cts_hot_db_access=>co_hot_status_inactive ).
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package4 ).
    conf_package_api_double_exist( i_cts_hot_package = ls_cts_hot_package4 i_exists = abap_true ).
    conf_package_api_double_update( i_cts_hot_package = ls_cts_hot_package4 ).

    "expect update call of hot_status, deployed_at and deployed_by for all 4 packages
    conf_db_access_double_update( ls_cts_hot_package1 ).
    conf_db_access_double_update( ls_cts_hot_package2 ).
    conf_db_access_double_update( ls_cts_hot_package3 ).
    conf_db_access_double_update( ls_cts_hot_package4 ).

    " test deletion of package
    ls_cts_hot_package5 = create_dummy_test_package( i_hana_package_id = 'test.package.5' i_hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted ).
    conf_db_access_double_read( i_cts_hot_package = ls_cts_hot_package5 ).
    conf_db_access_double_delete( i_abap_hana_package_id = ls_cts_hot_package5-abap_hana_package_id ).
    conf_package_api_double_delete( i_cts_hot_package = ls_cts_hot_package5 ).

    " prepare input for business method
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package1-hana_package_id ) TO it_packages.
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package2-hana_package_id ) TO it_packages.
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package3-hana_package_id ) TO it_packages.
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package4-hana_package_id ) TO it_packages.
    APPEND cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package5-hana_package_id ) TO it_packages.

    "actual/business method call
    m_cut->modify_packages_in_hana(
        EXPORTING
            i_itab_packages = it_packages
        IMPORTING
            e_created_packages = DATA(created_packages)
            e_updated_packages = DATA(updated_packages)
            e_deleted_packages = DATA(deleted_packages)
            e_skipped_packages = DATA(skipped_packages)
            e_failed_packages = DATA(failed_packages)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( created_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( updated_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( deleted_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( skipped_packages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( failed_packages ) ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD read_package.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lo_test_package_hot LIKE LINE OF it_packages,
          ls_cts_hot_package  TYPE cts_hot_package.

    ls_cts_hot_package = create_dummy_test_package( 'test.package' ).
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package ).
    conf_db_access_double_modify( i_cts_hot_package = ls_cts_hot_package ).

    " prepare input for business method
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.

    "actual/business method call
    m_cut->sync_packages_from_hana_to_hot( it_packages ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD read_package_not_existing.
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          it_packages        TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list.

    DATA(lo_cl_cts_hot_package) = cl_cts_hot_package=>create_instance( 'not.existing.package'  ).
    ls_cts_hot_package-abap_hana_package_id = lo_cl_cts_hot_package->abap_hana_package_id.
    ls_cts_hot_package-hana_package_id = lo_cl_cts_hot_package->hana_package_id.

    "configure read call to return not found error
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package i_error_code = '40132' i_error_msg = 'Package does not exist' ).
    "expect call to DB to delete the package
    conf_db_access_double_delete( lo_cl_cts_hot_package->abap_hana_package_id ).

    APPEND lo_cl_cts_hot_package TO it_packages.

    "actual/business method call
    m_cut->sync_packages_from_hana_to_hot( it_packages ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD read_multiple_packages.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lo_test_package_hot LIKE LINE OF it_packages,
          ls_cts_hot_package1 TYPE cts_hot_package,
          ls_cts_hot_package2 TYPE cts_hot_package,
          ls_cts_hot_package3 TYPE cts_hot_package.

    ls_cts_hot_package1 = create_dummy_test_package( 'test.package.1' ).
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package1 ).

    DATA(lo_cl_cts_hot_package) = cl_cts_hot_package=>create_instance( 'not.existing.package'  ).
    ls_cts_hot_package2-abap_hana_package_id = lo_cl_cts_hot_package->abap_hana_package_id.
    ls_cts_hot_package2-hana_package_id = lo_cl_cts_hot_package->hana_package_id.
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package2 i_error_code = '40132' i_error_msg = 'Package does not exist' ).

    ls_cts_hot_package3 = create_dummy_test_package( 'test.package.3' ).
    ls_cts_hot_package3-hana_pack_is_structural = 1.
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package3 ).

    conf_db_access_double_delete( lo_cl_cts_hot_package->abap_hana_package_id ). "for package2
    conf_db_access_double_modify( i_cts_hot_package = ls_cts_hot_package3 i_times = 2 ). "for package 1 and 3. currently ignoring real input value

    " prepare input for business method
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package1-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package2-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package3-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.

    "actual/business method call
    m_cut->sync_packages_from_hana_to_hot( it_packages ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD read_package_with_nhi_exceptn.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lo_test_package_hot LIKE LINE OF it_packages,
          ls_cts_hot_package  TYPE cts_hot_package,
          lx_hot_exc          TYPE REF TO cx_hana_object_transport.

    ls_cts_hot_package = create_dummy_test_package( 'test.package' ).
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package i_exception = abap_true ).

    " prepare input for business method
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.

    "actual/business method call
    TRY.
        m_cut->sync_packages_from_hana_to_hot( it_packages ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>cx_nhi_hana_repository_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_code ). "because call failed before HANA returned valid response
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_bound( lx_hot_exc->previous ).
        "##TODO check that previous is of type cx_nhi_hana_repository
        DATA(l_cx_nhi) = CAST cx_nhi_hana_repository( lx_hot_exc->previous ).
        cl_abap_unit_assert=>assert_equals( exp = cx_nhi_hana_repository=>error_remote_hana_call act = l_cx_nhi->if_t100_message~t100key ).
    ENDTRY.

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD read_package_with_null_respons.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lo_test_package_hot LIKE LINE OF it_packages,
          ls_cts_hot_package  TYPE cts_hot_package,
          lx_hot_exc          TYPE REF TO cx_hana_object_transport.

    ls_cts_hot_package = create_dummy_test_package( 'test.package' ).
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package i_null_response = abap_true ).

    " prepare input for business method
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.

    "actual/business method call
    TRY.
        m_cut->sync_packages_from_hana_to_hot( it_packages ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>response_is_null_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_code ). "because call failed before HANA returned valid response
        cl_abap_unit_assert=>assert_initial( lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_not_bound( lx_hot_exc->previous ).
    ENDTRY.

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD read_package_with_error_code.
    DATA: it_packages         TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lo_test_package_hot LIKE LINE OF it_packages,
          ls_cts_hot_package  TYPE cts_hot_package,
          lx_hot_exc          TYPE REF TO cx_hana_object_transport.

    ls_cts_hot_package = create_dummy_test_package( 'test.package' ).
    conf_package_api_double_read( i_cts_hot_package = ls_cts_hot_package i_error_code = '40158' i_error_msg = 'HANA error msg' ).

    " prepare input for business method
    lo_test_package_hot = cl_cts_hot_package=>create_instance( iv_hana_package_id = ls_cts_hot_package-hana_package_id ).
    APPEND lo_test_package_hot TO it_packages.

    "actual/business method call
    TRY.
        m_cut->sync_packages_from_hana_to_hot( it_packages ).
        cl_abap_unit_assert=>fail( 'Expected exception was not raised' ).
      CATCH cx_hana_object_transport INTO lx_hot_exc.
        "check that expected exception was caught
        cl_abap_unit_assert=>assert_equals( exp = cx_hana_object_transport=>read_package_error act = lx_hot_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '40158' act = lx_hot_exc->hana_error_code ).
        cl_abap_unit_assert=>assert_equals( exp = 'HANA error msg' act = lx_hot_exc->hana_error_msg ).
        cl_abap_unit_assert=>assert_not_bound( lx_hot_exc->previous ).
    ENDTRY.

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_package_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD conf_db_access_double_read.

    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( i_cts_hot_package )->and_expect( )->is_called_once( ).
    m_db_access_double->read_cts_hot_package( i_abap_hana_package_id = i_cts_hot_package-abap_hana_package_id i_abap_status = 'A' ).

  ENDMETHOD.

  METHOD conf_db_access_double_modify.

    "#TODO ignore parameter because modify_cts_hot_package will use get time stamp field and use sy_uname --> to be put into IF
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_parameter( 'i_cts_hot_package' )->and_expect( )->is_called_times( i_times ).
    m_db_access_double->modify_cts_hot_package( i_cts_hot_package ).

  ENDMETHOD.


  METHOD conf_db_access_double_update.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_package_after_deploymnt( i_old_package = i_old_package
                                                        i_new_status = i_new_status
                                                        i_new_deployed_at = m_timestamp_provider_double->gv_timestamp_to_return
                                                        i_new_deployed_by = sy-uname ).
  ENDMETHOD.


  METHOD conf_db_access_double_delete.

    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_parameter( 'i_abap_status' )->and_expect( )->is_called_once( ).
    m_db_access_double->delete_cts_hot_package( i_abap_hana_package_id ).

  ENDMETHOD.


  METHOD conf_update_obj_after_failed.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_after_failed_dep( i_old_object = VALUE #( abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                                                                abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                                                                abap_status = i_cts_hot_object-abap_status
                                                                                hot_status = i_cts_hot_object-hot_status
                                                                                hana_source_object_version = i_cts_hot_object-hana_source_object_version )
                                                        i_new_status = i_new_status ).
  ENDMETHOD.


  METHOD conf_no_call_activate.
    cl_abap_testdouble=>configure_call( m_object_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_object_api_double->activate( request = cl_nhi_activate_objects_req=>create_activate_objects_req(
                                             activationmode = ce_nhi_activation_mode=>activation_casc_2_phase
                                             objlist        = VALUE #( ( ) )
                                             session        = VALUE #( )
                                         ) ).
  ENDMETHOD.


  METHOD conf_package_api_double_exist.

    DATA lo_exists_package_req TYPE REF TO cl_nhi_exists_package_req.

    "configuration of package test double for call: create_exists_package_req
    lo_exists_package_req = cl_nhi_exists_package_req=>create_exists_package_req( tenant =  '' package = i_cts_hot_package-hana_package_id ).
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( lo_exists_package_req )->and_expect( )->is_called_once( ).
    m_package_api_double->create_exists_package_req( tenant = '' package = i_cts_hot_package-hana_package_id ).

    "configuration of package test double for exist call
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_exists_package_res=>create_exists_package_res(
                                                                           exists     = i_exists
                                                                           error_code = VALUE #( )
                                                                           error_msg  = VALUE #( )
                                                                           error_arg  = VALUE #( ) )
                                                             )->and_expect( )->is_called_once( ).
    m_package_api_double->exists( request = lo_exists_package_req ).

  ENDMETHOD.


  METHOD conf_package_api_double_create.

    DATA lo_create_package_req TYPE REF TO cl_nhi_create_package_req.

    "configuration of package test double for call: create_create_package_req
    lo_create_package_req = cl_nhi_create_package_req=>create_create_package_req(
                tenant                  = ''
                package                 = i_cts_hot_package-hana_package_id
                description             = i_cts_hot_package-hana_pack_description
                responsible             = i_cts_hot_package-hana_pack_responsible
                orig_lang               = i_cts_hot_package-hana_pack_orig_lang
                structural              = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( i_cts_hot_package-hana_pack_is_structural )
                delivery_unit           = i_cts_hot_package-hana_pack_delivery_unit
                du_vendor               = i_cts_hot_package-hana_pack_du_vendor
                text_collection         = i_cts_hot_package-hana_pack_text_collection
                text_status             = i_cts_hot_package-hana_pack_text_status
                text_terminology_domain = i_cts_hot_package-hana_pack_text_term_domain
                hints_for_translation   = i_cts_hot_package-hana_pack_hints_for_transl
                texts                   = VALUE #( )
            ).
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( lo_create_package_req )->and_expect( )->is_called_once( ).
    m_package_api_double->create_create_package_req(
              tenant                  = ''
              package                 = i_cts_hot_package-hana_package_id
              description             = i_cts_hot_package-hana_pack_description
              responsible             = i_cts_hot_package-hana_pack_responsible
              orig_lang               = i_cts_hot_package-hana_pack_orig_lang
              structural              = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( i_cts_hot_package-hana_pack_is_structural )
              delivery_unit           = i_cts_hot_package-hana_pack_delivery_unit
              du_vendor               = i_cts_hot_package-hana_pack_du_vendor
              text_collection         = i_cts_hot_package-hana_pack_text_collection
              text_status             = i_cts_hot_package-hana_pack_text_status
              text_terminology_domain = i_cts_hot_package-hana_pack_text_term_domain
              hints_for_translation   = i_cts_hot_package-hana_pack_hints_for_transl
              texts                   = VALUE #( )
    ).

    "configuration of package test double for call: create
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_create_package_res=>create_create_package_res(
                                                                           error_code = '0'
                                                                           error_msg  = ''
                                                                           error_arg  = '' )
                                                             )->and_expect( )->is_called_once( ).
    m_package_api_double->create( request = lo_create_package_req ).

  ENDMETHOD.

  METHOD conf_package_api_double_read.
    DATA: lo_read_package_req TYPE REF TO cl_nhi_read_package_req,
          l_package_id        TYPE string.

    l_package_id = i_cts_hot_package-hana_package_id.

    "configuration of package test double for call: create_read_package_req
    lo_read_package_req = cl_nhi_read_package_req=>create_read_package_req(
                          tenant        = ''
                          package       = l_package_id
                          lang          = ''
                          lang_fallback = ''
                      ).

    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( lo_read_package_req )->and_expect( )->is_called_once( ).
    m_package_api_double->create_read_package_req(
              tenant        = ''
              package       = l_package_id
              lang          = ''
              lang_fallback = ''
    ).

    "configuration of package test double for call: read
    IF i_exception = abap_true.
      DATA lo_nhi_exc TYPE REF TO cx_nhi_hana_repository.
      CREATE OBJECT lo_nhi_exc EXPORTING textid = cx_nhi_hana_repository=>error_remote_hana_call msgv1 = 'Exception error_remote_hana_call'.
      cl_abap_testdouble=>configure_call( m_package_api_double )->raise_exception( lo_nhi_exc )->and_expect( )->is_called_once( ).
    ELSEIF i_null_response = abap_true.
      "no need to configure return value in this case
      cl_abap_testdouble=>configure_call( m_package_api_double )->and_expect( )->is_called_once( ).
    ELSE.
      IF i_error_code IS INITIAL.
        cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_read_package_res=>create_read_package_res(
                       error_code              = VALUE #( )
                       error_msg               = VALUE #( )
                       error_arg               = VALUE #( )
                       tenant                  = ''
                       package                 = i_cts_hot_package-hana_package_id
                       src_system              = i_cts_hot_package-hana_pack_src_system
                       src_tenant              = i_cts_hot_package-hana_pack_src_tenant
                       description             = i_cts_hot_package-hana_pack_description
                       responsible             = i_cts_hot_package-hana_pack_responsible
                       orig_lang               = i_cts_hot_package-hana_pack_orig_lang
                       structural              = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( i_cts_hot_package-hana_pack_is_structural )
                       delivery_unit           = i_cts_hot_package-hana_pack_delivery_unit
                       du_vendor               = i_cts_hot_package-hana_pack_du_vendor
                       transportable           = abap_true
                       content                 = VALUE #( )
                       text_collection         = i_cts_hot_package-hana_pack_text_collection
                       text_status             = i_cts_hot_package-hana_pack_text_status
                       text_terminology_domain = i_cts_hot_package-hana_pack_text_term_domain
                       hints_for_translation   = i_cts_hot_package-hana_pack_hints_for_transl
                       texts                   = VALUE #( )
         ) )->and_expect( )->is_called_once( ).
      ELSE.
        cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_read_package_res=>create_read_package_res(
                      error_code              = i_error_code
                      error_msg               = i_error_msg
                      error_arg               = ''
                      tenant                  = VALUE #( )
                      package                 = VALUE #( )
                      src_system              = VALUE #( )
                      src_tenant              = VALUE #( )
                      description             = VALUE #( )
                      responsible             = VALUE #( )
                      orig_lang               = VALUE #( )
                      structural              = VALUE #( )
                      delivery_unit           = VALUE #( )
                      du_vendor               = VALUE #( )
                      transportable           = VALUE #( )
                      content                 = VALUE #( )
                      text_collection         = VALUE #( )
                      text_status             = VALUE #( )
                      text_terminology_domain = VALUE #( )
                      hints_for_translation   = VALUE #( )
                      texts                   = VALUE #( )
        ) )->and_expect( )->is_called_once( ).
      ENDIF.
    ENDIF.

    m_package_api_double->read( request = lo_read_package_req ).

  ENDMETHOD.

  METHOD conf_package_api_double_update.

    DATA lo_update_package_req TYPE REF TO cl_nhi_update_package_req.

    "configuration of package test double for call: create_update_package_req
    lo_update_package_req = cl_nhi_update_package_req=>create_update_package_req(
                tenant                  = ''
                package                 = i_cts_hot_package-hana_package_id
                src_system              = i_cts_hot_package-hana_pack_src_system
                src_tenant              = i_cts_hot_package-hana_pack_src_tenant
                description             = i_cts_hot_package-hana_pack_description
                responsible             = i_cts_hot_package-hana_pack_responsible
                orig_lang               = i_cts_hot_package-hana_pack_orig_lang
                structural              = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( i_cts_hot_package-hana_pack_is_structural )
                delivery_unit           = i_cts_hot_package-hana_pack_delivery_unit
                du_vendor               = i_cts_hot_package-hana_pack_du_vendor
                text_collection         = i_cts_hot_package-hana_pack_text_collection
                text_status             = i_cts_hot_package-hana_pack_text_status
                text_terminology_domain = i_cts_hot_package-hana_pack_text_term_domain
                hints_for_translation   = i_cts_hot_package-hana_pack_hints_for_transl
                texts                   = VALUE #( )
            ).
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( lo_update_package_req )->and_expect( )->is_called_once( ).
    m_package_api_double->create_update_package_req(
              tenant                  = ''
              src_system              = i_cts_hot_package-hana_pack_src_system
              src_tenant              = i_cts_hot_package-hana_pack_src_tenant
              package                 = i_cts_hot_package-hana_package_id
              description             = i_cts_hot_package-hana_pack_description
              responsible             = i_cts_hot_package-hana_pack_responsible
              orig_lang               = i_cts_hot_package-hana_pack_orig_lang
              structural              = cl_cts_hot_hana_connector=>conv_is_structural_2_abap_bool( i_cts_hot_package-hana_pack_is_structural )
              delivery_unit           = i_cts_hot_package-hana_pack_delivery_unit
              du_vendor               = i_cts_hot_package-hana_pack_du_vendor
              text_collection         = i_cts_hot_package-hana_pack_text_collection
              text_status             = i_cts_hot_package-hana_pack_text_status
              text_terminology_domain = i_cts_hot_package-hana_pack_text_term_domain
              hints_for_translation   = i_cts_hot_package-hana_pack_hints_for_transl
              texts                   = VALUE #( )
    ).

    "configuration of package test double for call: update
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_update_package_res=>create_update_package_res(
                                                                           error_code = '0'
                                                                           error_msg  = ''
                                                                           error_arg  = '' )
                                                             )->and_expect( )->is_called_once( ).
    m_package_api_double->update( request = lo_update_package_req ).

  ENDMETHOD.

  METHOD conf_package_api_double_delete.

    DATA lo_delete_package_req TYPE REF TO cl_nhi_delete_package_req.

    "configuration of package test double for call: create_delete_package_req
    lo_delete_package_req = cl_nhi_delete_package_req=>create_delete_package_req(
                tenant                  = ''
                package                 = i_cts_hot_package-hana_package_id
                ignore_inactive_objects = abap_true
            ).
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( lo_delete_package_req )->and_expect( )->is_called_once( ).
    m_package_api_double->create_delete_package_req(
                tenant                  = ''
                package                 = i_cts_hot_package-hana_package_id
                ignore_inactive_objects = abap_true
    ).

    "configuration of package test double for call: update
    cl_abap_testdouble=>configure_call( m_package_api_double )->returning( cl_nhi_delete_package_res=>create_delete_package_res(
                                                                           error_code = '0'
                                                                           error_msg  = ''
                                                                           error_arg  = '' )
                                                             )->and_expect( )->is_called_once( ).
    m_package_api_double->delete( request = lo_delete_package_req ).

  ENDMETHOD.


  METHOD conf_read_cts_hot_object.
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( i_cts_hot_object )->and_expect( )->is_called_once( ).
    m_db_access_double->read_cts_hot_object( i_abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                             i_abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                             i_abap_status = 'A' ).
  ENDMETHOD.


  METHOD create_dummy_test_package.
    DATA(l_cl_cts_package) = cl_cts_hot_package=>create_instance( i_hana_package_id ).
    "prepare test package data
    r_cts_hot_package-abap_hana_package_id = l_cl_cts_package->abap_hana_package_id.
    r_cts_hot_package-abap_status = 'A'.
    r_cts_hot_package-hot_status = i_hot_status.
    r_cts_hot_package-hana_package_id = i_hana_package_id.
    r_cts_hot_package-hana_pack_src_system = 'SRC'.
    r_cts_hot_package-hana_pack_src_tenant = ''.
    r_cts_hot_package-hana_pack_description = 'Description of hana package'.
    r_cts_hot_package-hana_pack_responsible = 'srsponsible'.
    r_cts_hot_package-hana_pack_orig_lang = 'DE'.
    r_cts_hot_package-hana_pack_is_structural = 0.
    r_cts_hot_package-hana_pack_delivery_unit = ''. "TODO## 'MYDUNAME'. "currently du must exist in HANA
    r_cts_hot_package-hana_pack_du_vendor = ''. "TODO## 'DUVENDOR'. "currently du must exist in HANA
    r_cts_hot_package-hana_pack_text_collection = 'Text Coll'.
    r_cts_hot_package-hana_pack_text_status = 'Text Status'.
    r_cts_hot_package-hana_pack_text_term_domain = 'Terminology Domain'.
    r_cts_hot_package-hana_pack_hints_for_transl = 'Hints for Translation'.
    r_cts_hot_package-hana_read_system = 'HAN'.
    r_cts_hot_package-abap_import_timestamp = 20140430152355.
    GET TIME STAMP FIELD r_cts_hot_package-abap_deployed_at.
    r_cts_hot_package-abap_deployed_by = 'abap_user'.
    r_cts_hot_package-abap_sync_system = 'DEV'.
    GET TIME STAMP FIELD r_cts_hot_package-abap_synced_at.
    r_cts_hot_package-abap_synced_by = 'abap_user'.
  ENDMETHOD.

  METHOD deploy_objects_import_error.
    "preapre DB double to return object to be imported
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_1 ).
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_2 ).

    "prepare NHI API double to return object not existing for read metadata call
    me->conf_read_metadata_not_existng( 2 ).

    "prepare NHI API double to return error for object import
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_write_object_res=>create_write_object_res(
                                                                          error_code = '12345'
                                                                          error_msg  = 'error during import'
                                                                          error_arg  = VALUE #( )
                                                                          metadata   = VALUE #( )
                                                                      ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 2 ).
    m_object_api_double->write( request = VALUE #( ) ).

    "don't expect activate call to NHI object API double
    me->conf_no_call_activate( ).

    "expect call to DB double for update object with hot_status = 'E' due to import error. (no update of object 2 because it is already 'E')
    me->conf_update_obj_after_failed( m_cts_hot_object_1 ).

    "expect commit work on DB double after writing of error object to DB.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

    "actual/business method call
    m_cut->deploy_objects_to_hana(
      EXPORTING
        i_objects                    = VALUE #( ( m_hot_object_1 ) ( m_hot_object_2 ) )
        i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
        i_max_nr_activation_attempts = 10
        i_activate_with_hints        = abap_true
      IMPORTING
        e_skipped_objects        = DATA(lt_skipped)
        e_successfull_objects    = DATA(lt_successful)
        e_failed_objects         = DATA(lt_failed)
        e_deploy_result          = DATA(lr_deploy_result)
    ).

    "expect the object in failed objects only
    cl_abap_unit_assert=>assert_initial( lt_skipped ).
    cl_abap_unit_assert=>assert_initial( lt_successful ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_failed ) exp = 2 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_2 ).
    cl_abap_unit_assert=>assert_initial( lr_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-import_result-objects_with_last_action line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-import_result-objects_with_last_action line = m_hot_object_2 ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
  ENDMETHOD.


  METHOD deploy_objects_activation_err.
    "prepare DB double to return object to be imported
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_1 ).
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_2 ).

    "prepare NHI API double to return object not existing for read metadata call
    me->conf_read_metadata_not_existng( 2 ).

    "prepare NHI API double to return OK for object import
    me->conf_write_object( 2 ).

    "prepare NHI API double to return error for activate call
    me->conf_activate_objects_failed( VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ) ).

    "expect call to DB double for update object with hot_status = 'E' due to activation error. (no update of object 2 because it is already 'E')
    me->conf_update_obj_after_failed( m_cts_hot_object_1 ).

    "expect commit work on DB double after writing of error object to DB.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

    "actual/business method call
    m_cut->deploy_objects_to_hana(
      EXPORTING
        i_objects                    = VALUE #( ( m_hot_object_1 ) ( m_hot_object_2 ) )
        i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
        i_max_nr_activation_attempts = 10
        i_activate_with_hints        = abap_true
      IMPORTING
        e_skipped_objects        = DATA(lt_skipped)
        e_successfull_objects    = DATA(lt_successful)
        e_failed_objects         = DATA(lt_failed)
        e_deploy_result          = DATA(lr_deploy_result)
    ).

    "expect the object in failed objects only
    cl_abap_unit_assert=>assert_initial( lt_skipped ).
    cl_abap_unit_assert=>assert_initial( lt_successful ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_failed ) exp = 2 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_2 ).
    cl_abap_unit_assert=>assert_initial( lr_deploy_result-import_result-objects_with_last_action ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-activation_results[ 1 ]-objects_with_last_action line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-activation_results[ 1 ]-objects_with_last_action line = m_hot_object_2 ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
  ENDMETHOD.


  METHOD deploy_objects_delete_error.
    "set initial hot status
    m_cts_hot_object_1-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted.
    m_cts_hot_object_2-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.

    "preapre DB double to return object to be deleted
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_1 ).
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_2 ).

    "prepare NHI API double to return objects metadata for read metadata call
    me->conf_read_metadata_existing( 2 ).

    "prepare NHI API double to return OK for object deletion
    me->conf_delete_object( 2 ).

    "prepare NHI API double to return error for activate call
    me->conf_activate_objects_failed( VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ) ).

    "expect call to DB double for update object with hot_status = 'Z' due to deletion error. (no update of object 2 because it is already 'Z')
    me->conf_update_obj_after_failed( i_cts_hot_object = m_cts_hot_object_1 i_new_status = if_cts_hot_db_access=>co_hot_status_delete_error ).

    "expect commit work on DB double after writing of error object to DB.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

    "actual/business method call
    m_cut->deploy_objects_to_hana(
      EXPORTING
        i_objects                = VALUE #( ( m_hot_object_1 ) ( m_hot_object_2 ) )
        i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
        i_max_nr_activation_attempts = 10
        i_activate_with_hints        = abap_true
      IMPORTING
        e_skipped_objects        = DATA(lt_skipped)
        e_successfull_objects    = DATA(lt_successful)
        e_failed_objects         = DATA(lt_failed)
        e_deploy_result          = DATA(lr_deploy_result)
    ).

    "expect the object in failed objects only
    cl_abap_unit_assert=>assert_initial( lt_skipped ).
    cl_abap_unit_assert=>assert_initial( lt_successful ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_failed ) exp = 2 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_2 ).
    cl_abap_unit_assert=>assert_initial( lr_deploy_result-import_result-objects_with_last_action ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-activation_results[ 1 ]-objects_with_last_action line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-activation_results[ 1 ]-objects_with_last_action line = m_hot_object_2 ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
  ENDMETHOD.


  METHOD deploy_objects_revert_error.
    "set initial hot status
    m_cts_hot_object_1-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted.
    m_cts_hot_object_2-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.

    "preapre DB double to return object to be deleted
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_1 ).
    me->conf_read_cts_hot_object( i_cts_hot_object = m_cts_hot_object_2 ).

    "prepare NHI API double to return objects metadata for read metadata call (for revert inactive no active object must exist)
    me->conf_read_metadata_not_existng( 2 ).

    "prepare NHI object API double to return abap_true for exists call so that objects are existing as inactiveonly and must be reverted
    me->conf_exists_object_inactive( 2 ).

    "prepare NHI API double to return OK for object deletion
    me->conf_delete_object_failed( 2 ).

    "prepare NHI API double to not expect actiavte call
    me->conf_no_call_activate( ).

    "expect call to DB double for update object with hot_status = 'Z' due to deletion error. (no update of object 2 because it is already 'Z')
    me->conf_update_obj_after_failed( i_cts_hot_object = m_cts_hot_object_1
                                      i_new_status = if_cts_hot_db_access=>co_hot_status_delete_error ).

    "expect commit work on DB double after writing of error object to DB.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

    "actual/business method call
    m_cut->deploy_objects_to_hana(
      EXPORTING
        i_objects                    = VALUE #( ( m_hot_object_1 ) ( m_hot_object_2 ) )
        i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
        i_max_nr_activation_attempts = 10
        i_activate_with_hints        = abap_true
      IMPORTING
        e_skipped_objects        = DATA(lt_skipped)
        e_successfull_objects    = DATA(lt_successful)
        e_failed_objects         = DATA(lt_failed)
        e_deploy_result          = DATA(lr_deploy_result)
    ).

    "expect the object in failed objects only
    cl_abap_unit_assert=>assert_initial( lt_skipped ).
    cl_abap_unit_assert=>assert_initial( lt_successful ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_failed ) exp = 2 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lt_failed line = m_hot_object_2 ).
    cl_abap_unit_assert=>assert_initial( lr_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-import_result-objects_with_last_action line = m_hot_object_1 ).
    cl_abap_unit_assert=>assert_table_contains( table = lr_deploy_result-import_result-objects_with_last_action line = m_hot_object_2 ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
  ENDMETHOD.

  METHOD append_log_message_not_ok_c.
    DATA lt_actual TYPE cl_cts_hot_hana_connector=>ty_log_messages.

    m_cut->mv_max_no_of_chars_for_log = 444.
    m_cut->append_log_message(
      EXPORTING
        i_severity        = '3'
        i_error_code      = '12345'
        i_message         = 'This is a long hana message with some success full content that needs to be longer than 310 charactres to get some of the characters' &&
                            ' split into severl lines. Because the message is only shortened when it is longer than 310 characters we need some content here.' &&
                            ' If the message is longer than 310 chars, the first 246 chars are logged plus an info that xyz chars are not logged.' &&
                            ' In addition the last 35 charcters are also logged because this is in success messages usually a timestamp.'
      CHANGING
        c_logs            = lt_actual
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_actual ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_log_message( error_code = '12345'
                                                                                               severity = '3'
                  message = 'This is a long hana message with some success full content that needs to be longer than 310 charactres to get some of the characters' &&
                            ' split into severl lines. Because the message is only shortened when it is longer than 310 characters we need some content here.' &&
                            ' If the message is longer than 310 chars, the first 246 chars are logged plus an info that xyz chars are not logged.' &&
                            ' In addition the last 35 charcters are also logged because this is i...39 chars not logged...'
                                                                                               is_hana_message = abap_true )
                                        act = lt_actual[ 1 ] ).
  ENDMETHOD.

  METHOD append_log_message_not_ok_l.
    DATA lt_actual TYPE cl_cts_hot_hana_connector=>ty_log_messages.

    DATA lv_message TYPE string.

    "create message with more than 10000 chars.
    DATA(lv_times) = ( 10000 / 62 ) + 1. "62=length of string from below (1 to Z)
    DO lv_times TIMES.
      lv_message = |{ lv_message }1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ|.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = 10044 act = strlen( lv_message ) ).

    m_cut->append_log_message(
      EXPORTING
        i_severity        = '3'
        i_error_code      = '12345'
        i_message         = lv_message
      CHANGING
        c_logs            = lt_actual
    ).

    data(lv_exp_message) = lv_message(10000).
    lv_exp_message = |{ lv_exp_message }...44 chars not logged...|.

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_actual ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_log_message( error_code = '12345'
                                                                                               severity = '3'
                                                                                               message = lv_exp_message
                                                                                               is_hana_message = abap_true )
                                        act = lt_actual[ 1 ] ).
  ENDMETHOD.

  METHOD append_log_message_not_ok_s.
    DATA lt_actual TYPE cl_cts_hot_hana_connector=>ty_log_messages.

    m_cut->append_log_message(
      EXPORTING
        i_severity        = '3'
        i_error_code      = '12345'
        i_message         = 'This is a short hana error message'
      CHANGING
        c_logs            = lt_actual
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_actual ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_log_message( error_code = '12345'
                                                                                               severity = '3'
                                                                                               message = 'This is a short hana error message'
                                                                                               is_hana_message = abap_true )
                                        act = lt_actual[ 1 ] ).
  ENDMETHOD.

  METHOD append_log_message_ok_long.
    DATA lt_actual TYPE cl_cts_hot_hana_connector=>ty_log_messages.

    m_cut->append_log_message(
      EXPORTING
        i_severity        = '1'
        i_error_code      = '0'
        i_message         = 'This is a long hana message with some success full content that needs to be longer than 310 charactres to get some of the characters' &&
                            ' split into severl lines. Because the message is only shortened when it is longer than 310 characters we need some content here.' &&
                            ' If the message is longer than 310 chars, the first 246 chars are logged plus an info that xyz chars are not logged.' &&
                            ' In addition the last 35 charcters are also logged because this is in success messages usually a timestamp.'
      CHANGING
        c_logs            = lt_actual
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_actual ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_log_message( error_code = '0'
                                                                                               severity = '1'
                  message = 'This is a long hana message with some success full content that needs to be longer than 310 charactres to get some of the characters' &&
                            ' split into severl lines. Because the message is only shortened when it is longer than 310 characters we need some' &&
                            '...202 chars not logged...ccess messages usually a timestamp.'
                                                                                               is_hana_message = abap_true )
                                        act = lt_actual[ 1 ] ).
  ENDMETHOD.

  METHOD append_log_message_ok_short.
    DATA lt_actual TYPE cl_cts_hot_hana_connector=>ty_log_messages.

    m_cut->append_log_message(
      EXPORTING
        i_severity        = '1'
        i_error_code      = '0'
        i_message         = 'This is a short hana message'
      CHANGING
        c_logs            = lt_actual
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_actual ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_log_message( error_code = '0'
                                                                                               severity = '1'
                                                                                               message = 'This is a short hana message'
                                                                                               is_hana_message = abap_true )
                                        act = lt_actual[ 1 ] ).
  ENDMETHOD.

  METHOD conf_read_metadata_not_existng.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_read_obj_metadata_res=>create_read_obj_metadata_res(
                                                                              error_code = '40112'
                                                                              error_msg  = 'Object not existing'
                                                                              error_arg  = VALUE #( )
                                                                              version    = VALUE #( )
                                                                              metadata   = VALUE #( )
                                                                              caption    = VALUE #( )
                                                                              cdata      = VALUE #( )
                                                                              bdata      = VALUE #( )
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->read_metadata( cl_nhi_read_obj_metadata_req=>create_read_obj_metadata_req( object = VALUE #( ) session = VALUE #( ) ) ).
  ENDMETHOD.


  METHOD conf_write_object.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_write_object_res=>create_write_object_res(
                                                                              error_code = '0'
                                                                              error_msg  = 'No error'
                                                                              error_arg  = VALUE #( )
                                                                              metadata   = VALUE #( )
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->write( request = VALUE #( ) ).
  ENDMETHOD.


  METHOD conf_delete_object.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_delete_object_res=>create_delete_object_res(
                                                                              error_code = '0'
                                                                              error_msg  = 'No error'
                                                                              error_arg  = VALUE #( )
                                                                              isreverted = '0'
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->delete( request = VALUE #( ) ).
  ENDMETHOD.


  METHOD conf_delete_object_failed.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_delete_object_res=>create_delete_object_res(
                                                                              error_code = '9876'
                                                                              error_msg  = 'Some delete'
                                                                              error_arg  = VALUE #( )
                                                                              isreverted = '0'
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->delete( request = VALUE #( ) ).
  ENDMETHOD.


  METHOD conf_activate_objects_failed.
    DATA lt_check_results TYPE cl_nhi_check_result=>ty_checkresults.
    LOOP AT i_nhi_objects INTO DATA(lr_nhi_object).
      APPEND cl_nhi_check_result=>create_check_result(
                                error_code = '1234' && sy-tabix
                                error_msg  = 'some error during activation' && sy-tabix
                                timestamp  = '2016-08-10 15:28:0' && sy-tabix
                                severity   = '3'
                                location   = 'my location' && sy-tabix
                                object     = lr_nhi_object
                            ) TO lt_check_results.
    ENDLOOP.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_activate_objects_res=>create_activate_objects_res(
                                                                              activationid = '543'
                                                                              checkresults = lt_check_results
                                                                              error_code   = '12345'
                                                                              error_msg    = 'Some error occured during activation'
                                                                              error_arg    = VALUE #( )
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
    m_object_api_double->activate( request = cl_nhi_activate_objects_req=>create_activate_objects_req(
                                             activationmode = ce_nhi_activation_mode=>activation_casc_2_phase
                                             objlist        = VALUE #( ( ) )
                                             session        = VALUE #( )
                                         ) ).
  ENDMETHOD.


  METHOD conf_read_metadata_existing.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_read_obj_metadata_res=>create_read_obj_metadata_res(
                                                                              error_code = VALUE #( )
                                                                              error_msg  = 'No error'
                                                                              error_arg  = VALUE #( )
                                                                              version    = VALUE #( )
                                                                              metadata   = cl_nhi_metadata_active_ver=>create_metadata(
                                                                                               version_id   = '5'
                                                                                               edit         = abap_false
                                                                                           )
                                                                              caption    = VALUE #( )
                                                                              cdata      = VALUE #( )
                                                                              bdata      = VALUE #( )
                                                                          ) )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->read_metadata( cl_nhi_read_obj_metadata_req=>create_read_obj_metadata_req( object = VALUE #( ) session = VALUE #( ) ) ).
  ENDMETHOD.


  METHOD conf_exists_object_inactive.
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( cl_nhi_object_exists_res=>create_object_exists_res( exists = abap_true )
                                                                          )->ignore_all_parameters( )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->exists( cl_nhi_object_exists_req=>create_object_exists_req( object = VALUE #( ) session = VALUE #( ) ) ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_my_matcher DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_abap_testdouble_matcher.
  PRIVATE SECTION.

    METHODS compare_strings
      IMPORTING
        i_string1       TYPE REF TO data
        i_string2       TYPE REF TO data
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS compare_obj_and_texts
      IMPORTING
        i_obj_1         TYPE REF TO cl_nhi_object_and_texts
        i_obj_2         TYPE REF TO cl_nhi_object_and_texts
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS compare_text
      IMPORTING
        i_text_1        TYPE REF TO cl_nhi_text_with_language
        i_text_2        TYPE REF TO cl_nhi_text_with_language
      RETURNING
        VALUE(r_result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_my_matcher IMPLEMENTATION.

  METHOD if_abap_testdouble_matcher~matches.
    IF method_name = 'CREATE_WRITE_ACTIVE_TS_CON_REQ'
      OR method_name = 'CREATE_WRITE_ACTIVE_T_CONT_REQ'.

      "verify lang
      result = compare_strings( i_string1 = actual_arguments->get_param_importing( 'LANG' )
                                i_string2 = configured_arguments->get_param_importing( 'LANG' ) ).
      IF result = abap_false.
        RETURN.
      ENDIF.

      "verify tenant
      result = compare_strings( i_string1 = actual_arguments->get_param_importing( 'TENANT' )
                                i_string2 = configured_arguments->get_param_importing( 'TENANT' ) ).
      IF result = abap_false.
        RETURN.
      ENDIF.

      "verify package
      result = compare_strings( i_string1 = actual_arguments->get_param_importing( 'PACKAGE' )
                                i_string2 = configured_arguments->get_param_importing( 'PACKAGE' ) ).
      IF result = abap_false.
        RETURN.
      ENDIF.

      "verify objects
      FIELD-SYMBOLS: <lt_act_objects>  TYPE cl_nhi_object_and_texts=>ty_object_and_texts,
                     <lt_conf_objects> TYPE cl_nhi_object_and_texts=>ty_object_and_texts.

      DATA(lv_act_objects_ref) = actual_arguments->get_param_importing( 'OBJECTS' ).
      ASSIGN lv_act_objects_ref->* TO <lt_act_objects>.

      DATA(lv_conf_objects_ref) = configured_arguments->get_param_importing( 'OBJECTS' ).
      ASSIGN lv_conf_objects_ref->* TO <lt_conf_objects>.
      IF lines( <lt_conf_objects> ) = 1 AND lines( <lt_act_objects> ) = 1
            AND compare_obj_and_texts( i_obj_1 = <lt_act_objects>[ 1 ] i_obj_2 = <lt_conf_objects>[ 1 ] ) = abap_true.
        result = abap_true.
      ELSE.
        result = abap_false.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD compare_strings.
    ASSIGN i_string1->* TO FIELD-SYMBOL(<lv_string1>).
    ASSIGN i_string2->* TO FIELD-SYMBOL(<lv_string2>).

    IF <lv_string1> = <lv_string2>.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD compare_obj_and_texts.
    IF i_obj_1->name = i_obj_2->name
        AND i_obj_1->suffix = i_obj_2->suffix
        AND i_obj_1->type = i_obj_2->type.
      IF lines( i_obj_1->texts ) = lines( i_obj_2->texts ).
        LOOP AT i_obj_1->texts INTO DATA(lr_text_1).
          IF compare_text( i_text_1 = CAST cl_nhi_text_with_language( lr_text_1 )
                           i_text_2 = CAST cl_nhi_text_with_language( i_obj_2->texts[ sy-tabix ] ) ) = abap_false.
            RETURN.
          ENDIF.
        ENDLOOP.
        r_result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD compare_text.
    IF i_text_1->content = i_text_2->content
        AND i_text_1->lang = i_text_2->lang
        AND i_text_1->max_length = i_text_2->max_length
        AND i_text_1->text_id = i_text_2->text_id
        AND i_text_1->text_type = i_text_2->text_type.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

"! Test class using test double framework testing object text deployment
CLASS ltcl_cts_hot_hana_conector_txt DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "********** Setup and helper Methods **********
      "! setup the test case by creating m_cut and m_package_api_double
      setup RAISING cx_hana_object_transport,
      "! Configure all NHI TEXT API methods that are used for text deployment as unexpected
      conf_nhi_text_api_unexpected RAISING cx_nhi_hana_repository,

      "********** Test Methods ************
      "! Write text to HANA for inactive object == expect text writing is skipped
      write_text_inactive_object FOR TESTING RAISING cx_static_check,
      "! Write text to HANA for to be deleted object == expect text writing is skipped
      write_text_tobedel_object FOR TESTING RAISING cx_static_check,
      "! Write text to HANA for synchronized (HOT_STATUS = 'N') object == expect text writing is skipped
      write_text_synched_object FOR TESTING RAISING cx_static_check,
      "! Write text to HANA for unknown object == expect text writing is skipped
      write_text_unknown_object FOR TESTING RAISING cx_static_check,
      "! Write text to HANA for object that has no texts == expect text writing is skipped
      write_text_skipped_object FOR TESTING RAISING cx_static_check,
      "! Write text to HANA for several inactive and several unknown objects == expect text writing is skipped
      write_text_inact_unknown_objs FOR TESTING RAISING cx_static_check,
      "! write 2 texts (1 object text, 1 content text) to HANA for 1 object in 1 language
      write_text_1obj_1lang_2text FOR TESTING RAISING cx_static_check,
      "! Try writing 2 texts (1 object text, 1 content text) to HANA for 1 object in 1 language, but object text is failing
      write_text_error_object_text FOR TESTING RAISING cx_static_check,
      "! Try writing 2 texts (1 object text, 1 content text) to HANA for 1 object in 1 language, but content text is failing
      write_text_error_content_text FOR TESTING RAISING cx_static_check.

    DATA:
      m_cut               TYPE REF TO cl_cts_hot_hana_connector,
      m_object_api_double TYPE REF TO if_nhi_object,
      m_text_api_double   TYPE REF TO if_nhi_text,
      m_db_access_double  TYPE REF TO if_cts_hot_db_access,
      "! Test object1: OBJECT1.suffix (com.package.test)
      m_hot_object_1      TYPE REF TO cl_cts_hot_object_v1,
      "! Test object2 with longer name: OBJECT1_WTIH_SOME_LONG_NAME.suffix (com.package.test.with.some.long.package.name)
      m_hot_object_2      TYPE REF TO cl_cts_hot_object_v1,
      "! Test object3: OBJECT3.suffix (com.package.test)
      m_hot_object_3      TYPE REF TO cl_cts_hot_object_v1,
      "! Test object4: hdbtextbundle (com.package.test)
      m_hot_object_4      TYPE REF TO cl_cts_hot_object_v1,
      "! Test object5: OBJECT5.suffix (com.package.test)
      m_hot_object_5      TYPE REF TO cl_cts_hot_object_v1.
ENDCLASS.

"make friendship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_hana_conector_txt.
CLASS ltcl_cts_hot_hana_conector_txt IMPLEMENTATION.

  METHOD setup.
    DATA: nhi_api_double    TYPE REF TO if_nhi_api,
          object_api_double TYPE REF TO if_nhi_object.

    "create test double objects
    nhi_api_double ?= cl_abap_testdouble=>create( 'if_nhi_api' ).
    m_object_api_double ?= cl_abap_testdouble=>create( 'if_nhi_object' ).
    m_text_api_double ?= cl_abap_testdouble=>create( 'if_nhi_text' ).
    m_db_access_double ?= cl_abap_testdouble=>create( 'if_cts_hot_db_access' ).

    "configuration of test double
    cl_abap_testdouble=>configure_call( nhi_api_double )->returning( m_object_api_double )->and_expect( )->is_called_once( ).
    nhi_api_double->get_object( ).

    cl_abap_testdouble=>configure_call( nhi_api_double )->returning( m_text_api_double )->and_expect( )->is_called_once( ).
    nhi_api_double->get_text( ).

    "injecting the test doubles into the object being tested
    m_cut = cl_cts_hot_hana_connector=>create_instance( i_nhi_api = nhi_api_double i_nhi_api_user = 'dummy' ).
    m_cut->m_cts_hot_db_access = m_db_access_double.

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
  ENDMETHOD.

  METHOD write_text_inactive_object.
    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_inactive )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for unexpected calls (all not expected DB calls inside method deploy_object_texts)
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = 'a' i_abap_hana_object_name_suffix = 'b' i_languages = VALUE #( ) ).

    conf_nhi_text_api_unexpected( ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_inactive_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_tobedel_object.
    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_to_be_deleted )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for unexpected calls (all not expected DB calls inside method deploy_object_texts)
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = 'a' i_abap_hana_object_name_suffix = 'b'  i_languages = VALUE #( ) ).

    conf_nhi_text_api_unexpected( ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_inactive_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_synched_object.
    "configuration of db test double for expected call read_hot_status_for_object
*    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_new )->and_expect( )->is_called_once( ).
*    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
*                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).
*
*    "configuration of db test double for unexpected calls (all not expected DB calls inside method deploy_object_texts)
*    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
*    m_db_access_double->read_texts_for_object( i_abap_hana_package_id = 'a' i_abap_hana_object_name_suffix = 'b' ).
*
*    "actual/business method call
*    m_cut->deploy_object_texts(
*        EXPORTING
*            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
*        IMPORTING
*            e_not_active_objects = DATA(lt_inactive_objects)
*            e_unknown_objects    = DATA(lt_unknown_objects)
*            e_skipped_objects    = DATA(lt_skipped_objects)
*            e_text_deploy_result = DATA(lt_text_deploy_result)
*        ).
*
*    "verify exporting parameter
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_inactive_objects ) ).
*    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_inactive_objects ).
*    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_unknown_objects ) ).
*    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_text_deploy_result ) ).
*
*    "verify interactions on testdoubles
*    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
*    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
*    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_unknown_object.
    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( VALUE cts_hot_object_status( ) )->set_parameter( name = 'e_return_code' value = 4 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for unexpected call to read_deployable_texts_for_obj
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = 'a' i_abap_hana_object_name_suffix = 'b'  i_languages = VALUE #( ) ).

    conf_nhi_text_api_unexpected( ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_unknown_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_skipped_object.
    DATA: ls_deploy_result    TYPE cl_cts_hot_hana_connector=>ty_text_deploy_result,
          lt_hot_object_texts TYPE if_cts_hot_db_access=>ty_object_texts.

    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_active )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for expected call read_texts_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( VALUE if_cts_hot_db_access=>ty_object_texts( ) )->and_expect( )->is_called_once( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                       i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_languages = VALUE #( ) ).

    "configuration of db test double for unexpected call to update_object_texts_after_dpl
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->update_object_texts_after_dpl( i_abap_object_reference = 'A'
                                                       i_text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                       i_language = 'D' ).

    conf_nhi_text_api_unexpected( ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_skipped_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_skipped_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_inact_unknown_objs.
    "configuration of db test double for expected calls read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( VALUE cts_hot_object_status( ) )->set_parameter( name = 'e_return_code' value = 4 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_to_be_deleted )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_2->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_2->abap_hana_object_name_suffix ).

    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_inactive )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_3->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_3->abap_hana_object_name_suffix ).

    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( VALUE cts_hot_object_status( ) )->set_parameter( name = 'e_return_code' value = 4 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_4->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_4->abap_hana_object_name_suffix ).

    "configuration of db test double for unexpected calls (all not expected DB calls inside method deploy_object_texts)
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = 'a' i_abap_hana_object_name_suffix = 'b' i_languages = VALUE #( ) ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  )
                                                                                              ( cts_hot_object = m_hot_object_2  )
                                                                                              ( cts_hot_object = m_hot_object_3  )
                                                                                              ( cts_hot_object = m_hot_object_4  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_unknown_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_1 table = lt_unknown_objects ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_4 table = lt_unknown_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_inactive_objects ) ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_2 table = lt_inactive_objects ).
    cl_abap_unit_assert=>assert_table_contains( line = m_hot_object_3 table = lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_1obj_1lang_2text.
    DATA: ls_deploy_result        TYPE cl_cts_hot_hana_connector=>ty_text_deploy_result,
          lt_hot_object_texts     TYPE if_cts_hot_db_access=>ty_object_texts,
          lt_nhi_object_and_texts TYPE cl_nhi_object_and_texts=>ty_object_and_texts,
          lr_matcher              TYPE REF TO lcl_my_matcher.

    "Custom matcher for abap test double framework because tables are used as parameter later which can't be handled by framework...
    CREATE OBJECT lr_matcher.

    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_active )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for expected call read_texts_for_object
    lt_hot_object_texts = VALUE if_cts_hot_db_access=>ty_object_texts( ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_content && '_XCOL'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                                         hana_text_type = 'XCOL'
                                                                         hana_text_max_length = 120
                                                                         hana_text_id = 'COLUMN1'
                                                                         hana_text_content = 'Some text'
                                                                         language = 'D' )
                                                                       ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_object && '_XTIT'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                                                                         hana_text_type = 'XTIT'
                                                                         hana_text_max_length = 255
                                                                         hana_text_id = 'caption'
                                                                         hana_text_content = 'Some caption'
                                                                         language = 'D' ) ).
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( lt_hot_object_texts )->and_expect( )->is_called_once( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                       i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_languages = VALUE #( ) ).

    "configuration of db test double for 2 expected calls of update_obj_texts_after_depl
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_texts_after_dpl( i_abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                       i_language = 'D' ).

    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_texts_after_dpl( i_abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                                                       i_language = 'D' ).

    "configuration of NHI_TEXT_API for expected calls.
    " 1. for write content text
    " 1.1 for call create_write_active_ts_con_req
    lt_nhi_object_and_texts = VALUE #( ( cl_nhi_object_and_texts=>create_object_and_texts(
              name   = m_hot_object_1->hana_object_name
              suffix = m_hot_object_1->hana_object_suffix
              texts  = VALUE #( ( cl_nhi_text_with_language=>create_text_with_language(
                  text_id    = 'COLUMN1'
                  text_type  = 'XCOL'
                  max_length = '120'
                  content    = 'Some text'
                  lang       = 'de'
              ) ) )
          ) ) ).
    DATA(lr_request1) = cl_nhi_write_active_ts_con_req=>create_write_active_ts_con_req(
                  tenant = '' package = m_hot_object_1->hana_package_id lang = 'de'
                  objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( lr_request1 )->set_matcher( lr_matcher )->and_expect( )->is_called_once( ).
    m_text_api_double->create_write_active_ts_con_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    " 1.2 for call write_active_object_content
    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( cl_nhi_write_active_ts_con_res=>create_write_active_ts_con_res(
                                                                        error_code = '0'
                                                                        error_msg  = ''
                                                                        error_arg  = ''
                                                                    ) )->and_expect( )->is_called_once( ).
    m_text_api_double->write_active_object_content( lr_request1 ).

    " 2. for write object text
    " 2.1 for call create_write_active_t_cont_req
    lt_nhi_object_and_texts = VALUE #( ( cl_nhi_object_and_texts=>create_object_and_texts(
              name   = m_hot_object_1->hana_object_name
              suffix = m_hot_object_1->hana_object_suffix
              texts  = VALUE #( ( cl_nhi_text_with_language=>create_text_with_language(
                  text_id    = 'caption'
                  text_type  = 'XTIT'
                  max_length = '255'
                  content    = 'Some caption'
                  lang       = 'de'
              ) ) )
          ) ) ).
    DATA(lr_request2) = cl_nhi_write_active_t_cont_req=>create_write_active_t_cont_req(
                  tenant = '' package = m_hot_object_1->hana_package_id lang = 'de'
                  objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( lr_request2 )->set_matcher( lr_matcher )->and_expect( )->is_called_once( ).
    m_text_api_double->create_write_active_t_cont_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    " 2.2 for call write_active_object
    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( cl_nhi_write_active_t_cont_res=>create_write_active_t_cont_res(
                                                                        error_code = '0'
                                                                        error_msg  = ''
                                                                        error_arg  = ''
                                                                    ) )->and_expect( )->is_called_once( ).
    m_text_api_double->write_active_object( lr_request2 ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).
    cl_abap_unit_assert=>assert_initial( lt_failed_text_deploy_result ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_ok_text_deploy_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'D' act = lt_ok_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-abap_lang ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_code ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_message ).
    cl_abap_unit_assert=>assert_equals( exp = 'de' act = lt_ok_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_lang ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lt_ok_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-imported_text_count ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).

  ENDMETHOD.

  METHOD write_text_error_content_text.
    DATA: ls_deploy_result        TYPE cl_cts_hot_hana_connector=>ty_text_deploy_result,
          lt_hot_object_texts     TYPE if_cts_hot_db_access=>ty_object_texts,
          lt_nhi_object_and_texts TYPE cl_nhi_object_and_texts=>ty_object_and_texts,
          lr_matcher              TYPE REF TO lcl_my_matcher.

    "Custom matcher for abap test double framework because tables are used as parameter later which can't be handled by framework...
    CREATE OBJECT lr_matcher.

    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_active )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for expected call read_texts_for_object
    lt_hot_object_texts = VALUE if_cts_hot_db_access=>ty_object_texts( ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_content && '_XCOL'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                                         hana_text_type = 'XCOL'
                                                                         hana_text_max_length = 120
                                                                         hana_text_id = 'COLUMN1'
                                                                         hana_text_content = 'Some text'
                                                                         language = 'D' )
                                                                       ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_object && '_XTIT'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                                                                         hana_text_type = 'XTIT'
                                                                         hana_text_max_length = 255
                                                                         hana_text_id = 'caption'
                                                                         hana_text_content = 'Some caption'
                                                                         language = 'D' ) ).
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( lt_hot_object_texts )->and_expect( )->is_called_once( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                       i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_languages = VALUE #( ) ).

    "configuration of db test double for not expected call of update_obj_texts_after_depl
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->update_object_texts_after_dpl( i_abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                                                       i_language = 'D' ).

    "configuration of NHI_TEXT_API for expected calls.
    " 1. for write content text
    " 1.1 for call create_write_active_ts_con_req
    lt_nhi_object_and_texts = VALUE #( ( cl_nhi_object_and_texts=>create_object_and_texts(
              name   = m_hot_object_1->hana_object_name
              suffix = m_hot_object_1->hana_object_suffix
              texts  = VALUE #( ( cl_nhi_text_with_language=>create_text_with_language(
                  text_id    = 'COLUMN1'
                  text_type  = 'XCOL'
                  max_length = '120'
                  content    = 'Some text'
                  lang       = 'de'
              ) ) )
          ) ) ).
    DATA(lr_request1) = cl_nhi_write_active_ts_con_req=>create_write_active_ts_con_req(
                  tenant = '' package = m_hot_object_1->hana_package_id lang = 'de'
                  objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( lr_request1 )->set_matcher( lr_matcher )->and_expect( )->is_called_once( ).
    m_text_api_double->create_write_active_ts_con_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    " 1.2 for call write_active_object_content
    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( cl_nhi_write_active_ts_con_res=>create_write_active_ts_con_res(
                                                                        error_code = '1234'
                                                                        error_msg  = 'some error'
                                                                        error_arg  = ''
                                                                    ) )->and_expect( )->is_called_once( ).
    m_text_api_double->write_active_object_content( lr_request1 ).

    " 2. do not expect calls for write object text
    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->create_write_active_t_cont_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->write_active_object( VALUE #( ) ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_failed_text_deploy_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'D' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-abap_lang ).
    cl_abap_unit_assert=>assert_equals( exp = '1234' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_code ).
    cl_abap_unit_assert=>assert_equals( exp = 'some error' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_message ).
    cl_abap_unit_assert=>assert_equals( exp = 'de' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_lang ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-imported_text_count ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD write_text_error_object_text.
    DATA: ls_deploy_result        TYPE cl_cts_hot_hana_connector=>ty_text_deploy_result,
          lt_hot_object_texts     TYPE if_cts_hot_db_access=>ty_object_texts,
          lt_nhi_object_and_texts TYPE cl_nhi_object_and_texts=>ty_object_and_texts,
          lr_matcher              TYPE REF TO lcl_my_matcher.

    "Custom matcher for abap test double framework because tables are used as parameter later which can't be handled by framework...
    CREATE OBJECT lr_matcher.

    "configuration of db test double for expected call read_hot_status_for_object
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( if_cts_hot_db_access=>co_hot_status_active )->set_parameter( name = 'e_return_code' value = 0 )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix ).

    "configuration of db test double for expected call read_texts_for_object
    lt_hot_object_texts = VALUE if_cts_hot_db_access=>ty_object_texts( ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_content && '_XCOL'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                                         hana_text_type = 'XCOL'
                                                                         hana_text_max_length = 120
                                                                         hana_text_id = 'COLUMN1'
                                                                         hana_text_content = 'Some text'
                                                                         language = 'D' )
                                                                       ( abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                                         abap_text_reference = if_cts_hot_db_access=>co_cts_hot_text_type_object && '_XTIT'
                                                                         text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                                                                         hana_text_type = 'XTIT'
                                                                         hana_text_max_length = 255
                                                                         hana_text_id = 'caption'
                                                                         hana_text_content = 'Some caption'
                                                                         language = 'D' ) ).
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( lt_hot_object_texts )->and_expect( )->is_called_once( ).
    m_db_access_double->read_deployable_texts_for_obj( i_abap_hana_package_id = m_hot_object_1->abap_hana_package_id
                                                       i_abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_languages = VALUE #( ) ).

    "configuration of db test double for 1 expected call of update_obj_texts_after_depl for content texts
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_texts_after_dpl( i_abap_object_reference = m_hot_object_1->abap_hana_object_name_suffix
                                                       i_text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                                                       i_language = 'D' ).

    "configuration of NHI_TEXT_API for expected calls.
    " 1. for write content text
    " 1.1 for call create_write_active_ts_con_req
    lt_nhi_object_and_texts = VALUE #( ( cl_nhi_object_and_texts=>create_object_and_texts(
              name   = m_hot_object_1->hana_object_name
              suffix = m_hot_object_1->hana_object_suffix
              texts  = VALUE #( ( cl_nhi_text_with_language=>create_text_with_language(
                  text_id    = 'COLUMN1'
                  text_type  = 'XCOL'
                  max_length = '120'
                  content    = 'Some text'
                  lang       = 'de'
              ) ) )
          ) ) ).
    DATA(lr_request1) = cl_nhi_write_active_ts_con_req=>create_write_active_ts_con_req(
                  tenant = '' package = m_hot_object_1->hana_package_id lang = 'de'
                  objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( lr_request1 )->set_matcher( lr_matcher )->and_expect( )->is_called_once( ).
    m_text_api_double->create_write_active_ts_con_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    " 1.2 for call write_active_object_content
    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( cl_nhi_write_active_ts_con_res=>create_write_active_ts_con_res(
                                                                        error_code = '0'
                                                                        error_msg  = ''
                                                                        error_arg  = ''
                                                                    ) )->and_expect( )->is_called_once( ).
    m_text_api_double->write_active_object_content( lr_request1 ).

    " 2. for write object text
    " 2.1 for call create_write_active_t_cont_req
    lt_nhi_object_and_texts = VALUE #( ( cl_nhi_object_and_texts=>create_object_and_texts(
              name   = m_hot_object_1->hana_object_name
              suffix = m_hot_object_1->hana_object_suffix
              texts  = VALUE #( ( cl_nhi_text_with_language=>create_text_with_language(
                  text_id    = 'caption'
                  text_type  = 'XTIT'
                  max_length = '255'
                  content    = 'Some caption'
                  lang       = 'de'
              ) ) )
          ) ) ).
    DATA(lr_request2) = cl_nhi_write_active_t_cont_req=>create_write_active_t_cont_req(
                  tenant = '' package = m_hot_object_1->hana_package_id lang = 'de'
                  objects = lt_nhi_object_and_texts ).

    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( lr_request2 )->set_matcher( lr_matcher )->and_expect( )->is_called_once( ).
    m_text_api_double->create_write_active_t_cont_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = lt_nhi_object_and_texts ).

    " 2.2 for call write_active_object
    cl_abap_testdouble=>configure_call( m_text_api_double )->returning( cl_nhi_write_active_t_cont_res=>create_write_active_t_cont_res(
                                                                        error_code = '1234'
                                                                        error_msg  = 'some error'
                                                                        error_arg  = ''
                                                                    ) )->and_expect( )->is_called_once( ).
    m_text_api_double->write_active_object( lr_request2 ).

    "actual/business method call
    m_cut->deploy_object_texts(
        EXPORTING
            i_hot_objects_with_lang = VALUE cl_cts_hot_hana_connector=>ty_text_deploy_inputs( ( cts_hot_object = m_hot_object_1  ) )
        IMPORTING
            e_not_active_objects = DATA(lt_inactive_objects)
            e_unknown_objects    = DATA(lt_unknown_objects)
            e_skipped_objects    = DATA(lt_skipped_objects)
            e_success_text_deploy_result = DATA(lt_ok_text_deploy_result)
            e_failed_text_deploy_result = DATA(lt_failed_text_deploy_result)
        ).

    "verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_inactive_objects ).
    cl_abap_unit_assert=>assert_initial( lt_unknown_objects ).
    cl_abap_unit_assert=>assert_initial( lt_skipped_objects ).
    cl_abap_unit_assert=>assert_initial( lt_ok_text_deploy_result ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_failed_text_deploy_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'D' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-abap_lang ).
    cl_abap_unit_assert=>assert_equals( exp = '1234' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_code ).
    cl_abap_unit_assert=>assert_equals( exp = 'some error' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_error_message ).
    cl_abap_unit_assert=>assert_equals( exp = 'de' act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-hana_lang ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lt_failed_text_deploy_result[ cts_hot_object = m_hot_object_1 ]-imported_text_count ).

    "verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_text_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.

  METHOD conf_nhi_text_api_unexpected.
    "configuration of NHI_TEXT_API double for unexpected call to create_write_active_ts_con_req
    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->create_write_active_ts_con_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = VALUE cl_nhi_object_and_texts=>ty_object_and_texts( ) ).

    "configuration of NHI_TEXT_API double for unexpected call to write_active_object_content
    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->write_active_object_content( VALUE #( ) ).

    "configuration of NHI_TEXT_API double for unexpected call to create_write_active_t_cont_req
    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->create_write_active_t_cont_req( tenant = '' package = m_hot_object_1->hana_package_id lang = 'de' objects = VALUE cl_nhi_object_and_texts=>ty_object_and_texts( ) ).

    "configuration of NHI_TEXT_API double for unexpected call to create_write_active_t_cont_req
    cl_abap_testdouble=>configure_call( m_text_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_text_api_double->write_active_object( VALUE #( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_nhi_api_matcher DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_abap_testdouble_matcher.
ENDCLASS.

CLASS lcl_nhi_api_matcher IMPLEMENTATION.

  METHOD if_abap_testdouble_matcher~matches.
    IF method_name = 'CREATE_READ_OBJ_METADATA_REQ'.
      "verify object and session
      FIELD-SYMBOLS: <lr_act_object>   TYPE REF TO cl_nhi_object_id,
                     <lr_conf_object>  TYPE REF TO cl_nhi_object_id,
                     <lr_act_session>  TYPE REF TO cl_nhi_session,
                     <lr_conf_session> TYPE REF TO cl_nhi_session.

      DATA(lv_act_object_ref) = actual_arguments->get_param_importing( 'OBJECT' ).
      ASSIGN lv_act_object_ref->* TO <lr_act_object>.

      DATA(lv_conf_object_ref) = configured_arguments->get_param_importing( 'OBJECT' ).
      ASSIGN lv_conf_object_ref->* TO <lr_conf_object>.

      DATA(lv_act_session_ref) = actual_arguments->get_param_importing( 'SESSION' ).
      ASSIGN lv_act_session_ref->* TO <lr_act_session>.

      DATA(lv_conf_session_ref) = configured_arguments->get_param_importing( 'SESSION' ).
      ASSIGN lv_conf_session_ref->* TO <lr_conf_session>.

      IF <lr_conf_object>->package = <lr_act_object>->package
          AND <lr_conf_object>->name = <lr_act_object>->name
          AND <lr_conf_object>->suffix = <lr_act_object>->suffix
          AND <lr_conf_session>->sessiontype = <lr_act_session>->sessiontype.
        result = abap_true.
      ELSE.
        result = abap_false.
        RETURN.
      ENDIF.
    ELSEIF method_name = 'CREATE_ACTIVATE_OBJECTS_REQ'.
      "verify all paramaters
      FIELD-SYMBOLS: <lr_act_mode>      TYPE REF TO ce_nhi_activation_mode,
                     <lr_conf_mode>     TYPE REF TO ce_nhi_activation_mode,
                     <lr_act_objects>   TYPE cl_nhi_object_id=>ty_objlist,
                     <lr_conf_objects>  TYPE cl_nhi_object_id=>ty_objlist,
                     <lr_act_session2>  TYPE REF TO cl_nhi_inactive_session,
                     <lr_conf_session2> TYPE REF TO cl_nhi_inactive_session,
                     <lr_act_version>   TYPE REF TO cl_nhi_inactive_version,
                     <lr_conf_version>  TYPE REF TO cl_nhi_inactive_version,
                     <lr_act_hints>     TYPE abap_bool,
                     <lr_conf_hints>    TYPE abap_bool.

      DATA(lv_act_mode_ref) = actual_arguments->get_param_importing( 'ACTIVATIONMODE' ).
      ASSIGN lv_act_mode_ref->* TO <lr_act_mode>.

      DATA(lv_conf_mode_ref) = configured_arguments->get_param_importing( 'ACTIVATIONMODE' ).
      ASSIGN lv_conf_mode_ref->* TO <lr_conf_mode>.

      DATA(lv_act_objlist_ref) = actual_arguments->get_param_importing( 'OBJLIST' ).
      ASSIGN lv_act_objlist_ref->* TO <lr_act_objects>.

      DATA(lv_conf_objlist_ref) = configured_arguments->get_param_importing( 'OBJLIST' ).
      ASSIGN lv_conf_objlist_ref->* TO <lr_conf_objects>.

      lv_act_session_ref = actual_arguments->get_param_importing( 'SESSION' ).
      ASSIGN lv_act_session_ref->* TO <lr_act_session2>.

      lv_conf_session_ref = configured_arguments->get_param_importing( 'SESSION' ).
      ASSIGN lv_conf_session_ref->* TO <lr_conf_session2>.

      DATA(lv_act_version_ref) = actual_arguments->get_param_importing( 'VERSION' ).
      ASSIGN lv_act_version_ref->* TO <lr_act_version>.

      DATA(lv_conf_version_ref) = configured_arguments->get_param_importing( 'VERSION' ).
      ASSIGN lv_conf_version_ref->* TO <lr_conf_version>.

      DATA(lv_act_hints_ref) = actual_arguments->get_param_importing( 'ACT_WITH_HINTS' ).
      ASSIGN lv_act_hints_ref->* TO <lr_act_hints>.

      DATA(lv_conf_hints_ref) = configured_arguments->get_param_importing( 'ACT_WITH_HINTS' ).
      ASSIGN lv_conf_hints_ref->* TO <lr_conf_hints>.

      IF <lr_conf_mode> = <lr_act_mode>
          AND <lr_conf_objects> = <lr_act_objects>
          AND <lr_conf_session2>->owner = <lr_act_session2>->owner
          AND <lr_conf_session2>->sessiontype = <lr_act_session2>->sessiontype
          AND <lr_conf_session2>->workspace = <lr_act_session2>->workspace
          AND <lr_conf_version>->owner = <lr_act_version>->owner
          AND <lr_conf_version>->versiontype = <lr_act_version>->versiontype
          AND <lr_conf_version>->workspace = <lr_act_version>->workspace
          AND <lr_conf_hints> = <lr_act_hints>.
        result = abap_true.
      ELSE.
        result = abap_false.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


"! Test class using test double framework testing object activation
CLASS ltcl_cts_hot_hana_conector_act DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS:
      "********** Setup and helper Methods **********
      "! setup the test case by creating m_cut and test objects and test doubles
      setup RAISING cx_hana_object_transport,

      "! Configure nhi_object_api double for call activate.<br/>
      "! For this also required method create_activate_objects_request is configured.
      "! @parameter i_objects_to_activate | List of nhi_objects that should be activated
      "! @parameter i_check_results_to_return | List of check results to be returned by activation
      "! @parameter i_error_code | error code to be returned in activation result
      "! @parameter i_error_msg | error message to be returned in activation result
      "! @parameter i_error_arg | error arg to be returned in activation result
      "! @parameter r_result | Activation result
      "! @raising cx_nhi_hana_repository |
      configure_activate
        IMPORTING
          i_objects_to_activate     TYPE cl_nhi_object_id=>ty_objlist
          i_check_results_to_return TYPE cl_nhi_check_result=>ty_checkresults
          i_error_code              TYPE string DEFAULT '0'
          i_error_msg               TYPE string DEFAULT 'No error' ##NO_TEXT
          i_error_arg               TYPE string OPTIONAL
          i_times                   TYPE i DEFAULT 1
        RETURNING
          VALUE(r_result)           TYPE REF TO cl_nhi_activate_objects_res
        RAISING
          cx_nhi_hana_repository,

      "! Configure nhi_object_api double for call read_metadata.<br/>
      "! For this also required method create_read_obj_metadata_req is configured.
      "! @parameter i_nhi_object | Object for which metadata should be read
      "! @parameter R_RESULT | Read metadata result
      configure_read_metadata
        IMPORTING
          i_nhi_object    TYPE REF TO cl_nhi_object_id
          i_version_id    TYPE string DEFAULT '123'
          i_activated_at  TYPE string DEFAULT '2016-03-01 09:59:01.9260000'
          i_activated_by  TYPE string DEFAULT 'someone'
        RETURNING
          VALUE(r_result) TYPE REF TO cl_nhi_read_obj_metadata_res
        RAISING
          cx_nhi_hana_repository,

      "! Configure nhi_object_api double to expect no call to create_read_obj_metadata_req and read_metadata
      configure_no_call_read_metadat
        RAISING
          cx_nhi_hana_repository,

      "! Configure db access double to expect no call for read_cts_hot_object and modify_cts_hot_object
      configure_no_call_to_db_access,

      "! Configuration of db access double for expected call update_obj_after_success_dep for OK objects.
      "! @parameter i_cts_hot_object | HOT object for which the data should be updated with data read from HANA (i_hana_metadata) but only if no parallel import happened. <br/>
      "!                               Only following fields are used: abap_hana_package_id, abap_hana_object_name_suffix, abap_status, hot_status, hana_source_object_version.
      "! @parameter i_hana_metadata | Object metadata read from HANA after successful activation
      config_update_obj_after_succes
        IMPORTING
          i_cts_hot_object TYPE cts_hot_object
          i_hana_metadata  TYPE REF TO cl_nhi_read_obj_metadata_res,

      "! Configuration of db access double for expected call update_obj_after_failed_dep for failed objects.
      "! @parameter i_cts_hot_object | HOT object for which hot_status should be updated with 'E' or 'Z' but only if no parallel import happened. <br/>
      "!                               Only following fields are used abap_hana_package_id, abap_hana_object_name_suffix, abap_status, hot_status, hana_source_object_version.
      config_update_obj_after_failed
        IMPORTING
          i_cts_hot_object TYPE cts_hot_object
          i_new_status     TYPE cts_hot_object_status DEFAULT if_cts_hot_db_access=>co_hot_status_deploy_error,

      "! Configuration of db access double for expected call read_cts_hot_object.
      "! @parameter i_cts_hot_object | HOT object data to be returned by read_cts_hot_object
      config_read_cts_hot_object
        IMPORTING
          i_cts_hot_object TYPE cts_hot_object,

      "! Configuration of db access double for expected call read_hot_status_for_object.
      "! @parameter i_cts_hot_object | HOT object data with key fields filled
      "! @parameter i_hot_status | HOT Status to be returned by read_hot_status_for_object
      config_read_hot_status_for_obj
        IMPORTING
          i_cts_hot_object TYPE cts_hot_object
          i_hot_status     TYPE cts_hot_object_status DEFAULT if_cts_hot_db_access=>co_hot_status_inactive,

      "! Helper for executing the tests 'activate_1_obj_ok_X'
      activate_1_obj_ok
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'activate_1_obj_ok_2_regen_X'
      activate_1_obj_ok_2_regen
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'activate_1_obj_error_X'
      activate_1_obj_error
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'activate_2_obj_1_ok_1_error_X' because only 1 validation is different
      activate_2_obj_1_ok_1_error
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_2_obj_err_in_unkn_obj_X' because of few differences only
      act_2_obj_err_in_unkn_obj
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_3_obj_1_ok_1_err_1_ni_X' because of few differences only
      act_3_obj_1_ok_1_err_1_ni
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_3_obj_1_ok_2_err_regen_X' because of few differences only
      act_3_obj_1_ok_2_err_regen
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_3_obj_2_ok_1_err_X' because of few differences only
      act_3_obj_2_ok_1_err
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_4_obj_2_ok_1_err_1_ni_X' because of few differences only
      act_4_obj_2_ok_1_err_1_ni
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests act_3_obj_1ok_1err_1ni_1att_X' because of few differences only
      act_3_obj_1ok_1err_1ni_1att
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "! Helper for executing the tests 'act_3_obj_1ok_1err_1ni_2att_X' because of few differences only
      act_3_obj_1ok_1err_1ni_2att
        IMPORTING
          i_activation_mode TYPE c
        RAISING
          cx_nhi_hana_repository
          cx_hana_object_transport,

      "********** Test Methods ************
      "! Activate 1 object with result success with co_hot_activation_mode_ok
      activate_1_obj_ok_0 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result success with co_hot_activation_mode_ok_rec
      activate_1_obj_ok_1 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result success with co_hot_activation_mode_all
      activate_1_obj_ok_2 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result success with co_hot_activation_mode_ok<br/>
      "! 2 objects are regenerated, 1 is OK, 1 is failing.
      activate_1_obj_ok_2_regen_0 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result success with co_hot_activation_mode_ok_rec<br/>
      "! 2 objects are regenerated, 1 is OK, 1 is failing.
      activate_1_obj_ok_2_regen_1 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result success with co_hot_activation_mode_all<br/>
      "! 2 objects are regenerated, 1 is OK, 1 is failing.
      activate_1_obj_ok_2_regen_2 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result error with co_hot_activation_mode_ok
      activate_1_obj_error_0 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result error with co_hot_activation_mode_ok_rec
      activate_1_obj_error_1 FOR TESTING RAISING cx_static_check,

      "! Activate 1 object with result error with co_hot_activation_mode_all
      activate_1_obj_error_2 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_ok, 1 object is OK and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Finally Object 1 should be successfully activated and Object 2 should be failed.
      activate_2_obj_1_ok_1_error_0 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_ok_rec, 1 object is OK and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Finally Object 1 should be successfully activated and Object 2 should be failed.
      activate_2_obj_1_ok_1_error_1 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_all, 1 object is OK and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Finally Object 1 should be successfully activated and Object 2 should be failed.
      activate_2_obj_1_ok_1_error_2 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_ok, 2 objects are OK but error in another object
      "! that was automatically added to activation list by HANA repository because it is 'between' the 2
      "! input objects with regards to dependencies.<br/>
      "! Therefore HANA returns overall result as error.<br/>
      "! Finally the 2 passed objects should be failed.
      act_2_obj_err_in_unkn_obj_0 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_ok_rec, 2 objects are OK but error in another
      "! object that was automatically added to activation list by HANA repository because it is 'between'
      "! the 2 input objects with regards to dependencies.<br/>
      "! Therefore HANA returns overall result as error<br/>
      "! Finally the 2 passed objects should be failed.
      act_2_obj_err_in_unkn_obj_1 FOR TESTING RAISING cx_static_check,

      "! Activate 2 objects with co_hot_activation_mode_all, 2 objects are OK but error in another object
      "! that was automatically added to activation list by HANA repository because it is 'between' the 2
      "! input objects with regards to dependencies.<br/>
      "! Therefore HANA returns overall result as error<br/>
      "! Finally the 2 passed objects should be failed.
      act_2_obj_err_in_unkn_obj_2 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok, 1 object is OK, 1 object has no info and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Attempt 3, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 3 without result (mapped to OK due to overall success)</li></ul>
      "! Finally Object 1 and 3 should be successfully activated and Object 2 should be failed.
      act_3_obj_1_ok_1_err_1_ni_0 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok_rec, 1 object is OK, 1 object has no info and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Attempt 3, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 3 without result (mapped to OK due to overall success)</li></ul>
      "! Finally Object 1 and 3 should be successfully activated and Object 2 should be failed.
      act_3_obj_1_ok_1_err_1_ni_1 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_all, 1 object is OK, 1 object has no info and 1 object is failing:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 3 without result (mapped to OK due to overall success)</li></ul>
      "! Finally Object 1 and 3 should be successfully activated and Object 2 should be failed.
      act_3_obj_1_ok_1_err_1_ni_2 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok, 1 object is OK, 2 objects are failing at activation.<br/>
      "! During attempt 2, object 1 is activated successfully and the 2 failed objects of attempt 1 are regenerated, 1 object with OK, 1 with object error:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as OK:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with regeneration result Error</li>
      "!        <li>Object 4 with regeneration result OK</li></ul>
      "! Finally Object 1 was activated and object 2 and 4 are failed
      act_3_obj_1_ok_2_err_regen_0 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok_rec, 1 object is OK, 2 objects are failing at activation.<br/>
      "! During attempt 2, object 1 is activated successfully and the 2 failed objects of attempt 1 are regenerated, 1 object with OK, 1 with object error:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as OK:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with regeneration result Error</li>
      "!        <li>Object 4 with regeneration result OK</li></ul>
      "! Finally Object 1 was activated and object 2 and 4 are failed
      act_3_obj_1_ok_2_err_regen_1 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_all, 1 object is OK, 2 objects are failing at activation.<br/>
      "! During attempt 2, object 1 is activated successfully and the 2 failed objects of attempt 1 are regenerated, 1 object with OK, 1 with object error:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 2, HANA returns overall result as OK:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with regeneration result Error</li>
      "!        <li>Object 4 with regeneration result OK</li></ul>
      "! Finally Object 1 was activated and object 2 and 4 are failed
      act_3_obj_1_ok_2_err_regen_2 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok, 2 objects are OK, 1 object is failing.
      "! During attempt 2 for the 2 OK objects, the other object is added again by HANA repository due to dependencies and failing again:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error (object was not part of activation call but added by HANA repository automatically)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 3, no HANA activation but info that activation cancelled due to object list not decreasing anymore
      "!    <ul><li>Object 1 with result Error (list not decreasing)</li>
      "!        <li>Object 4 with result Error (list not decreasing)</li></ul>
      "! Finally Object 1, 2 and 4 should be failed.
      act_3_obj_2_ok_1_err_0 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok_rec, 2 objects are OK, 1 object is failing.
      "! During attempt 2 for the 2 OK objects, the other object is added again by HANA repository due to dependencies and failing again:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error (object was not part of activation call but added by HANA repository automatically)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 3, no HANA activation but info that activation cancelled due to object list not decreasing anymore
      "!    <ul><li>Object 1 with result Error (list not decreasing)</li>
      "!        <li>Object 4 with result Error (list not decreasing)</li></ul>
      "! Finally Object 1, 2 and 4 should be failed.
      act_3_obj_2_ok_1_err_1 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_all, 2 objects are OK, 1 object is failing.
      "! During attempt 2 for the 2 OK objects, the other object is added again by HANA repository due to dependencies and failing again:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error (object was not part of activation call but added by HANA repository automatically)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 3, no HANA activation but info that activation cancelled due to object list not decreasing anymore
      "!    <ul><li>Object 1 with result Error (list not decreasing)</li>
      "!        <li>Object 4 with result Error (list not decreasing)</li></ul>
      "! Finally Object 1, 2 and 4 should be failed.
      act_3_obj_2_ok_1_err_2 FOR TESTING RAISING cx_static_check,

      "! Activate 4 objects with co_hot_activation_mode_ok, 2 objects are OK, 1 object has no info and 1 object is failing:<br/>
      "! (In this case successful activation of object 4 depends on the object without info)<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 3, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK (object 1 again because attempt 2 was not successful)</li>
      "!        <li>Object 3 without result (mapped to OK due to overall success)</li>
      "!        <li>Object 4 with result OK (object 1 again because attempt 2 was not successful)</li></ul>
      "! Finally Object 1,3 and 4 should be successfully activated and Object 2 should be failed.
      act_4_obj_2_ok_1_err_1_ni_0 FOR TESTING RAISING cx_static_check,

      "! Activate 4 objects with co_hot_activation_mode_ok_rec, 2 objects are OK, 1 object has no info and 1 object is failing:<br/>
      "! (In this case successful activation of object 4 depends on the object without info)<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 3, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Attempt 4, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 3 without result (mapped to OK due to overall success)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Finally Object 1,3 and 4 should be successfully activated and Object 2 should be failed.
      act_4_obj_2_ok_1_err_1_ni_1 FOR TESTING RAISING cx_static_check,

      "! Activate 4 objects with co_hot_activation_mode_ok_rec, 2 objects are OK, 1 object has no info and 1 object is failing:<br/>
      "! (In this case successful activation of object 4 depends on the object without info and object 1 is failing in attempt 3)<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 3, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result Error</li></ul>
      "! Attempt 4, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 3 without result (mapped to OK due to overall success)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Finally Object 1,3 and 4 should be successfully activated and Object 2 should be failed.
      act_4_obj_2_ok_1_err_1_ni_1_v2 FOR TESTING RAISING cx_static_check,

      "! Activate 4 objects with co_hot_activation_mode_all, 2 objects are OK, 1 object has no info and 1 object is failing:<br/>
      "! (In this case successful activation of object 4 depends on the object without info)<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Attempt 2, HANA returns overall result as success:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 3 without result (mapped to OK due to overall success)</li>
      "!        <li>Object 4 with result OK</li></ul>
      "! Finally Object 1,3 and 4 should be successfully activated and Object 2 should be failed.
      act_4_obj_2_ok_1_err_1_ni_2 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 1 activation attempt:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 1 and Object 2 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1, 2 and 3 should be returned as failed.
      act_3_obj_1ok_1err_1ni_1att_0 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok_rec, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 1 activation attempt:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 1 and Object 2 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1, 2 and 3 should be returned as failed.
      act_3_obj_1ok_1err_1ni_1att_1 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_all, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 1 activation attempt:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 3 without result</li></ul>
      "! Attempt 2, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 1 and Object 2 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1, 2 and 3 should be returned as failed.
      act_3_obj_1ok_1err_1ni_1att_2 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 2 activation attempts:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 without result</li></ul>
      "! Attempt 2, HANA returns overall result as OK:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Attempt 3, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 4 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1 should be successfully activated and Object 2 and 4 should be failed.
      act_3_obj_1ok_1err_1ni_2att_0 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_ok_rec, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 2 activation attempts:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 without result</li></ul>
      "! Attempt 2, HANA returns overall result as OK:<br/>
      "!    <ul><li>Object 1 with result OK</li></ul>
      "! Attempt 3, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 4 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1 should be successfully activated and Object 2 and 4 should be failed.
      act_3_obj_1ok_1err_1ni_2att_1 FOR TESTING RAISING cx_static_check,

      "! Activate 3 objects with co_hot_activation_mode_all, 1 object is OK, 1 object is failing and 1 object has no info but allow
      "! only 2 activation attempts:<br/>
      "! Attempt 1, HANA returns overall result as error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 2 with result Error</li>
      "!        <li>Object 4 without result</li></ul>
      "! Attempt 2, HANA returns overall result as Error:<br/>
      "!    <ul><li>Object 1 with result OK</li>
      "!        <li>Object 4 with result Error</li></ul>
      "! Attempt 3, no HANA activation due to maximum number of attempts reached:<br/>
      "!    <ul><li>Set Object 1 to failed because max number of attempts was reached</li></ul>
      "! Finally Object 1, 2 and 4 should be returned as failed.
      act_3_obj_1ok_1err_1ni_2att_2 FOR TESTING RAISING cx_static_check.

    DATA:
      m_cut                          TYPE REF TO cl_cts_hot_hana_connector,
      m_object_api_double            TYPE REF TO if_nhi_object,
      m_db_access_double             TYPE REF TO if_cts_hot_db_access,
      m_timestamp_provider_double    TYPE REF TO ltd_timestamp_provider,
      "! Test object1: OBJECT1.suffix (com.package.test)
      m_hot_object_1                 TYPE REF TO cl_cts_hot_object_v1,
      "! Test object2 with longer name: OBJECT1_WTIH_SOME_LONG_NAME.suffix (com.package.test.with.some.long.package.name)
      m_hot_object_2                 TYPE REF TO cl_cts_hot_object_v1,
      "! Test object3: OBJECT3.suffix (com.package.test)
      m_hot_object_3                 TYPE REF TO cl_cts_hot_object_v1,
      "! Test object4: hdbtextbundle (com.package.test)
      m_hot_object_4                 TYPE REF TO cl_cts_hot_object_v1,
      "! Test object5: OBJECT5.suffix (com.package.test)
      m_hot_object_5                 TYPE REF TO cl_cts_hot_object_v1,
      "! m_hot_object_1 as cl_nhi_object_id
      m_nhi_object_1                 TYPE REF TO cl_nhi_object_id,
      "! m_hot_object_2 as cl_nhi_object_id
      m_nhi_object_2                 TYPE REF TO cl_nhi_object_id,
      "! m_hot_object_3 as cl_nhi_object_id
      m_nhi_object_3                 TYPE REF TO cl_nhi_object_id,
      "! m_hot_object_4 as cl_nhi_object_id
      m_nhi_object_4                 TYPE REF TO cl_nhi_object_id,
      "! m_hot_object_5 as cl_nhi_object_id
      m_nhi_object_5                 TYPE REF TO cl_nhi_object_id,
      "! m_hot_object_1 as cts_hot_object
      m_cts_hot_object_1             TYPE cts_hot_object,
      "! m_hot_object_2 as cts_hot_object
      m_cts_hot_object_2             TYPE cts_hot_object,
      "! m_hot_object_3 as cts_hot_object
      m_cts_hot_object_3             TYPE cts_hot_object,
      "! m_hot_object_4 as cts_hot_object
      m_cts_hot_object_4             TYPE cts_hot_object,
      "! m_hot_object_5 as cts_hot_object
      m_cts_hot_object_5             TYPE cts_hot_object,
      "! Table with all objects and their hot status and version
      m_object_status_versions       TYPE ty_hot_obj_status_versions,
      "! CheckResult with Finished Activation information. To be used for successful activations only
      m_cr_finished_activation       TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 1 indicating successful activation
      m_cr_object_1_ok               TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 1 indicating error during activation
      m_cr_object_1_error            TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 2 indicating error during activation
      m_cr_object_2_error            TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 2 indicating error during regeneration
      m_cr_object_2_regen_error      TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 4 indicating successful activation
      m_cr_object_4_ok               TYPE REF TO cl_nhi_check_result,
      "! CheckResult line 1 for test object 4 indicating successful regeneration
      m_cr_object_4_regen_ok_1       TYPE REF TO cl_nhi_check_result,
      "! CheckResult line 2 for test object 4 indicating successful regeneration
      m_cr_object_4_regen_ok_2       TYPE REF TO cl_nhi_check_result,
      "! CheckResult for test object 4 indicating error during activation
      m_cr_object_4_error            TYPE REF TO cl_nhi_check_result,
      "! HTA log message for object 1
      m_log_message_object_1_ok      TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 1 that it would be OK and is retried
      m_log_message_object_1_ok_rtry TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 1 if list is not decreasing anymore
      m_log_message_object_1_list_nd TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 1 for failed activation
      m_log_message_object_1_error   TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 2 for failed activation
      m_log_message_object_2_error   TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 2 for failed regeneration
      m_log_message_obj_2_regen_err  TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 3 if list is not decreasing anymore
      m_log_message_object_3_list_nd TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 4
      m_log_message_object_4_ok      TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message line 1 for object 4 for successful regeneration
      m_log_message_obj_4_regen_ok_1 TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message line 2 for object 4 for successful regeneration
      m_log_message_obj_4_regen_ok_2 TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 4 that it would be OK and is retried
      m_log_message_object_4_ok_rtry TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for object 4 for failed activation
      m_log_message_object_4_error   TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for activation finished
      m_log_message_act_finished     TYPE cl_cts_hot_hana_connector=>ty_log_message,
      "! HTA log message for 'List of objects for activation is not decreasing anymore. Set all left objects to error.'
      m_log_message_list_not_decreas TYPE cl_cts_hot_hana_connector=>ty_log_message.
ENDCLASS.

"make friendship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_hana_conector_act.
CLASS ltcl_cts_hot_hana_conector_act IMPLEMENTATION.

  METHOD setup.
    DATA: lr_nhi_api_double    TYPE REF TO if_nhi_api.

    "create test double objects
    lr_nhi_api_double ?= cl_abap_testdouble=>create( 'if_nhi_api' ).
    m_object_api_double ?= cl_abap_testdouble=>create( 'if_nhi_object' ).
    m_db_access_double ?= cl_abap_testdouble=>create( 'if_cts_hot_db_access' ).

    "configuration of test double
    cl_abap_testdouble=>configure_call( lr_nhi_api_double )->returning( m_object_api_double )->and_expect( )->is_called_once( ).
    lr_nhi_api_double->get_object( ).

    "injecting the test doubles into the object being tested
    m_cut = cl_cts_hot_hana_connector=>create_instance( i_nhi_api = lr_nhi_api_double i_nhi_api_user = 'dummy' ).
    m_cut->m_cts_hot_db_access = m_db_access_double.
    m_timestamp_provider_double = NEW ltd_timestamp_provider( ).
    GET TIME STAMP FIELD m_timestamp_provider_double->gv_timestamp_to_return.
    m_cut->m_timestamp_provider = m_timestamp_provider_double.

    "test data (objects, db entries, ...)
    m_hot_object_1 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT1'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_2 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test.with.some.long.package.name'
                     iv_hana_object_name      = 'OBJECT2_WTIH_SOME_LONG_NAME'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_3 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT3'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_hot_object_4 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'obj4'
                     iv_hana_object_suffix    = 'hdbtextbundle'
                 ).

    m_hot_object_5 = cl_cts_hot_object_v1=>create_instance(
                     iv_hana_package_id       = 'com.package.test'
                     iv_hana_object_name      = 'OBJECT5'
                     iv_hana_object_suffix    = 'suffix'
                 ).

    m_nhi_object_1 = cl_nhi_object_id=>create_object_id( tenant = ''
                     package = m_hot_object_1->hana_package_id
                     name = m_hot_object_1->hana_object_name
                     suffix = m_hot_object_1->hana_object_suffix
                     version = cl_nhi_active_version=>create_active_version( )
                     metadata = VALUE #( )
                 ).

    m_nhi_object_2 = cl_nhi_object_id=>create_object_id( tenant = ''
                     package = m_hot_object_2->hana_package_id
                     name = m_hot_object_2->hana_object_name
                     suffix = m_hot_object_2->hana_object_suffix
                     version = cl_nhi_active_version=>create_active_version( )
                     metadata = VALUE #( )
                 ).

    m_nhi_object_3 = cl_nhi_object_id=>create_object_id( tenant = ''
                     package = m_hot_object_3->hana_package_id
                     name = m_hot_object_3->hana_object_name
                     suffix = m_hot_object_3->hana_object_suffix
                     version = cl_nhi_active_version=>create_active_version( )
                     metadata = VALUE #( )
                 ).

    m_nhi_object_4 = cl_nhi_object_id=>create_object_id( tenant = ''
                     package = m_hot_object_4->hana_package_id
                     name = m_hot_object_4->hana_object_name
                     suffix = m_hot_object_4->hana_object_suffix
                     version = cl_nhi_active_version=>create_active_version( )
                     metadata = VALUE #( )
                 ).

    m_nhi_object_5 = cl_nhi_object_id=>create_object_id( tenant = ''
                     package = m_hot_object_5->hana_package_id
                     name = m_hot_object_5->hana_object_name
                     suffix = m_hot_object_5->hana_object_suffix
                     version = cl_nhi_active_version=>create_active_version( )
                     metadata = VALUE #( )
                 ).

    m_cts_hot_object_1-abap_hana_package_id = m_hot_object_1->abap_hana_package_id.
    m_cts_hot_object_1-abap_hana_object_name_suffix = m_hot_object_1->abap_hana_object_name_suffix.
    m_cts_hot_object_1-abap_status = 'A'.
    m_cts_hot_object_1-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    m_cts_hot_object_1-hana_package_id = m_hot_object_1->hana_package_id.
    m_cts_hot_object_1-hana_object_name = m_hot_object_1->hana_object_name.
    m_cts_hot_object_1-hana_object_suffix = m_hot_object_1->hana_object_suffix.
    m_cts_hot_object_1-hana_source_object_version = 1.

    m_cts_hot_object_2-abap_hana_package_id = m_hot_object_2->abap_hana_package_id.
    m_cts_hot_object_2-abap_hana_object_name_suffix = m_hot_object_2->abap_hana_object_name_suffix.
    m_cts_hot_object_2-abap_status = 'A'.
    m_cts_hot_object_2-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    m_cts_hot_object_2-hana_package_id = m_hot_object_2->hana_package_id.
    m_cts_hot_object_2-hana_object_name = m_hot_object_2->hana_object_name.
    m_cts_hot_object_2-hana_object_suffix = m_hot_object_2->hana_object_suffix.
    m_cts_hot_object_2-hana_source_object_version = 2.

    m_cts_hot_object_3-abap_hana_package_id = m_hot_object_3->abap_hana_package_id.
    m_cts_hot_object_3-abap_hana_object_name_suffix = m_hot_object_3->abap_hana_object_name_suffix.
    m_cts_hot_object_3-abap_status = 'A'.
    m_cts_hot_object_3-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    m_cts_hot_object_3-hana_package_id = m_hot_object_3->hana_package_id.
    m_cts_hot_object_3-hana_object_name = m_hot_object_3->hana_object_name.
    m_cts_hot_object_3-hana_object_suffix = m_hot_object_3->hana_object_suffix.
    m_cts_hot_object_3-hana_source_object_version = 3.

    m_cts_hot_object_4-abap_hana_package_id = m_hot_object_4->abap_hana_package_id.
    m_cts_hot_object_4-abap_hana_object_name_suffix = m_hot_object_4->abap_hana_object_name_suffix.
    m_cts_hot_object_4-abap_status = 'A'.
    m_cts_hot_object_4-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    m_cts_hot_object_4-hana_package_id = m_hot_object_4->hana_package_id.
    m_cts_hot_object_4-hana_object_name = m_hot_object_4->hana_object_name.
    m_cts_hot_object_4-hana_object_suffix = m_hot_object_4->hana_object_suffix.
    m_cts_hot_object_4-hana_source_object_version = 4.

    m_cts_hot_object_5-abap_hana_package_id = m_hot_object_5->abap_hana_package_id.
    m_cts_hot_object_5-abap_hana_object_name_suffix = m_hot_object_5->abap_hana_object_name_suffix.
    m_cts_hot_object_5-abap_status = 'A'.
    m_cts_hot_object_5-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    m_cts_hot_object_5-hana_package_id = m_hot_object_5->hana_package_id.
    m_cts_hot_object_5-hana_object_name = m_hot_object_5->hana_object_name.
    m_cts_hot_object_5-hana_object_suffix = m_hot_object_5->hana_object_suffix.
    m_cts_hot_object_5-hana_source_object_version = 5.

    m_object_status_versions = VALUE #( ( object = m_hot_object_1
                                          abap_status = 'A'
                                          hot_status = m_cts_hot_object_1-hot_status
                                          hana_source_object_version = m_cts_hot_object_1-hana_source_object_version )
                                        ( object = m_hot_object_2
                                          abap_status = 'A'
                                          hot_status = m_cts_hot_object_2-hot_status
                                          hana_source_object_version = m_cts_hot_object_2-hana_source_object_version )
                                        ( object = m_hot_object_3
                                          abap_status = 'A'
                                          hot_status = m_cts_hot_object_3-hot_status
                                          hana_source_object_version = m_cts_hot_object_3-hana_source_object_version )
                                        ( object = m_hot_object_4
                                          abap_status = 'A'
                                          hot_status = m_cts_hot_object_4-hot_status
                                          hana_source_object_version = m_cts_hot_object_4-hana_source_object_version )
                                        ( object = m_hot_object_5
                                          abap_status = 'A'
                                          hot_status = m_cts_hot_object_5-hot_status
                                          hana_source_object_version = m_cts_hot_object_5-hana_source_object_version
                                        ) ).

    m_cr_finished_activation = cl_nhi_check_result=>create_check_result(
                                    error_code = '40137'
                                    error_msg  = 'Finished activation phase. Starting regeneration phase now.'
                                    timestamp  = '2016-02-26 14:38:33.1610000'
                                    severity   = '4'
                                    location   = ''
                                    object     = VALUE #( ) ).

    m_cr_object_1_ok = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 1 activated successfully'
                                    timestamp  = '2016-02-26 14:38:32.1550000'
                                    severity   = '1'
                                    location   = ''
                                    object     = m_nhi_object_1 ).

    m_cr_object_1_error = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 1 could not be activated'
                                    timestamp  = '2016-01-26 13:32:12.1460000'
                                    severity   = '3'
                                    location   = 'somewhere else'
                                    object     = m_nhi_object_1 ).

    m_cr_object_2_error = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 2 could not be activated'
                                    timestamp  = '2016-02-26 13:37:12.2460000'
                                    severity   = '3'
                                    location   = 'somewhere'
                                    object     = m_nhi_object_2 ).

    m_cr_object_2_regen_error = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 2 failure during regeneration'
                                    timestamp  = '2016-03-26 13:37:12.2460000'
                                    severity   = '3'
                                    location   = 'somewhere2'
                                    object     = m_nhi_object_2 ).

    m_cr_object_4_ok = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 4 activated successfully'
                                    timestamp  = '2016-02-26 14:39:32.1550000'
                                    severity   = '1'
                                    location   = ''
                                    object     = m_nhi_object_4 ).

    m_cr_object_4_regen_ok_1 = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 4 regenerated successfully - line 1'
                                    timestamp  = '2016-03-26 14:39:32.1550000'
                                    severity   = '1'
                                    location   = ''
                                    object     = m_nhi_object_4 ).

    m_cr_object_4_regen_ok_2 = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 4 regenerated successfully - line 2'
                                    timestamp  = '2016-03-26 14:39:32.1560000'
                                    severity   = '1'
                                    location   = ''
                                    object     = m_nhi_object_4 ).

    m_cr_object_4_error = cl_nhi_check_result=>create_check_result(
                                    error_code = '0'
                                    error_msg  = 'Object 4 could not be activated'
                                    timestamp  = '2016-02-26 14:36:12.2460000'
                                    severity   = '3'
                                    location   = 'somewhere'
                                    object     = m_nhi_object_4 ).

    m_log_message_object_1_ok-error_code = m_cr_object_1_ok->error_code.
    m_log_message_object_1_ok-severity = m_cr_object_1_ok->severity.
    m_log_message_object_1_ok-is_hana_message = abap_true.
    m_log_message_object_1_ok-message = m_cr_object_1_ok->error_msg.
    m_log_message_object_1_ok-timestamp = m_cr_object_1_ok->timestamp.
    m_log_message_object_1_ok-cts_hot_object = m_hot_object_1.

    m_log_message_object_1_ok_rtry-error_code = '0'.
    m_log_message_object_1_ok_rtry-severity = '1111'.
    m_log_message_object_1_ok_rtry-is_hana_message = abap_false.
    m_log_message_object_1_ok_rtry-message = 'OBJECT1.suffix (com.package.test) would be OK. Process again.'.
    m_log_message_object_1_ok_rtry-cts_hot_object = m_hot_object_1.

    m_log_message_object_1_list_nd-error_code = '0'.
    m_log_message_object_1_list_nd-severity = '3'.
    m_log_message_object_1_list_nd-message = 'Object still in activation list but list not decreasing anymore.'.
    m_log_message_object_1_list_nd-cts_hot_object = m_hot_object_1.

    m_log_message_object_1_error-error_code = m_cr_object_1_error->error_code.
    m_log_message_object_1_error-severity = m_cr_object_1_error->severity.
    m_log_message_object_1_error-is_hana_message = abap_true.
    m_log_message_object_1_error-message = m_cr_object_1_error->error_msg.
    m_log_message_object_1_error-location = m_cr_object_1_error->location.
    m_log_message_object_1_error-timestamp = m_cr_object_1_error->timestamp.
    m_log_message_object_1_error-cts_hot_object = m_hot_object_1.

    m_log_message_object_2_error-error_code = m_cr_object_2_error->error_code.
    m_log_message_object_2_error-severity = m_cr_object_2_error->severity.
    m_log_message_object_2_error-is_hana_message = abap_true.
    m_log_message_object_2_error-message = m_cr_object_2_error->error_msg.
    m_log_message_object_2_error-location = m_cr_object_2_error->location.
    m_log_message_object_2_error-timestamp = m_cr_object_2_error->timestamp.
    m_log_message_object_2_error-cts_hot_object = m_hot_object_2.

    m_log_message_obj_2_regen_err-error_code = m_cr_object_2_regen_error->error_code.
    m_log_message_obj_2_regen_err-severity = m_cr_object_2_regen_error->severity.
    m_log_message_obj_2_regen_err-is_hana_message = abap_true.
    m_log_message_obj_2_regen_err-message = m_cr_object_2_regen_error->error_msg.
    m_log_message_obj_2_regen_err-location = m_cr_object_2_regen_error->location.
    m_log_message_obj_2_regen_err-timestamp = m_cr_object_2_regen_error->timestamp.
    m_log_message_obj_2_regen_err-cts_hot_object = m_hot_object_2.

    m_log_message_object_3_list_nd-error_code = '0'.
    m_log_message_object_3_list_nd-severity = '3'.
    m_log_message_object_3_list_nd-message = 'Object still in activation list but list not decreasing anymore.'.
    m_log_message_object_3_list_nd-cts_hot_object = m_hot_object_3.

    m_log_message_object_4_ok-error_code = m_cr_object_4_ok->error_code.
    m_log_message_object_4_ok-severity = m_cr_object_4_ok->severity.
    m_log_message_object_4_ok-is_hana_message = abap_true.
    m_log_message_object_4_ok-message = m_cr_object_4_ok->error_msg.
    m_log_message_object_4_ok-timestamp = m_cr_object_4_ok->timestamp.
    m_log_message_object_4_ok-cts_hot_object = m_hot_object_4.

    m_log_message_obj_4_regen_ok_1-error_code = m_cr_object_4_regen_ok_1->error_code.
    m_log_message_obj_4_regen_ok_1-severity = m_cr_object_4_regen_ok_1->severity.
    m_log_message_obj_4_regen_ok_1-is_hana_message = abap_true.
    m_log_message_obj_4_regen_ok_1-message = m_cr_object_4_regen_ok_1->error_msg.
    m_log_message_obj_4_regen_ok_1-timestamp = m_cr_object_4_regen_ok_1->timestamp.
    m_log_message_obj_4_regen_ok_1-cts_hot_object = m_hot_object_4.

    m_log_message_obj_4_regen_ok_2-error_code = m_cr_object_4_regen_ok_2->error_code.
    m_log_message_obj_4_regen_ok_2-severity = m_cr_object_4_regen_ok_2->severity.
    m_log_message_obj_4_regen_ok_2-is_hana_message = abap_true.
    m_log_message_obj_4_regen_ok_2-message = m_cr_object_4_regen_ok_2->error_msg.
    m_log_message_obj_4_regen_ok_2-timestamp = m_cr_object_4_regen_ok_2->timestamp.
    m_log_message_obj_4_regen_ok_2-cts_hot_object = m_hot_object_4.

    m_log_message_object_4_ok_rtry-error_code = '0'.
    m_log_message_object_4_ok_rtry-severity = '1111'.
    m_log_message_object_4_ok_rtry-is_hana_message = abap_false.
    m_log_message_object_4_ok_rtry-message = 'obj4.hdbtextbundle (com.package.test) would be OK. Process again.'.
    m_log_message_object_4_ok_rtry-cts_hot_object = m_hot_object_4.

    m_log_message_object_4_error-error_code = m_cr_object_4_error->error_code.
    m_log_message_object_4_error-severity = m_cr_object_4_error->severity.
    m_log_message_object_4_error-is_hana_message = abap_true.
    m_log_message_object_4_error-message = m_cr_object_4_error->error_msg.
    m_log_message_object_4_error-location = m_cr_object_4_error->location.
    m_log_message_object_4_error-timestamp = m_cr_object_4_error->timestamp.
    m_log_message_object_4_error-cts_hot_object = m_hot_object_4.

    m_log_message_act_finished-error_code = m_cr_finished_activation->error_code.
    m_log_message_act_finished-severity = m_cr_finished_activation->severity.
    m_log_message_act_finished-is_hana_message = abap_true.
    m_log_message_act_finished-message = m_cr_finished_activation->error_msg.
    m_log_message_act_finished-timestamp = m_cr_finished_activation->timestamp.

    m_log_message_list_not_decreas-error_code = '0'.
    m_log_message_list_not_decreas-severity = '3'.
    m_log_message_list_not_decreas-message = 'List of objects for activation is not decreasing anymore. Set all left objects to error.'.
  ENDMETHOD.


  METHOD activate_1_obj_ok.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res) = configure_activate(
                                    i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                    i_check_results_to_return = VALUE #( ( m_cr_object_1_ok )
                                                                         ( m_cr_finished_activation ) ) ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res) = configure_read_metadata( m_nhi_object_1 ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res ).

    "configure expected call to commit work
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 ) )
        i_object_status_versions = m_object_status_versions
    CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_failed_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_true
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '0'
                                                    hana_error_msg = 'No error'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                    failed_objects = VALUE #( ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_0.
    me->activate_1_obj_ok( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_1.
    me->activate_1_obj_ok( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_2.
    me->activate_1_obj_ok( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_2_regen.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res) = configure_activate(
                                    i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                    i_check_results_to_return = VALUE #( ( m_cr_object_1_ok )
                                                                         ( m_cr_finished_activation )
                                                                         ( m_cr_object_2_regen_error )
                                                                         ( m_cr_object_4_regen_ok_1 )
                                                                         ( m_cr_object_4_regen_ok_2 ) )
                                    i_error_code = '40136'
                                    i_error_msg = 'Activation OK but error during revalidation.' ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res) = configure_read_metadata( m_nhi_object_1 ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res ).

    "configure expected call to commit work
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 ) )
        i_object_status_versions = m_object_status_versions
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_failed_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( ls_deploy_result-activation_results ) ).

    "replace expected object2 and object4 with the object returned by activation because instance is created during activation. Thats's because object2 and 4 are regenerated and not passed as to be activated
    m_log_message_obj_2_regen_err-cts_hot_object = ls_deploy_result-activation_results[ 1 ]-log_messages[ 3 ]-cts_hot_object.
    m_log_message_obj_4_regen_ok_1-cts_hot_object = ls_deploy_result-activation_results[ 1 ]-log_messages[ 4 ]-cts_hot_object.
    m_log_message_obj_4_regen_ok_2-cts_hot_object = ls_deploy_result-activation_results[ 1 ]-log_messages[ 4 ]-cts_hot_object. "use same object to check that both check results have same object

    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_true
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Activation OK but error during revalidation.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok )
                                                                            ( m_log_message_act_finished )
                                                                            ( m_log_message_obj_2_regen_err )
                                                                            ( m_log_message_obj_4_regen_ok_1 )
                                                                            ( m_log_message_obj_4_regen_ok_2 ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                    failed_objects = VALUE #( ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_2_regen_0.
    me->activate_1_obj_ok_2_regen( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_2_regen_1.
    me->activate_1_obj_ok_2_regen( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD activate_1_obj_ok_2_regen_2.
    me->activate_1_obj_ok_2_regen( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD activate_1_obj_error.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res) = configure_activate(
                                    i_objects_to_activate  = VALUE #( ( m_nhi_object_2 ) )
                                    i_check_results_to_return = VALUE #( ( m_cr_object_2_error ) )
                                    i_error_code           = '40136'
                                    i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    " do not expect read of metadata because activation failed
    configure_no_call_read_metadat( ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to expect 1 call to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_2 ).

    "expect commit work on DB double after writing of error objects to DB.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).


* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) )
                                        act = lt_failed_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD activate_1_obj_error_0.
    me->activate_1_obj_error( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD activate_1_obj_error_1.
    me->activate_1_obj_error( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD activate_1_obj_error_2.
    me->activate_1_obj_error( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD activate_2_obj_1_ok_1_error.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).
    "2. activation attempt
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_finished_activation ) ) ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res) = configure_read_metadata( m_nhi_object_1 ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    "read hot status after successful activation
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res ).

    "expect 1 call to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_2 ).

    "configure expected call to commit work (1 for OK object, 1 for failed object)
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) )
                                        act = lt_failed_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).
    DATA(lv_ok_objects_only) = abap_false.
    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok
        OR i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec.
      lv_ok_objects_only = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_true
                                                    ok_objects_only = lv_ok_objects_only
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '0'
                                                    hana_error_msg = 'No error'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                    failed_objects = VALUE #(  ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD activate_2_obj_1_ok_1_error_0.
    me->activate_2_obj_1_ok_1_error( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD activate_2_obj_1_ok_1_error_1.
    me->activate_2_obj_1_ok_1_error( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD activate_2_obj_1_ok_1_error_2.
    me->activate_2_obj_1_ok_1_error( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_2_obj_err_in_unkn_obj.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "We expect only 1 activation attempt because all passed objects are OK and next attempt would end with same result because same input
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) ( m_cr_object_4_ok ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    " do not expect read of metadata because activation failed
    configure_no_call_read_metadat( ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    "expect 2 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_1 ).
    config_update_obj_after_failed( m_cts_hot_object_4 ).

    "configure expected call to commit work
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) ( m_hot_object_4 ) )
                                        act = lt_failed_objects ). "Object2 is failing but must not be part of lt_failed_objects because lt_failed_objects should contain only objects that we try to activate

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
    "replace expected object2 with the object returned by activation because instance is created during activation. Thats's because object2 is not passed as to be activated
    m_log_message_object_2_error-cts_hot_object = ls_deploy_result-activation_results[ 1 ]-log_messages[ 2 ]-cts_hot_object.
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) ( m_log_message_object_4_ok_rtry ) )
                                                    objects_with_last_action = VALUE #( )
                                                    failed_objects = VALUE #( ) ) "Object2 is failing but it was not part of objects to be activated and therefore not returned in this failed_objects list
                                        act = ls_deploy_result-activation_results[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = 'SCTS_HOT'
                                                    hana_error_code = '618'
                                                    hana_error_msg = ''
                                                    log_messages = VALUE #( )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_1 ) ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_2_obj_err_in_unkn_obj_0.
    me->act_2_obj_err_in_unkn_obj( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_2_obj_err_in_unkn_obj_1.
    me->act_2_obj_err_in_unkn_obj( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_2_obj_err_in_unkn_obj_2.
    me->act_2_obj_err_in_unkn_obj( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_1_err_1_ni.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_3 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) ) "no check result for object 3!
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      "2. activation attempt - in mode all, OK object 1 and object without info from 1. attempt
      DATA(lr_activate_objects_res_2) = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_3 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_finished_activation ) ) ).
    ELSE.
      "2. activation attempt - only OK object 1 from 1. attempt
      lr_activate_objects_res_2 = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_finished_activation ) ) ).

      "3. activation attempt - object without result, object 3, from 1. attempt
      DATA(lr_activate_objects_res_3) = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_3 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_finished_activation ) ) ).
    ENDIF.

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res_1) = configure_read_metadata( m_nhi_object_1 ).
    DATA(lr_read_metadata_res_3) = configure_read_metadata( i_nhi_object = m_nhi_object_3
                                                            i_version_id = '456'
                                                            i_activated_at = '2016-01-01 07:59:41.5430000'
                                                            i_activated_by = 'somebody' ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).
    config_read_hot_status_for_obj( m_cts_hot_object_3 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res_1 ).
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_3
                                    i_hana_metadata = lr_read_metadata_res_3 ).

    "expect 1 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure of object 2
    config_update_obj_after_failed( m_cts_hot_object_2 ).

    "configure expected call to commit work
    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    ELSE.
      cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 3 ).
    ENDIF.
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_3->hana_package_id
                                                object_name = m_hot_object_3->hana_object_name
                                                object_suffix = m_hot_object_3->hana_object_suffix
                                                hot_object = m_hot_object_3
                                                nhi_object = m_nhi_object_3 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) ( m_hot_object_3 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_not_initial( ls_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
      cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                      activation_counter = 2
                                                      nr_of_attempted_objects = 2
                                                      activation_success = abap_true
                                                      ok_objects_only = abap_false
                                                      hana_activation_id = '12345'
                                                      hana_error_code = '0'
                                                      hana_error_msg = 'No error'
                                                      log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                      objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) )
                                                      failed_objects = VALUE #( ) )
                                          act = ls_deploy_result-activation_results[ 2 ] ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_deploy_result-activation_results ) ).
      cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                      activation_counter = 2
                                                      nr_of_attempted_objects = 1
                                                      activation_success = abap_true
                                                      ok_objects_only = abap_true
                                                      hana_activation_id = '12345'
                                                      hana_error_code = '0'
                                                      hana_error_msg = 'No error'
                                                      log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                      objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                      failed_objects = VALUE #( ) )
                                          act = ls_deploy_result-activation_results[ 2 ] ).

      cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                      activation_counter = 3
                                                      nr_of_attempted_objects = 1
                                                      activation_success = abap_true
                                                      ok_objects_only = abap_false
                                                      hana_activation_id = '12345'
                                                      hana_error_code = '0'
                                                      hana_error_msg = 'No error'
                                                      log_messages = VALUE #( ( m_log_message_act_finished ) )
                                                      objects_with_last_action = VALUE #( ( m_hot_object_3 ) )
                                                      failed_objects = VALUE #( ) )
                                          act = ls_deploy_result-activation_results[ 3 ] ).
    ENDIF.

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_1_err_1_ni_0.
    me->act_3_obj_1_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_1_err_1_ni_1.
    me->act_3_obj_1_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_1_err_1_ni_2.
    me->act_3_obj_1_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_2_err_regen.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lv_ok_objects_only    TYPE abap_bool.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) ( m_cr_object_4_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "2. activation attempt - object 1 activated OK and 2 regenerations, object 2 failing and object 4 regenerated OK
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok )
                                                                             ( m_cr_finished_activation )
                                                                             ( m_cr_object_2_regen_error )
                                                                             ( m_cr_object_4_regen_ok_1 )
                                                                             ( m_cr_object_4_regen_ok_2 ) ) ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res_1) = configure_read_metadata( m_nhi_object_1 ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res_1 ).


    "expect 2 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure of object 2 and 4
    config_update_obj_after_failed( m_cts_hot_object_2 ).
    config_update_obj_after_failed( m_cts_hot_object_4 ).

    "configure expected call to commit work
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) ( m_hot_object_4 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_not_initial( ls_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) ( m_log_message_object_4_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      lv_ok_objects_only = abap_false.
    ELSE.
      lv_ok_objects_only = abap_true.
    ENDIF.

    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_true
                                                    ok_objects_only = lv_ok_objects_only
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '0'
                                                    hana_error_msg = 'No error'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok )
                                                                            ( m_log_message_act_finished )
                                                                            ( m_log_message_obj_2_regen_err )
                                                                            ( m_log_message_obj_4_regen_ok_1 )
                                                                            ( m_log_message_obj_4_regen_ok_2 ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                    failed_objects = VALUE #( ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_2_err_regen_0.
    me->act_3_obj_1_ok_2_err_regen( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_2_err_regen_1.
    me->act_3_obj_1_ok_2_err_regen( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_3_obj_1_ok_2_err_regen_2.
    me->act_3_obj_1_ok_2_err_regen( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_3_obj_2_ok_1_err.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lv_ok_objects_only    TYPE abap_bool.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) ( m_cr_object_4_ok ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "2. activation attempt - all OK objects from attempt 1 as input but error in the object already failed in attempt 1 because of implicitly added by HANA due to dependencies
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) ( m_cr_object_4_ok ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    " do not expect read of metadata because activation failed
    configure_no_call_read_metadat( ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    "expect 3 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_1 ).
    config_update_obj_after_failed( m_cts_hot_object_2 ).
    config_update_obj_after_failed( m_cts_hot_object_4 ).

    "expect commit work on DB double after writing of error objects to DB.
    "1 call for failed object in attempt 1 due to activation error and 1 call for failed objects due to not decreasing list
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) ( m_hot_object_1 ) ( m_hot_object_4 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_not_initial( ls_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) ( m_log_message_object_4_ok_rtry ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

*    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_deploy_result-activation_results ) ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      lv_ok_objects_only = abap_false.
    ELSE.
      lv_ok_objects_only = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = lv_ok_objects_only
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) ( m_log_message_object_4_ok_rtry ) )
                                                    objects_with_last_action = VALUE #( ) "do not expect object 2 here because it had already last action in first attempt
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 3
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = 'SCTS_HOT'
                                                    hana_error_code = '618'
                                                    hana_error_msg = ''
                                                    log_messages = VALUE #( )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_1 ) ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 3 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_2_ok_1_err_0.
    me->act_3_obj_2_ok_1_err( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_3_obj_2_ok_1_err_1.
    me->act_3_obj_2_ok_1_err( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_3_obj_2_ok_1_err_2.
    me->act_3_obj_2_ok_1_err( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_4_obj_2_ok_1_err_1_ni.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_ok ) ( m_cr_object_2_error ) ) "no check result for object 3!
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      "2. activation attempt - in mode all, OK objects and objects without info from 1. attempt
      "order of i_objects_to_activate must be obj1-obj4-obj3because of standard table and obj3 has no info result and thus is added to retry list as last object
      DATA(lr_activate_objects_res_2) = configure_activate(
                                          i_objects_to_activate = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_ok ) ( m_cr_finished_activation ) ) ).
    ELSE.
      "2. activation attempt - only OK object 1 and 4 from 1. attempt
      lr_activate_objects_res_2 = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_4 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_error ) )
                                          i_error_code           = '40136'
                                          i_error_msg            = 'Some errors during activation. See CheckResults.' ).

      IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok.
        "3. activation attempt - OK and error objects from 2. attempt and object without result from 1. attempt (object 3)
        DATA(lr_activate_objects_res_3) = configure_activate(
                                            i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                            i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_ok ) ( m_cr_finished_activation ) ) ).
      ELSEIF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec.
        "3. activation attempt - only OK object 1 from 2. attempt, but only in recursive mode
        DATA(lr_activate_objects_res_3_rec) = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_finished_activation ) ) ).

        "4. activation attempt - object without info from 1. attempt and error object from 2. attempt
        DATA(lr_activate_objects_res_4) = configure_activate(
                                          i_objects_to_activate  = VALUE #( ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                          i_check_results_to_return = VALUE #( ( m_cr_object_4_ok ) ( m_cr_finished_activation ) ) ).
      ENDIF.
    ENDIF.

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res_1) = configure_read_metadata( m_nhi_object_1 ).
    DATA(lr_read_metadata_res_3) = configure_read_metadata( i_nhi_object = m_nhi_object_3
                                                            i_version_id = '456'
                                                            i_activated_at = '2016-01-01 07:59:41.5430000'
                                                            i_activated_by = 'somebody' ).
    DATA(lr_read_metadata_res_4) = configure_read_metadata( i_nhi_object = m_nhi_object_4
                                                            i_version_id = '789'
                                                            i_activated_at = '2016-01-04 13:59:41.5430000'
                                                            i_activated_by = 'somebody else' ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).
    config_read_hot_status_for_obj( m_cts_hot_object_3 ).
    config_read_hot_status_for_obj( m_cts_hot_object_4 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res_1 ).
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_3
                                    i_hana_metadata = lr_read_metadata_res_3 ).
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_4
                                    i_hana_metadata = lr_read_metadata_res_4 ).

    "expect 1 call to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_2 ).

    "configure expected call to commit work
    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec.
      cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 3 ).
    ELSE.
      cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    ENDIF.
    m_db_access_double->commit_work( ).


* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_3->hana_package_id
                                                object_name = m_hot_object_3->hana_object_name
                                                object_suffix = m_hot_object_3->hana_object_suffix
                                                hot_object = m_hot_object_3
                                                nhi_object = m_nhi_object_3 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_not_initial( ls_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 4
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_4_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

    IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all.
      "In mode all we expect only 2 calls and then activation is done.
      cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
      cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                      activation_counter = 2
                                                      nr_of_attempted_objects = 3
                                                      activation_success = abap_true
                                                      ok_objects_only = abap_false
                                                      hana_activation_id = '12345'
                                                      hana_error_code = '0'
                                                      hana_error_msg = 'No error'
                                                      log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_object_4_ok ) ( m_log_message_act_finished ) )
                                                      objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                                      failed_objects = VALUE #( ) )
                                          act = ls_deploy_result-activation_results[ 2 ] ).
    ELSE.
      "in mode deploy ok objects (also in ok_rec) we expect as 2nd call all OK objects from 1 call
      cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                      activation_counter = 2
                                                      nr_of_attempted_objects = 2
                                                      activation_success = abap_false
                                                      ok_objects_only = abap_true
                                                      hana_activation_id = '12345'
                                                      hana_error_code = '40136'
                                                      hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                      log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_4_error ) )
                                                      objects_with_last_action = VALUE #( )
                                                      failed_objects = VALUE #( ( m_hot_object_4 ) ) )
                                          act = ls_deploy_result-activation_results[ 2 ] ).

      IF i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok.
        cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_deploy_result-activation_results ) ).
        cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                        activation_counter = 3
                                                        nr_of_attempted_objects = 3
                                                        activation_success = abap_true
                                                        ok_objects_only = abap_false
                                                        hana_activation_id = '12345'
                                                        hana_error_code = '0'
                                                        hana_error_msg = 'No error'
                                                        log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_object_4_ok ) ( m_log_message_act_finished ) )
                                                        objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                                        failed_objects = VALUE #( ) )
                                            act = ls_deploy_result-activation_results[ 3 ] ).
      ELSE.
        "recursive OK mode expects as 3rd call the OK object from 2nd call and the rest (object 3 and 4) in 4 th call.
        cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( ls_deploy_result-activation_results ) ).
        cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                        activation_counter = 3
                                                        nr_of_attempted_objects = 1
                                                        activation_success = abap_true
                                                        ok_objects_only = abap_true
                                                        hana_activation_id = '12345'
                                                        hana_error_code = '0'
                                                        hana_error_msg = 'No error'
                                                        log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                        objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                        failed_objects = VALUE #( ) )
                                            act = ls_deploy_result-activation_results[ 3 ] ).
        cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                        activation_counter = 4
                                                        nr_of_attempted_objects = 2
                                                        activation_success = abap_true
                                                        ok_objects_only = abap_false
                                                        hana_activation_id = '12345'
                                                        hana_error_code = '0'
                                                        hana_error_msg = 'No error'
                                                        log_messages = VALUE #( ( m_log_message_object_4_ok ) ( m_log_message_act_finished ) )
                                                        objects_with_last_action = VALUE #( ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                                        failed_objects = VALUE #( ) )
                                            act = ls_deploy_result-activation_results[ 4 ] ).
      ENDIF.
    ENDIF.

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_4_obj_2_ok_1_err_1_ni_0.
    me->act_4_obj_2_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_4_obj_2_ok_1_err_1_ni_1.
    me->act_4_obj_2_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_4_obj_2_ok_1_err_1_ni_2.
    me->act_4_obj_2_ok_1_err_1_ni( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_4_obj_2_ok_1_err_1_ni_1_v2.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    "1. activation attempt
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_ok ) ( m_cr_object_2_error ) ) "no check result for object 3!
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "2. activation attempt - only OK object 1 and 4 from 1. attempt
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "3. activation attempt - only OK object 1 from 2. attempt, but this time failing but no check result...
    DATA(lr_activate_objects_res_3_rec) = configure_activate(
                                      i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                      i_check_results_to_return = VALUE #( ( m_cr_object_1_error ) )
                                      i_error_code           = '40136'
                                      i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "4. activation attempt - object without info from 1. attempt and error object from 2. and 3. attempt
    DATA(lr_activate_objects_res_4) = configure_activate(
                                      i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_3 ) ( m_nhi_object_4 ) )
                                      i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_4_ok ) ( m_cr_finished_activation ) ) ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res_1) = configure_read_metadata( m_nhi_object_1 ).
    DATA(lr_read_metadata_res_3) = configure_read_metadata( i_nhi_object = m_nhi_object_3
                                                            i_version_id = '456'
                                                            i_activated_at = '2016-01-01 07:59:41.5430000'
                                                            i_activated_by = 'somebody' ).
    DATA(lr_read_metadata_res_4) = configure_read_metadata( i_nhi_object = m_nhi_object_4
                                                            i_version_id = '789'
                                                            i_activated_at = '2016-01-04 13:59:41.5430000'
                                                            i_activated_by = 'somebody else' ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).
    config_read_hot_status_for_obj( m_cts_hot_object_3 ).
    config_read_hot_status_for_obj( m_cts_hot_object_4 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res_1 ).
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_3
                                    i_hana_metadata = lr_read_metadata_res_3 ).
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_4
                                    i_hana_metadata = lr_read_metadata_res_4 ).

    "expect 1 call to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_2 ).

    "configure expected call to commit work
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_3->hana_package_id
                                                object_name = m_hot_object_3->hana_object_name
                                                object_suffix = m_hot_object_3->hana_object_suffix
                                                hot_object = m_hot_object_3
                                                nhi_object = m_nhi_object_3 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_not_initial( ls_deploy_result-activation_results ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 4
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_4_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).

    "we expect as 2nd call all OK objects from 1. call
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_true
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_4_error ) )
                                                    objects_with_last_action = VALUE #( )
                                                    failed_objects = VALUE #( ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

    "recursive OK mode expects as 3rd call the OK object from 2nd call and the rest (object 3 and 4) in 4th call.
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 3
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_true
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_error ) )
                                                    objects_with_last_action = VALUE #(  )
                                                    failed_objects = VALUE #( ( m_hot_object_1 ) ) )
                                        act = ls_deploy_result-activation_results[ 3 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 4
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_true
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '0'
                                                    hana_error_msg = 'No error'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_object_4_ok ) ( m_log_message_act_finished ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ) )
                                        act = ls_deploy_result-activation_results[ 4 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_1att.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_3 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    "expect 3 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_1 ).
    config_update_obj_after_failed( m_cts_hot_object_2 ).
    config_update_obj_after_failed( m_cts_hot_object_3 ).

    "expect commit work on DB double after writing of error objects to DB.
    "1 call for failed object due to activation error and 1 call for failed objects due to max attempts reached
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 2 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_3->hana_package_id
                                                object_name = m_hot_object_3->hana_object_name
                                                object_suffix = m_hot_object_3->hana_object_suffix
                                                hot_object = m_hot_object_3
                                                nhi_object = m_nhi_object_3 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode
        i_max_nr_activation_attempts = 1
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) ( m_hot_object_1 ) ( m_hot_object_3 ) )
                                        act = lt_failed_objects ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = 'SCTS_HOT'
                                                    hana_error_code = '617'
                                                    hana_error_msg = ''
                                                    log_messages = VALUE #( )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_1 ) ( m_hot_object_3 ) ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_1att_0.
    me->act_3_obj_1ok_1err_1ni_1att( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_1att_1.
    me->act_3_obj_1ok_1err_1ni_1att( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_1att_2.
    me->act_3_obj_1ok_1err_1ni_1att( if_cts_hot_db_access=>co_hot_activation_mode_all ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_2att.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "2. activation attempt - only OK object 1 from 1. attempt, object 1 is OK
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_finished_activation ) ) ).

    "configuration of object test double for expected call read_metadata and create_read_obj_metadata_req
    DATA(lr_read_metadata_res_1) = configure_read_metadata( m_nhi_object_1 ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    config_read_hot_status_for_obj( m_cts_hot_object_1 ).

    "configuration of DB test double for expected call update_object_after_succes_dep
    config_update_obj_after_succes( i_cts_hot_object = m_cts_hot_object_1
                                    i_hana_metadata = lr_read_metadata_res_1 ).

    "object 2 and 4 with HOT_STATUS = 'E'
    config_update_obj_after_failed( m_cts_hot_object_2 ).
    config_update_obj_after_failed( m_cts_hot_object_4 ).

    "expect commit work on DB double after writing of error objects to DB.
    "1 call for failed object of attempt 1, 1 call for OK object of attempt 2 and 1 call for failed object due to max attempts reached
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 3 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = i_activation_mode "this test is only for _OK and _OK_REC!!!
        i_max_nr_activation_attempts = 2
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_1 ) )
                                        act = lt_successful_objects ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) ( m_hot_object_4 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                   activation_counter = 2
                                                   nr_of_attempted_objects = 1
                                                   activation_success = abap_true
                                                   ok_objects_only = abap_true
                                                   hana_activation_id = '12345'
                                                   hana_error_code = '0'
                                                   hana_error_msg = 'No error'
                                                   log_messages = VALUE #( ( m_log_message_object_1_ok ) ( m_log_message_act_finished ) )
                                                   objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                   failed_objects = VALUE #( ) )
                                       act = ls_deploy_result-activation_results[ 2 ] ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 3
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = 'SCTS_HOT'
                                                    hana_error_code = '617'
                                                    objects_with_last_action = VALUE #( ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 3 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_2att_0.
    me->act_3_obj_1ok_1err_1ni_2att( if_cts_hot_db_access=>co_hot_activation_mode_ok ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_2att_1.
    me->act_3_obj_1ok_1err_1ni_2att( if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
  ENDMETHOD.


  METHOD act_3_obj_1ok_1err_1ni_2att_2.
    DATA: ls_deploy_result      TYPE cl_cts_hot_hana_connector=>ty_deploy_result,
          lt_failed_objects     TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects,
          lt_successful_objects TYPE cl_cts_hot_hana_connector=>ty_cts_hot_objects.

* 0. Configure NHI_OBJECT_API double to return expected values for expected inputs
    "configuration of object test double for expected call activate and create_activate_objects_request
    DATA(lr_activate_objects_res_1) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_2 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_1_ok ) ( m_cr_object_2_error ) )
                                        i_error_code           = '40136'
                                        i_error_msg            = 'Some errors during activation. See CheckResults.' ).

    "2. activation attempt - in mode all, OK object 1 and object without info from 1. attempt but this time object 1 is failing to force 3rd attempt
    DATA(lr_activate_objects_res_2) = configure_activate(
                                        i_objects_to_activate  = VALUE #( ( m_nhi_object_1 ) ( m_nhi_object_4 ) )
                                        i_check_results_to_return = VALUE #( ( m_cr_object_4_error ) )
                                        i_error_code           = '40137'
                                        i_error_msg            = 'Other errors during activation. See CheckResults.' ).

    configure_no_call_read_metadat( ).

* 1. Configure HOT_DB_ACCESS_DOUBLE double to return expected values for expected inputs
    "expect 3 calls to update_obj_after_failed_dep for changing hot_status to 'E' due to activation failure
    config_update_obj_after_failed( m_cts_hot_object_1 ).
    config_update_obj_after_failed( m_cts_hot_object_2 ).
    config_update_obj_after_failed( m_cts_hot_object_4 ).

    "expect commit work on DB double after writing of error objects to DB.
    "1 call for failed object of attempt 1, 1 call for failed object of attempt 2 and 1 call for failed object due to max attempts reached
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_times( 3 ).
    m_db_access_double->commit_work( ).

* 2. actual/business method call
    m_cut->activate_objects(
      EXPORTING
        i_objects_to_be_activated = VALUE #( ( package_id = m_hot_object_1->hana_package_id
                                                object_name = m_hot_object_1->hana_object_name
                                                object_suffix = m_hot_object_1->hana_object_suffix
                                                hot_object = m_hot_object_1
                                                nhi_object = m_nhi_object_1 )
                                              ( package_id = m_hot_object_2->hana_package_id
                                                object_name = m_hot_object_2->hana_object_name
                                                object_suffix = m_hot_object_2->hana_object_suffix
                                                hot_object = m_hot_object_2
                                                nhi_object = m_nhi_object_2 )
                                              ( package_id = m_hot_object_4->hana_package_id
                                                object_name = m_hot_object_4->hana_object_name
                                                object_suffix = m_hot_object_4->hana_object_suffix
                                                hot_object = m_hot_object_4
                                                nhi_object = m_nhi_object_4 ) )
        i_object_status_versions = m_object_status_versions
        i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_all
        i_max_nr_activation_attempts = 2
      CHANGING
        c_deploy_result            = ls_deploy_result
        c_failed_objects           = lt_failed_objects
        c_successful_objects       = lt_successful_objects
    ).

* 3. verify exporting parameter
    cl_abap_unit_assert=>assert_initial( lt_successful_objects ).
*    SORT lt_failed_objects BY table_line->hana_package_id table_line->hana_object_name.
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_cts_hot_objects( ( m_hot_object_2 ) ( m_hot_object_4 ) ( m_hot_object_1 ) )
                                        act = lt_failed_objects ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_deploy_result-activation_results ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 1
                                                    nr_of_attempted_objects = 3
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40136'
                                                    hana_error_msg = 'Some errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_1_ok_rtry ) ( m_log_message_object_2_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_2 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_2 ) ) )
                                        act = ls_deploy_result-activation_results[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 2
                                                    nr_of_attempted_objects = 2
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = '12345'
                                                    hana_error_code = '40137'
                                                    hana_error_msg = 'Other errors during activation. See CheckResults.'
                                                    log_messages = VALUE #( ( m_log_message_object_4_error ) )
                                                    objects_with_last_action = VALUE #( ( m_hot_object_4 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_4 ) ) )
                                        act = ls_deploy_result-activation_results[ 2 ] ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE cl_cts_hot_hana_connector=>ty_activation_result(
                                                    activation_counter = 3
                                                    nr_of_attempted_objects = 1
                                                    activation_success = abap_false
                                                    ok_objects_only = abap_false
                                                    hana_activation_id = 'SCTS_HOT'
                                                    hana_error_code = '617'
                                                    objects_with_last_action = VALUE #( ( m_hot_object_1 ) )
                                                    failed_objects = VALUE #( ( m_hot_object_1 ) ) )
                                        act = ls_deploy_result-activation_results[ 3 ] ).

* 4. verify interactions on testdoubles
    cl_abap_testdouble=>verify_expectations( m_object_api_double ).
    cl_abap_testdouble=>verify_expectations( m_db_access_double ).
  ENDMETHOD.


  METHOD configure_activate.
    "configuration of object test double for expected call create_activate_objects_req
    DATA(lr_activate_objects_req) = cl_nhi_activate_objects_req=>create_activate_objects_req(
                                   activationmode = ce_nhi_activation_mode=>activation_casc_2_phase
                                   objlist        = i_objects_to_activate
                                   session        = cl_nhi_inactive_session=>create_inactive_session( owner = m_cut->m_nhi_api_user workspace = '' )
                                   version        = cl_nhi_inactive_version=>create_inactive_version( owner = m_cut->m_nhi_api_user workspace = '' )
                                   act_with_hints = abap_false ).
*  cl_abap_testdouble=>configure_call( m_object_api_double )->returning( lr_activate_objects_req )->ignore_parameter( 'SESSION' )->ignore_parameter( 'VERSION' )->and_expect( )->is_called_times( i_times ).
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( lr_activate_objects_req )->set_matcher( NEW lcl_nhi_api_matcher( ) )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->create_activate_objects_req(
                                   activationmode = ce_nhi_activation_mode=>activation_casc_2_phase
                                   objlist        = i_objects_to_activate
                                   session        = cl_nhi_inactive_session=>create_inactive_session( owner = m_cut->m_nhi_api_user workspace = '' )
                                   version        = cl_nhi_inactive_version=>create_inactive_version( owner = m_cut->m_nhi_api_user workspace = '' )
                                   act_with_hints = abap_false ).

    r_result = cl_nhi_activate_objects_res=>create_activate_objects_res(
                                   activationid = '12345'
                                   checkresults = i_check_results_to_return
                                   error_code   = i_error_code
                                   error_msg    = i_error_msg
                                   error_arg    = i_error_arg
                               ).
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( r_result )->and_expect( )->is_called_times( i_times ).
    m_object_api_double->activate( lr_activate_objects_req ).
  ENDMETHOD.


  METHOD configure_read_metadata.
    "configuration of object test double for expected call create_read_obj_metadata_req
    DATA(lr_read_obj_metadata_req) = cl_nhi_read_obj_metadata_req=>create_read_obj_metadata_req(
                                    object        = i_nhi_object
                                    session       = cl_nhi_active_session=>create_active_session(  ) ).
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( lr_read_obj_metadata_req )->set_matcher( NEW lcl_nhi_api_matcher( ) )->and_expect( )->is_called_once( ).
    m_object_api_double->create_read_obj_metadata_req(
                                    object        = i_nhi_object
                                    session       = cl_nhi_active_session=>create_active_session(  ) ).

    "configuration of object test double for expected call read_metadata
    r_result = cl_nhi_read_obj_metadata_res=>create_read_obj_metadata_res(
                                 error_code = VALUE #( )
                                 error_msg  = 'No error'
                                 error_arg  = VALUE #( )
                                 version    = VALUE #( )
                                 metadata   = cl_nhi_metadata_active_ver=>create_metadata(
                                                  version_id   = i_version_id
                                                  activated_at = i_activated_at
                                                  activated_by = i_activated_by
                                                  edit         = abap_false
                                              )
                                 caption    = VALUE #( )
                                 cdata      = VALUE #( )
                                 bdata      = VALUE #( )
                             ).
    cl_abap_testdouble=>configure_call( m_object_api_double )->returning( r_result )->and_expect( )->is_called_once( ).
    m_object_api_double->read_metadata( lr_read_obj_metadata_req ).
  ENDMETHOD.


  METHOD config_read_cts_hot_object.
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( i_cts_hot_object )->and_expect( )->is_called_once( ).
    m_db_access_double->read_cts_hot_object( i_abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                             i_abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                             i_abap_status = 'A' ).
  ENDMETHOD.


  METHOD configure_no_call_to_db_access.
    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->read_cts_hot_object( i_abap_hana_package_id = 'a'
                                             i_abap_hana_object_name_suffix = 'b' ).

    cl_abap_testdouble=>configure_call( m_db_access_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_db_access_double->modify_cts_hot_object( VALUE #( ) ).
  ENDMETHOD.


  METHOD configure_no_call_read_metadat.
    cl_abap_testdouble=>configure_call( m_object_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_object_api_double->create_read_obj_metadata_req(
                                    object        = VALUE #( )
                                    session       = VALUE #( ) ).

    "configuration of object test double for expected call read_metadata
    cl_abap_testdouble=>configure_call( m_object_api_double )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    m_object_api_double->read_metadata( VALUE #( ) ).
  ENDMETHOD.


  METHOD config_update_obj_after_succes.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_after_succes_dep( i_old_object = VALUE #( abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                                                                abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                                                                abap_status = i_cts_hot_object-abap_status
                                                                                hot_status = i_cts_hot_object-hot_status
                                                                                hana_source_object_version = i_cts_hot_object-hana_source_object_version )
                                                        i_new_deployed_at = m_timestamp_provider_double->gv_timestamp_to_return
                                                        i_new_deployed_by = sy-uname
                                                        i_new_hana_activated_at = m_cut->conv_hana_actvted_at_to_timest( CAST cl_nhi_metadata_active_ver( i_hana_metadata->metadata )->activated_at )
                                                        i_new_hana_activated_by = CAST cl_nhi_metadata_active_ver( i_hana_metadata->metadata )->activated_by
                                                        i_new_hana_object_version = CONV #( i_hana_metadata->metadata->version_id ) ).
  ENDMETHOD.


  METHOD config_update_obj_after_failed.
    cl_abap_testdouble=>configure_call( m_db_access_double )->and_expect( )->is_called_once( ).
    m_db_access_double->update_object_after_failed_dep( i_old_object = VALUE #( abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                                                                abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                                                                abap_status = i_cts_hot_object-abap_status
                                                                                hot_status = i_cts_hot_object-hot_status
                                                                                hana_source_object_version = i_cts_hot_object-hana_source_object_version )
                                                        i_new_status = i_new_status ).
  ENDMETHOD.


  METHOD config_read_hot_status_for_obj.
    cl_abap_testdouble=>configure_call( m_db_access_double )->returning( i_hot_status )->and_expect( )->is_called_once( ).
    m_db_access_double->read_hot_status_for_object( i_abap_hana_package_id = i_cts_hot_object-abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = i_cts_hot_object-abap_hana_object_name_suffix
                                                    i_abap_status = i_cts_hot_object-abap_status ).
  ENDMETHOD.

ENDCLASS.

"! Test class for doing integration testing with HANA. So test can only run on ABAP on HANA systems
CLASS ltcl_cts_hot_hana_integration DEFINITION FINAL FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS:
      co_test_package_id     TYPE string VALUE 'system-local.private.hot-aunit',
      co_test_package_id_sid TYPE string VALUE 'system-local.private.hot-aunit.sid',
      co_test_attview_name   TYPE string VALUE 'HOT_AUNIT_INTEGRATION_VIEW',
      co_test_attview_suffix TYPE string VALUE 'attributeview',
      co_test_package_id2    TYPE string VALUE 'tmp.hta.aunit.list', "reuse root package folder of other aunit tests outside system-local
      co_test_package_id3    TYPE string VALUE 'tmp.hta.aunit.list.1', "reuse root package folder of other aunit tests outside system-local
      co_test_package_id4    TYPE string VALUE 'tmp.hta.aunit.list.2', "reuse root package folder of other aunit tests outside system-local
      co_test_package_id5    TYPE string VALUE 'tmp.hta.aunit.list.2.1'. "reuse root package folder of other aunit tests outside system-local

    CLASS-DATA:
      go_nhi_api TYPE REF TO if_nhi_api.

    CLASS-METHODS:
      "! to check whether we are on HANA or not.
      class_setup.

    DATA:
      m_cut TYPE REF TO cl_cts_hot_hana_connector.

    METHODS:
      setup RAISING cx_static_check,

      "***Helper Methods***
      "! creates or modifies the attributeview in HANA to get up to date timestamp.
      modify_test_view_in_hana RAISING cx_static_check,
      "! creates the cdata for the attribute view if attrbiteu view is created the first time.
      create_attview_cdata RETURNING VALUE(r_result) TYPE string,
      "! creates a structure cts_hot_package with package data for mock db (object to be created/modified in HANA during test)
      create_cts_hot_package RETURNING VALUE(r_cts_hot_package) TYPE cts_hot_package RAISING cx_hana_object_transport,
      "! creates a structure cts_hot_object with object data for mock db (object to be created/modifed in HANA during test)
      create_cts_hot_object RETURNING VALUE(rs_cts_hot_object) TYPE cts_hot_object RAISING cx_hana_object_transport,
      "! Check whether system_local exists and has expected values (not transportable)
      precheck_tests_w_system_local RAISING cx_nhi_hana_repository,
      "! creates HANA package in HANA.<br/>
      "! If i_delete_if_existing = abap_true then package is deleted first if it exists and then created again.<br/>
      "! If i_delete_if_existing = abap_false then nothing is done if package exists.<br/>
      create_package_in_hana IMPORTING i_package_id TYPE string i_delete_if_existing TYPE abap_bool DEFAULT abap_false RAISING cx_nhi_hana_repository,

      "***Test Methods***
      "! Tests whether CL_CTS_HOT_HANA_CONNECTOR did read the correct HANA SID.
      read_hana_sid FOR TESTING RAISING cx_static_check,
      "! Tests whether CL_CTS_HOT_HANA_CONNECTOR did read the correct HANA build_version value.
      read_hana_build_version FOR TESTING RAISING cx_static_check,
      "! Tests whether CL_CTS_HOT_HANA_CONNECTOR did read the correct HANA timezone_offset value.
      read_hana_timezone_offset FOR TESTING RAISING cx_static_check,
      "! check hana activated_at timestamp format that it is in a format that we can handle
      check_hana_timestamp_format FOR TESTING RAISING cx_static_check,
      "! Lists packages and finds only 1 package for 'tmp.hta.aunit.list.1'
      list_packages_1 FOR TESTING RAISING cx_static_check,
      "! Lists packages and finds 3 packages for '*hta.aunit.list*'
      list_packages_3 FOR TESTING RAISING cx_static_check,
      "! Try to lists package for 'system-local'. System-local is not transportable and thus should not be returned
      list_packages_system_local FOR TESTING RAISING cx_static_check,
      "! Try to lists package for 'system-local.private.hot-aunit'. System-local and directories below should be not transportable and thus should not be returned
      list_packages_system_local_sub FOR TESTING RAISING cx_static_check,
      "! Try to read package details for system_local but should not read the data because not transportable.
      read_package_data_system_local FOR TESTING RAISING cx_static_check,
      "! Try to read package details for a package that does not exist
      read_package_data_not_existing FOR TESTING RAISING cx_static_check.

ENDCLASS.

"make friednship with class to access private attributes
CLASS cl_cts_hot_hana_connector DEFINITION LOCAL FRIENDS ltcl_cts_hot_hana_integration.
CLASS ltcl_cts_hot_hana_integration IMPLEMENTATION.

  METHOD class_setup.
    TRY.
        go_nhi_api = cl_nhi_api=>create_instance( ).
      CATCH cx_nhi_not_supported.
        cl_abap_unit_assert=>abort( msg = 'Tests can only be executed on an ABAP on HANA System, but this system is not an ABAP on HANA system' ).
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    m_cut = cl_cts_hot_hana_connector=>create_instance( ).
  ENDMETHOD.

  METHOD read_hana_sid.
    "delete and create test package in system-local and read its SID and compare with SID got during read metadata.
    create_package_in_hana( i_package_id = co_test_package_id_sid i_delete_if_existing = abap_true ).

    DATA(lo_package_api) = go_nhi_api->get_package( ).
    DATA(lo_read_pack_response) = lo_package_api->read( lo_package_api->create_read_package_req( tenant = '' package = co_test_package_id_sid ) ).
    IF lo_read_pack_response->error_code IS NOT INITIAL.
      cl_abap_unit_assert=>fail( |Prepare test failed while reading package '{ co_test_package_id_sid }' from HANA: { lo_read_pack_response->error_msg }| ).
    ENDIF.

    "verify. SID should have been already set in setup method
    IF lo_read_pack_response->src_system = m_cut->g_hana_sid.
      RETURN.
    ELSEIF lo_read_pack_response->src_system = m_cut->g_hana_sid && '@' && m_cut->g_hana_sid.
      "As of HANA 2.0 SP 1 each HANA is MultiTenant and SID in repository is then tenantsid@sid.
      RETURN.
    ELSE.
      cl_abap_unit_assert=>fail( msg = |HANA SID different. Expected: { lo_read_pack_response->src_system } but was { m_cut->g_hana_sid }| ).
    ENDIF.
  ENDMETHOD.

  METHOD read_hana_build_version.
    "read build_version here from HANA to be able to compare values
    CONSTANTS: co_sql_statement TYPE string VALUE 'select value from M_HOST_INFORMATION where key = ''build_version''' ##NO_TEXT.

    DATA:
      lv_build_version TYPE string,
      lt_values        TYPE STANDARD TABLE OF string,
      lv_value         TYPE                   string,
      lo_values        TYPE REF TO            data.

    DATA(lo_database_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
    DATA(lo_sql_statement) = NEW cl_sql_statement( con_ref = lo_database_connection ).
    DATA(lo_sql_result) = lo_sql_statement->execute_query( statement = co_sql_statement ).

    GET REFERENCE OF lt_values INTO lo_values.

    lo_sql_result->set_param_table( lo_values ).
    lo_sql_result->next_package( ).
    lo_sql_result->close( ).

    IF lines( lt_values ) = 1.
      READ TABLE lt_values INDEX 1 INTO lv_build_version.
    ENDIF.

    "verify. timezone_offset should have been already set in setup method
    cl_abap_unit_assert=>assert_equals( msg = 'HANA build_version must not be different.' exp = lv_build_version act = m_cut->g_hana_build_version ).

  ENDMETHOD.

  METHOD read_hana_timezone_offset.
    "read timezone offset here from HANA to be able to compare values
    CONSTANTS: co_sql_statement TYPE string VALUE 'select value from M_HOST_INFORMATION where key = ''timezone_offset''' ##NO_TEXT.

    DATA:
      lv_timezone_offset TYPE i,
      lt_values          TYPE STANDARD TABLE OF string,
      lv_value           TYPE                   string,
      lo_values          TYPE REF TO            data.

    DATA(lo_database_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
    DATA(lo_sql_statement) = NEW cl_sql_statement( con_ref = lo_database_connection ).
    DATA(lo_sql_result) = lo_sql_statement->execute_query( statement = co_sql_statement ).

    GET REFERENCE OF lt_values INTO lo_values.

    lo_sql_result->set_param_table( lo_values ).
    lo_sql_result->next_package( ).
    lo_sql_result->close( ).

    IF lines( lt_values ) = 1.
      READ TABLE lt_values INDEX 1 INTO lv_timezone_offset.
    ENDIF.

    "verify. timezone_offset should have been already set in setup method
    cl_abap_unit_assert=>assert_equals( msg = 'HANA timezone offset must not be different.' exp = lv_timezone_offset act = m_cut->g_hana_timezone_offset ).
  ENDMETHOD.

  METHOD check_hana_timestamp_format.
    modify_test_view_in_hana( ).

    "read test object to get activated_at timestamp to check format.
    DATA(lo_object_api) = go_nhi_api->get_object( ).
    DATA(lo_nhi_object_id) = cl_nhi_object_id=>create_object_id(
                                tenant = ''
                                package = co_test_package_id
                                name = co_test_attview_name
                                suffix = co_test_attview_suffix
                                version = cl_nhi_active_version=>create_active_version( )
                                metadata = cl_nhi_metadata_active_ver=>create_metadata(
                                  version_id = ''
                                  activated_at = ''
                                  activated_by = ''
                                  edit = '' )
                                ).
    DATA(lo_resp_object_metadata) = lo_object_api->read_metadata( lo_object_api->create_read_obj_metadata_req(
              object        = lo_nhi_object_id
              session       = cl_nhi_active_session=>create_active_session(  )
           ) ).

    IF lo_resp_object_metadata->error_code IS NOT INITIAL.
      cl_abap_unit_assert=>fail( msg = 'Unexpected error_code (' && lo_resp_object_metadata->error_code
                                      && ') when reading test attributeview ' && co_test_attview_name && ' of package '
                                      && co_test_package_id && ': ' && lo_resp_object_metadata->error_msg ).
    ENDIF.

    DATA(lv_activated_at) = CAST cl_nhi_metadata_active_ver( lo_resp_object_metadata->metadata )->activated_at.

    "validate lv_activated_at - current format of HANA is '2014-06-11 16:18:49.9450000'' (june 2014)
    DATA(lo_regex) = NEW cl_abap_regex( pattern = '2[0-9]{3}-[01][0-9]-[0123][0-9]\s[012][0-9]:[0-5][0-9]:[0-5][0-9]\.[0-9]{3,7}' ).
    IF lo_regex->create_matcher( text = lv_activated_at ) IS INITIAL.
      cl_abap_unit_assert=>fail( 'Activated_at format not as expected (Did HANA change format?): ' && lv_activated_at ).
    ENDIF.
  ENDMETHOD.

  METHOD modify_test_view_in_hana.
    "use productive implementation to create the test attribute view in HANA.
    DATA: lo_db_access_double TYPE REF TO if_cts_hot_db_access,
          ls_cts_hot_package  TYPE cts_hot_package,
          ls_cts_hot_object   TYPE cts_hot_object.

    "create mock DB with test data and inject to m_cut
    lo_db_access_double ?= cl_abap_testdouble=>create( 'if_cts_hot_db_access' ).
    m_cut->m_cts_hot_db_access = lo_db_access_double.

    "configure test package in mock db
    ls_cts_hot_package = create_cts_hot_package( ).
    cl_abap_testdouble=>configure_call( lo_db_access_double )->returning( ls_cts_hot_package )->and_expect( )->is_called_once( ).
    lo_db_access_double->read_cts_hot_package( i_abap_hana_package_id = ls_cts_hot_package-abap_hana_package_id
                                               i_abap_status = 'A' ).

    "configure test object in mock db
    ls_cts_hot_object = create_cts_hot_object( ).
    cl_abap_testdouble=>configure_call( lo_db_access_double )->returning( ls_cts_hot_object )->and_expect( )->is_called_times( 2 ).
    lo_db_access_double->read_cts_hot_object( i_abap_hana_package_id = ls_cts_hot_object-abap_hana_package_id
                                              i_abap_hana_object_name_suffix = ls_cts_hot_object-abap_hana_object_name_suffix
                                              i_abap_status = 'A' ).


    "create test objetc in HANA and activate it
    "create/modify test package in HANA
    m_cut->modify_packages_in_hana( VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list(
              ( cl_cts_hot_package=>create_instance( co_test_package_id ) )
            ) ).

    "create/modify test object in HANA
    m_cut->deploy_objects_to_hana( i_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list(
              ( cl_cts_hot_object_v1=>create_instance(
                        iv_hana_package_id = co_test_package_id
                        iv_hana_object_name = co_test_attview_name
                        iv_hana_object_suffix = co_test_attview_suffix
              ) ) )
              i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
              i_max_nr_activation_attempts = 10
              i_activate_with_hints        = abap_true ).
  ENDMETHOD.

  METHOD create_cts_hot_package.
    DATA(l_cl_cts_package) = cl_cts_hot_package=>create_instance( co_test_package_id ).

    r_cts_hot_package-abap_hana_package_id = l_cl_cts_package->abap_hana_package_id.
    r_cts_hot_package-abap_status = 'A'.
    r_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    r_cts_hot_package-hana_package_id = l_cl_cts_package->hana_package_id.
    r_cts_hot_package-hana_pack_src_system = 'SRC'.
    r_cts_hot_package-hana_pack_src_tenant = ''.
    r_cts_hot_package-hana_pack_description = 'test package for a unit test of hot'.
    r_cts_hot_package-hana_pack_responsible = 'hot development'.
    r_cts_hot_package-hana_pack_orig_lang = 'DE'.
    r_cts_hot_package-hana_pack_is_structural = 0.
    r_cts_hot_package-hana_pack_delivery_unit = ''.
    r_cts_hot_package-hana_pack_du_vendor = ''.
    r_cts_hot_package-hana_pack_text_collection = ''.
    r_cts_hot_package-hana_pack_text_status = ''.
    r_cts_hot_package-hana_pack_text_term_domain = ''.
    r_cts_hot_package-hana_pack_hints_for_transl = ''.
    r_cts_hot_package-hana_read_system = 'HAN'.
    r_cts_hot_package-abap_import_timestamp = 0.
    GET TIME STAMP FIELD r_cts_hot_package-abap_deployed_at.
    r_cts_hot_package-abap_deployed_by = 'abap_user'.
    r_cts_hot_package-abap_sync_system = 'DEV'.
    GET TIME STAMP FIELD r_cts_hot_package-abap_synced_at.
    r_cts_hot_package-abap_synced_by = 'abap_user'.
  ENDMETHOD.

  METHOD create_cts_hot_object.
    DATA(l_cl_cts_object) = cl_cts_hot_object_v1=>create_instance(
                                  iv_hana_package_id = co_test_package_id
                                  iv_hana_object_name = co_test_attview_name
                                  iv_hana_object_suffix = co_test_attview_suffix ).

    rs_cts_hot_object-abap_hana_package_id = l_cl_cts_object->abap_hana_package_id.
    rs_cts_hot_object-abap_hana_object_name_suffix = l_cl_cts_object->abap_hana_object_name_suffix.
    rs_cts_hot_object-abap_status = 'A'.
    rs_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    rs_cts_hot_object-hana_package_id = co_test_package_id.
    rs_cts_hot_object-hana_object_name = co_test_attview_name.
    rs_cts_hot_object-hana_object_suffix = co_test_attview_suffix.
    rs_cts_hot_object-abap_sync_system = 'DEV'.
    rs_cts_hot_object-hana_read_system = 'HAN'.
    rs_cts_hot_object-hana_source_build_version = '1.00.12.00.123456'.
    rs_cts_hot_object-hana_source_object_version = ''.
    rs_cts_hot_object-hana_object_version = ''.
    rs_cts_hot_object-abap_import_timestamp = 0.
    GET TIME STAMP FIELD rs_cts_hot_object-hana_activated_at.
    rs_cts_hot_object-hana_activated_by = 'HANA User'.
    rs_cts_hot_object-hana_content_bdata = 'bdata'.
    rs_cts_hot_object-hana_content_cdata = create_attview_cdata( ).
    GET TIME STAMP FIELD rs_cts_hot_object-abap_synced_at.
    rs_cts_hot_object-abap_synced_by = 'abap user'.
  ENDMETHOD.

  METHOD create_attview_cdata.
    r_result = '<?xml version="1.0" encoding="UTF-8"?>' &&
                  '<Dimension:dimension xmlns:Dimension="http://www.sap.com/ndb/BiModelDimension.ecore" schemaVersion="1.2" id="' && co_test_attview_name && '" applyPrivilegeType="ANALYTIC_PRIVILEGE" checkAnalyticPrivileges="true"' &&
                          ' defaultClient="$$client$$" defaultLanguage="$$language$$" visibility="internal" dimensionType="Standard">' &&
                  '  <origin/>' &&
                  '  <descriptions defaultDescription="mein text"/>' &&
                  '  <attributes>' &&
                  '    <attribute id="DELIVERY_UNIT" order="1" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="DELIVERY_UNIT"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="DELIVERY_UNIT"/>' &&
                  '    </attribute>' &&
                  '    <attribute id="VERSION" order="3" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="VERSION"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="VERSION"/>' &&
                  '    </attribute>' &&
                  '    <attribute id="VENDOR" order="4" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="VENDOR"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="VENDOR"/>' &&
                  '    </attribute>' &&
                  '  </attributes>' &&
                  '  <calculatedAttributes/>' &&
                  '  <privateDataFoundation>' &&
                  '    <tableProxies>' &&
                  '      <tableProxy>' &&
                  '        <table schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS"/>' &&
                  '      </tableProxy>' &&
                  '    </tableProxies>' &&
                  '    <joins/>' &&
                  '    <layout>' &&
                  '      <shapes>' &&
                  '        <shape modelObjectName="DELIVERY_UNITS" modelObjectNameSpace="_SYS_REPO" modelObjectType="catalog">' &&
                  '          <upperLeftCorner x="70" y="30"/>' &&
                  '        </shape>' &&
                  '        <shape modelObjectName="DataFoundation" modelObjectNameSpace="DataFoundation" modelObjectType="repository">' &&
                  '          <upperLeftCorner x="40" y="85"/>' &&
                  '          <rectangleSize height="0" width="0"/>' &&
                  '        </shape>' &&
                  '      </shapes>' &&
                  '    </layout>' &&
                  '  </privateDataFoundation>' &&
                  '  <hierarchies/>' &&
                  '</Dimension:dimension>'.
  ENDMETHOD.

  METHOD list_packages_1.
    create_package_in_hana( co_test_package_id2 ).

    DATA(lt_packages) = m_cut->if_cts_hot_hana_conn_internal~list_hana_packages( co_test_package_id2 ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = co_test_package_id2 ).
  ENDMETHOD.

  METHOD list_packages_3.
    create_package_in_hana( co_test_package_id3 ).
    create_package_in_hana( co_test_package_id5 ).

    DATA(lt_packages) = m_cut->if_cts_hot_hana_conn_internal~list_hana_packages( '*hta.aunit.list*' ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 4 ).
    cl_abap_unit_assert=>assert_table_contains( line = co_test_package_id2 table = lt_packages ).
    cl_abap_unit_assert=>assert_table_contains( line = co_test_package_id3 table = lt_packages ).
    cl_abap_unit_assert=>assert_table_contains( line = co_test_package_id4 table = lt_packages ).
    cl_abap_unit_assert=>assert_table_contains( line = co_test_package_id5 table = lt_packages ).
  ENDMETHOD.

  METHOD list_packages_system_local.
    DATA lt_package_names    TYPE if_cts_hot_hana_conn_internal=>ty_hana_package_names.

    precheck_tests_w_system_local( ).
    lt_package_names = m_cut->if_cts_hot_hana_conn_internal~list_hana_packages( 'system-local' ).

    cl_abap_unit_assert=>assert_initial( act = lt_package_names msg = 'system-local must not be returned because it should be ignored because it is not transportable' ).
  ENDMETHOD.

  METHOD list_packages_system_local_sub.
    DATA: lt_package_names    TYPE if_cts_hot_hana_conn_internal=>ty_hana_package_names,
          lr_read_package_res TYPE REF TO cl_nhi_read_package_res.

    "precheck whether test can be executed by using NHI api directly.
    create_package_in_hana( co_test_package_id ).

    lr_read_package_res = m_cut->m_nhi_package_api->read( m_cut->m_nhi_package_api->create_read_package_req(
                                    tenant        = ''
                                    package       = co_test_package_id
                                ) ).
    IF lr_read_package_res->error_code IS NOT INITIAL.
      cl_abap_unit_assert=>abort( msg = |Test precheck: Reading package { co_test_package_id } via HANA API failed with { lr_read_package_res->error_code }: { lr_read_package_res->error_msg } . Skip test.|
                                  quit = if_aunit_constants=>method ).
    ENDIF.

    IF lr_read_package_res->transportable = abap_true.
      cl_abap_unit_assert=>abort( msg = |Test precheck: Package { co_test_package_id } is marked as transportable, which is not expected. Skip test.|
                                  quit = if_aunit_constants=>method ).
    ENDIF.

    lt_package_names = m_cut->if_cts_hot_hana_conn_internal~list_hana_packages( co_test_package_id ).

    cl_abap_unit_assert=>assert_initial( act = lt_package_names msg = |{ co_test_package_id } must not be returned because it should be ignored because it is not transportable| ).
  ENDMETHOD.


  METHOD create_package_in_hana.
    DATA(lr_exists_package_res) = m_cut->m_nhi_package_api->exists( m_cut->m_nhi_package_api->create_exists_package_req(
                                        tenant      = ''
                                        package     = i_package_id
                                    ) ).
    IF lr_exists_package_res->error_code IS NOT INITIAL.
    ENDIF.

    IF lr_exists_package_res->exists = abap_true AND i_delete_if_existing = abap_true.
      DATA(lr_delete_package_res) = m_cut->m_nhi_package_api->delete( m_cut->m_nhi_package_api->create_delete_package_req(
                                    tenant                  = ''
                                    package                 = i_package_id
                                ) ).
      IF lr_delete_package_res->error_code <> '0'.
        cl_abap_unit_assert=>abort( msg = |Test precheck: Delete package { i_package_id } failed with { lr_delete_package_res->error_code }: { lr_delete_package_res->error_msg }. Skip test.|
                                    quit = if_aunit_constants=>method ).
      ENDIF.
    ELSEIF lr_exists_package_res->exists = abap_true AND i_delete_if_existing = abap_false.
      RETURN.
    ENDIF.

    DATA(lr_create_package_res) = m_cut->m_nhi_package_api->create( m_cut->m_nhi_package_api->create_create_package_req(
                                    tenant                  = ''
                                    package                 = i_package_id
                                    description             = 'HTA Aunit Tests'
                                    responsible             = 'HTA team'
                                    orig_lang               = 'en_US'
                                    delivery_unit           = ''
                                    du_vendor               = ''
                                    text_collection         = ''
                                    text_status             = ''
                                    text_terminology_domain = ''
                                    hints_for_translation   = ''
                                    texts                   = VALUE #( )
                                ) ).
    IF lr_create_package_res->error_code <> '0'.
      cl_abap_unit_assert=>abort( msg = |Test precheck: Create package { i_package_id } failed with { lr_create_package_res->error_code }: { lr_create_package_res->error_msg }. Skip test.|
                                  quit = if_aunit_constants=>method ).
    ENDIF.
  ENDMETHOD.

  METHOD read_package_data_system_local.
    precheck_tests_w_system_local( ).

    cl_abap_unit_assert=>assert_initial( m_cut->read_package_data_from_hana( 'system-local' ) ).
  ENDMETHOD.

  METHOD read_package_data_not_existing.
    cl_abap_unit_assert=>assert_initial( m_cut->read_package_data_from_hana( 'xyz.this.is.some.package.that.must.not.exist.at.all.xyz' ) ).
  ENDMETHOD.

  METHOD precheck_tests_w_system_local.
    DATA lr_read_package_res TYPE REF TO cl_nhi_read_package_res.

    "precheck whether test can be executed by using NHI api directly.
    lr_read_package_res = m_cut->m_nhi_package_api->read( m_cut->m_nhi_package_api->create_read_package_req(
                                    tenant        = ''
                                    package       = 'system-local'
                                ) ).
    IF lr_read_package_res->error_code = '40132'.
      cl_abap_unit_assert=>abort( msg = 'Test precheck: Package system-local does not exist. Skip test.'
                                  quit = if_aunit_constants=>method ).
    ELSEIF lr_read_package_res->error_code IS NOT INITIAL.
      cl_abap_unit_assert=>abort( msg = 'Test precheck: Error reading package system-local via HANA API. Skip test.'
                                  quit = if_aunit_constants=>method ).
    ENDIF.

    IF lr_read_package_res->transportable = abap_true.
      cl_abap_unit_assert=>abort( msg = 'Test precheck: Package system-local is marked as transportable, which is not expected. Skip test.'
                                  quit = if_aunit_constants=>method ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.