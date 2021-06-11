CLASS ltcx_cts_hta_wrong_status DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      wrong_status_package FOR TESTING RAISING cx_static_check,
      wrong_status_object FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcx_cts_hta_wrong_status IMPLEMENTATION.

  METHOD wrong_status_package.
    DATA: lr_cut     TYPE REF TO cx_cts_hta,
          lr_package TYPE REF TO if_cts_hta_package.

    lr_package = cl_cts_hta_package=>create_instance_from_hana_key( 'package.wrong.status' ).
    lr_cut = NEW cx_cts_hta_wrong_status(
        textid                 = cx_cts_hta_wrong_status=>package_requires_deployment
        name_of_obj_or_package = lr_package->hana_package_name
        hot_status             = if_cts_hot_db_access=>co_hot_status_inactive
        cts_hta_component      = lr_package
    ).

    cl_abap_unit_assert=>assert_equals( act = lr_cut->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>package_requires_deployment  ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_1 exp = 'package.wrong.status' ).
    cl_abap_unit_assert=>assert_initial( lr_cut->message_variable_2 ).
    cl_abap_unit_assert=>assert_initial( lr_cut->message_variable_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_4 exp = if_cts_hot_db_access=>co_hot_status_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->cts_hta_component exp = lr_package ).
  ENDMETHOD.

  METHOD wrong_status_object.
    DATA: lr_cut    TYPE REF TO cx_cts_hta,
          lr_object TYPE REF TO if_cts_hta_object.

    lr_object = cl_cts_hta_api_factory=>create_instance( )->create_object_by_hana_name(
              i_hana_package_name = 'test.com'
              i_hana_object_name = 'OBJECT_NAME_WITH_WRONG_STATUS_AND_SOME_LONG_NAME'
              i_hana_object_suffix = 'LONGER_SUFFIX_SO_THAT_WE_GET_MORE_THAN_100_chars_FOR_TEST' ).
    lr_cut = NEW cx_cts_hta_wrong_status(
        textid                 = cx_cts_hta_wrong_status=>object_requires_deployment
        name_of_obj_or_package = |{ lr_object->object_key-hana_object_name }.{ lr_object->object_key-hana_object_suffix }|
        hot_status             = if_cts_hot_db_access=>co_hot_status_to_be_deleted
        cts_hta_component      = lr_object
    ).

    cl_abap_unit_assert=>assert_equals( act = lr_cut->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>object_requires_deployment ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_1 exp = 'OBJECT_NAME_WITH_WRONG_STATUS_AND_SOME_LONG_NAME.L' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_2 exp = 'ONGER_SUFFIX_SO_THAT_WE_GET_MORE_THAN_100_chars_FO' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_3 exp = 'R_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_4 exp = if_cts_hot_db_access=>co_hot_status_to_be_deleted ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->cts_hta_component exp = lr_object ).
  ENDMETHOD.

ENDCLASS.