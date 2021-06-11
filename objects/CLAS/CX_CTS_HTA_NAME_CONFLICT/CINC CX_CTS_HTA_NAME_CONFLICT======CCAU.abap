CLASS ltcx_cts_hta_name_conflict DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      name_conflict_package FOR TESTING RAISING cx_static_check,
      name_conflict_object FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcx_cts_hta_name_conflict IMPLEMENTATION.

  METHOD name_conflict_package.
    DATA: lr_cut     TYPE REF TO cx_cts_hta,
          lr_package TYPE REF TO if_cts_hta_package.

    lr_package = cl_cts_hta_package=>create_instance_from_hana_key( 'package.name.that.Was.changed.in.hana.after.sync.to.hta' ).
    lr_cut = NEW cx_cts_hta_name_conflict(
        textid                      = cx_cts_hta_name_conflict=>package_name_conflict
        name_of_obj_or_package_conf = 'package.name.that.Was.changed.in.hana.after.sync.to.hta'
        name_of_obj_or_package_hta  = 'package.name.that.was.changed.in.hana.after.sync.to.hta'
        cts_hta_component           = lr_package
    ).

    cl_abap_unit_assert=>assert_equals( act = lr_cut->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>package_name_conflict ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_1 exp = 'package.name.that.Was.changed.in.hana.after.sync.t' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_2 exp = 'o.hta' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_3 exp = 'package.name.that.was.changed.in.hana.after.sync.t' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_4 exp = 'o.hta' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->cts_hta_component exp = lr_package ).
  ENDMETHOD.

  METHOD name_conflict_object.
    DATA: lr_cut     TYPE REF TO cx_cts_hta,
          lr_object TYPE REF TO if_cts_hta_object.

    lr_object = cl_cts_hta_api_factory=>create_instance( )->create_object_by_hana_name(
              i_hana_package_name = 'test.com'
              i_hana_object_name = 'OBJECT_NAME_THAT_WAS_changed_IN_HANA_AFTER_SYNC_TO_HTA'
              i_hana_object_suffix = '' ).
    lr_cut = NEW cx_cts_hta_name_conflict(
        textid                      = cx_cts_hta_name_conflict=>package_name_conflict
        name_of_obj_or_package_conf = 'OBJECT_NAME_THAT_WAS_changed_IN_HANA_AFTER_SYNC.SUFFIX'
        name_of_obj_or_package_hta  = 'OBJECT_NAME_THAT_WAS_CHANGED_IN_HANA_AFTER_SYNC.SUFFIX'
        cts_hta_component           = lr_object
    ).

    cl_abap_unit_assert=>assert_equals( act = lr_cut->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>package_name_conflict ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_1 exp = 'OBJECT_NAME_THAT_WAS_changed_IN_HANA_AFTER_SYNC.SU' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_2 exp = 'FFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_3 exp = 'OBJECT_NAME_THAT_WAS_CHANGED_IN_HANA_AFTER_SYNC.SU' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->message_variable_4 exp = 'FFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_cut->cts_hta_component exp = lr_object ).
  ENDMETHOD.

ENDCLASS.