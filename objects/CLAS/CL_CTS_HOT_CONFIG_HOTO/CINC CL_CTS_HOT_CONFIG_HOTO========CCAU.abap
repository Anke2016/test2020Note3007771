*"* use this source file for your ABAP unit test classes
CLASS:
  ltc_subobj_data DEFINITION DEFERRED,
  cl_cts_hot_config_hoto DEFINITION LOCAL FRIENDS ltc_subobj_data.

CLASS ltc_subobj_data DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mr_cut        TYPE REF TO cl_cts_hot_config_hoto,
      ms_hdi_object TYPE cts_hdi_object,
      ms_hot_object TYPE cts_hot_object.
    METHODS:
      setup,
      get_hdi_object_data FOR TESTING RAISING cx_static_check,
      get_hot_object_data FOR TESTING RAISING cx_static_check,
      throw_unsupported_feature_excp FOR TESTING RAISING cx_static_check,
      get_src_sysid_4m_abap_sync_sys FOR TESTING RAISING cx_static_check
      .
ENDCLASS.


CLASS ltc_subobj_data IMPLEMENTATION.

  METHOD setup.
    " Given
    mr_cut = NEW cl_cts_hot_config_hoto( iv_objtype = 'TEST' ).
  ENDMETHOD.

  METHOD get_hdi_object_data.
    " Given...
    ms_hdi_object = VALUE #(
                      hdi_content_bdata = '23217461626C65646174610A237B0A'
                    ).

    " When...
    mr_cut->read_subobject_data(
      EXPORTING
        iv_subobject          = 'CTS_HDI_OBJECT'
        ir_data               = REF #( ms_hdi_object )
      IMPORTING
        ev_content_as_string  = DATA(lv_content_string)
    ).

    " Then...
    cl_abap_unit_assert=>assert_equals(
      msg = 'Binary Data is retrieved correctly'
      exp = cl_abap_codepage=>convert_from( source = ms_hdi_object-hdi_content_bdata )
      act = lv_content_string
    ).

  ENDMETHOD.

  METHOD get_hot_object_data.

    " Given...
    ms_hot_object = VALUE #(
                      hana_content_cdata = '23217461626C65646174610A237B0A'
                    ).

    " When...
    mr_cut->read_subobject_data(
      EXPORTING
        iv_subobject                  = 'CTS_HOT_OBJECT'
        ir_data                       = REF #( ms_hot_object )
      IMPORTING
        ev_content_as_string              = DATA(lv_content_string)
    ).

    " Then...
    cl_abap_unit_assert=>assert_equals(
      msg = 'Binary Data is retrieved correctly'
      exp =  ms_hot_object-hana_content_cdata
      act = lv_content_string
    ).

  ENDMETHOD.

  METHOD throw_unsupported_feature_excp.

    " Given...
    ms_hot_object = VALUE #(
                      hana_content_bdata = '23217461626C65646174610A237B0A'
                    ).

    " When...
    TRY.
        mr_cut->read_subobject_data(
        EXPORTING
          iv_subobject                  = 'CTS_HOTO_OBJECT' " <<< Invalid Sub-Object
          ir_data                       = REF #( ms_hot_object )
        IMPORTING
          ev_content_as_string              = DATA(lv_content_bdata)
      ).
      CATCH cx_svrs_feature_not_supported INTO DATA(lx_unsupported_feature).
    ENDTRY.

    " Then...
    cl_abap_unit_assert=>assert_bound(
      act              = lx_unsupported_feature    " Reference variable to be checked
      msg              = |Unsupported Feature exception is be raised|    " Description
    ).

  ENDMETHOD.

  METHOD get_src_sysid_4m_abap_sync_sys.
    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for EDITOR-AT3'
                                        exp = |AT3|
                                        act = mr_cut->get_source_system_id( |EDITOR-AT3| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for SAPCWB-AT6'
                                        exp = |AT6|
                                        act = mr_cut->get_source_system_id( |SAPCWB-AT6| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for SAPUDO-AT5'
                                        exp = |AT5|
                                        act = mr_cut->get_source_system_id( |SAPUDO-AT5| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for SAP-SNOTE'
                                        exp = |SAP|
                                        act = mr_cut->get_source_system_id( |SAP-SNOTE| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for AT6'
                                        exp = |AT6|
                                        act = mr_cut->get_source_system_id( |AT6| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for UI'
                                        exp = |UI|
                                        act = mr_cut->get_source_system_id( |UI| )
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( msg = 'Source system not correct for space'
                                        exp = space
                                        act = mr_cut->get_source_system_id( space )
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.