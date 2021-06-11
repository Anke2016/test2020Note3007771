*"* use this source file for your ABAP unit test classes
CLASS ltcl_cl_cts_hot_db_access DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      teardown,
      create_dummy_package RETURNING VALUE(rs_cts_hot_package) TYPE cts_hot_package,
      create_dummy_object RETURNING VALUE(rs_cts_hot_object_in) TYPE cts_hot_object,
      "##TODO enable again for otext
*      create_dummy_texts RETURNING VALUE(rt_cts_hot_otexts_in) TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,

      modify_cts_hot_object FOR TESTING RAISING cx_static_check,
      delete_cts_hot_object FOR TESTING RAISING cx_static_check,
      modify_cts_hot_package FOR TESTING RAISING cx_static_check,
      read_cts_hot_package FOR TESTING RAISING cx_static_check,
      delete_cts_hot_package FOR TESTING RAISING cx_static_check,
      read_cts_hot_object FOR TESTING RAISING cx_static_check,
      delete_cts_hot_objects FOR TESTING RAISING cx_static_check,
      "! Testing that all DB access calls for cts_hot_object use ABAP_STATUS = 'A' because only SNote will create 'I' entries there and we should use 'A' only.
      "! Exception: modify_cts_hot_object which works on input structure
      using_abap_status_a_only_objct FOR TESTING RAISING cx_static_check,
      "! Testing that all DB access calls for cts_hot_package use ABAP_STATUS = 'A' because only SNote will create 'I' entries there and we should use 'A' only.
      "! Exception: modify_cts_hot_package which works on input structure
      using_abap_status_a_only_pack FOR TESTING RAISING cx_static_check,
      test_modify FOR TESTING RAISING cx_static_check,
      test_update_package_after_depl FOR TESTING RAISING cx_static_check.

    "##TODO enable again for otext
*      modify_cts_hot_otexts FOR TESTING RAISING cx_static_check,
*      read_cts_hot_object_texts_mstr FOR TESTING RAISING cx_static_check.

    CONSTANTS: co_test_abap_hana_package_id TYPE cts_hot_package_id VALUE 'TMP.HTA.AUNIT.PACKAGE.TEST'.
    DATA: m_cut TYPE REF TO if_cts_hot_db_access.

ENDCLASS.

CLASS ltcl_cl_cts_hot_db_access IMPLEMENTATION.

  METHOD setup.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_test_abap_hana_package_id.
    DELETE FROM cts_hot_object WHERE abap_hana_package_id = co_test_abap_hana_package_id.
    CREATE OBJECT m_cut TYPE cl_cts_hot_db_access.
  ENDMETHOD.

  METHOD teardown.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_test_abap_hana_package_id.
    DELETE FROM cts_hot_object WHERE abap_hana_package_id = co_test_abap_hana_package_id.
  ENDMETHOD.

  METHOD modify_cts_hot_object.
    DATA: ls_cts_hot_object_in  TYPE cts_hot_object,
          lt_cts_hot_object_out TYPE STANDARD TABLE OF cts_hot_object.

    "prepare input
    ls_cts_hot_object_in = create_dummy_object( ).

    "call business function
    m_cut->modify_cts_hot_object( ls_cts_hot_object_in ).

    "verify object exists in DB
    SELECT * FROM cts_hot_object
      INTO TABLE lt_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object_in-abap_hana_object_name_suffix.

    "verify
    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_object_out ) ).
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_object_in act = lt_cts_hot_object_out[ 1 ] ).
  ENDMETHOD.

  METHOD delete_cts_hot_object.
    DATA: ls_cts_hot_object_in  TYPE cts_hot_object,
          ls_cts_hot_object_out TYPE cts_hot_object,
          lt_cts_hot_object_out TYPE STANDARD TABLE OF cts_hot_object.

    "prepare input
    ls_cts_hot_object_in = create_dummy_object( ).

    "read all data from DB before testing
    SELECT abap_hana_package_id, abap_hana_object_name_suffix, abap_status FROM cts_hot_object INTO TABLE @DATA(lt_cts_hot_objects_before).

    "create object on DB
    MODIFY cts_hot_object FROM ls_cts_hot_object_in.
    "verify object does exist in DB
    SELECT SINGLE * FROM cts_hot_object
      INTO ls_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object_in-abap_hana_object_name_suffix
        AND abap_status = ls_cts_hot_object_in-abap_status.
    cl_aunit_assert=>assert_not_initial( ls_cts_hot_object_out ).

    "create object and call business function
    m_cut->delete_cts_hot_object(
                i_abap_hana_package_id = ls_cts_hot_object_in-abap_hana_package_id
                i_abap_hana_object_name_suffix = ls_cts_hot_object_in-abap_hana_object_name_suffix
         ).

    "verify object does not exist in DB
    CLEAR ls_cts_hot_object_out.
    SELECT * FROM cts_hot_object
      INTO TABLE lt_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object_in-abap_hana_object_name_suffix.

    "verify
    cl_aunit_assert=>assert_initial( lt_cts_hot_object_out ).

    "verify deletion did not delete more entries by reading all entries again after testing
    SELECT abap_hana_package_id, abap_hana_object_name_suffix, abap_status FROM cts_hot_object INTO TABLE @DATA(lt_cts_hot_objects_after).
    cl_aunit_assert=>assert_equals( exp = lt_cts_hot_objects_before act = lt_cts_hot_objects_after ).
  ENDMETHOD.

  METHOD modify_cts_hot_package.
    DATA: ls_cts_hot_package_in  TYPE cts_hot_package,
          lt_cts_hot_package_out TYPE STANDARD TABLE OF cts_hot_package.

    "prepare input
    ls_cts_hot_package_in = create_dummy_package( ).

    "call business function
    m_cut->modify_cts_hot_package( ls_cts_hot_package_in ).

    "verify object exists in DB
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id.

    "verify
    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).
  ENDMETHOD.

  METHOD read_cts_hot_package.
    DATA: ls_cts_hot_package_in  TYPE cts_hot_package,
          ls_cts_hot_package_out TYPE cts_hot_package.

    "prepare input
    ls_cts_hot_package_in = create_dummy_package( ).
    MODIFY cts_hot_package FROM ls_cts_hot_package_in.

    "call business function
    ls_cts_hot_package_out = m_cut->read_cts_hot_package( ls_cts_hot_package_in-abap_hana_package_id ).

    "verify
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = ls_cts_hot_package_out ).
  ENDMETHOD.

  METHOD delete_cts_hot_package.
    DATA: ls_cts_hot_package  TYPE cts_hot_package.

    "prepare input
    ls_cts_hot_package = create_dummy_package( ).
    MODIFY cts_hot_package FROM ls_cts_hot_package.
    cl_aunit_assert=>assert_not_initial( m_cut->read_cts_hot_package( ls_cts_hot_package-abap_hana_package_id ) ).

    "create object and call business function
    m_cut->delete_cts_hot_package( ls_cts_hot_package-abap_hana_package_id ).

    "verify
    cl_aunit_assert=>assert_initial( m_cut->read_cts_hot_package( ls_cts_hot_package-abap_hana_package_id ) ).
  ENDMETHOD.

  METHOD read_cts_hot_object.
    DATA: ls_cts_hot_object_in  TYPE cts_hot_object,
          ls_cts_hot_object_out TYPE cts_hot_object.

    "prepare input
    ls_cts_hot_object_in = create_dummy_object( ).
    MODIFY cts_hot_object FROM ls_cts_hot_object_in.

    "call business function
    ls_cts_hot_object_out = m_cut->read_cts_hot_object(
                            i_abap_hana_package_id = ls_cts_hot_object_in-abap_hana_package_id
                            i_abap_hana_object_name_suffix = ls_cts_hot_object_in-abap_hana_object_name_suffix ).

    "verify
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_object_in act = ls_cts_hot_object_out ).
  ENDMETHOD.

  METHOD delete_cts_hot_objects.
    DATA: ls_cts_hot_object1_in TYPE cts_hot_object,
          ls_cts_hot_object2_in TYPE cts_hot_object,
          ls_cts_hot_object3_in TYPE cts_hot_object,
          ls_cts_hot_object_out TYPE cts_hot_object.

    "prepare input
    "object 1
    ls_cts_hot_object1_in = create_dummy_object( ).
    "object 2
    ls_cts_hot_object2_in = create_dummy_object( ).
    ls_cts_hot_object2_in-abap_hana_object_name_suffix = 'OBJECT2.suffix'.
    "object 3
    ls_cts_hot_object3_in = create_dummy_object( ).
    ls_cts_hot_object3_in-abap_hana_object_name_suffix = 'OBJECT3.suffix'.

    "read all data from DB before testing
    SELECT abap_hana_package_id, abap_hana_object_name_suffix, abap_status FROM cts_hot_object INTO TABLE @DATA(lt_cts_hot_objects_before).

    "create object 1 on DB
    MODIFY cts_hot_object FROM ls_cts_hot_object1_in.
    "verify object1 does exist in DB
    SELECT SINGLE * FROM cts_hot_object
      INTO ls_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object1_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object1_in-abap_hana_object_name_suffix
        AND abap_status = ls_cts_hot_object1_in-abap_status.
    cl_aunit_assert=>assert_not_initial( ls_cts_hot_object_out ).
    "object 2
    MODIFY cts_hot_object FROM ls_cts_hot_object2_in.
    "verify object2 does exist in DB
    CLEAR ls_cts_hot_object_out.
    SELECT SINGLE * FROM cts_hot_object
      INTO ls_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object2_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object2_in-abap_hana_object_name_suffix
        AND abap_status = ls_cts_hot_object2_in-abap_status.
    cl_aunit_assert=>assert_not_initial( ls_cts_hot_object_out ).
    "object 3
    MODIFY cts_hot_object FROM ls_cts_hot_object3_in.
    "verify object2 does exist in DB
    CLEAR ls_cts_hot_object_out.
    SELECT SINGLE * FROM cts_hot_object
      INTO ls_cts_hot_object_out
      WHERE abap_hana_package_id = ls_cts_hot_object3_in-abap_hana_package_id
        AND abap_hana_object_name_suffix = ls_cts_hot_object3_in-abap_hana_object_name_suffix
        AND abap_status = ls_cts_hot_object3_in-abap_status.
    cl_aunit_assert=>assert_not_initial( ls_cts_hot_object_out ).

    "create object and call business function
    m_cut->delete_cts_hot_objects( i_abap_hana_package_id = ls_cts_hot_object1_in-abap_hana_package_id ).

    "verify objects do not exist in DB anymore
    SELECT * FROM cts_hot_object INTO TABLE @DATA(lt_cts_hot_objects)
      WHERE abap_hana_package_id = @ls_cts_hot_object1_in-abap_hana_package_id.

    "verify
    cl_aunit_assert=>assert_initial( lt_cts_hot_objects ).

    "verify deletion did not delete more entries by reading all entries again after testing
    SELECT abap_hana_package_id, abap_hana_object_name_suffix, abap_status FROM cts_hot_object INTO TABLE @DATA(lt_cts_hot_objects_after).
    cl_aunit_assert=>assert_equals( exp = lt_cts_hot_objects_before act = lt_cts_hot_objects_after ).
  ENDMETHOD.

  METHOD using_abap_status_a_only_objct.
    "create normal object with ABAP_STATUS = 'A' in DB
    DATA(hot_object) = create_dummy_object( ).
    m_cut->modify_cts_hot_object( hot_object ).

    "create object with ABAP_STATUS = 'I' in DB
    hot_object-abap_status = 'I'.
    m_cut->modify_cts_hot_object( hot_object ).

    "make sure two entries are in DB (I and A)
    SELECT COUNT(*) FROM cts_hot_object WHERE abap_hana_package_id = @hot_object-abap_hana_package_id AND abap_hana_object_name_suffix = @hot_object-abap_hana_object_name_suffix INTO @DATA(cnt).
    cl_aunit_assert=>assert_equals( exp = 2 act = cnt ).

    "read entry and check that it is status 'A'
    DATA(hot_object_read) = m_cut->read_cts_hot_object( i_abap_hana_package_id = hot_object-abap_hana_package_id i_abap_hana_object_name_suffix = hot_object-abap_hana_object_name_suffix ).
    cl_aunit_assert=>assert_equals( exp = 'A' act = hot_object_read-abap_status ).

    "delete entry and make sure that I is still there
    m_cut->delete_cts_hot_object( i_abap_hana_package_id = hot_object-abap_hana_package_id i_abap_hana_object_name_suffix = hot_object-abap_hana_object_name_suffix ).

    SELECT COUNT(*) FROM cts_hot_object WHERE abap_hana_package_id = @hot_object-abap_hana_package_id AND abap_hana_object_name_suffix = @hot_object-abap_hana_object_name_suffix INTO @cnt.
    cl_aunit_assert=>assert_equals( exp = 1 act = cnt ).

    "verify read does not return the 'I' instance
    hot_object_read = m_cut->read_cts_hot_object( i_abap_hana_package_id = hot_object-abap_hana_package_id i_abap_hana_object_name_suffix = hot_object-abap_hana_object_name_suffix ).
    cl_aunit_assert=>assert_initial( hot_object_read ).

    "test delete_hot_objects only deletes A entries (therefore add as A again and create another object with abap_status I and A in same package)
    hot_object-abap_status = 'A'.
    m_cut->modify_cts_hot_object( hot_object ).
    hot_object-abap_hana_object_name_suffix = 'Object2.suffix'.
    m_cut->modify_cts_hot_object( hot_object ).
    hot_object-abap_status = 'I'.
    m_cut->modify_cts_hot_object( hot_object ).
    "check that there are 4 entries for this package now
    SELECT COUNT(*) FROM cts_hot_object WHERE abap_hana_package_id = @hot_object-abap_hana_package_id INTO @cnt.
    cl_aunit_assert=>assert_equals( exp = 4 act = cnt ).
    "call business method
    m_cut->delete_cts_hot_objects( i_abap_hana_package_id = hot_object-abap_hana_package_id ).
    "verify that 2 entries are left and that both have abap_status I
    SELECT abap_hana_package_id, abap_hana_object_name_suffix, abap_status FROM cts_hot_object WHERE abap_hana_package_id = @hot_object-abap_hana_package_id INTO TABLE @DATA(lt_hot_i_objects).
    cl_aunit_assert=>assert_equals( exp = 2 act = lines( lt_hot_i_objects ) ).
    LOOP AT lt_hot_i_objects INTO DATA(ls_hot_object).
      cl_aunit_assert=>assert_equals( exp = 'I' act = ls_hot_object-abap_status ).
    ENDLOOP.

  ENDMETHOD.

  METHOD using_abap_status_a_only_pack.
    "create normal package with ABAP_STATUS = 'A' in DB
    DATA(hot_package) = create_dummy_package( ).
    m_cut->modify_cts_hot_package( hot_package ).

    "create package with ABAP_STATUS = 'I' in DB
    hot_package-abap_status = 'I'.
    m_cut->modify_cts_hot_package( hot_package ).

    "make sure two entries are in DB
    SELECT COUNT(*) FROM cts_hot_package WHERE abap_hana_package_id = @hot_package-abap_hana_package_id INTO @DATA(cnt).
    cl_aunit_assert=>assert_equals( exp = 2 act = cnt ).

    "read entry and check that it is status 'A'
    DATA(hot_package_read) = m_cut->read_cts_hot_package( i_abap_hana_package_id = hot_package-abap_hana_package_id ).
    cl_aunit_assert=>assert_equals( exp = 'A' act = hot_package_read-abap_status ).

    "delete entry and make sure that I is still there and that read does not return the 'I' instance
    m_cut->delete_cts_hot_package( i_abap_hana_package_id = hot_package-abap_hana_package_id ).

    SELECT COUNT(*) FROM cts_hot_package WHERE abap_hana_package_id = @hot_package-abap_hana_package_id INTO @cnt.
    cl_aunit_assert=>assert_equals( exp = 1 act = cnt ).

    hot_package_read = m_cut->read_cts_hot_package( i_abap_hana_package_id = hot_package-abap_hana_package_id ).
    cl_aunit_assert=>assert_initial( hot_package_read ).
  ENDMETHOD.


  "##TODO enable again for otext
*  METHOD modify_cts_hot_otexts.
*    DATA: lt_cts_hot_otexts_in  TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,
*          lt_cts_hot_otexts_out TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,
*          ls_cts_hot_otexts_in  LIKE LINE OF lt_cts_hot_otexts_in,
*          ls_cts_hot_otexts_out LIKE LINE OF lt_cts_hot_otexts_out.
*
*    "prepare input
*    lt_cts_hot_otexts_in = create_dummy_texts( ).
*
*    "call business function
*    m_cut->modify_cts_hot_otexts( lt_cts_hot_otexts_in ).
*
*    "verify object exists in DB
*    LOOP AT lt_cts_hot_otexts_in INTO ls_cts_hot_otexts_in.
*      SELECT * FROM cts_hot_otexts
*        INTO ls_cts_hot_otexts_out
*        WHERE abap_hana_package_id = ls_cts_hot_otexts_in-abap_hana_package_id
*          AND abap_hana_object_name_suffix = ls_cts_hot_otexts_in-abap_hana_object_name_suffix
*          AND language = ls_cts_hot_otexts_in-language
*          AND isolanguage = ls_cts_hot_otexts_in-isolanguage
*          AND text_type = ls_cts_hot_otexts_in-text_type
*          AND hana_text_id = ls_cts_hot_otexts_in-hana_text_id.
*      ENDSELECT.
*
*      "verify
*      cl_aunit_assert=>assert_equals( exp = ls_cts_hot_otexts_in act = ls_cts_hot_otexts_out ).
*    ENDLOOP.
*  ENDMETHOD.
*
*  METHOD read_cts_hot_object_texts_mstr.
*    DATA: lt_cts_hot_otexts_in  TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,
*          lt_cts_hot_otexts_out TYPE if_cts_hot_db_access=>ty_cts_hot_object_texts,
*          ls_cts_hot_otexts_in  LIKE LINE OF lt_cts_hot_otexts_in,
*          ls_cts_hot_otexts_out LIKE LINE OF lt_cts_hot_otexts_out,
*          ls_cts_hot_package    type cts_hot_package.
*
*    "prepare input
*    lt_cts_hot_otexts_in = create_dummy_texts( ).
*    MODIFY cts_hot_otexts FROM TABLE lt_cts_hot_otexts_in.
*
*    "also create package with correct language
*    ls_cts_hot_package = create_dummy_package( ).
*    ls_cts_hot_package-abap_hana_package_id = lt_cts_hot_otexts_in[ 1 ]-abap_hana_package_id.
*    ls_cts_hot_package-hana_pack_orig_lang = lt_cts_hot_otexts_in[ 1 ]-isolanguage.
*    modify cts_hot_package from ls_cts_hot_package.
*
*    "call business function
*    lt_cts_hot_otexts_out = m_cut->read_cts_hot_object_texts_mstr(
*                            cl_cts_hot_object_v1=>create_instance(
*                                iv_hana_package_id    = 'package.test'
*                                iv_hana_object_name   = 'objname'
*                                iv_hana_object_suffix = 'suffix'
*                            ) ).
*
*    "verify
*    cl_aunit_assert=>assert_equals( exp = lt_cts_hot_otexts_in act = lt_cts_hot_otexts_out ).
*  ENDMETHOD.


  METHOD create_dummy_package.
    rs_cts_hot_package-abap_hana_package_id = co_test_abap_hana_package_id.
    rs_cts_hot_package-abap_status = 'A'.
    rs_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_new.
    rs_cts_hot_package-hana_package_id = 'tmp.hta.aunit.package.test'.
    rs_cts_hot_package-hana_pack_src_system = 'SRC'.
    rs_cts_hot_package-hana_pack_src_tenant = ''.
    rs_cts_hot_package-hana_pack_description = 'Descriptin of hana package'.
    rs_cts_hot_package-hana_pack_responsible = 'srsponsible'.
    rs_cts_hot_package-hana_pack_orig_lang = 'DE'.
    rs_cts_hot_package-hana_pack_is_structural = 0.
    rs_cts_hot_package-hana_pack_delivery_unit = 'MYDUNAME'.
    rs_cts_hot_package-hana_pack_du_vendor = 'DUVENDOR'.
    rs_cts_hot_package-hana_pack_text_collection = 'Text Coll'.
    rs_cts_hot_package-hana_pack_text_status = 'Text Status'.
    rs_cts_hot_package-hana_pack_text_term_domain = 'Terminology Domain'.
    rs_cts_hot_package-hana_pack_hints_for_transl = 'Hints for Translation'.
    rs_cts_hot_package-hana_read_system = 'HAN'.
    rs_cts_hot_package-abap_import_timestamp = 20140430152355123.
    GET TIME STAMP FIELD rs_cts_hot_package-abap_deployed_at.
    rs_cts_hot_package-abap_deployed_by = 'abap_user'.
    rs_cts_hot_package-abap_sync_system = 'DEV'.
    GET TIME STAMP FIELD rs_cts_hot_package-abap_synced_at.
    rs_cts_hot_package-abap_synced_by = 'abap user'.
  ENDMETHOD.


  METHOD create_dummy_object.
    rs_cts_hot_object_in-abap_hana_package_id = co_test_abap_hana_package_id.
    rs_cts_hot_object_in-abap_hana_object_name_suffix = 'OBJNAME.SUFFIX'.
    rs_cts_hot_object_in-abap_status = 'A'.
    rs_cts_hot_object_in-hot_status = if_cts_hot_db_access=>co_hot_status_new.
    rs_cts_hot_object_in-hana_package_id = 'tmp.hta.aunit.package.test'.
    rs_cts_hot_object_in-hana_object_name = 'objname'.
    rs_cts_hot_object_in-hana_object_suffix = 'suffix'.
    rs_cts_hot_object_in-abap_sync_system = 'DEV'.
    rs_cts_hot_object_in-hana_read_system = 'HAN'.
    rs_cts_hot_object_in-hana_source_build_version = '1.00.14.00.123456'.
    rs_cts_hot_object_in-hana_source_object_version = '123'.
    rs_cts_hot_object_in-hana_object_version = '234'.
    rs_cts_hot_object_in-abap_import_timestamp = 20140430152355123.
    GET TIME STAMP FIELD rs_cts_hot_object_in-hana_activated_at.
    rs_cts_hot_object_in-hana_activated_by = 'HANA User'.
    rs_cts_hot_object_in-hana_content_bdata = 'bdata'.
    rs_cts_hot_object_in-hana_content_cdata = 'cdata'.
    GET TIME STAMP FIELD rs_cts_hot_object_in-abap_synced_at.
    rs_cts_hot_object_in-abap_synced_by = 'abap user'.
  ENDMETHOD.

  "##TODO enable again for otexts
*  METHOD create_dummy_texts.
*    DATA ls_cts_hot_otext TYPE cts_hot_otexts.
*
*    DO 2 TIMES.
*      ls_cts_hot_otext-abap_hana_package_id = 'PACKAGE.TEST'.
*      ls_cts_hot_otext-abap_hana_object_name_suffix = 'OBJNAME.SUFFIX'.
*      ls_cts_hot_otext-abap_import_timestamp = 20140430152355123.
*      GET TIME STAMP FIELD  ls_cts_hot_otext-abap_synced_at.
*      ls_cts_hot_otext-abap_synced_by = sy-uname.
*      ls_cts_hot_otext-abap_sync_system = sy-sysid.
*      ls_cts_hot_otext-hana_read_system = 'HAN'.
*      ls_cts_hot_otext-hana_source_object_version = '123'.
*      ls_cts_hot_otext-hana_text_content = 'Some text' && sy-index.
*      ls_cts_hot_otext-hana_text_id = 'mytext_id' && sy-index.
*      ls_cts_hot_otext-hana_text_max_length = 155.
*      ls_cts_hot_otext-hana_text_type = 'XMSG'.
*      ls_cts_hot_otext-isolanguage = 'en_US'.
*      ls_cts_hot_otext-language = 'E'.
*      ls_cts_hot_otext-text_type = if_cts_hot_db_access=>co_cts_hot_otexts_type_text.
*
*      APPEND ls_cts_hot_otext TO rt_cts_hot_otexts_in.
*    ENddo.
*  ENDMETHOD.

  METHOD test_modify.
    DATA: ls_cts_hot_package_in  TYPE cts_hot_package,
          lt_cts_hot_package_out TYPE STANDARD TABLE OF cts_hot_package.

* Test 1 with ABAP_STATUS = 'A'
    "prepare input
    ls_cts_hot_package_in = create_dummy_package( ).

    "call business function
    m_cut->modify_cts_hot_package( ls_cts_hot_package_in ).

    "verify package exists in DB
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id.

    "verify
    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).

* Test 2 with ABAP status = 'I'
    CLEAR lt_cts_hot_package_out.
    ls_cts_hot_package_in-abap_status = 'I'.
    m_cut->modify_cts_hot_package( ls_cts_hot_package_in ).

    "verify package exists in DB
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id
      ORDER BY abap_status.

    "verify
    cl_aunit_assert=>assert_equals( exp = 2 act = lines( lt_cts_hot_package_out ) ).
    ls_cts_hot_package_in-abap_status = 'A'.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).
    ls_cts_hot_package_in-abap_status = 'I'.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 2 ] ).
  ENDMETHOD.

  METHOD test_update_package_after_depl.
    DATA: ls_cts_hot_package_in  TYPE cts_hot_package,
          lt_cts_hot_package_out TYPE STANDARD TABLE OF cts_hot_package,
          lv_deployed_at         TYPE timestampl.

*Test 1 - deployment with error
    "prepare test data (simulate TP import)
    ls_cts_hot_package_in = create_dummy_package( ).
    CLEAR ls_cts_hot_package_in-abap_deployed_at.
    CLEAR ls_cts_hot_package_in-abap_deployed_by.
    ls_cts_hot_package_in-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    MODIFY cts_hot_package FROM ls_cts_hot_package_in.

    "call business function
    m_cut->update_package_after_deploymnt( i_old_package = ls_cts_hot_package_in
                                           i_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error ).

    "Read package from DB for verification
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id.

    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    "verify only hot_status was changed
    ls_cts_hot_package_in-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).

* Test2 successful deployment after previous deploy error
    GET TIME STAMP FIELD lv_deployed_at.

    "call business function
    m_cut->update_package_after_deploymnt( i_old_package = ls_cts_hot_package_in
                                           i_new_status = if_cts_hot_db_access=>co_hot_status_active
                                           i_new_deployed_at = lv_deployed_at
                                           i_new_deployed_by = 'new_user' ).

    "Read package from DB for verification
    CLEAR lt_cts_hot_package_out.
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id AND abap_status = ls_cts_hot_package_in-abap_status.

    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    "verify hot_status, abap_deployed_at and abap_deploy_by were changed correctly.
    ls_cts_hot_package_in-hot_status = if_cts_hot_db_access=>co_hot_status_active.
    ls_cts_hot_package_in-abap_deployed_at = lv_deployed_at.
    ls_cts_hot_package_in-abap_deployed_by = 'new_user'.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).

* Test3 redeployment with error (deploy_at and by must not be changed)
    "call business function
    m_cut->update_package_after_deploymnt( i_old_package = ls_cts_hot_package_in
                                           i_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error ).

    "Read package from DB for verification
    CLEAR lt_cts_hot_package_out.
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id.

    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    "verify only hot_status was changed correctly (and deployed_at and deployed_by are unchanged).
    ls_cts_hot_package_in-hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).

* Test4 simulate deployment and parallel change by TP. (Package had hot_status = 'E', is read from DB into ABAP and shortly
* afterwards but before successful update of successful package modification in DB, TP imports a new package version).
* Expect hot_status, deployed_at and deployed_by were not updated and are still 'I' from TP and previous deployed_at/by values.
    "simulate tp import
    UPDATE cts_hot_package SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id
                                                                                           AND abap_status = ls_cts_hot_package_in-abap_status.

    WAIT UP TO 1 SECONDS. "so that deployed_at is different.
    GET TIME STAMP FIELD lv_deployed_at.
    "call business function
    m_cut->update_package_after_deploymnt( i_old_package = ls_cts_hot_package_in
                                           i_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error
                                           i_new_deployed_at = lv_deployed_at
                                           i_new_deployed_by = 'other_user' ).

    "Read package from DB for verification
    CLEAR lt_cts_hot_package_out.
    SELECT * FROM cts_hot_package
      INTO TABLE lt_cts_hot_package_out
      WHERE abap_hana_package_id = ls_cts_hot_package_in-abap_hana_package_id.

    cl_aunit_assert=>assert_equals( exp = 1 act = lines( lt_cts_hot_package_out ) ).
    "verify no change on hot_status, deployed_at and deployed_by
    ls_cts_hot_package_in-hot_status = if_cts_hot_db_access=>co_hot_status_inactive.
    cl_aunit_assert=>assert_equals( exp = ls_cts_hot_package_in act = lt_cts_hot_package_out[ 1 ] ).
  ENDMETHOD.

ENDCLASS.