CLASS ltcl_cts_hta_ext_call_internal DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA m_cut TYPE REF TO if_cts_hot_ext_call_internal.

    METHODS:
      setup,
      "! Test determine_masterlang_for_tadir for valid language string 'de' with and without suppress dialog
      determine_lang_de FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for invalid language string 'A0' with and without suppress dialog
      determine_lang_a0 FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for valid language string 'en_US' with and without suppress dialog
      determine_lang_en_us FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for valid language string 'de_de' with and without suppress dialog
      determine_lang_de_de FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for invalid language string 'hugo' with and without suppress dialog
      determine_lang_hugo FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for not set language in HANA with suppress dialog and with not_relevant for translation
      determine_lang_no_lang FOR TESTING RAISING cx_static_check,
      "! Test determine_masterlang_for_tadir for valid language string 'th_TH' but language not active in ABAP, with and without suppress dialog
      determine_lang_inactive_lang FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_cts_hta_ext_call_internal IMPLEMENTATION.

  METHOD determine_lang_de.
    DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
        i_hana_package_name            = 'pack.test.hta'
        i_hana_original_language       = 'de'
        i_suppress_dialog              = 'X'
        i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
    ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'D' ).

    lv_act = m_cut->determine_masterlang_for_tadir(
        i_hana_package_name            = 'pack.test.hta'
        i_hana_original_language       = 'de'
        i_suppress_dialog              = space
        i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
    ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'D' ).
  ENDMETHOD.

  METHOD determine_lang_en_us.
    DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
            i_hana_package_name            = 'pack.test.hta'
            i_hana_original_language       = 'en_US'
            i_suppress_dialog              = 'X'
            i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
        ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'E' ).

    lv_act = m_cut->determine_masterlang_for_tadir(
        i_hana_package_name            = 'pack.test.hta'
        i_hana_original_language       = 'en_US'
        i_suppress_dialog              = space
        i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
    ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'E' ).
  ENDMETHOD.

  METHOD determine_lang_de_de.
    DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
            i_hana_package_name            = 'pack.test.hta'
            i_hana_original_language       = 'de_de'
            i_suppress_dialog              = 'X'
            i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
        ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'D' ).

    lv_act = m_cut->determine_masterlang_for_tadir(
        i_hana_package_name            = 'pack.test.hta'
        i_hana_original_language       = 'de_de'
        i_suppress_dialog              = space
        i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
    ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = 'D' ).
  ENDMETHOD.

  METHOD determine_lang_hugo.
    TRY.
        DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
                i_hana_package_name            = 'pack.test.hta'
                i_hana_original_language       = 'HUGO'
                i_suppress_dialog              = 'X'
                i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
            ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'HUGO' ).
    ENDTRY.

    TRY.
        lv_act = m_cut->determine_masterlang_for_tadir(
            i_hana_package_name            = 'pack.test.hta'
            i_hana_original_language       = 'HUGO'
            i_suppress_dialog              = space
            i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
        ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO lr_exc.
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'HUGO' ).
    ENDTRY.
  ENDMETHOD.

  METHOD determine_lang_a0.
    "might be that A0 is supported some when, then replace A0 with an entry that does not exist in table t002x as laiso
    TRY.
        DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
                i_hana_package_name            = 'pack.test.hta'
                i_hana_original_language       = 'A0'
                i_suppress_dialog              = 'X'
                i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
            ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'A0' ).
    ENDTRY.

    TRY.
        lv_act = m_cut->determine_masterlang_for_tadir(
            i_hana_package_name            = 'pack.test.hta'
            i_hana_original_language       = 'A0'
            i_suppress_dialog              = space
            i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
        ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO lr_exc.
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'A0' ).
    ENDTRY.
  ENDMETHOD.

  METHOD determine_lang_no_lang.
    TRY.
        DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
                  i_hana_package_name            = 'pack.test.hta'
                  i_hana_original_language       = space
                  i_suppress_dialog              = 'X'
                  i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
              ).
        cl_abap_unit_assert=>fail( 'cx_cts_hta_unknown_master_lang expected.' ).
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>no_master_language ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_4 ).
    ENDTRY.

    TRY.
        lv_act = m_cut->determine_masterlang_for_tadir(
                  i_hana_package_name            = 'pack.test.hta'
                  i_hana_original_language       = space
                  i_suppress_dialog              = 'X'
                  i_translation_relevance        = VALUE #( )
              ).
        cl_abap_unit_assert=>fail( 'cx_cts_hta_unknown_master_lang expected.' ).
      CATCH cx_cts_hta_unknown_master_lang INTO lr_exc.
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>no_master_language ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_4 ).
    ENDTRY.

    lv_act = m_cut->determine_masterlang_for_tadir(
              i_hana_package_name            = 'pack.test.hta'
              i_hana_original_language       = space
              i_suppress_dialog              = space
              i_translation_relevance        = ce_cts_hta_translation=>not_relevant_for_translation
          ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = sy-langu ).
  ENDMETHOD.

  METHOD determine_lang_inactive_lang.
    TRY.
        DATA(lv_act) = m_cut->determine_masterlang_for_tadir(
                i_hana_package_name            = 'pack.test.hta'
                i_hana_original_language       = 'th_TH'
                i_suppress_dialog              = 'X'
                i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
            ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'th_TH' ).
    ENDTRY.

    TRY.
        lv_act = m_cut->determine_masterlang_for_tadir(
            i_hana_package_name            = 'pack.test.hta'
            i_hana_original_language       = 'th_TH'
            i_suppress_dialog              = space
            i_translation_relevance        = ce_cts_hta_translation=>relevant_for_translation
        ).
        cl_abap_unit_assert=>fail( msg = 'cx_cts_hta_unknown_master_lang expected' ).
      CATCH cx_cts_hta_unknown_master_lang INTO lr_exc.
        cl_abap_unit_assert=>assert_equals( act = lr_exc->if_t100_message~t100key exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_1 exp = 'pack.test.hta' ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc->message_variable_4 exp = 'th_TH' ).
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    m_cut = NEW cl_cts_hot_ext_call_intenal( ).
  ENDMETHOD.

ENDCLASS.