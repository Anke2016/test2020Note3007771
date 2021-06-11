*"* use this source file for your ABAP unit test classes
CLASS ltcx_cts_hta_unknown_master_la DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Test exception no language
      test_no_lang FOR TESTING RAISING cx_static_check,
      "! Test exception no language with package id too long for message variables, last chars starting at position 201 are skipped in message output
      test_no_lang_204 FOR TESTING RAISING cx_static_check,
      "! Test exception unsupported language
      test_unsupported FOR TESTING RAISING cx_static_check,
      "! Test exception unsupported language with package id too long for message variables, last chars starting at position 151 are skipped in message output
      test_unsupported_152 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcx_cts_hta_unknown_master_la IMPLEMENTATION.

  METHOD test_no_lang.
    TRY.
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid       = cx_cts_hta_unknown_master_lang=>no_master_language
            hana_package = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabc'.
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( exp = cx_cts_hta_unknown_master_lang=>no_master_language act = lr_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = lr_exc->message_variable_1 ).
        cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_equals( exp = 'c' act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_4 ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_lang_204.
    TRY.
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid       = cx_cts_hta_unknown_master_lang=>no_master_language
            hana_package = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGH'.
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( exp = cx_cts_hta_unknown_master_lang=>no_master_language act = lr_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = lr_exc->message_variable_1 ).
        cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( exp = 'QRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCD' act = lr_exc->message_variable_4 ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_unsupported.
    TRY.
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid        = cx_cts_hta_unknown_master_lang=>unsupported_master_lang
            hana_language = 'some-language' "usually de_DE or en_US, ...
            hana_package  = '1234567890ABCDEFGHIJKLMNOPQRSTUVWX'.
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang act = lr_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWX' act = lr_exc->message_variable_1 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_initial( act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( exp = 'some-language' act = lr_exc->message_variable_4 ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_unsupported_152.
    TRY.
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid        = cx_cts_hta_unknown_master_lang=>unsupported_master_lang
            hana_language = 'en_US'
            hana_package  = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQR'.
      CATCH cx_cts_hta_unknown_master_lang INTO DATA(lr_exc).
        cl_abap_unit_assert=>assert_equals( exp = cx_cts_hta_unknown_master_lang=>unsupported_master_lang act = lr_exc->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = lr_exc->message_variable_1 ).
        cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = lr_exc->message_variable_2 ).
        cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = lr_exc->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( exp = 'en_US' act = lr_exc->message_variable_4 ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.