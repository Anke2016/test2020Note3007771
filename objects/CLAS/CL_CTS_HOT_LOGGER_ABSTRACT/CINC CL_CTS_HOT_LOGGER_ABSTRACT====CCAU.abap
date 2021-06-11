CLASS ltcl_log_helper_4_scts_hot DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

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
      split_message_2c_2spaces_alter FOR TESTING RAISING cx_static_check,
      "! test split message with a text with maximum length for  line
      split_message_max_length FOR TESTING RAISING cx_static_check,
      "! test split message if there is no text at all
      split_message_empty_text FOR TESTING RAISING cx_static_check,
      "! test split message if there are spaces only
      split_message_spaces_only FOR TESTING RAISING cx_static_check,
      "! test split message with space at the end
      split_message_space_at_end FOR TESTING RAISING cx_static_check,
      "! test split message with spaces at the end
      split_message_spaces_at_end FOR TESTING RAISING cx_static_check,
      "! test split message with many many spaces at the end
      split_message_spaces_at_end_2 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_log_helper_4_scts_hot IMPLEMENTATION.

  METHOD split_message_empty_text.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = || ).

    cl_abap_unit_assert=>assert_initial( act = lt_split_messages ).
  ENDMETHOD.


  METHOD split_message_test1.
    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
        message = 'MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) - version of AT3K900022' ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'MERGEPROCEDURE_1_4_0.procedure' act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = ' (tester1.merge1) - version of A' act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'T3K900022' act = lt_split_message[ 1 ]-var3 ).

  ENDMETHOD.

  METHOD split_message_test2.
    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
        message          = |create olap scenario '<?xml version='1.0' encoding='utf-8'?><cubeSchema defaultLanguage='EN' defaultSchema='_SYS_BIC' operation='createHanaCube' version=''><sqlScriptView name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' | &&
                           |schema='_SYS_BIC' sqlScriptF...3369 chars skipped by HTA...ensionAttribute></dimensionAttributes><measures><measure column='CLV' '| &&
                           |name='CLV' aggregationType='sum'/><measure column='CHRN_RATE' name='CHURN_RATE' aggregationType='sum'/></measures></sqlScriptView></cubeSchema>'|
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
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 5 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 5 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 5 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_test3.

    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
         message          = |AT_CPRED_DEMO_INSURANCE_JS_IC.attributeview (sap.hana-app.cuan.cpred.demo.insurance_ic.datafoundation.internal) would be OK, see next attempt|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |AT_CPRED_DEMO_INSURANCE_JS_IC.a| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ttributeview (sap.hana-app.cuan| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |.cpred.demo.insurance_ic.datafo| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |undation.internal) would be OK,| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |   see next attempt| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_test4.

    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
         message          = |PR_T100_1.procedure (tmp.hta.aunit) regeneration failed - see longtext|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |PR_T100_1.procedure (tmp.hta.au| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |nit) regeneration failed - see| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | longtext| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = || act = lt_split_message[ 1 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_char_space_alter.

    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
         message          = |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                            |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                            |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9|
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

    DATA(lt_split_message) = lcl_log_helper_4_scts_hot=>split_message(
         message          = |aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll mm nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy  zz  AA  BB  CC  DD  EE  FF  GG  HH  II  JJ|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = |aa  bb  cc  dd  ee  ff  gg  hh| act = lt_split_message[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |  ii  jj  kk  ll mm nn  oo  pp| act = lt_split_message[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | qq  rr  ss  tt  uu  vv  ww  xx| act = lt_split_message[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  yy  zz  AA  BB  CC  DD  EE  FF| act = lt_split_message[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |    GG  HH  II  JJ| act = lt_split_message[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_message[ 2 ]-var4 ).

  ENDMETHOD.

  METHOD split_message_max_length.
    DATA(lt_split_messages) = lcl_log_helper_4_scts_hot=>split_message( |      Adding a 'required for deploy' dependency to the file 'db://sap.bc.epm.oia::SNWD_BPA' (as extracted from the metadata header)| ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |      Adding a 'required for de| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ploy' dependency to the file 'd| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |b://sap.bc.epm.oia::SNWD_BPA' (| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |as extracted from the metadata| act = lt_split_messages[ 1 ]-var4 ).
    cl_abap_unit_assert=>assert_equals( exp = |  header)| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var4 ).
  ENDMETHOD.

  METHOD split_message_spaces_only.
    "Test verification is different compared to split_message_spaces_only in ltcl_log_helper because of different handling of spaces in split_message methods.
    "This test should only ensure that there is no endless loop in case of only many spaces!!! so it is OK that there are different empty results in case of
    "different empty string lengths
    DATA(lt_split_messages) = lcl_log_helper_4_scts_hot=>split_message( message = |    | ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages ).

    DATA lv_empty_string TYPE string.
    DO 200 TIMES.
      lv_empty_string = | { lv_empty_string }|.
      cl_abap_unit_assert=>assert_equals( exp = sy-index act = strlen( lv_empty_string ) ). "check string is constructed and gets longer

      lt_split_messages = lcl_log_helper_4_scts_hot=>split_message( message = lv_empty_string ).

      IF sy-index < 94.
        cl_abap_unit_assert=>assert_initial( lt_split_messages ).
      ELSE.
        cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) msg = |{ sy-index }| ).
        cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var1 ).
        cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var2 ).
        cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
        cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var4 ).
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD split_message_space_at_end.
    DATA(lt_split_messages) = lcl_log_helper_4_scts_hot=>split_message( message = |    src/sap.bc.epm.oia/dfg/perf/AN_DATA_PROVIDER_PERFORMANCE.hdbcalculationview.properties (SEPM_OIA_RUNT) | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '    src/sap.bc.epm.oia/dfg/perf' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = '/AN_DATA_PROVIDER_PERFORMANCE.h' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'dbcalculationview.properties (S' act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'EPM_OIA_RUNT)' act = lt_split_messages[ 1 ]-var4 ).
  ENDMETHOD.

  METHOD split_message_spaces_at_end.
    DATA(lt_split_messages) = lcl_log_helper_4_scts_hot=>split_message( message = |      src/sap.bc.esh.nlq.CES715/query/config/nlq.hdbtextconfig                                 | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '      src/sap.bc.esh.nlq.CES715' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = '/query/config/nlq.hdbtextconfig' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var4 ).
  ENDMETHOD.

  METHOD split_message_spaces_at_end_2.
    DATA(lt_split_messages) = lcl_log_helper_4_scts_hot=>split_message( message = |      src/sap.bc.esh.nlq.CES715/query/config/nlq.hdbtextconfig                                  | &&
                                                                                     |                                                                                            | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '      src/sap.bc.esh.nlq.CES715' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = '/query/config/nlq.hdbtextconfig' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var4 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_log_helper DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      split_message_empty_text FOR TESTING RAISING cx_static_check,
      "! Tests the split message method for a fixed bug
      "! "MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) - version of AT3K900022" was handled wrong
      split_message_test1 FOR TESTING RAISING cx_static_check,
      "! Tests the split messages after change to have two spaces in front as of second line with real life example data
      split_message_test2 FOR TESTING RAISING cx_static_check,
      "! another test with real string data
      split_message_test3 FOR TESTING RAISING cx_static_check,
      "! another test with real string data
      split_message_test4 FOR TESTING RAISING cx_static_check,
      "! another test with real string data
      split_message_test5 FOR TESTING RAISING cx_static_check,
      "! test split message with alternating char and space (a b c d ...) to verify space handling
      split_message_char_space_alter FOR TESTING RAISING cx_static_check,
      "! test split message with alternating 2 char and 2 space (aa  bb  cc  ...) to verify space handling
      split_message_2c_2spaces_alter FOR TESTING RAISING cx_static_check,
      "! test split message with a text that spans 3 lines and has initial indent of 2 chars.<br/>
      "! Indent should be kept for following lines and even increased because as of 2nd line 2 chars are added in fron.
      split_message_keep_indent_2 FOR TESTING RAISING cx_static_check,
      "! test split message with a text that spans 3 lines and has initial indent of 4 chars.<br/>
      "! Indent should be kept for following lines and even increased because as of 2nd line 2 chars are added in fron.
      split_message_keep_indent_4 FOR TESTING RAISING cx_static_check,
      "! test split message with a text with maximum length for  line
      split_message_max_length FOR TESTING RAISING cx_static_check,
      "! test split message if there are spaces only
      split_message_spaces_only FOR TESTING RAISING cx_static_check,
      "! test split message with space at the end
      split_message_space_at_end FOR TESTING RAISING cx_static_check,
      "! test split message with spaces at the end
      split_message_spaces_at_end FOR TESTING RAISING cx_static_check,
      "! test split message with many many spaces at the end
      split_message_spaces_at_end_2 FOR TESTING RAISING cx_static_check,
      "! test split message with real string data
      split_message_test6 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_log_helper IMPLEMENTATION.

  METHOD split_message_2c_2spaces_alter.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll  mm  nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy  zz  AA  BB  CC  DD  EE  FF  GG  HH  II  JJ|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll  mm| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |  nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |  zz  AA  BB  CC  DD  EE  FF  GG| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |   HH  II  JJ| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_char_space_alter.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                      |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 | &&
                      |a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |a b c d e f g h i j k l m n o p q r s t u v w x y| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | z A B C D E F G H I J K L M N O P R S T U V W X Y| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | Z 0 1 2 3 4 5 6 7 8 9 a b c d e| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |   f g h i j k l m n o p q r s t u v w x y z A B C| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | D E F G H I J K L M N O P R S T U V W X Y Z 0 1 2| act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | 3 4 5 6 7 8 9 a b c d e f g h| act = lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  i j k l m n o p q r s t u v w x y z A B C D E F| act = lt_split_messages[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | G H I J K L M N O P R S T U V W X Y Z 0 1 2 3 4 5| act = lt_split_messages[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | 6 7 8 9| act = lt_split_messages[ 3 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_test1.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = 'MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) - version of AT3K900022'
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'MERGEPROCEDURE_1_4_0.procedure (tester1.merge1) -' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = ' version of AT3K900022' act = lt_split_messages[ 1 ]-var2 ).
  ENDMETHOD.

  METHOD split_message_test2.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |create olap scenario '<?xml version='1.0' encoding='utf-8'?><cubeSchema defaultLanguage='EN' defaultSchema='_SYS_BIC' operation='createHanaCube' version=''><sqlScriptView name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' | &&
                      |schema='_SYS_BIC' sqlScriptF...3369 chars skipped by HTA...ensionAttribute></dimensionAttributes><measures><measure column='CLV' '| &&
                      |name='CLV' aggregationType='sum'/><measure column='CHRN_RATE' name='CHURN_RATE' aggregationType='sum'/></measures></sqlScriptView></cubeSchema>'|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |create olap scenario '<?xml version='1.0' encoding| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |='utf-8'?><cubeSchema defaultLanguage='EN' default| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |Schema='_SYS_BIC' operation='cr| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  eateHanaCube' version=''><sqlScriptView name='sa| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |p.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' schema='_SY| act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |S_BIC' sqlScriptF...3369 chars| act = lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  skipped by HTA...ensionAttribute></dimensionAttr| act = lt_split_messages[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ibutes><measures><measure column='CLV' 'name='CLV'| act = lt_split_messages[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = | aggregationType='sum'/><measur| act = lt_split_messages[ 3 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  e column='CHRN_RATE' name='CHURN_RATE' aggregati| act = lt_split_messages[ 4 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |onType='sum'/></measures></sqlScriptView></cubeSch| act = lt_split_messages[ 4 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |ema>'| act = lt_split_messages[ 4 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_test3.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |AT_CPRED_DEMO_INSURANCE_JS_IC.attributeview (sap.hana-app.cuan.cpred.demo.insurance_ic.datafoundation.internal) would be OK, see next attempt|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |AT_CPRED_DEMO_INSURANCE_JS_IC.attributeview (sap.h| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ana-app.cuan.cpred.demo.insurance_ic.datafoundatio| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |n.internal) would be OK, see ne| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |  xt attempt| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 2 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_test4.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |PR_T100_1.procedure (tmp.hta.aunit) regeneration failed - see longtext|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |PR_T100_1.procedure (tmp.hta.aunit) regeneration f| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ailed - see longtext| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_test5.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |  Configuring libraries in the container "DANIEL_DEMO_A_001"; removing []; updating or adding [com.sap.hana.di.cds, com.sap.hana.di.c|
                   && |onstraint, com.sap.hana.di.dropcreatetable, com.sap.hana.di.fulltextindex, com.sap.hana.di.index, com.sap.hana.di.logicalschema|
                   && |, com.sap.hana.di.sequence, com.sap.hana.di.statistics, com.sap.hana.di.table, com.sap.hana.di.tabledata, com.sap.hana.di.textc|
                   && |onfig, com.sap.hana.di.textdictionary, com.sap.hana.di.textminingconfig, com.sap.hana.di.textrule]...|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |  Configuring libraries in the container "DANIEL_D| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |EMO_A_001"; removing []; updating or adding [com.s| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |ap.hana.di.cds, com.sap.hana.di| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |    .constraint, com.sap.hana.di.dropcreatetable,| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | com.sap.hana.di.fulltextindex, com.sap.hana.di.in| act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |dex, com.sap.hana.di.logicalsche| act = lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |    ma, com.sap.hana.di.sequence, com.sap.hana.di.| act = lt_split_messages[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |statistics, com.sap.hana.di.table, com.sap.hana.di| act = lt_split_messages[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |.tabledata, com.sap.hana.di.tex| act = lt_split_messages[ 3 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |    tconfig, com.sap.hana.di.textdictionary, com.s| act = lt_split_messages[ 4 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ap.hana.di.textminingconfig, com.sap.hana.di.textr| act = lt_split_messages[ 4 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |ule]...| act = lt_split_messages[ 4 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_empty_text.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = || ).

    cl_abap_unit_assert=>assert_initial( act = lt_split_messages ).
  ENDMETHOD.

  METHOD split_message_keep_indent_2.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |  create olap scenario '<?xml version='1.0' encoding='utf-8'?><cubeSchema defaultLanguage='EN' defaultSchema='_SYS_BIC' operation='createHanaCube' version=''><sqlScriptView name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' | &&
                      |schema='_SYS_BIC' sqlScriptF...3369 chars skipped by HTA...ensionAttribute></dimensionAttributes><measures>|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |  create olap scenario '<?xml version='1.0' encodi| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ng='utf-8'?><cubeSchema defaultLanguage='EN' defau| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |ltSchema='_SYS_BIC' operation='| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |    createHanaCube' version=''><sqlScriptView name| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' schema=| act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |'_SYS_BIC' sqlScriptF...3369 ch| act = lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |    ars skipped by HTA...ensionAttribute></dimensi| act = lt_split_messages[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |onAttributes><measures>| act = lt_split_messages[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 3 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_keep_indent_4.
    DATA(lt_split_messages) = lcl_log_helper=>split_message(
         iv_message = |    create olap scenario '<?xml version='1.0' encoding='utf-8'?><cubeSchema defaultLanguage='EN' defaultSchema='_SYS_BIC' operation='createHanaCube' version=''><sqlScriptView name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' | &&
                      |schema='_SYS_BIC' sqlScriptF...3369 chars skipped by HTA...ensionAttribute></dimensionAttributes><measures>|
    ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |    create olap scenario '<?xml version='1.0' enco| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |ding='utf-8'?><cubeSchema defaultLanguage='EN' def| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |aultSchema='_SYS_BIC' operation| act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |      ='createHanaCube' version=''><sqlScriptView| act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | name='sap.hana-app.cuan.ai/CA_AI_CUSTOMER_CCR' sc| act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |hema='_SYS_BIC' sqlScriptF...336| act = lt_split_messages[ 2 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = |      9 chars skipped by HTA...ensionAttribute></d| act = lt_split_messages[ 3 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = |imensionAttributes><measures>| act = lt_split_messages[ 3 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 3 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_max_length.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( |      Adding a 'required for deploy' dependency to the file 'db://sap.bc.epm.oia::SNWD_BPA' (as extracted from the metadata header)| ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = |      Adding a 'required for deploy' dependency to| act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = | the file 'db://sap.bc.epm.oia::SNWD_BPA' (as extr| act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = |acted from the metadata header)| act = lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_spaces_only.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = |   | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_space_at_end.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = |    src/sap.bc.epm.oia/dfg/perf/AN_DATA_PROVIDER_PERFORMANCE.hdbcalculationview.properties (SEPM_OIA_RUNT) | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '    src/sap.bc.epm.oia/dfg/perf/AN_DATA_PROVIDER_P' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'ERFORMANCE.hdbcalculationview.properties (SEPM_OIA' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = '_RUNT)' act = lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_spaces_at_end.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = |      src/sap.bc.esh.nlq.CES715/query/config/nlq.hdbtextconfig                                 | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '      src/sap.bc.esh.nlq.CES715/query/config/nlq.h' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'dbtextconfig' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_spaces_at_end_2.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = |      src/sap.bc.esh.nlq.CES715/query/config/nlq.hdbtextconfig                                                                                                           | ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '      src/sap.bc.esh.nlq.CES715/query/config/nlq.h' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'dbtextconfig' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_initial( lt_split_messages[ 1 ]-var3 ).
  ENDMETHOD.

  METHOD split_message_test6.
    DATA(lt_split_messages) = lcl_log_helper=>split_message( iv_message = |      LIMU HOTO SAP.BC.EPM.OIA/SEPM_OIA_RUNT            SRC/SAP.BC.EPM.OIA//JLMLLP8X2S5RVQKITD39YGRYLJFG5D/TIONVIEW.PROPERTIES|
                                                                       && | --> src/sap.bc.epm.oia/apps/CA_GET_INPUT_PARAMETERS.hdbcalculationview.properties (SEPM_OIA_RUNT)| ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_split_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = '      LIMU HOTO SAP.BC.EPM.OIA/SEPM_OIA_RUNT' act = lt_split_messages[ 1 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = '            SRC/SAP.BC.EPM.OIA//JLMLLP8X2S5RVQKITD' act = lt_split_messages[ 1 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = '39YGRYLJFG5D/TIONVIEW.PROPERTIES -->' act = lt_split_messages[ 1 ]-var3 ).
    cl_abap_unit_assert=>assert_equals( exp = '        src/sap.bc.epm.oia/apps/CA_GET_INPUT_PARAM' act = lt_split_messages[ 2 ]-var1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'ETERS.hdbcalculationview.properties (SEPM_OIA_RUNT' act = lt_split_messages[ 2 ]-var2 ).
    cl_abap_unit_assert=>assert_equals( exp = ')' act = lt_split_messages[ 2 ]-var3 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_hot_logger_abstract DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO cl_cts_hot_logger_abstract.

    METHODS:
      setup RAISING cx_static_check,
      abnormal_with_no_previous FOR TESTING RAISING cx_static_check,
      abnormal_with_one_previous FOR TESTING RAISING cx_static_check,
      abnormal_with_two_previous FOR TESTING RAISING cx_static_check,
      abnormal_with_cts_exception FOR TESTING RAISING cx_static_check,
      error_with_no_previous FOR TESTING RAISING cx_static_check,
      error_with_one_previous FOR TESTING RAISING cx_static_check,
      error_with_two_previous FOR TESTING RAISING cx_static_check,
      error_with_cts_exception FOR TESTING RAISING cx_static_check,
      not_t100_exception_1 FOR TESTING RAISING cx_static_check,
      not_t100_exception_2 FOR TESTING RAISING cx_static_check,
      warning_with_cts_exception FOR TESTING RAISING cx_static_check,

      "! Test set_max_severity with iv_severity = any severity and cv_severity is already 'A'.<br/>
      "! cv_severity should stay a for any severity input.
      set_max_severity_a FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with iv_severity = any severity and cv_severity is 'E'.<br/>
      "! cv_severity should stay 'E' for any severity but change to 'A' if iv_severity = 'A'.
      set_max_severity_e FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with iv_severity = any severity and cv_severity is 'P'.<br/>
      "! cv_severity should stay 'P' for any severity but change to 'A' or 'E' if iv_severity = 'A' or 'E'.
      set_max_severity_p FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with iv_severity = any severity and cv_severity is 'W'.<br/>
      "! cv_severity should stay 'W' for any severity but change to 'A' or 'E' or 'P' if iv_severity = 'A' or 'E' or 'P'.
      set_max_severity_w FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with iv_severity = any severity and cv_severity is 'I'.<br/>
      "! cv_severity should change to 'W' or 'P' or 'E' or 'A' if iv_severity = 'W' or 'P' or 'E' or 'A'.
      set_max_severity_i FOR TESTING RAISING cx_static_check,
      "! Test set_max_severity with iv_severity = any severity and cv_severity is space.<br/>
      "! cv_severity should change to 'W' or 'P' or 'E' or 'A' if iv_severity = 'W' or 'P' or 'E' or 'A'.
      set_max_severity_space FOR TESTING RAISING cx_static_check.
ENDCLASS.

* Helper exception class
CLASS ltd_cx_helper_exception_class DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      ##NO_TEXT
      co_short_text TYPE string VALUE 'ABCD shorttext',
      ##NO_TEXT
      co_long_text  TYPE string VALUE '2 * ABCD longtext = ABCD longtext ABCD longtext'.

    METHODS:
      get_text     REDEFINITION,
      get_longtext REDEFINITION.
ENDCLASS.

CLASS ltd_cx_helper_exception_class IMPLEMENTATION.
  METHOD get_text.
    result = co_short_text.
  ENDMETHOD.

  METHOD get_longtext.
    result = co_long_text.
  ENDMETHOD.
ENDCLASS.

CLASS cl_cts_hot_logger_abstract DEFINITION LOCAL FRIENDS ltcl_hot_logger_abstract.
CLASS ltcl_hot_logger_abstract IMPLEMENTATION.

  METHOD setup.
    mr_cut = CAST cl_cts_hot_logger_abstract( cl_cts_hot_logger_memory=>create_instance( ) ).
  ENDMETHOD.

  METHOD abnormal_with_no_previous.
    DATA(lr_exc) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                                 msgv1 = 'text for var1'
                                                 msgv2 = 'text for var2'
                                                 msgv3 = 'text for var3'
                                                 msgv4 = 'text for var4' ).

    mr_cut->if_cts_hot_logger~abnormal_termination_exception( lr_exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.

  METHOD abnormal_with_one_previous.
    DATA(lr_exc1) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                                  msgv1 = 'text for var1'
                                                  msgv2 = 'text for var2'
                                                  msgv3 = 'text for var3'
                                                  msgv4 = 'text for var4' ).

    DATA(lr_exc2) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                                  msgv1 = 'text var1'
                                                  msgv2 = 'text var2'
                                                  hana_error_code = '12345'
                                                  hana_error_msg = 'error message'
                                                  previous = lr_exc1 ).
    mr_cut->if_cts_hot_logger~abnormal_termination_exception( lr_exc2 ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>read_package_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text var1'
                                                             var2 = 'text var2'
                                                             var3 = '12345'
                                                             var4 = 'error message' )
                                        act = mr_cut->mt_messages[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                        act = mr_cut->mt_messages[ 2 ] ).
  ENDMETHOD.


  METHOD abnormal_with_two_previous.
    DATA(lr_exc1) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                              msgv1 = 'text for var1'
                                              msgv2 = 'text for var2'
                                              msgv3 = 'text for var3'
                                              msgv4 = 'text for var4' ).

    DATA(lr_exc2) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                                  msgv1 = 'text var1'
                                                  msgv2 = 'text var2'
                                                  hana_error_code = '12345'
                                                  hana_error_msg = 'error message'
                                                  previous = lr_exc1 ).
    DATA(lr_exc3) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                              msgv1 = 'text var1'
                                              msgv2 = 'text var2'
                                              hana_error_code = '12345'
                                              hana_error_msg = 'error message'
                                              previous = lr_exc2 ).
    mr_cut->if_cts_hot_logger~abnormal_termination_exception( lr_exc3 ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>read_package_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text var1'
                                                             var2 = 'text var2'
                                                             var3 = '12345'
                                                             var4 = 'error message' )
                                        act = mr_cut->mt_messages[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>read_package_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text var1'
                                                             var2 = 'text var2'
                                                             var3 = '12345'
                                                             var4 = 'error message' )
                                                             act = mr_cut->mt_messages[ 2 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                        act = mr_cut->mt_messages[ 3 ] ).
  ENDMETHOD.


  METHOD error_with_no_previous.
    DATA(lr_exc) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                               msgv1 = 'text for var1'
                                               msgv2 = 'text for var2'
                                               msgv3 = 'text for var3'
                                               msgv4 = 'text for var4' ).

    mr_cut->if_cts_hot_logger~error_exception( lr_exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.


  METHOD error_with_one_previous.
    DATA(lr_exc1) = NEW cx_sql_exception( sql_message = 'error message with a long long long text exceeding 50 char and even more to make a really long long long exception message as never before' ).
    DATA(lr_exc2) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                                msgv1 = 'text for var1'
                                                msgv2 = 'text for var2'
                                                msgv3 = 'text for var3'
                                                msgv4 = 'text for var4'
                                                previous = lr_exc1 ).
    mr_cut->if_cts_hot_logger~error_exception( lr_exc2 ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                        act = mr_cut->mt_messages[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = 'SCTS_HDI'
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             msgnr = '100'
                                                             var1 = 'error message with a long long long text exceeding'
                                                             var2 = ' 50 char and even more to make a really long long'
                                                             var3 = ' long exception message as never' )
                                        act = mr_cut->mt_messages[ 2 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = 'SCTS_HDI'
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             msgnr = '100'
                                                             var1 = '   before')
                                        act = mr_cut->mt_messages[ 3 ] ).
  ENDMETHOD.


  METHOD error_with_two_previous.
    DATA(lr_exc1) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>cx_nhi_hana_repository_error
                                                  msgv1 = 'text for var1'
                                                  msgv2 = 'text for var2'
                                                  msgv3 = 'text for var3'
                                                  msgv4 = 'text for var4' ).

    DATA(lr_exc2) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                                  msgv1 = 'text var1'
                                                  msgv2 = 'text var2'
                                                  hana_error_code = '12345'
                                                  hana_error_msg = 'error message'
                                                  previous = lr_exc1 ).
    DATA(lr_exc3) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                                  msgv1 = 'text var1'
                                                  msgv2 = 'text var2'
                                                  hana_error_code = '12345'
                                                  hana_error_msg = 'error message'
                                                  previous = lr_exc2 ).
    mr_cut->if_cts_hot_logger~error_exception( lr_exc3 ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>read_package_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             var1 = 'text var1'
                                                             var2 = 'text var2'
                                                             var3 = '12345'
                                                             var4 = 'error message' )
                                                             act = mr_cut->mt_messages[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>read_package_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             var1 = 'text var1'
                                                             var2 = 'text var2'
                                                             var3 = '12345'
                                                             var4 = 'error message' )
                                                             act = mr_cut->mt_messages[ 2 ] ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgid
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             msgnr = cx_hana_object_transport=>cx_nhi_hana_repository_error-msgno
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             var1 = 'text for var1'
                                                             var2 = 'text for var2'
                                                             var3 = 'text for var3'
                                                             var4 = 'text for var4' )
                                                             act = mr_cut->mt_messages[ 3 ] ).
  ENDMETHOD.


  METHOD not_t100_exception_1.

    DATA(lr_exc_no_t100)  = NEW ltd_cx_helper_exception_class( ).
    DATA(lr_exc_hot_t100) = NEW cx_hana_object_transport( textid = cx_hana_object_transport=>read_package_error
                                                    msgv1 = 'text var1'
                                                    msgv2 = 'text var2'
                                                    hana_error_code = '12345'
                                                    hana_error_msg = 'error message'
                                                          previous        = lr_exc_no_t100 ).

*   1. test if_cts_hot_logger~error_exception for "T100 exception" and "no T100 exception"
    mr_cut->if_cts_hot_logger~error_exception( ix_exception = lr_exc_hot_t100 ).

    DATA(lt_expected_msgs) = VALUE sprot_u_t( ( ag = cx_hana_object_transport=>read_package_error-msgid
                                                               langu = sy-langu
                                                               level = if_cts_hot_logger=>co_level_2
                                                               msgnr = cx_hana_object_transport=>read_package_error-msgno
                                                               severity = if_cts_hot_logger=>co_severity_error
                                                               var1 = 'text var1'
                                                               var2 = 'text var2'
                                                               var3 = '12345'
                                                               var4 = 'error message' )
                                              ( ag = 'SCTS_HDI'
                                                langu = sy-langu
                                                level = if_cts_hot_logger=>co_level_2
                                                severity = if_cts_hot_logger=>co_severity_error
                                                msgnr = '100'
                                                var1 = ltd_cx_helper_exception_class=>co_short_text )
                                              ( ag = 'SCTS_HDI'
                                                                 langu = sy-langu
                                                                 level = if_cts_hot_logger=>co_level_2
                                                                 severity = if_cts_hot_logger=>co_severity_error
                                                msgnr = '100'
                                                var1 = ltd_cx_helper_exception_class=>co_long_text ) ).

    cl_abap_unit_assert=>assert_equals( exp = lt_expected_msgs act = mr_cut->mt_messages ).

*   2. test if_cts_hot_logger~warning_exception for "T100 exception" and "no T100 exception"
    CLEAR mr_cut->mt_messages.
    mr_cut->if_cts_hot_logger~warning_exception( ix_exception = lr_exc_hot_t100 ).

    lt_expected_msgs = VALUE sprot_u_t( ( ag       = cx_hana_object_transport=>read_package_error-msgid
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_3
                                          msgnr    = cx_hana_object_transport=>read_package_error-msgno
                                          severity = if_cts_hot_logger=>co_severity_warning
                                          var1     = 'text var1'
                                          var2     = 'text var2'
                                          var3     = '12345'
                                          var4     = 'error message' )
                                        ( ag       = 'SCTS_HDI'
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_3
                                          severity = if_cts_hot_logger=>co_severity_warning
                                          msgnr    = '100'
                                          var1     = ltd_cx_helper_exception_class=>co_short_text )
                                        ( ag       = 'SCTS_HDI'
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_3
                                          severity = if_cts_hot_logger=>co_severity_warning
                                          msgnr    = '100'
                                          var1     = ltd_cx_helper_exception_class=>co_long_text ) ).

    cl_abap_unit_assert=>assert_equals( exp = lt_expected_msgs act = mr_cut->mt_messages ).

*   3. test if_cts_hot_logger~abnormal_termination_exception for "T100 exception" and "no T100 exception"
    CLEAR mr_cut->mt_messages.
    mr_cut->if_cts_hot_logger~abnormal_termination_exception( ix_exception = lr_exc_hot_t100 ).

    lt_expected_msgs = VALUE sprot_u_t( ( ag       = cx_hana_object_transport=>read_package_error-msgid
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_2
                                          msgnr    = cx_hana_object_transport=>read_package_error-msgno
                                          severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                          var1     = 'text var1'
                                          var2     = 'text var2'
                                          var3     = '12345'
                                          var4     = 'error message' )
                                        ( ag       = 'SCTS_HDI'
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_2
                                          severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                          msgnr    = '100'
                                          var1     = ltd_cx_helper_exception_class=>co_short_text )
                                        ( ag       = 'SCTS_HDI'
                                          langu    = sy-langu
                                          level    = if_cts_hot_logger=>co_level_2
                                          severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                          msgnr    = '100'
                                          var1     = ltd_cx_helper_exception_class=>co_long_text ) ).

    cl_abap_unit_assert=>assert_equals( exp = lt_expected_msgs act = mr_cut->mt_messages ).


  ENDMETHOD.


  METHOD not_t100_exception_2.
    DATA: exc TYPE REF TO cx_cts_no_authority.
    TRY.
        cl_cts_authority=>check_request_action( trfunction = '0'
                                                action     = if_cts_authority=>co_req_edit ).
      CATCH cx_cts_no_authority INTO exc.    " Keine Berechtigung fuer diese Aktion
    ENDTRY.

    mr_cut->if_cts_hot_logger~error_exception( ix_exception = exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag = 'PU'
                                                             langu = sy-langu
                                                             level = if_cts_hot_logger=>co_level_2
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             msgnr = '628'
                                                             var1 = sy-uname
                                                             var2 = 'EDIT' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.

  METHOD set_max_severity_a.
    DATA(lv_severity) = 'A'.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_e.
    DATA(lv_severity) = 'E'.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_i.
    DATA(lv_severity) = 'I'.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = space ).

    lv_severity = 'I'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = space ).

    lv_severity = 'I'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    lv_severity = 'I'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    lv_severity = 'I'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = 'I'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_p.
    DATA(lv_severity) = 'P'.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = 'P'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_space.
    DATA(lv_severity) = space.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = space ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = space ).

    lv_severity = space. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    lv_severity = space. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    lv_severity = space. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = space. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD set_max_severity_w.
    DATA(lv_severity) = 'W'.
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = space
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'I'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'W'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'W' ).

    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'P'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'P' ).

    lv_severity = 'W'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'E'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'E' ).

    lv_severity = 'W'. "reset previous test
    cl_cts_hot_logger_abstract=>set_max_severity( EXPORTING iv_severity = 'A'
                                                  CHANGING  cv_severity = lv_severity ).
    cl_abap_unit_assert=>assert_equals( act = lv_severity exp = 'A' ).
  ENDMETHOD.

  METHOD error_with_cts_exception.
    DATA(lr_exc) = NEW cx_cts_xsa_rest_cli_exception( action        = 'PING(GET)'
                                                      http_code     = 200
                                                      http_response = 'this is the http response' ).
    mr_cut->if_cts_hot_logger~error_exception( lr_exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag       = 'HTA_HDI_WSM'
                                                             langu    = sy-langu
                                                             level    = if_cts_hot_logger=>co_level_2
                                                             severity = if_cts_hot_logger=>co_severity_error
                                                             msgnr    = '001'
                                                             var1     = 'PING(GET)'
                                                             var2     = '200'
                                                             var3     = 'this is the http response' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.

  METHOD abnormal_with_cts_exception.
    DATA(lr_exc) = NEW cx_cts_xsa_rest_cli_exception( action        = 'PING(GET)'
                                                      http_code     = 200
                                                      http_response = 'this is the http response' ).
    mr_cut->if_cts_hot_logger~abnormal_termination_exception( lr_exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag       = 'HTA_HDI_WSM'
                                                             langu    = sy-langu
                                                             level    = if_cts_hot_logger=>co_level_2
                                                             severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                                             msgnr    = '001'
                                                             var1     = 'PING(GET)'
                                                             var2     = '200'
                                                             var3     = 'this is the http response' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.

  METHOD warning_with_cts_exception.
    DATA(lr_exc) = NEW cx_cts_xsa_rest_cli_exception(
                          action        = 'PING(GET)'
                          http_code     = 200
                          http_response = 'this is the http response with more than 50 characters for var4' ).
    mr_cut->if_cts_hot_logger~warning_exception( lr_exc ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mr_cut->mt_messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE sprot_u( ag       = 'HTA_HDI_WSM'
                                                             langu    = sy-langu
                                                             level    = if_cts_hot_logger=>co_level_3
                                                             severity = if_cts_hot_logger=>co_severity_warning
                                                             msgnr    = '001'
                                                             var1     = 'PING(GET)'
                                                             var2     = '200'
                                                             var3     = 'this is the http response with more than 50 charac'
                                                             var4     = 'ters for var4' )
                                        act = mr_cut->mt_messages[ 1 ] ).
  ENDMETHOD.

ENDCLASS.