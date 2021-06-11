CLASS ltcl_cts_hot_utility DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_hex_to_base32 FOR TESTING RAISING cx_static_check,
      test_string_to_hash_as_base32 FOR TESTING RAISING cx_static_check,
      test_hex_to_base41 FOR TESTING RAISING cx_static_check,
      test_string_to_hash_as_base41 FOR TESTING RAISING cx_static_check,

      test_split_34 FOR TESTING RAISING cx_static_check,
      test_split_50 FOR TESTING RAISING cx_static_check,
      test_split_51 FOR TESTING RAISING cx_static_check,
      test_split_100 FOR TESTING RAISING cx_static_check,
      test_split_101 FOR TESTING RAISING cx_static_check,
      test_split_150 FOR TESTING RAISING cx_static_check,
      test_split_151 FOR TESTING RAISING cx_static_check,
      test_split_200 FOR TESTING RAISING cx_static_check,
      test_split_201 FOR TESTING RAISING cx_static_check,

      get_formatted_timestamp FOR TESTING RAISING cx_static_check,
      conv_string_2_text_table FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hot_utility DEFINITION LOCAL FRIENDS ltcl_cts_hot_utility.
CLASS ltcl_cts_hot_utility IMPLEMENTATION.

  METHOD test_hex_to_base32.

    cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000000000'
                                        act = cl_cts_hot_utility=>hex_to_base32( '0000000000000000000000000000000000000000' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '00VS000000000000000000000000000A'
                                        act = cl_cts_hot_utility=>hex_to_base32( '003FC0000000000000000000000000000000000A' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'R8SQ7RIUDD5GQCILNVNPAO0OI2NTG1O9'
                                        act = cl_cts_hot_utility=>hex_to_base32( 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '0EJ6BEEUSVB5IVB83VI1OASQLIF2ID0R'
                                        act = cl_cts_hot_utility=>hex_to_base32( '03A665B9DEE7D6597D681FE41C2B9AAC9E29341B' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'JDJM0M97ONH89BEMKH85CHKPVBFD054B'
                                        act = cl_cts_hot_utility=>hex_to_base32( '9B67605927C5E284ADD6A450564699FADED0148B' ) ).

  ENDMETHOD.

  METHOD test_string_to_hash_as_base32.
    "expected base32 values were taken from online converter tools in the internet (first sha1 converter and then abase16 to base32 converter)

    "some dummy names
    cl_abap_unit_assert=>assert_equals( exp = 'GRRU8DVQKMJVPOAT3NEBJQNAT8RNCPTO'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'a' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'KVN3HERRSJU486CCM9K5R5G1RJPBJTB9'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'K' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'LOOKACGT42SP5PRSDCHCA8LLOM6U70MS'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'hUgO' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '0EJ6BEEUSVB5IVB83VI1OASQLIF2ID0R'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'com.sap.some-long-package.id' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '856IDLQGKH3BIISBSGUMPCPF7J185D28'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'COM.SAP.SOME-LONG-PACKAGE.ID' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '42G7PR85BC5I71PJU69HN61MDVLTRVHG'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'COM.SAP.HUGO.IS.A.BOXER.AND.SOME.OTHER.NAMES.ARE.HERE' ) ).

    "real HANA package names from HPAs
    cl_abap_unit_assert=>assert_equals( exp = 'RT0S30LBVIEIJ5HV2VCNK1T2S9BS57R0'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SNSIN7K57VQQRMC27HPVM7DRIM0KH3JA'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.hana-app.cuan.cpred.hrf.ANA_HRF.".settings"' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'G1IKOS8LD2BN5RC8BT2MKSU8AN043I5G'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'SAP.HANA-APP.CUAN.CPRED.HRF.ANA_HRF.".SETTINGS"' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '75D9KVSAM6TQAEB7CFRJST1P4TB90ILJ'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.hana-app.mo.public.logic.HandleEmailSending' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SM04LG9F98117NJCPJ3805OL94VUHTTK'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'SAP.HANA-APP.MO.PUBLIC.LOGIC.HANDLEEMAILSENDING' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'UGM1NRT804PG32JPUGILG7RKCJG5M7SB'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.hana-app.mo.public.logic.HandleEmailSending.".settings"' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '78BMB38STUBQROSEVCRCPVHGI7068T76'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.hana-app.prodreco.algorithm.association.apriori.modelgeneration' ) ).

    "real HANA object names from HPAs
    cl_abap_unit_assert=>assert_equals( exp = 'O80N5UQ3F1V91DJJRVV3E2AIOSDQU864'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'EVALUATION_TEXTS_7E072F64C0696A60458F542EEE38A8AFB862D473D25E18929CAE63585C3455FC.hdbti' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'URA52PS4O6U3PFP415DSJINLCKIE6SJS'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'EVALUATION_FILTERS_7CE07A38B956FBEF0F862C7FC1FD67E1EB6162BAEDAE7E6CF72CA800374F4C67.csv' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '4GI4VC7JLSE2GGTQIHV6JQ7VV9QRMCNG'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'EVALUATION_FILTERS_7CE07A38B956FBEF0F862C7FC1FD67E1EB6162BAEDAE7E6CF72CA800374F4C67.CSV' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '4Q3KRVNL8E8GRRSD25LFHFERI8IJP2C4'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'sap.ushell.components.tiles.indicatorTileUtils.odata4analytics.SortOrder.html' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'AMF0JCMV7KDDULPQ9AEHFMR1FV1S95M9'
                                        act = cl_cts_hot_utility=>string_to_hash_as_base32( 'CashLiquidityAnalysisNewOriginalOnAccountingForecast.calculationview' ) ).

  ENDMETHOD.

  METHOD test_hex_to_base41.
    cl_abap_unit_assert=>assert_equals( exp = '000000000000000000000000000000'
                                        act = cl_cts_hot_utility=>hex_to_base41( '0000000000000000000000000000000000000000' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '000000000000000000000000000Q6T'
                                        act = cl_cts_hot_utility=>hex_to_base41( '000000000000000000000000000000000000ABCD' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000A614'
                                        act = cl_cts_hot_utility=>hex_to_base41( '00000000000000000000000000000000000AABCD' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '000000000000000000000000OTBY8O'
                                        act = cl_cts_hot_utility=>hex_to_base41( '00000000000000000000000000000000AAAAAAAA' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '000000000000000000000000PJ1462'
                                        act = cl_cts_hot_utility=>hex_to_base41( '00000000000000000000000000000000AFD80709' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '0LRKT8WBI7-HI6NEON43AG-NM_1O=0'
                                        act = cl_cts_hot_utility=>hex_to_base41( '03A665B9DEE7D6597D681FE41C2B9AAC9E29341B' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'MKRIEC5V5X82P74.5HCK9YATWA-OS9'
                                        act = cl_cts_hot_utility=>hex_to_base41( '9B67605927C5E284ADD6A450564699FADED0148B' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '_2.AC-_2.AC-_2.AC-_2.AC-_2.AC-'
                                        act = cl_cts_hot_utility=>hex_to_base41( 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' ) ).

  ENDMETHOD.

  METHOD test_string_to_hash_as_base41.
    "expected base41 values were taken from online converter tools in the internet (first sha1 converter and then base16 to base41 converter)

    "possible real HDI namespace
    cl_abap_unit_assert=>assert_equals( exp = 'SM43QDL-0HPW8.O8O6NHTN;CRMROKE' "SHA1: c513ffa0971505633dbe88eca1d018b0be44e566
                                        act = cl_cts_hot_utility=>string_to_hash_as_base41( 'sap.hana-app.cuan.cpred/SAP_CUAN_CPRED_RUNTIME' ) ).

    "possible real HDI object names from HPAs
    cl_abap_unit_assert=>assert_equals( exp = 'VO=M1XHC0VQQKWLS.A6RY_Y7I-=VC6' "SHA1: da46fd5e776b41388f97197d2e1fdb318286e8f6
                                        act = cl_cts_hot_utility=>string_to_hash_as_base41( 'src/sap.hana-app.cuan/texts/EVALUATION_TEXTS_7E072F64C0696A60458F542EEE38A8AFB862D473D25E18929CAE63585C3455FC.hdbti' ) ).

  ENDMETHOD.

  METHOD test_split_100.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_101.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabc' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'c' act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_150.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_151.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQ' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'Q' act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_200.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars(
                         '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCD' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'QRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCD' act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_201.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars(
                        '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCDE' ).

    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'opqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZab' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'cdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP' act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'QRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890ABCD' act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_34.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWX' ).
    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWX' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_50.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars( '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' ).
    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD test_split_51.
    DATA(ls_split) = cl_cts_hot_utility=>split_text_50_chars('1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmno' ).
    cl_abap_unit_assert=>assert_equals( exp = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn' act = ls_split-chunk1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'o' act = ls_split-chunk2 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk3 ).
    cl_abap_unit_assert=>assert_initial( act = ls_split-chunk4 ).
  ENDMETHOD.

  METHOD get_formatted_timestamp.
    DATA(lv_timestamp) = cl_cts_hot_utility=>get_formatted_timestamp( '20180125143358' ).
    cl_abap_unit_assert=>assert_equals( exp = '2018-01-25 14:33:58' act = lv_timestamp ).
  ENDMETHOD.

  METHOD conv_string_2_text_table.

    TYPES lty_char5 TYPE c LENGTH 5.

    DATA:
      lt_text_actual   TYPE STANDARD TABLE OF lty_char5,
      lt_text_expected TYPE STANDARD TABLE OF lty_char5.

    cl_cts_hot_utility=>convert_string_to_text_table(
      EXPORTING
        iv_buffer  = to_lower( sy-abcde )
      IMPORTING
        et_texttab = lt_text_actual
    ).

    lt_text_expected = VALUE #( ( 'abcde' ) ( 'fghij' ) ( 'klmno' ) ( 'pqrst' ) ( 'uvwxy' ) ( 'z' ) ).

    cl_abap_unit_assert=>assert_equals(
      msg = 'Text table should be filled'
      exp = lt_text_expected
      act = lt_text_actual ).

  ENDMETHOD.

ENDCLASS.