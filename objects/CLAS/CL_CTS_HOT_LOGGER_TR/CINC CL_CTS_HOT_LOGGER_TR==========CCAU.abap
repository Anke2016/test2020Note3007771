CLASS ltcl_cts_hot_logger_tr DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      create_hdi_logfile_name FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hot_logger_tr IMPLEMENTATION.
  METHOD create_hdi_logfile_name.
    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '/usr/sap/trans/data/ABCC123456.EFG' )
      exp = 'ABCC123456_HDI.EFG'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '\usr\sap\trans\data\ABCC123456.EFG' )
      exp = 'ABCC123456_HDI.EFG'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '/usr/sap/trans/data\ABCC123456.EFG' )
      exp = 'ABCC123456_HDI.EFG'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '/usr/sap/trans\data/ABCC123456.EFG' )
      exp = 'ABCC123456_HDI.EFG'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '/usr/sap/trans/data/ABCC123456EFG' )
      exp = 'ABCC123456EFG_HDI'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( '\usr\sap\trans\data\ABCC123456EFG' )
      exp = 'ABCC123456EFG_HDI'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cl_cts_hot_logger_tr=>create_hdi_logfile_name( 'ABCC123456.EFG' )
      exp = 'ABCC123456_HDI.EFG'
    ).

  ENDMETHOD.

ENDCLASS.