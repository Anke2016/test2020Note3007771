  METHOD create_instance_from_objname.
    DATA: lv_hana_package_id TYPE string.
    SELECT SINGLE hana_package_id FROM cts_hot_package INTO lv_hana_package_id WHERE abap_hana_package_id = iv_objname AND abap_status = iv_abap_status. " case problematic not solved
    CREATE OBJECT r_result
      EXPORTING
        iv_hana_package_id      = lv_hana_package_id
        iv_abap_hana_package_id = CONV #( iv_objname ).
  ENDMETHOD.