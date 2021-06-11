  METHOD add_to_be_deleted_pkgs_objs.
    DATA lt_packages TYPE STANDARD TABLE OF cts_hot_package.
    DATA lt_objects TYPE STANDARD TABLE OF cts_hot_object.
    DATA lt_prework_pkgs TYPE STANDARD TABLE OF cts_hot_prework.

    TRY.
        me->execute_query(
          EXPORTING iv_query = |SELECT PACK_BCK.* FROM "CTS_HOT_PACKAGE~BCK" AS PACK_BCK LEFT OUTER JOIN "CTS_HOT_PACKAGE" AS PACK | &&
                                                          | ON PACK_BCK.ABAP_HANA_PACKAGE_ID = PACK.ABAP_HANA_PACKAGE_ID | &&
                                                          |   AND PACK_BCK.ABAP_STATUS = PACK.ABAP_STATUS | &&
                                                          | WHERE PACK.ABAP_HANA_PACKAGE_ID IS NULL | &&
                                                          |   AND PACK_BCK.ABAP_STATUS = 'A'|
          IMPORTING et_result = lt_packages ).

        GET TIME STAMP FIELD DATA(lv_timestamp).
        MODIFY lt_packages FROM VALUE #( hot_status = 'D' abap_synced_by = 'HTA_REVOKE' abap_synced_at = lv_timestamp )
                           TRANSPORTING hot_status abap_synced_by abap_synced_at WHERE hot_status <> 'D'.
        INSERT cts_hot_package FROM TABLE lt_packages.

        lt_prework_pkgs = VALUE #( FOR <fs> IN lt_packages WHERE ( hot_activation_mode = 'P' ) ( abap_hana_package_id = <fs>-abap_hana_package_id prework_done = 'X' ) ).
        MODIFY cts_hot_prework FROM TABLE lt_prework_pkgs.

        me->execute_query(
          EXPORTING iv_query = |SELECT OBJ_BCK.* FROM "CTS_HOT_OBJECT~BCK" AS OBJ_BCK LEFT OUTER JOIN "CTS_HOT_OBJECT" AS OBJ | &&
                                                          | ON OBJ_BCK.ABAP_HANA_PACKAGE_ID = OBJ.ABAP_HANA_PACKAGE_ID | &&
                                                          |   AND OBJ_BCK.ABAP_HANA_OBJECT_NAME_SUFFIX = OBJ.ABAP_HANA_OBJECT_NAME_SUFFIX | &&
                                                          |   AND OBJ_BCK.ABAP_STATUS = OBJ.ABAP_STATUS | &&
                                                          | WHERE OBJ.ABAP_HANA_PACKAGE_ID IS NULL | &&
                                                          |   AND OBJ_BCK.ABAP_STATUS = 'A'|
          IMPORTING et_result = lt_objects ).

        GET TIME STAMP FIELD lv_timestamp.
        MODIFY lt_objects FROM VALUE #( hot_status = 'D' abap_synced_by = 'HTA_REVOKE' abap_synced_at = lv_timestamp )
                          TRANSPORTING hot_status abap_synced_by abap_synced_at WHERE hot_status <> 'D'.
        INSERT cts_hot_object FROM TABLE lt_objects.

        mr_logger->message( iv_msg_nr = '419' "  &1 Pakete und &2 Objekte im HTA-Repository als zu löschen erstellt
                            iv_level = 3
                            iv_var1 = |{ lines( lt_packages ) }|
                            iv_var2 = |{ lines( lt_objects ) }| ).
      CATCH cx_sql_exception INTO DATA(lr_exc).
        mr_logger->abnormal_termination_exception( lr_exc ).
        RAISE EXCEPTION TYPE lcx_error_prepare_redeployment.
    ENDTRY.
  ENDMETHOD.