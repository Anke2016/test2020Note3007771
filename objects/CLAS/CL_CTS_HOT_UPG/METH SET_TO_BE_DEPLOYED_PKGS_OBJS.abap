  METHOD set_to_be_deployed_pkgs_objs.
    DATA: lt_packages_deleted_in_v2  TYPE STANDARD TABLE OF cts_hot_package-abap_hana_package_id,
          lt_objects_deleted_in_v2   TYPE STANDARD TABLE OF cts_hot_object-abap_hana_package_id,
          lt_packages_changed_in_v2  TYPE STANDARD TABLE OF cts_hot_package-abap_hana_package_id,
          lt_objects_changed_in_v2   TYPE STANDARD TABLE OF cts_hot_object-abap_hana_package_id,
          lv_objects_differ_hta_hana TYPE abap_bool,
          lv_updated_packs           TYPE i,
          lv_updated_objs            TYPE i.

    TRY.
        "find packages that were deleted in upgrade and need to be recreated
        me->execute_query(
          EXPORTING iv_query = |SELECT PACK.ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_PACKAGE" AS PACK LEFT OUTER JOIN "CTS_HOT_PACKAGE~BCK" AS PACK_BCK | &&
                                                      | ON PACK.ABAP_HANA_PACKAGE_ID = PACK_BCK.ABAP_HANA_PACKAGE_ID | &&
                                                      |   AND PACK.ABAP_STATUS = PACK_BCK.ABAP_STATUS | &&
                                                      | WHERE PACK_BCK.ABAP_HANA_PACKAGE_ID IS NULL | &&
                                                      |   AND PACK.ABAP_STATUS = 'A'|
          IMPORTING et_result = lt_packages_deleted_in_v2 ).

        IF lt_packages_deleted_in_v2 IS INITIAL.
          "find packages that were changed in upgrade and need to be changed to V1
          me->execute_query(
            EXPORTING iv_query = |SELECT PACK.ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_PACKAGE" AS PACK JOIN "CTS_HOT_PACKAGE~BCK" AS PACK_BCK | &&
                                                        | ON PACK.ABAP_HANA_PACKAGE_ID = PACK_BCK.ABAP_HANA_PACKAGE_ID | &&
                                                        |   AND PACK.ABAP_STATUS = PACK_BCK.ABAP_STATUS | &&
                                                        | WHERE PACK.ABAP_STATUS = 'A' | &&
                                                        |   AND ( PACK.HANA_PACK_DESCRIPTION <> PACK_BCK.HANA_PACK_DESCRIPTION | &&
                                                        |      OR PACK.HANA_PACK_RESPONSIBLE <> PACK_BCK.HANA_PACK_RESPONSIBLE | &&
                                                        |      OR PACK.HANA_PACK_ORIG_LANG <> PACK_BCK.HANA_PACK_ORIG_LANG | &&
                                                        |      OR PACK.HANA_PACK_IS_STRUCTURAL <> PACK_BCK.HANA_PACK_IS_STRUCTURAL | &&
                                                        |      OR PACK.HANA_PACK_TEXT_COLLECTION <> PACK_BCK.HANA_PACK_TEXT_COLLECTION | &&
                                                        |      OR PACK.HANA_PACK_TEXT_STATUS <> PACK_BCK.HANA_PACK_TEXT_STATUS | &&
                                                        |      OR PACK.HANA_PACK_TEXT_TERM_DOMAIN <> PACK_BCK.HANA_PACK_TEXT_TERM_DOMAIN | &&
                                                        |      OR PACK.HANA_PACK_HINTS_FOR_TRANSL <> PACK_BCK.HANA_PACK_HINTS_FOR_TRANSL )|
            IMPORTING et_result = lt_packages_changed_in_v2 ).
        ENDIF.

        "if any package was changed during upgrade, set all packages to "to be deployed" (HOT_STATUS = 'I')
        IF lt_packages_deleted_in_v2 IS NOT INITIAL
              OR lt_packages_changed_in_v2 IS NOT INITIAL.
          UPDATE cts_hot_package SET hot_status = 'I' WHERE abap_status = 'A' AND hot_status NOT IN ( 'D', 'Z' ).
          lv_updated_packs = sy-dbcnt.
        ENDIF.

        "find objects that were deleted in upgrade and need to be recreated
        me->execute_query(
          EXPORTING iv_query = |SELECT OBJ.ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_OBJECT" AS OBJ LEFT OUTER JOIN "CTS_HOT_OBJECT~BCK" AS OBJ_BCK | &&
                                                      | ON OBJ.ABAP_HANA_PACKAGE_ID = OBJ_BCK.ABAP_HANA_PACKAGE_ID | &&
                                                      |   AND OBJ.ABAP_HANA_OBJECT_NAME_SUFFIX = OBJ_BCK.ABAP_HANA_OBJECT_NAME_SUFFIX | &&
                                                      |   AND OBJ.ABAP_STATUS = OBJ_BCK.ABAP_STATUS | &&
                                                      | WHERE OBJ_BCK.ABAP_HANA_PACKAGE_ID IS NULL | &&
                                                      |   AND OBJ.ABAP_STATUS = 'A'|
          IMPORTING et_result = lt_objects_deleted_in_v2 ).

        IF lt_objects_deleted_in_v2 IS INITIAL.
          TRY.
              "find objects that were changed in upgrade and need to be changed to V1
              me->execute_query(
                EXPORTING iv_query = |SELECT OBJ.ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_OBJECT" AS OBJ JOIN "CTS_HOT_OBJECT~BCK" AS OBJ_BCK | &&
                                                            | ON OBJ.ABAP_HANA_PACKAGE_ID = OBJ_BCK.ABAP_HANA_PACKAGE_ID | &&
                                                            |   AND OBJ.ABAP_HANA_OBJECT_NAME_SUFFIX = OBJ_BCK.ABAP_HANA_OBJECT_NAME_SUFFIX | &&
                                                            |   AND OBJ.ABAP_STATUS = OBJ_BCK.ABAP_STATUS | &&
                                                            | WHERE OBJ.ABAP_STATUS = 'A' | &&
                                                            |   AND ( HASH_SHA256(TO_BINARY(OBJ.HANA_CONTENT_BDATA)) <> HASH_SHA256(TO_BINARY(OBJ_BCK.HANA_CONTENT_BDATA)) | &&
                                                            |      OR HASH_SHA256(TO_BINARY(OBJ.HANA_CONTENT_CDATA)) <> HASH_SHA256(TO_BINARY(OBJ_BCK.HANA_CONTENT_CDATA)) )|
                IMPORTING et_result = lt_objects_changed_in_v2 ).
            CATCH cx_sql_exception INTO DATA(lr_exc_seldom).
              IF lr_exc_seldom->sql_code <> 384. "string is too long: length can't exceed maximum length(16384bytes):  at function to_binary()
                RAISE EXCEPTION lr_exc_seldom.
              ENDIF.
              "we are on very old HANA very to_binary() is limited. try another SQL
              me->execute_query(
                EXPORTING iv_query = |SELECT OBJ.ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_OBJECT" AS OBJ JOIN "CTS_HOT_OBJECT~BCK" AS OBJ_BCK | &&
                                                            | ON OBJ.ABAP_HANA_PACKAGE_ID = OBJ_BCK.ABAP_HANA_PACKAGE_ID | &&
                                                            |   AND OBJ.ABAP_HANA_OBJECT_NAME_SUFFIX = OBJ_BCK.ABAP_HANA_OBJECT_NAME_SUFFIX | &&
                                                            |   AND OBJ.ABAP_STATUS = OBJ_BCK.ABAP_STATUS | &&
                                                            | WHERE OBJ.ABAP_STATUS = 'A' | &&
                                                            |   AND ( HASH_SHA256(TO_BINARY(OBJ.HANA_CONTENT_BDATA)) <> HASH_SHA256(TO_BINARY(OBJ_BCK.HANA_CONTENT_BDATA)) | &&
                                                            |      OR OBJ.HANA_CONTENT_CDATA NOT LIKE OBJ_BCK.HANA_CONTENT_CDATA )|
                IMPORTING et_result = lt_objects_changed_in_v2 ).
          ENDTRY.

          IF lt_objects_changed_in_v2 IS INITIAL.
            "find objects that have different version and different content in HANA and in CTS_HOT_OBJECT
            IF me->check_object_versions( iv_all_objects_deployed = abap_true
                                          iv_check_consistency = abap_false ) = abap_true.
              lv_objects_differ_hta_hana = abap_false.
            ELSE.
              lv_objects_differ_hta_hana = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lt_objects_deleted_in_v2 IS NOT INITIAL
              OR lt_objects_changed_in_v2 IS NOT INITIAL
              OR lv_objects_differ_hta_hana = abap_true.
          UPDATE cts_hot_object SET hot_status = 'I' WHERE abap_status = 'A' AND hot_status NOT IN ( 'D', 'Z' ). "#EC CI_NOFIRST
          lv_updated_objs = sy-dbcnt.
        ENDIF.

        mr_logger->message( iv_msg_nr = '420' "  &1 Pakete und &2 Objekte im HTA-Repository als zu deployen gesetzt
                            iv_level = 3
                            iv_var1 = |{ lv_updated_packs }|
                            iv_var2 = |{ lv_updated_objs }| ).
      CATCH cx_sql_exception INTO DATA(lr_exc).
        mr_logger->abnormal_termination_exception( lr_exc ).
        RAISE EXCEPTION TYPE lcx_error_prepare_redeployment.
    ENDTRY.
  ENDMETHOD.