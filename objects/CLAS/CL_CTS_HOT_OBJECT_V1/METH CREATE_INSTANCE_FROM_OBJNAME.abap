  METHOD create_instance_from_objname.

    DATA: ls_cts_hot_object TYPE cts_hot_object.

    SELECT SINGLE hana_package_id hana_object_name hana_object_suffix FROM cts_hot_object
            INTO corresponding fields of ls_cts_hot_object
            WHERE abap_hana_package_id = iv_objname(40) AND abap_hana_object_name_suffix = iv_objname+40(70) AND
                  abap_status = iv_abap_status.
*   If no entry found (e.g. delete case) fields of ls_cts_hot_object are all empty, which is ok.

    CREATE OBJECT r_result
      EXPORTING
        iv_abap_hana_package_id        = iv_objname(40)
        iv_abap_hana_object_name_suffx = iv_objname+40(70)
        iv_hana_package_id             = ls_cts_hot_object-hana_package_id
        iv_hana_object_name            = ls_cts_hot_object-hana_object_name
        iv_hana_object_suffix          = ls_cts_hot_object-hana_object_suffix.

  ENDMETHOD.