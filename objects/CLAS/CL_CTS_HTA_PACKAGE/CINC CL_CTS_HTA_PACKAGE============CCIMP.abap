*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_tadir_access DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_tadir_access.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_tadir_access IMPLEMENTATION.

  METHOD lif_tadir_access~read_hota_objnames_for_devclas.
    IF i_devclasses IS NOT INITIAL. "otherwise whole tadir is returned
      SELECT obj_name FROM tadir INTO TABLE r_result FOR ALL ENTRIES IN i_devclasses WHERE pgmid = if_cts_hta_full_package=>co_pgmid
                                                                                       AND object = if_cts_hta_full_package=>co_object_type
                                                                                       AND devclass = i_devclasses-table_line.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_db_access DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_db_access.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_db_access IMPLEMENTATION.

  METHOD lif_db_access~read_packages.
    IF i_sobj_names IS NOT INITIAL. "otherwise whole cts_hot_package is returned
      IF i_deployable_only = abap_true.
        SELECT abap_hana_package_id FROM cts_hot_package INTO TABLE r_result FOR ALL ENTRIES IN i_sobj_names
                                                                              WHERE abap_hana_package_id = i_sobj_names-table_line
                                                                                AND abap_status = cl_cts_hta_component=>co_active_version
                                                                                AND ( hot_status = if_cts_hot_db_access=>co_hot_status_inactive
                                                                                    OR hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                                                                                    OR hot_status = if_cts_hot_db_access=>co_hot_status_deploy_error
                                                                                    OR hot_status = if_cts_hot_db_access=>co_hot_status_delete_error ).
      ELSE.
        SELECT abap_hana_package_id FROM cts_hot_package INTO TABLE r_result FOR ALL ENTRIES IN i_sobj_names
                                                                              WHERE abap_hana_package_id = i_sobj_names-table_line
                                                                                AND abap_status = cl_cts_hta_component=>co_active_version.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_db_access~set_prework.
    DATA: ls_cts_hot_prework TYPE cts_hot_prework,
          lv_deploy_mode     TYPE cts_hot_activation_mode.

    SELECT SINGLE hot_activation_mode FROM cts_hot_package INTO lv_deploy_mode WHERE abap_hana_package_id = i_abap_hana_package_id.
    "Set prework done only for packages with deploy mode = P.
    IF lv_deploy_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework.
      ls_cts_hot_prework-abap_hana_package_id = i_abap_hana_package_id.
      ls_cts_hot_prework-prework_done = i_prework->value.

      MODIFY cts_hot_prework FROM ls_cts_hot_prework.
    ENDIF.
  ENDMETHOD.

  METHOD lif_db_access~find_hta_package_names.
    SELECT abap_hana_package_id FROM cts_hot_package INTO TABLE r_result WHERE hana_package_id LIKE i_hana_package_name and abap_status = 'A'.
  ENDMETHOD.

ENDCLASS.