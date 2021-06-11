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

  METHOD lif_db_access~read_objects_for_package.
    IF i_package_id IS NOT INITIAL.
      SELECT abap_hana_package_id abap_hana_object_name_suffix hot_status FROM cts_hot_object INTO TABLE r_result
                                                                            WHERE abap_hana_package_id = i_package_id
                                                                              AND abap_status = cl_cts_hta_component=>co_active_version.
    ENDIF.
  ENDMETHOD.

ENDCLASS.