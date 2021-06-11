*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_hana_deployer DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_hana_deployer.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_hana_deployer IMPLEMENTATION.

  METHOD lif_hana_deployer~execute_deployment.
    PERFORM deploy IN PROGRAM rddhanadeployment USING i_packages i_objects i_abap_status CHANGING e_successful_packages e_successful_objects e_deploy_messages e_max_severity.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_hta_db_access DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_db_access.
ENDCLASS.

CLASS lcl_hta_db_access IMPLEMENTATION.

  METHOD lif_db_access~prepare_force_deploy_of_pkg.
    UPDATE cts_hot_package SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = i_package->abap_hana_package_id
                                                                                           AND hot_status =  if_cts_hot_db_access=>co_hot_status_active
                                                                                           AND abap_status = cl_cts_hta_component=>co_active_version.
  ENDMETHOD.

  METHOD lif_db_access~prepare_force_deploy_of_obj.
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = i_object->abap_hana_package_id
                                                                                           AND abap_hana_object_name_suffix = i_object->abap_hana_object_name_suffix
                                                                                           AND hot_status =  if_cts_hot_db_access=>co_hot_status_active
                                                                                           AND abap_status = cl_cts_hta_component=>co_active_version.

    UPDATE cts_hot_otexts_s SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_object_reference =
                                          ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = i_object->abap_hana_package_id
                                                                                               AND abap_hana_object_name_suffix = i_object->abap_hana_object_name_suffix
                                                                                               AND abap_status = cl_cts_hta_component=>co_active_version )
                                                                                            AND hot_status = if_cts_hot_db_access=>co_hot_status_active.

    UPDATE cts_hot_otexts_l SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_object_reference =
                                          ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = i_object->abap_hana_package_id
                                                                                               AND abap_hana_object_name_suffix = i_object->abap_hana_object_name_suffix
                                                                                               AND abap_status = cl_cts_hta_component=>co_active_version )
                                                                                            AND hot_status = if_cts_hot_db_access=>co_hot_status_active.
  ENDMETHOD.

ENDCLASS.