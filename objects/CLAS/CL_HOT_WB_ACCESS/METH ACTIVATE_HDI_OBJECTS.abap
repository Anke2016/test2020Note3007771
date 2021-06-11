  METHOD activate_hdi_objects.
    DATA lr_object TYPE REF TO cl_cts_hdi_object.

    CLEAR: et_log, et_successful_objects, ev_max_severity.

    IF it_objects IS INITIAL.
      RETURN.
    ENDIF.

    "in case of no HANA, move objects from ABAP_STATUS=I to A without real deployment
    IF NEW cl_cts_hdb_info( )->is_hdb( ) = abap_false.
      LOOP AT it_objects INTO lr_object.
        lr_object->save_as_deployed_successfully( ).
      ENDLOOP.
      et_successful_objects = it_objects.
      RETURN.
    ENDIF.

    DATA(lr_hot_logger) = cl_cts_hot_logger_memory=>create_instance( 'SCTS_HDI' ).
    DATA(lr_hdi_deployer) = NEW cl_cts_hta_hdi_rdd_obj_deploy( ).
    lr_hdi_deployer->deploy_objects( it_objects = it_objects
                                     ir_hot_logger = lr_hot_logger ).

    NEW cl_cts_hot_aftrburnr_scheduler( iv_broken_hdi_exists_before = NEW cl_cts_hdi_object_db_access( )->if_cts_hdi_object_db_access~exists_broken_object( )
                                        iv_broken_repo_exists_before = abap_false
                                      )->schedule_job( ir_logger = lr_hot_logger ).

    ev_max_severity = lr_hot_logger->get_max_severity( ).

    "find out all successfully deployed objects
    LOOP AT it_objects INTO lr_object.
      IF lr_object->ms_cts_hdi_object IS INITIAL OR lr_object->ms_cts_hdi_object-hot_status = if_cts_hdi_abap_types=>co_hot_status_active.
        INSERT lr_object INTO TABLE et_successful_objects.
      ENDIF.
    ENDLOOP.

    et_log = CAST cl_cts_hot_logger_memory( lr_hot_logger )->get_log_messages( ).
  ENDMETHOD.