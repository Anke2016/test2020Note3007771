  METHOD analyze_activation_response.
    CONSTANTS: co_finished_activation TYPE string VALUE 'Finished activation phase.' ##NO_TEXT.

    DATA: lr_hana_2_hot_2_nhi       TYPE REF TO ty_map_hana_2_hot_2_nhi_obj,
          lr_cts_hot_object_unknown TYPE REF TO cl_cts_hot_object_v1,
          lr_check_result           TYPE REF TO cl_nhi_check_result.

    CLEAR: e_activation_success, e_failed_objects, e_successful_objects.

* General remark: same handling for error code <> 0 and error_code = 0 is intended because also error code 40136 could be success if
* check result with text 'Finished activation phase.' is found. In success case, 40136 often means that at least 1 object faile during regeneration

* 1. Find out activation success/failure and collect all failed objects
    IF i_activation_response->error_code = '0'.
      e_activation_success = abap_true.
    ELSE.
      IF i_activation_response->checkresults IS INITIAL.
        "if no messages are returned, all objects are failed objects
        e_failed_objects = i_map_hana_2_hot_2_nhi_obj.
      ENDIF.

      "1.) add all known failed objects of this run to e_failed_objects
      " and all unknown failed object to e_unknown_failed_objects.
      LOOP AT i_activation_response->checkresults INTO lr_check_result.
        IF lr_check_result->error_code = '40137'.
          FIND FIRST OCCURRENCE OF co_finished_activation IN lr_check_result->error_msg.
          IF sy-subrc = 0.
            e_activation_success = abap_true.
            EXIT. "end loop because activation is finished with this line.
          ENDIF.
        ENDIF.
        "all check results with objects and severity 3 are failed_objects, add them to e_failed_objects or to e_unknown_failed_objects
        IF lr_check_result->severity = '3' AND lr_check_result->object IS BOUND AND
            ( lr_check_result->object->name IS NOT INITIAL OR lr_check_result->object->suffix IS NOT INITIAL ) .
          READ TABLE i_map_hana_2_hot_2_nhi_obj
                      WITH TABLE KEY package_id = lr_check_result->object->package
                                     object_name = lr_check_result->object->name
                                     object_suffix = lr_check_result->object->suffix
                      REFERENCE INTO lr_hana_2_hot_2_nhi.
          IF sy-subrc = 0.
            INSERT lr_hana_2_hot_2_nhi->* INTO TABLE e_failed_objects.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

* 2. Map HANA logs to HTA logs for all objects. For OK objects log only the info that it was processed as the detailed log comes with next activation attempt.
*    Add all found objects not yet part of e_failed_objects to e_successful_objects
    LOOP AT i_activation_response->checkresults INTO lr_check_result.
      IF lr_check_result->object IS BOUND AND
          ( lr_check_result->object->name IS NOT INITIAL OR lr_check_result->object->suffix IS NOT INITIAL ).
        "read hot object so that each log for this object has this object as hot object.
        READ TABLE i_map_hana_2_hot_2_nhi_obj
                      WITH TABLE KEY package_id = lr_check_result->object->package
                                     object_name = lr_check_result->object->name
                                     object_suffix = lr_check_result->object->suffix
                      REFERENCE INTO lr_hana_2_hot_2_nhi.
        IF sy-subrc = 0. "known object
          IF e_activation_success = abap_true.
            append_log_message( EXPORTING i_severity = lr_check_result->severity i_error_code = lr_check_result->error_code i_message = lr_check_result->error_msg
                                          i_hot_object = lr_hana_2_hot_2_nhi->hot_object i_timestamp = lr_check_result->timestamp i_location = lr_check_result->location
                                CHANGING  c_logs = c_log_messages ).

            INSERT lr_hana_2_hot_2_nhi->* INTO TABLE e_successful_objects.
          ELSE. "activation was not successful
            IF line_exists( e_failed_objects[ package_id = lr_hana_2_hot_2_nhi->package_id
                                              object_name = lr_hana_2_hot_2_nhi->object_name
                                              object_suffix = lr_hana_2_hot_2_nhi->object_suffix ] ).
              append_log_message( EXPORTING i_severity = lr_check_result->severity i_error_code = lr_check_result->error_code i_message = lr_check_result->error_msg
                                            i_hot_object = lr_hana_2_hot_2_nhi->hot_object i_timestamp = lr_check_result->timestamp i_location = lr_check_result->location
                                  CHANGING  c_logs = c_log_messages ).
            ELSEIF NOT line_exists( e_successful_objects[ package_id = lr_hana_2_hot_2_nhi->package_id
                                                          object_name = lr_hana_2_hot_2_nhi->object_name
                                                          object_suffix = lr_hana_2_hot_2_nhi->object_suffix ] ).
              "obj was OK and can be processed in next run again. map to internal hot log message
              "only append log if NOT yet appended.
              append_log_message( EXPORTING i_severity = '1111' i_error_code = '0' i_is_hana_message = abap_false
                                            i_message = |{ lr_check_result->object->name }.{ lr_check_result->object->suffix } ({ lr_check_result->object->package }) would be OK. Process again.| "no text symbol because of 256 chars each name!
                                            i_hot_object = lr_hana_2_hot_2_nhi->hot_object
                                  CHANGING  c_logs = c_log_messages ).
              INSERT lr_hana_2_hot_2_nhi->* INTO TABLE e_successful_objects.
            ENDIF.
          ENDIF.
        ELSE. "unknown object (maybe revalidation or implicitely added object by HANA repo during activation)
          READ TABLE c_map_hana_2_hot_2_nhi_obj_un
                     WITH TABLE KEY package_id = lr_check_result->object->package
                                    object_name = lr_check_result->object->name
                                    object_suffix = lr_check_result->object->suffix
                     REFERENCE INTO DATA(lr_hana_2_hot_2_nhi_un).
          IF sy-subrc = 0.
            "unknown object known, add log message
            append_log_message( EXPORTING i_severity = lr_check_result->severity i_error_code = lr_check_result->error_code i_message = lr_check_result->error_msg
                                          i_hot_object = lr_hana_2_hot_2_nhi_un->hot_object i_timestamp = lr_check_result->timestamp i_location = lr_check_result->location
                                CHANGING  c_logs = c_log_messages ).
          ELSE.
            "object not yet known at all
            lr_cts_hot_object_unknown = cl_cts_hot_object_v1=>create_instance( iv_hana_package_id = lr_check_result->object->package
                                                                               iv_hana_object_name = lr_check_result->object->name
                                                                               iv_hana_object_suffix = lr_check_result->object->suffix ).
            INSERT VALUE #( package_id = lr_check_result->object->package
                            object_name = lr_check_result->object->name
                            object_suffix = lr_check_result->object->suffix
                            hot_object = lr_cts_hot_object_unknown
                            nhi_object = lr_check_result->object ) INTO TABLE c_map_hana_2_hot_2_nhi_obj_un.
            append_log_message( EXPORTING i_severity = lr_check_result->severity i_error_code = lr_check_result->error_code i_message = lr_check_result->error_msg
                                          i_hot_object = lr_cts_hot_object_unknown i_timestamp = lr_check_result->timestamp i_location = lr_check_result->location
                                CHANGING  c_logs = c_log_messages ).
          ENDIF.
        ENDIF.
      ELSE. "no object in check result. add message as is.
        append_log_message( EXPORTING i_severity = lr_check_result->severity i_error_code = lr_check_result->error_code i_message = lr_check_result->error_msg
                                      i_timestamp = lr_check_result->timestamp i_location = lr_check_result->location
                            CHANGING  c_logs = c_log_messages ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.