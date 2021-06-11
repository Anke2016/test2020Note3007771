  METHOD activate_objects.
    DATA:
      "! Table that initially holds all objects to be activated and gets reduced after each activation round by successfully activated
      "! or failed objects. But reduction for failed objects only in case activation was done with lv_ok_objects_only = false.
      "! Because in abap_true case, all error objects should be retried during "normal" activation.
      lt_not_yet_ok_or_failed_objs   TYPE ty_map_hana_2_hot_2_nhi_objs,
      "! Objects for next activation attempt
      lt_objs_for_act                TYPE ty_map_hana_2_hot_2_nhi_objs,
      "! successful objects of current activation attempt
      lt_successful_objects          TYPE ty_map_hana_2_hot_2_nhi_objs,
      "! failed objects of current activation attempt
      lt_failed_objects              TYPE ty_map_hana_2_hot_2_nhi_objs,
      "! all unknown objects (maybe revalidated or implicitely added by HANA due to dependencies)
      lt_map_hana_2_hot_2_nhi_obj_un TYPE ty_map_hana_2_hot_2_nhi_objs,
      lr_activation_result           TYPE REF TO ty_activation_result,
      "! Indicates whether next activation attempt should be with OK objects only (abap_true)
      "! or whether all not yet activated/failed objects (lt_not_yet_ok_or_failed_objs) should be used(abap_false)
      lv_next_activation_ok_only     TYPE abap_bool VALUE abap_false,
      lr_activate_response           TYPE REF TO cl_nhi_activate_objects_res,
      lr_hana_2_hot_2_nhi            TYPE REF TO ty_map_hana_2_hot_2_nhi_obj,
      "! Remember number of attempted objects of last activation attempt with all not yet activated/failed objects
      "! to stop activation if number of objects is not decreasing
      lv_nr_of_attempted_objects     TYPE i,
      lr_nhi_inactive_version        TYPE REF TO cl_nhi_inactive_version,
      lr_nhi_inactive_session        TYPE REF TO cl_nhi_inactive_session.

    IF i_objects_to_be_activated IS INITIAL.
      RETURN.
    ENDIF.

    lr_nhi_inactive_session = cl_nhi_inactive_session=>create_inactive_session( owner = m_nhi_api_user workspace = '' ).
    lr_nhi_inactive_version = cl_nhi_inactive_version=>create_inactive_version( owner = m_nhi_api_user workspace = '' ).

    lt_not_yet_ok_or_failed_objs = i_objects_to_be_activated.
    lt_objs_for_act = lt_not_yet_ok_or_failed_objs.

    DO i_max_nr_activation_attempts TIMES. "exit of do loop will also be triggered if no more objects to be activated or number of objects to be activated is not decreasing
      APPEND VALUE #( ) TO c_deploy_result-activation_results. "add already here empty activation result to have logs in case of exception
      READ TABLE c_deploy_result-activation_results REFERENCE INTO lr_activation_result INDEX lines( c_deploy_result-activation_results ).
      lr_activation_result->activation_counter = lines( c_deploy_result-activation_results ).
      lr_activation_result->ok_objects_only = lv_next_activation_ok_only.
      lr_activation_result->nr_of_attempted_objects = lines( lt_objs_for_act ).

      IF lv_next_activation_ok_only = abap_false.
        lv_nr_of_attempted_objects = lr_activation_result->nr_of_attempted_objects.
      ENDIF.

      "activate the objects, either all remaining objects or only OK objects of previous round
      lr_activate_response = m_nhi_object_api->activate( m_nhi_object_api->create_activate_objects_req(
                                  activationmode = ce_nhi_activation_mode=>activation_casc_2_phase
                                  objlist        = VALUE cl_nhi_object_id=>ty_objlist( FOR line IN lt_objs_for_act ( line-nhi_object ) )
                                  session        = lr_nhi_inactive_session
                                  version        = lr_nhi_inactive_version
                                  act_with_hints = i_activate_with_hints ) ).

      raise_exc_if_resp_is_not_bound( i_response = lr_activate_response i_what = cl_nhi_activate_objects_req=>co_what i_action = cl_nhi_activate_objects_req=>co_action ).

      "map overall activation response
      lr_activation_result->hana_activation_id = lr_activate_response->activationid.
      lr_activation_result->hana_error_code = lr_activate_response->error_code.
      lr_activation_result->hana_error_msg = lr_activate_response->error_msg.
      lr_activation_result->hana_error_arg = lr_activate_response->error_arg.

      me->analyze_activation_response( EXPORTING i_activation_response = lr_activate_response
                                                 i_map_hana_2_hot_2_nhi_obj = i_objects_to_be_activated
                                       IMPORTING e_activation_success = lr_activation_result->activation_success
                                                 e_successful_objects = lt_successful_objects
                                                 e_failed_objects = lt_failed_objects
                                       CHANGING  c_map_hana_2_hot_2_nhi_obj_un = lt_map_hana_2_hot_2_nhi_obj_un
                                                 c_log_messages = lr_activation_result->log_messages ).

      IF lr_activation_result->activation_success = abap_true.
        LOOP AT lt_objs_for_act REFERENCE INTO lr_hana_2_hot_2_nhi. "Loop is not using lt_successful_objects returned by analyse_activation_response
          "because it contains only objects for which HANA returned check results (=log messages)
          "and there might be objects successfully activated without any check result
          INSERT lr_hana_2_hot_2_nhi->hot_object INTO TABLE lr_activation_result->objects_with_last_action.
          INSERT lr_hana_2_hot_2_nhi->hot_object INTO TABLE c_successful_objects.
          DELETE TABLE lt_not_yet_ok_or_failed_objs FROM lr_hana_2_hot_2_nhi->*.
        ENDLOOP.

        update_objs_in_hot_after_depl( i_successfull_deploy_objects = lr_activation_result->objects_with_last_action
                                       i_abap_status = i_abap_status
                                       i_object_status_versions = i_object_status_versions ).

        lv_next_activation_ok_only = abap_false. "next activation round should use all not yet activated/failed objects
      ELSE.
        IF lr_activation_result->ok_objects_only = abap_false.
          "only map failed objects to activation result in "normal" activation case, else these objects are retried with next "normal" activation
          LOOP AT lt_failed_objects REFERENCE INTO lr_hana_2_hot_2_nhi.
            INSERT lr_hana_2_hot_2_nhi->hot_object INTO TABLE lr_activation_result->failed_objects.

            IF line_exists( lt_objs_for_act[ table_line = lr_hana_2_hot_2_nhi->* ] ). "do not map objects that were not passed for this activation attempt.
              INSERT lr_hana_2_hot_2_nhi->hot_object INTO TABLE lr_activation_result->objects_with_last_action.
              INSERT lr_hana_2_hot_2_nhi->hot_object INTO TABLE c_failed_objects.
              DELETE TABLE lt_not_yet_ok_or_failed_objs FROM lr_hana_2_hot_2_nhi->*.
            ENDIF.
          ENDLOOP.

          "Do not use lt_failed_objects because there might be more contained than passed for current activation attempt.
          update_error_objs_in_hot( i_error_objects = lr_activation_result->objects_with_last_action
                                    i_abap_status = i_abap_status
                                    i_object_status_versions = i_object_status_versions ).

          IF lt_successful_objects IS NOT INITIAL
             AND ( i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok
                   OR i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec ).
            lv_next_activation_ok_only = abap_true. "next activation round should use only successful objects of this round.
          ENDIF.
        ELSE.
          lr_activation_result->failed_objects = VALUE #( FOR line IN lt_failed_objects ( line-hot_object ) ).

          IF lt_successful_objects IS NOT INITIAL
             AND i_activation_mode = if_cts_hot_db_access=>co_hot_activation_mode_ok_rec
             AND lines( lt_successful_objects ) <> lr_activation_result->nr_of_attempted_objects. "do not continue recursive mode if no decrease in number of objects to be activated
            lv_next_activation_ok_only = abap_true. "next activation round should use only successful objects of this round.
          ELSE.
            lv_next_activation_ok_only = abap_false. "next activation round should use all not yet activated/failed objects
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_next_activation_ok_only = abap_false.
        lt_objs_for_act = lt_not_yet_ok_or_failed_objs.
      ELSE.
        lt_objs_for_act = lt_successful_objects. "successful objects from previous activation attempt
      ENDIF.

      "stop the activation loop if:
      IF lt_not_yet_ok_or_failed_objs IS INITIAL "no objects to activate anymore
         OR ( lr_activation_result->activation_success = abap_false "or current round failed
              AND lines( lt_objs_for_act ) = lr_activation_result->nr_of_attempted_objects )"and number of objects for next round is same as for this round and would result in same failure
         OR lv_nr_of_attempted_objects = lines( lt_objs_for_act ). "or next attempt would try same objects as last time for all not yet activated/failed objects
        EXIT.
      ENDIF.
    ENDDO.

    "if still not activated objects are there, set them to error
    IF lt_not_yet_ok_or_failed_objs IS NOT INITIAL.
      CREATE DATA lr_activation_result.
      APPEND lr_activation_result->* TO c_deploy_result-activation_results. "add already here empty activation result to have logs in case of exception
      READ TABLE c_deploy_result-activation_results REFERENCE INTO lr_activation_result INDEX lines( c_deploy_result-activation_results ).
      lr_activation_result->activation_counter = lines( c_deploy_result-activation_results ).

      lr_activation_result->hana_activation_id = 'SCTS_HOT'. "to know that it should be logged as T100 message
      IF lr_activation_result->activation_counter = i_max_nr_activation_attempts + 1.
        lr_activation_result->hana_error_code = '617'. " Cancel activation, maximum number of attempts reached
      ELSE.
        lr_activation_result->hana_error_code = '618'. " Cancel activation, number of to be activated objects not decreasing
      ENDIF.
      lr_activation_result->ok_objects_only = abap_false. "Because all left objects are set to failed it is not a ok_objects_only activation round

      LOOP AT lt_not_yet_ok_or_failed_objs REFERENCE INTO DATA(lr_not_yet_ok_or_failed_obj).
        INSERT lr_not_yet_ok_or_failed_obj->hot_object INTO TABLE c_failed_objects.
        INSERT lr_not_yet_ok_or_failed_obj->hot_object INTO TABLE lr_activation_result->failed_objects.
        INSERT lr_not_yet_ok_or_failed_obj->hot_object INTO TABLE lr_activation_result->objects_with_last_action.
      ENDLOOP.

      lr_activation_result->nr_of_attempted_objects = lines( lr_activation_result->objects_with_last_action ).

      update_error_objs_in_hot( i_error_objects = VALUE #( FOR line IN lt_not_yet_ok_or_failed_objs ( line-hot_object ) )
                                i_abap_status = i_abap_status
                                i_object_status_versions = i_object_status_versions ).
    ENDIF.
  ENDMETHOD.