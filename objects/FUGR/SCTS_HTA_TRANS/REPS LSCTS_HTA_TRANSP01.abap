CLASS lcl_hta_deployment IMPLEMENTATION.

  METHOD create_instance.
    CREATE OBJECT r_result
      EXPORTING
        i_transport_objects = i_transport_objects.
  ENDMETHOD.

  METHOD execute_deployment.
    DATA: lr_hta_api         TYPE REF TO if_cts_hta_api_factory,
          lr_component       TYPE REF TO if_cts_hta_component,
          lr_component_list  TYPE REF TO if_cts_hta_component_list,
          lr_e071            TYPE REF TO e071,
          lr_message         TYPE REF TO if_cts_hta_types=>ty_deploy_message,
          lt_deploy_messages TYPE if_cts_hta_types=>ty_deploy_messages,
          lv_deploy_status   TYPE if_cts_hta_types=>ty_deploy_status,
          lt_hdi_object_e071 TYPE tr_objects.

    TRY.
        lr_hta_api = cl_cts_hta_api_factory=>create_instance( ).
        lr_component_list = lr_hta_api->create_component_list( ).
        LOOP AT me->m_transport_objects REFERENCE INTO lr_e071 WHERE pgmid = 'R3TR' AND object = 'HOTA'.
          IF lr_e071->obj_name CA '/'.
            APPEND lr_e071->* TO lt_hdi_object_e071.
          ELSE.
            lr_component = lr_hta_api->create_full_package_trobj_name( lr_e071->obj_name ).
            lr_component_list->add_component( lr_component ).
          ENDIF.
        ENDLOOP.

        LOOP AT me->m_transport_objects REFERENCE INTO lr_e071 WHERE pgmid = 'LIMU' AND object = 'HOTP'.
          lr_component = lr_hta_api->create_package_by_trobj_name( lr_e071->obj_name ).
          lr_component_list->add_component( lr_component ).
        ENDLOOP.

        LOOP AT me->m_transport_objects REFERENCE INTO lr_e071 WHERE pgmid = 'LIMU' AND object = 'HOTO'.
          IF lr_e071->obj_name CA '/'.
            APPEND lr_e071->* TO lt_hdi_object_e071.
          ELSE.
            lr_component = lr_hta_api->create_object_by_trobj_name( lr_e071->obj_name ).
            lr_component_list->add_component( lr_component ).
          ENDIF.
        ENDLOOP.

        me->log_hdi_skipped_warning( lt_hdi_object_e071 ).

        lr_component_list->deploy( IMPORTING e_deploy_messages = lt_deploy_messages ).

        LOOP AT lt_deploy_messages REFERENCE INTO lr_message.
          me->m_transport_log->append(
            i_severity       = lr_message->severity
            i_message_class  = lr_message->ag
            i_message_number = lr_message->msgnr
            i_new_object     = lr_message->newobj
            i_level          = lr_message->level
            i_var1           = lr_message->var1
            i_var2           = lr_message->var2
            i_var3           = lr_message->var3
            i_var4           = lr_message->var4
          ).
        ENDLOOP.
      CATCH cx_cts_hta_not_found cx_cts_hta INTO DATA(lx_cts_hta).
        me->m_transport_log->append(
            i_severity       = 'A'
            i_message_class  = lx_cts_hta->if_t100_message~t100key-msgid
            i_message_number = CONV msgnr( lx_cts_hta->if_t100_message~t100key-msgno )
            i_new_object     = 'X'
            i_level          = 2
            i_var1           = lx_cts_hta->message_variable_1
            i_var2           = lx_cts_hta->message_variable_2
            i_var3           = lx_cts_hta->message_variable_3
            i_var4           = lx_cts_hta->message_variable_4
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    me->m_transport_objects = i_transport_objects.
    CREATE OBJECT m_transport_log.
  ENDMETHOD.


  METHOD log_hdi_skipped_warning.
    DATA ls_chunk TYPE cl_cts_hot_utility=>ty_split_text_50.

    IF it_hdi_objects IS NOT INITIAL.
      me->m_transport_log->append(
        i_severity       = if_cts_hot_logger=>co_severity_warning
        i_message_class  = 'SCTS_HDI'
        i_message_number = '252' "  Folgende HDI-Objekte wurden nicht deployt:
        i_new_object     = 'X'
        i_level          = if_cts_hot_logger=>co_level_3 ).

      LOOP AT it_hdi_objects REFERENCE INTO DATA(lr_hdi_object).
        ls_chunk = cl_cts_hot_utility=>split_text_50_chars( CONV #( lr_hdi_object->obj_name ) ).
        me->m_transport_log->append(
            i_severity       = if_cts_hot_logger=>co_severity_warning
            i_message_class  = 'SCTS_HDI'
            i_message_number = '100'
            i_new_object     = space
            i_level          = if_cts_hot_logger=>co_level_3
            i_var1          = |{ lr_hdi_object->pgmid } { lr_hdi_object->object }|
            i_var2          = ls_chunk-chunk1
            i_var3          = ls_chunk-chunk2
            i_var4          = ls_chunk-chunk3 ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_transport_log IMPLEMENTATION.
  METHOD append.
    DATA: log_message     TYPE sprot_u,
          log_message_tab TYPE sprot_u_tab.

    CLEAR: log_message, log_message_tab.
    log_message-level       = i_level.
    log_message-severity    = i_severity.
    log_message-langu       = sy-langu.
    log_message-ag          = i_message_class.
    log_message-msgnr       = i_message_number.
    log_message-newobj      = i_new_object.
    log_message-var1        = i_var1.
    log_message-var2        = i_var2.
    log_message-var3        = i_var3.
    log_message-var4        = i_var4.
    INSERT log_message INTO TABLE log_message_tab.

    do_append( log_message_tab ).
  ENDMETHOD.

  METHOD do_append.
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES
        xmsg = log_message_tab.
  ENDMETHOD.
ENDCLASS.