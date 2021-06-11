  METHOD if_cts_hot_ext_call_internal~is_update_upgrade_running.
    CALL FUNCTION 'OCS_API_GET_UPG_INFO'
      EXCEPTIONS
        no_upg_found   = 1
        internal_error = 2
        no_authority   = 3
        OTHERS         = 4.

    IF sy-subrc = 0.
      rv_result = abap_true. " Upgrade/update is running
    ELSEIF sy-subrc = 1.
*                              Upgrade/update is NOT running
    ELSE.
      cx_hana_object_transport=>raise_message_exception( ). " error occured during check if upgrade/update is running or not
    ENDIF.
  ENDMETHOD.