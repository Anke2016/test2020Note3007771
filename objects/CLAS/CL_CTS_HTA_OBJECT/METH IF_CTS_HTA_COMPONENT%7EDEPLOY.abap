  METHOD if_cts_hta_component~deploy.
    execute_deploy(
      EXPORTING
        i_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( me ) )
        i_force       = i_force
      IMPORTING
        e_deploy_status = e_overall_deploy_status
        e_deploy_messages = e_deploy_messages
    ).
  ENDMETHOD.