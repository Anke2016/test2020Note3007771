  METHOD if_cts_hta_component~get_deploy_state.
    DATA: lr_deploy_state_packages      TYPE REF TO ce_cts_hta_deploy_state,
          lr_deploy_state_objects       TYPE REF TO ce_cts_hta_deploy_state,
          lr_deploy_state_full_packages TYPE REF TO ce_cts_hta_deploy_state.

    IF me->m_hta_packages IS INITIAL AND me->m_hta_objects IS INITIAL AND me->m_hta_full_packages IS INITIAL.
      r_result = ce_cts_hta_deploy_state=>deployed.
      RETURN.
    ENDIF.

    lr_deploy_state_packages = get_deploy_state_aggregated( me->m_hta_packages ).
    " If we already have partly deployed, we do not need to check the other tables (Performance)
    IF lr_deploy_state_packages = ce_cts_hta_deploy_state=>partly_deployed.
      r_result = ce_cts_hta_deploy_state=>partly_deployed.
      RETURN.
    ENDIF.

    lr_deploy_state_objects = get_deploy_state_aggregated( me->m_hta_objects ).
    " If we already have partly deployed, we do not need to check the full package table (Performance)
    IF lr_deploy_state_objects = ce_cts_hta_deploy_state=>partly_deployed
       OR ( lr_deploy_state_objects = ce_cts_hta_deploy_state=>deployed AND lr_deploy_state_packages = ce_cts_hta_deploy_state=>not_deployed )
       OR ( lr_deploy_state_objects = ce_cts_hta_deploy_state=>not_deployed AND lr_deploy_state_packages = ce_cts_hta_deploy_state=>deployed ).
      r_result = ce_cts_hta_deploy_state=>partly_deployed.
      RETURN.
    ENDIF.

    lr_deploy_state_full_packages = get_deploy_state_aggregated( i_components = me->m_hta_full_packages i_partly_deployed_supported = abap_true ).

    "finally calculate what to return. (states are initial if no packages/objects/full packages are in the list)
    IF ( lr_deploy_state_packages IS INITIAL OR lr_deploy_state_packages = ce_cts_hta_deploy_state=>deployed )
          AND ( lr_deploy_state_objects IS INITIAL OR lr_deploy_state_objects = ce_cts_hta_deploy_state=>deployed )
          AND ( lr_deploy_state_full_packages IS INITIAL OR lr_deploy_state_full_packages = ce_cts_hta_deploy_state=>deployed ).
      r_result = ce_cts_hta_deploy_state=>deployed.
    ELSEIF ( lr_deploy_state_packages IS INITIAL OR lr_deploy_state_packages = ce_cts_hta_deploy_state=>not_deployed )
          AND ( lr_deploy_state_objects IS INITIAL OR lr_deploy_state_objects = ce_cts_hta_deploy_state=>not_deployed )
          AND ( lr_deploy_state_full_packages IS INITIAL OR lr_deploy_state_full_packages = ce_cts_hta_deploy_state=>not_deployed ).
      r_result = ce_cts_hta_deploy_state=>not_deployed.
    ELSE.
      r_result = ce_cts_hta_deploy_state=>partly_deployed.
    ENDIF.
  ENDMETHOD.