  METHOD if_cts_hta_component~set_deploy_mode.
    "Deploy mode only needs to be set on the package of this full package because all objects of this list (full package) belong to same package
    DATA(lt_packages) = me->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    LOOP AT lt_packages INTO DATA(lr_package).
      lr_package->set_deploy_mode( i_deploy_mode = i_deploy_mode
                                   i_trkorr = i_trkorr
                                   i_suppress_dialog = i_suppress_dialog ).
    ENDLOOP.
  ENDMETHOD.