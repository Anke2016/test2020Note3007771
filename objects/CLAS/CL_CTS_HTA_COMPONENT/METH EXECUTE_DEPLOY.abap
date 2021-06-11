  METHOD execute_deploy.
    DATA: lt_packages            TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lt_objects             TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lt_successful_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
          lt_successful_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
          lt_log                 TYPE if_cts_hta_types=>ty_deploy_messages,
          lv_max_severity        TYPE sprot_u-severity.

    LOOP AT i_hta_packages INTO DATA(lr_hta_package).
      APPEND CAST cl_cts_hta_component( lr_hta_package )->m_hot_package TO lt_packages.
    ENDLOOP.

    LOOP AT i_hta_objects INTO DATA(lr_hta_object).
      APPEND CAST cl_cts_hta_object( lr_hta_object )->m_hot_object TO lt_objects.
    ENDLOOP.

    IF i_force = abap_true.
      LOOP AT lt_packages INTO DATA(lr_hot_package).
        IF lr_hot_package->abap_hana_package_id IS NOT INITIAL.
          me->m_db_access->prepare_force_deploy_of_pkg( lr_hot_package ).
        ENDIF.
      ENDLOOP.

      LOOP AT lt_objects INTO DATA(lr_hot_object).
        IF lr_hot_object->abap_hana_package_id IS NOT INITIAL AND lr_hot_object->abap_hana_object_name_suffix IS NOT INITIAL.
          me->m_db_access->prepare_force_deploy_of_obj( lr_hot_object ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    me->m_hana_deployer->execute_deployment(
          EXPORTING i_packages = lt_packages
                    i_objects = lt_objects
                    i_abap_status = m_abap_status
          IMPORTING e_successful_packages = lt_successful_packages
                    e_successful_objects = lt_successful_objects
                    e_deploy_messages = e_deploy_messages
                    e_max_severity = lv_max_severity ).

    "map severity to deploy_status
    IF lv_max_severity = space.
      e_deploy_status = 'I'.
    ELSE.
      e_deploy_status = lv_max_severity.
    ENDIF.

  ENDMETHOD.