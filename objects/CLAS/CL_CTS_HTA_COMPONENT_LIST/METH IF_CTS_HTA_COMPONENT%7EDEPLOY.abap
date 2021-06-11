  METHOD if_cts_hta_component~deploy.

    DATA: lt_packages     TYPE  if_cts_hta_types=>ty_cts_hta_packages,
          lt_objects      TYPE  if_cts_hta_types=>ty_cts_hta_objects,
          lr_full_package TYPE REF TO if_cts_hta_full_package,
          lr_components   TYPE if_cts_hta_types=>ty_cts_hta_components,
          lr_component    TYPE REF TO if_cts_hta_component.

    lt_packages = m_hta_packages.
    lt_objects = m_hta_objects.

    LOOP AT m_hta_full_packages INTO lr_full_package.
      "get package and add to lt_packages
      lr_components = lr_full_package->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
      LOOP AT lr_components INTO lr_component.
        APPEND CAST if_cts_hta_package( lr_component ) TO lt_packages.
      ENDLOOP.

      "get all objects and add to lt_objects
      lr_components = lr_full_package->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
      LOOP AT lr_components INTO lr_component.
        APPEND CAST if_cts_hta_object( lr_component ) TO lt_objects.
      ENDLOOP.
    ENDLOOP.

    execute_deploy(
      EXPORTING
        i_hta_packages = lt_packages
        i_hta_objects  = lt_objects
        i_force        = i_force
      IMPORTING
        e_deploy_status = e_overall_deploy_status
        e_deploy_messages = e_deploy_messages
    ).
  ENDMETHOD.