  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_package_akh,
        package TYPE cts_hot_package_id,
        akh     TYPE ufps_posid,
      END OF ty_package_akh,
      ty_package_akhs TYPE SORTED TABLE OF ty_package_akh WITH UNIQUE KEY package.

    CLASS-DATA:
      "! Remember whether transport tool check was done or not per user session.
      g_transport_tool_check_done TYPE abap_bool.

    DATA:
      m_cache_package_akh TYPE ty_package_akhs.