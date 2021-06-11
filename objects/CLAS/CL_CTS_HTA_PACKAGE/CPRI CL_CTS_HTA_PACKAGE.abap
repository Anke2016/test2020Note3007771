  PRIVATE SECTION.
    ALIASES:
      m_hana_package_name FOR if_cts_hta_package~hana_package_name.

    CLASS-DATA:
      g_db_access    TYPE REF TO lif_db_access,
      g_tadir_access TYPE REF TO lif_tadir_access.

    "! Package data that is currently stored in HTA tables.
    DATA m_package_data_in_hta TYPE cts_hot_package.

    "! Package data read from HANA. (not all fields might be filled)
    DATA m_package_data_in_hana TYPE cts_hot_package.

    "! TADIR entry. Required for synchronization. (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_tadir TYPE tadir.
    "! Transport request to which the package should be added during synchronization.  (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_trkorr TYPE trkorr.
    "! Translation relevance of this package
    DATA m_translation_relevance TYPE REF TO ce_cts_hta_translation.
    "! Deploy Mode of this package
    DATA m_deploy_mode TYPE REF TO ce_cts_hta_deploy_mode.

    METHODS:
      constructor
        IMPORTING
          i_hot_package TYPE REF TO cl_cts_hot_package " reuse old classes for now as they are used everywhere...
          i_abap_status TYPE cts_hot_abap_status DEFAULT co_active_version,

      "! checks whether sync is required or not. (compare package data and take i_force into account).
      "! Requires that read_hana_data and read_hta_data was called before!
      is_sync_required
        IMPORTING
          i_force         TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(r_result) TYPE abap_bool.