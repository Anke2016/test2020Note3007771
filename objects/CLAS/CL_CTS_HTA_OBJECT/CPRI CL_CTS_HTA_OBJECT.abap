  PRIVATE SECTION.
    ALIASES:
        m_object_key FOR if_cts_hta_object~object_key.

    CLASS-DATA:
      g_db_access    TYPE REF TO lif_db_access,
      g_tadir_access TYPE REF TO lif_tadir_access.

    DATA: m_hta_package TYPE REF TO if_cts_hta_package.

    "! Object data that is currently stored in HTA tables. (not all fields might be filled, e.g. cdata/bdata)
    DATA m_object_data_in_hta TYPE cts_hot_object.

    "! Object data read from HANA. (not all fields might be filled, e.g. cdata/bdata)
    DATA m_object_data_in_hana TYPE cts_hot_object.

    "! TADIR entry. Required for synchronization. (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_tadir TYPE tadir.
    "! Transport request to which the package should be added during synchronization.  (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_trkorr TYPE trkorr.

    METHODS:
      constructor
        IMPORTING
          i_hta_package TYPE REF TO if_cts_hta_package
          i_hot_object  TYPE REF TO cl_cts_hot_object_v1
          i_abap_status TYPE cts_hot_abap_status DEFAULT cl_cts_hta_component=>co_active_version,

      "! checks whether sync is required or not. (compare object versions and take i_force into account).
      "! Requires that read_hana_data and read_hta_data was called before!
      is_sync_required
        IMPORTING
          i_force         TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(r_result) TYPE abap_bool.