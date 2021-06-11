  PRIVATE SECTION.
    DATA:
      "! Indicating whether we are still in constructor or not (to enable "no action" for add and remove calls)
      m_instantiation_finished TYPE abap_bool VALUE abap_false.

    "! TADIR entry. Required for synchronization. (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_tadir TYPE tadir.
    "! Transport request to which the package should be added during synchronization.  (set in rs_corr_check and read in rs_corr_insert
    DATA m_sync_trkorr TYPE trkorr.
    "! Translation relevance of this package
    DATA m_translation_relevance TYPE REF TO ce_cts_hta_translation.

    METHODS:
      constructor
        IMPORTING
          i_cts_hta_package TYPE REF TO if_cts_hta_package
          i_cts_hta_objects TYPE if_cts_hta_types=>ty_cts_hta_objects.
