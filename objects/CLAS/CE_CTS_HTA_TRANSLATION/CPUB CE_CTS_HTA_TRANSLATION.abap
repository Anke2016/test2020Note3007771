"! Enumeration over all possible translation modes in HTA that can be set on package level.
CLASS CE_CTS_HTA_TRANSLATION DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Indicates that the texts of the objects of a HANA package should be translated
      relevant_for_translation TYPE REF TO CE_CTS_HTA_TRANSLATION READ-ONLY,
      "! Indicates that the texts of the objects of a HANA package should not be translated
      not_relevant_for_translation TYPE REF TO CE_CTS_HTA_TRANSLATION READ-ONLY.

    CLASS-METHODS:
      class_constructor.

    DATA:
      value TYPE cts_hot_abap_no_translation READ-ONLY.
