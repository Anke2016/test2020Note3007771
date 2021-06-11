"! Enumeration for all possible prework settings of an IF_CTS_HTA_COMPONENT.
CLASS ce_cts_hta_prework DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Indicates that prework was done
      prework_done     TYPE REF TO ce_cts_hta_prework READ-ONLY,
      "! Indicates that prework was not done
      prework_not_done TYPE REF TO ce_cts_hta_prework READ-ONLY.

    CLASS-METHODS:
      class_constructor.

    DATA:
      value TYPE if_cts_hta_types=>ty_prework_flag READ-ONLY.
