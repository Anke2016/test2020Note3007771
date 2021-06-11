"! Enumeration for all possible synchronization states of an IF_CTS_HTA_COMPONENT.
CLASS ce_cts_hta_sync_state DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Indicates that the version of a package/object is the same in HANA repository and HTA(ABAP) repository (including the case,
      "! that package/object does not exist in both repositories)<br/>
      in_sync                 TYPE REF TO ce_cts_hta_sync_state READ-ONLY,
      "! Indicates that a package/object is not in sync, the version is not the same in HANA repository and HTA(ABAP) repository.
      "! (Including the case that the package/object exists only in one of the repositories)<br/>
      not_in_sync             TYPE REF TO ce_cts_hta_sync_state READ-ONLY,
      "! Indicates that a package/object can not be synchronized.<br/>
      can_not_be_synchronized TYPE REF TO ce_cts_hta_sync_state READ-ONLY.

    CLASS-METHODS:
      class_constructor.
