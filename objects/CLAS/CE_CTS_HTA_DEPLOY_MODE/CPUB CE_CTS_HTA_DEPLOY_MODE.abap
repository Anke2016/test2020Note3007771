"! Enumeration over all possible deploy modes in HTA that can be set on package level.
CLASS ce_cts_hta_deploy_mode DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Packages/objects are deployed directly (default)
      directly         TYPE REF TO ce_cts_hta_deploy_mode READ-ONLY,
      "! Packages/objects are deployed only after the prework is done.
      prework_required TYPE REF TO ce_cts_hta_deploy_mode READ-ONLY.

    CLASS-METHODS:
      class_constructor.

    DATA:
      value TYPE cts_hot_activation_mode READ-ONLY.
