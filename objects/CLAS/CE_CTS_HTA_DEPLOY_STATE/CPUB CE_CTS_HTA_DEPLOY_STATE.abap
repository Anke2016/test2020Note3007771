"! Enumeration for all possible deploy states of an IF_CTS_HTA_COMPONENT.
CLASS ce_cts_hta_deploy_state DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Indicates that an IF_CTS_HTA_COMPONENT is not deployed.
      not_deployed    TYPE REF TO ce_cts_hta_deploy_state READ-ONLY,
      "! Indicates that an IF_CTS_HTA_COMPONENT is partly deployed only, e.g. only some
      "! components of an IF_CTS_HTA_COMPONENT_LIST or IF_CTS_HTA_FULL_PACKAGE are deployed
      partly_deployed TYPE REF TO ce_cts_hta_deploy_state READ-ONLY,
      "! Indicates that an IF_CTS_HTA_COMPONENT is (completely) deployed.
      deployed        TYPE REF TO ce_cts_hta_deploy_state READ-ONLY.

    CLASS-METHODS:
      class_constructor.
