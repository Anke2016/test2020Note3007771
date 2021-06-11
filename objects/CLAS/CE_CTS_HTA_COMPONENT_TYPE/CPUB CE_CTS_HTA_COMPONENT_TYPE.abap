"! Enumeration over all possible sub components extending IF_CTS_HTA_COMPONENT so that type safe casting is easily possible and fast.
CLASS CE_CTS_HTA_COMPONENT_TYPE DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      "! Indicates that current IF_CTS_HTA_COMPONENT is unknown to HTA API. (Should never be the case!)
      ct_unknown TYPE REF TO CE_CTS_HTA_COMPONENT_TYPE READ-ONLY,
      "! Indicates that current IF_CTS_HTA_COMPONENT is of type IF_CTS_HTA_OBJECT
      ct_if_cts_hta_object TYPE REF TO CE_CTS_HTA_COMPONENT_TYPE READ-ONLY,
      "! Indicates that current IF_CTS_HTA_COMPONENT is of type IF_CTS_HTA_PACKAGE
      ct_if_cts_hta_package TYPE REF TO CE_CTS_HTA_COMPONENT_TYPE READ-ONLY,
      "! Indicates that current IF_CTS_HTA_COMPONENT is of type IF_CTS_HTA_COMPONENT_LIST
      ct_if_cts_hta_component_list TYPE REF TO CE_CTS_HTA_COMPONENT_TYPE READ-ONLY,
      "! Indicates that current IF_CTS_HTA_COMPONENT is of type IF_CTS_HTA_FULL_PACKAGE
      ct_if_cts_hta_full_package TYPE REF TO CE_CTS_HTA_COMPONENT_TYPE READ-ONLY.

    CLASS-METHODS:
      class_constructor.
