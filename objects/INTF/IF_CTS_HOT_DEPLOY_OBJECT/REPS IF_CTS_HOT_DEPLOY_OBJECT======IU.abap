INTERFACE if_cts_hot_deploy_object
  PUBLIC .

  TYPES:
    ty_t_deploy_objects TYPE STANDARD TABLE OF REF TO if_cts_hot_deploy_object WITH DEFAULT KEY.

  DATA:
    "! Display name of the deploy object how it should be logged in log
    mv_display_name     TYPE string READ-ONLY,
    "! Indicates whether the deploy_object exists in HTA or not. If it exists, mv_display_name
    "! can be used and object can be deployed.
    mv_exists_in_hta    TYPE abap_bool READ-ONLY,
    "! Indicates that this object is already deployed (HOT_STATUS = IF_CTS_HDI_ABAP_TYPES=&gt;CO_HOT_STATUS_ACTIVE or HOT_STATUS = IF_CTS_HDI_ABAP_TYPES=&gt;CO_HOT_STATUS_NEW)
    "! or can be deployed (all other states)
    mv_already_deployed TYPE abap_bool READ-ONLY.

ENDINTERFACE.