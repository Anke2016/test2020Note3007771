"! Interface for a package in HANA Transport for ABAP (HTA), LIMU HOTP.<br/>
"! The package represents a SAP HANA repository package in HANA repository and in HTA repository.<br/>
"! <br/>
"! With this interface you can work on these packages, e.g. sync them from HANA repository into HTA repository or
"! deploy them from HTA repository to HANA repository.<br/>
"! <br/>
"! Do not implement this interface but use CL_CTS_HTA_API_FACTORY to get an instance of it.
"! Exception: In test code (TDD), you can implement it.<br/>
"! <br/>
"! This interface might be extended in next deliveries.
INTERFACE if_cts_hta_package
  PUBLIC .
  INTERFACES:
    if_cts_hta_component.

  ALIASES:
    component_type FOR if_cts_hta_component~component_type,
    transport_object_name FOR if_cts_hta_component~transport_object_name,
    deploy FOR if_cts_hta_component~deploy,
    get_deploy_state FOR if_cts_hta_component~get_deploy_state,
    get_sync_state FOR if_cts_hta_component~get_sync_state,
    set_prework FOR if_cts_hta_component~set_prework,
    synchronize FOR if_cts_hta_component~synchronize,
    set_deploy_mode FOR if_cts_hta_component~set_deploy_mode,
    set_translation_relevance FOR if_cts_hta_component~set_translation_relevance.

  CONSTANTS:
    co_pgmid       TYPE pgmid VALUE 'LIMU',
    co_object_type TYPE trobjtype VALUE 'HOTP'.

  DATA:
    "! m_hana_package_name either containing data passed during creation of instance or data read from HTA repository
    "! if instance was created from transport object name.
    hana_package_name TYPE if_cts_hta_types=>ty_hana_package_name READ-ONLY.

ENDINTERFACE.