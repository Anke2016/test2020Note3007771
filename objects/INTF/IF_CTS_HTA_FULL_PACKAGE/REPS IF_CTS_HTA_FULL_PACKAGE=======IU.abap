"! Interface for a full package in HANA Transport for ABAP (HTA), R3TR HOTA<br/>
"! The package represents a SAP HANA repository package in HANA repository and in HTA repository with its package meta data and all its objects.<br/>
"! <br/>
"! With this interface you can work on full packages (1 package with all its objects), e.g. sync them from HANA repository into HTA repository or
"! deploy them from HTA repository to HANA repository.<br/>
"! This interface extends if_cts_hta_component_list, but with restriction that it contains only 1 if_cts_hta_package and all the objects that belong
"! to this if_cts_hta_package.<br/>
"! Adding a different package will result in nothing added with return abap_false. Adding an object that does not belong to the package of this full
"! package, the object is not added and result is abap_false.<br/>
"! <br/>
"! Do not implement this interface but use CL_CTS_HTA_API_FACTORY to get an instance of it.
"! Exception: In test code (TDD), you can implement it.<br/>
"! <br/>
"! This interface might be extended in next deliveries.
INTERFACE if_cts_hta_full_package
  PUBLIC .
  INTERFACES:
    if_cts_hta_component_list.

  ALIASES:
    component_type FOR if_cts_hta_component~component_type,
    transport_object_name FOR if_cts_hta_component~transport_object_name,
    deploy FOR if_cts_hta_component~deploy,
    get_components FOR if_cts_hta_component_list~get_components,
    get_deploy_state FOR if_cts_hta_component~get_deploy_state,
    get_sync_state FOR if_cts_hta_component~get_sync_state,
    set_prework FOR if_cts_hta_component~set_prework,
    synchronize FOR if_cts_hta_component~synchronize,
    set_deploy_mode FOR if_cts_hta_component~set_deploy_mode,
    set_translation_relevance FOR if_cts_hta_component~set_translation_relevance.

  CONSTANTS:
    co_pgmid       TYPE pgmid VALUE 'R3TR',
    co_object_type TYPE trobjtype VALUE 'HOTA'.

ENDINTERFACE.