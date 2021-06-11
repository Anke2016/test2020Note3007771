"! Interface for a component_list in HANA Transport for ABAP (HTA), the composite of the composite design pattern.<br/>
"! The component_list contains a list of any HTA component (IF_CTS_HTA_PACKAGE, IF_CTS_HTA_OBJECT, ...).<br/>
"! <br/>
"! With this interface you can work on several objects in one call, e.g. sync all from HANA repository into HTA repository or
"! deploy all from HTA repository to HANA repository.<br/>
"! <br/>
"! Do not implement this interface but use CL_CTS_HTA_API_FACTORY to get an instance of it.
"! Exception: In test code (TDD), you can implement it.<br/>
"! <br/>
"! This interface might be extended in next deliveries.
INTERFACE if_cts_hta_component_list
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

  METHODS:
    "! Adds a HTA component of type IF_CTS_HTA_COMPENENT (IF_CTS_HTA_OBJECT or IF_CTS_HTA_PACKAGE, IF_CTS_HTA_FULL_PACKAGE) to this component list.<br/>
    "! If the component to be added is already part of the list (either directly, as LIMU, or as part of an IF_CTS_HTA_FULL_PACKAGE [R3TR]), it is not added.<br/>
    "! If the component to be added is of type IF_CTS_HTA_COMPONENT_LIST, then all its parts are added individually.
    "!
    "! @parameter i_cts_hta_component | The component to add. If i_cts_hta_component is of type IF_CTS_HTA_COMPONENT_LIST,
    "!                                  the entries of the list are added, not the list itself. (duplicates are not added)<br/>
    "!                                  For IF_CTS_HTA_FULL_PACKAGE, only the if_cts_hta_full_package is added, not it's parts.
    "! @parameter r_result | abap_true  if i_cts_hta_component was added<br/>
    "!                       abap_false if it was not added (either i_cts_hta_component was not bound or already part of the list or
    "!                                  has unsupported type)
    add_component
      IMPORTING
        i_cts_hta_component TYPE REF TO if_cts_hta_component
      RETURNING
        VALUE(r_result)     TYPE abap_bool,

    "! Returns all components of passed component type that are currently in this list.<br/>
    "! If i_component_type is IF_CTS_HTA_COMPONENT_LIST, the result is empty because lists are not part
    "! of the list but when added are splitted into it's parts. See also add_component.<br/>
    "! If i_component_type is IF_CTS_HTA_OBJECT or IF_CTS_HTA_PACKAGE, only components that were added as
    "! IF_CTS_HTA_OBJECT or IF_CTS_HTA_PACKAGE are returned or in other words, IF_CTS_HTA_OBJECTs and
    "! IF_CTS_HTA_PACKAGEs that are part of an IF_CTS_HTA_FULL_PACKAGE are only returned as part of a
    "! IF_CTS_HTA_FULL_PACKAGE)
    "!
    "! @parameter i_component_type | The component type for which the components should be returned.
    "! @parameter r_result         | All components of the passed I_COMPONENT_TYPE with restrictions, see above.
    get_components
      importing
        i_component_type type ref to ce_cts_hta_component_type
      RETURNING
        VALUE(r_result)     TYPE if_cts_hta_types=>ty_cts_hta_components,

    "! Removes a HTA component of type IF_CTS_HTA_COMPENENT (IF_CTS_HTA_OBJECT or IF_CTS_HTA_PACKAGE) from this component list.<br/>
    "! If the component to be removed is of type IF_CTS_HTA_COMPONENT_LIST, it's parts are removed from this list.
    "!
    "! @parameter i_cts_hta_component | The component to be removed.
    "! @parameter r_result | abap_true  if i_cts_hta_component was removed
    "!                       abap_false if it was not removed (either i_cts_hta_component was not bound or not part of the list or
    "!                                  has unsupported type)
    remove_component
      IMPORTING
        i_cts_hta_component TYPE REF TO if_cts_hta_component
      RETURNING
        VALUE(r_result)     TYPE abap_bool.

ENDINTERFACE.