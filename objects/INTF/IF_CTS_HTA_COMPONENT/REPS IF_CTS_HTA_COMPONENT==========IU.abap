"! Interface for a general component in HANA Transport for ABAP (HTA) using composite design pattern.<br/>
"! <br/>
"! To create an instance of a if_cts_hta_component(object or package or component_list (list of packages/objects)) use CL_CTS_HTA_API_FACTORY.<br/>
"! <br/>
"! Do not implement this interface but use CL_CTS_HTA_API_FACTORY to get an instance of it.<br/>
"! Exception: In test code (TDD), you can implement it.<br/>
"! <br/>
"! This interface might be extended in next deliveries.
INTERFACE if_cts_hta_component
  PUBLIC .

  DATA:
    "! KEY field for HTA processing of packages and objects representing the
    "! transport object name in transport requests/tasks.<br/>
    "! E.g.: Transport object name for a HANA package name is a char40 containing HANA package in upper
    "! case or a HASH value in case HANA package name is longer than char40.<br/>
    "! Transport object name for a HANA object is a char110 containing the HANA package name as char40 in
    "! upper case or its hash in case it is longer than char40 and the HANA object name + suffix as char70
    "! in upper case or its hash in case name + suffix exceed char70.
    transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name READ-ONLY,

    "! Component type indicating the subcomponent of IF_CTS_HTA_COMPONENT.<br/>
    "! With this type safe casting is easily possible and fast.
    component_type        TYPE REF TO ce_cts_hta_component_type READ-ONLY.

  METHODS:

    "! Deploys the component (object or package or full_package or component_list) from HTA repository into SAP HANA repository.<br/>
    "! In HTA deploy means import and activate.
    "!
    "! @parameter i_force | With i_force = abap_true, the deployment also deploys components again that were already deployed successfully earlier.<br/>
    "!                      Default value is abap_false.
    "! @parameter e_overall_deploy_status | Overall status of deployment.<br/>
    "!                                      'I' for deployment was successful (RC = 0)<br/>
    "!                                      'W' for deployment ended with warning(s) (RC = 4)<br/>
    "!                                      'E' for deployment ended with error(s) (RC = 8)<br/>
    "!                                      'F' for deployment ended with fatal error(s) (RC = 12)<br/>
    "! @parameter e_deploy_messages | Log messages created during deployment containing deployment details
    deploy
      IMPORTING
        i_force                 TYPE abap_bool DEFAULT abap_false
      EXPORTING
        e_overall_deploy_status TYPE if_cts_hta_types=>ty_deploy_status
        e_deploy_messages       TYPE if_cts_hta_types=>ty_deploy_messages,

    "! Synchronizes the component (object or package or full_package or component_list) from SAP HANA repository into HTA repository if the version in
    "! HANA repository is newer than IN HTA repository and if synchronization is allowed. Synchronization is not allowed if the object or package was
    "! imported to HTA by transport or SNOTE but not yet deployed to HANA. In this case the object or package first needs to be deployed.<br/>
    "! <br/>
    "! Before the synchronization is done, the component(s) are tried to be locked on a task of a transport request, same as creating/changing any other
    "! ABAP object. For this the user will get a SAP GUI dialog to select/create a transport request/task. To prevent this dialog you can pass a transport
    "! request or task to be used (i_trkorr). If the component to synchronize is already locked on a different transport request than provided, the dialog
    "! will be shown anyhow to be able to select the correct transport request or cancel the synchronization. If you do not want to have the SAP GUI dialogs
    "! shown in any case, you can use i_suppress_dialog. In this case the exception cx_cts_hta_wbo will be raised if the provided transport request/task
    "! can not be used.<br/>
    "! <br/>
    "! In HTA the main transport object (R3TR) consists of one HANA repository package and it's objects. If a package (and its objects) is unknown to HTA
    "! it must be assigned to a ABAP Package (development class) at first sync. This will also be done automatically using SAP GUI dialog whenever a new
    "! package is synchronized or an object is synchronized for a package that is unknown to HTA so far.<br/>
    "! To prevent this dialog you can pass an ABAP package (i_devclass) to be used for the new package.<br/>
    "! <br/>
    "! For objects the current active state in SAP HANA repository is transferred into HTA repository.<br/>
    "! Transport requests and tasks are not used for local objects, if ABAP package/devclass is $TMP.
    "!
    "! @parameter i_trkorr | Transport request to be used for change recording of the component(s) to be synced.
    "! @parameter i_devclass | Development class (ABAP package) to be used in case a new package or new object of new package is synced.
    "! @parameter i_suppress_dialog | If set to 'X', no dialogs will be shown, e.g. for transport request selection or object catalog entry creation or...<br/>
    "!                                In this case make sure to provide necessary information (i_trkorr, i_devclass) if it might be required during synchronization.
    "! @parameter i_force | With i_force = abap_true, the synchronization will also synchronize components that are already synchronized (same version in HTA
    "!                      repository and HANA repository) <br/>
    "!                      Default value is abap_false.
    "! @parameter r_result | Transport requests (not tasks) used for the synchronization. If multiple objects are synchronized (e.g. by using IF_CTS_HTA_COMPONENT_LIST)
    "!                       a sorted list of transport requests is returned, containing all HTA components synchronized per transport request.
    "! @raising cx_cts_hta_canceled_by_user | In case the user does cancel some action in dialog mode
    "! @raising cx_cts_hta_name_conflict    | In case the created component (object/package) has different case compared to data already available in HTA
    "! @raising cx_cts_hta_no_hana_database | In case the current system is not running on HANA
    "! @raising cx_cts_hta_unknown_master_lang | Exception in case master lang is either not maintained in HANA or can not be converted to ABAP language. See cx_cts_hta_unknown_master_lang
    "! @raising cx_cts_hta_wbo | In case objects can not be locked on a task of a transport request or in case catalog entry can not be created
    "! @raising cx_cts_hta_wrong_status | In case the status does not allow synchronization and requires a deployment first.
    "! @raising cx_cts_hta | Exception in case of other errors before or during sync.
    synchronize
      IMPORTING
        i_trkorr          TYPE if_cts_hta_types=>ty_trkorr OPTIONAL
        i_devclass        TYPE if_cts_hta_types=>ty_devclass OPTIONAL
        i_suppress_dialog TYPE c DEFAULT space
        i_force           TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_result)   TYPE if_cts_hta_types=>ty_sync_results
      RAISING
        cx_cts_hta_canceled_by_user
        cx_cts_hta_name_conflict
        cx_cts_hta_no_hana_database
        cx_cts_hta_unknown_master_lang
        cx_cts_hta_wbo
        cx_cts_hta_wrong_status
        cx_cts_hta,

    "! Returns the deploy_state of this IF_CTS_HTA_COMPONENT indicating whether the component was deployed to HANA or not.<br/>
    "!
    "! @parameter r_result | An instance of enumeration CE_CTS_HTA_DEPLOY_STATE.<br/>
    "!                       For IF_CTS_HTA_PACKAGE and IF_CTS_HTA_OBJECT either CE_CTS_HTA_DEPLOY_STATE=&gt;DEPLOYED
    "!                         or CE_CTS_HTA_DEPLOY_STATE=&gt;NOT_DEPLOYED.<br/>
    "!                       For if_cts_hta_full_package or if_cts_hta_component_list
    "!                       <ul><li>CE_CTS_HTA_DEPLOY_STATE=&gt;DEPLOYED if ALL contained components have deploy_state CE_CTS_HTA_DEPLOY_STATE=&gt;DEPLOYED or if IF_CTS_HTA_COMPONENT_LIST/IF_CTS_HTA_FULL_PACKAGE is empty</li>
    "!                           <li>CE_CTS_HTA_DEPLOY_STATE=&gt;PARTLY_DEPLOYED if at least 1 contained component has deploy_state CE_CTS_HTA_DEPLOY_STATE=&gt;NOT_DEPLOYED
    "!                                          or a contained IF_CTS_HTA_FULL_PACKAGE has deploy state CE_CTS_HTA_DEPLOY_STATE=&gt;PARTLY_DEPLOYED</li>
    "!                           <li>CE_CTS_HTA_DEPLOY_STATE=&gt;NOT_DEPLOYED if ALL contained component have deploy_state CE_CTS_HTA_DEPLOY_STATE=&gt;NOT_DEPLOYED</li></ul>
    "! @raising cx_cts_hta_not_found | In case the package or object for which get_deploy_state is called does not exist in HTA repository.<br/>
    "!                                 Exception can also occur if IF_CTS_HTA_COMPONENT is of type IF_CTS_HTA_COMPONENT_LIST or IF_CTS_HTA_FULL_PACKAGE
    "!                                 in case at least one of its contained objects/packages does not exist in HTA repository.
    get_deploy_state
      RETURNING
        VALUE(r_result) TYPE REF TO ce_cts_hta_deploy_state
      RAISING
        cx_cts_hta_not_found,

    "! Sets the prework entry for the package(s) of the component(s) so that the component(s) can be deployed.<br/>
    "! Prework done is a flag on package level to prevent deployments of packages and their objects during imports. This flag
    "! is only evaluated if the package was configured with deploymode 'prework required'. In all other cases this flag is
    "! ignored and not set.<br/>
    "! If set_prework is called on an IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE or IF_CTS_HTA_OBJECT, the prework is set for
    "! the package itself or for the package of the object.<br/>
    "! If set_prework is called on an IF_CTS_HTA_COMPONENT_LIST, the prework is set for all contained packages in this list be
    "! it as package directly (IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE) or as package of an object (IF_CTS_HTA_OBJECT).
    "! @parameter i_prework | Value for the prework_done flag. By default prework is set to done.<br/>
    "!                        To set prework to 'not done', use CE_CTS_HTA_PREWORK=&gt;PREWORK_NOT_DONE
    set_prework
      IMPORTING
        i_prework TYPE REF TO ce_cts_hta_prework DEFAULT ce_cts_hta_prework=>prework_done,

    "! Setting deploy mode of a package and the contained objects.<br/>
    "! The deploy mode defines whether SAP HANA repository packages/objects are deployed directly to SAP HANA when
    "! imported:<br/>
    "! <ul><li>Mode ce_cts_hta_deploy_mode=&gt;directly (default): Packages/objects are deployed directly.</li>
    "! <li>Mode ce_cts_hta_deploy_mode=&gt;prework_required: Packages/objects are deployed only after the prework is done.</li></ul><br/>
    "! The deploy mode is set at package level and applies to the deployment of all objects in the package.<br/><br/>
    "! If set_deploy_mode is called on an IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE the deploy mode is set for
    "! the package itself.<br/>
    "! If set_deploy_mode is called on an IF_CTS_HTA_OBJECT, the deploy mode is set for the package of the object.<br/>
    "! If set_deploy_mode is called on an IF_CTS_HTA_COMPONENT_LIST, the deploy mode is set for all contained packages in this list be
    "! it as package directly (IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE) or as package of an object (IF_CTS_HTA_OBJECT).<br/>
    "! <br/>
    "! Changes to deploy mode will require the package (LIMU HOTP) to be locked on a task of a transport request.<br/>
    "! For this the user will get a SAP GUI dialog to select/create a transport request/task. To prevent this dialog you can pass a transport
    "! request or task to be used (i_trkorr). If the package is already locked on a different transport request than provided, the dialog
    "! will be shown anyhow to be able to select the correct transport request or cancel the synchronization. If you do not want to have the SAP GUI dialogs
    "! shown in any case, you can use i_suppress_dialog. In this case the exception cx_cts_hta_wbo will be raised if the provided transport request/task
    "! can not be used.<br/><br/>
    "! Note 1: Deploy mode will only be changed if the provided deploy mode is different than current setting. Also the package is only locked on a
    "!         transport request if deploy mode was changed.<br/>
    "! Note 2: If you change deploy mode from prework_required to directly, consider also to transport the objects of the package if they should be
    "!         deployed during import of the change of the prework setting.
    "!
    "! @parameter i_deploy_mode | Deploy mode to be set.
    "! @parameter i_trkorr | Transport request to be used for change recording of the deploy mode change.
    "! @parameter i_suppress_dialog | If set to 'X', no dialogs will be shown, e.g. for transport request selection.<br/>
    "!                                In this case make sure package was already locked before or provide correct
    "!                                transport/request (i_trkorr).
    "! @raising cx_cts_hta_wbo | In case objects can not be locked on a task of a transport request
    "! @raising cx_cts_hta | Exception in case of other errors
    set_deploy_mode
      IMPORTING
        i_deploy_mode     TYPE REF TO ce_cts_hta_deploy_mode
        i_trkorr          TYPE if_cts_hta_types=>ty_trkorr OPTIONAL
        i_suppress_dialog TYPE c DEFAULT space
      RAISING
        cx_cts_hta_wbo
        cx_cts_hta,

    "! Setting the translation relevance of a package and the contained objects.<br/>
    "! The translation relevance defines whether texts of SAP HANA repository objects should be read to ABAP to enable
    "! translation in ABAP translation process. The translation relevance defined in HTA is only considered if the ABAP
    "! package (development class) is flagged as to be translated.<br/>
    "! <ul><li>ce_cts_hta_translation=&gt;relevant_for_translation (default): Object texts should be synchronized and
    "! made available for translation. CAUTION: If translation relevance is changed from not_relevant_for_translation to
    "! relevant_for_translation the texts are only available for translation after next synchronization of the objects. Consider
    "! parameter i_force in method synchronize objects did not change in HANA.</li>
    "! <li>ce_cts_hta_translation=&gt;not_relevant_for_translation: Object texts should not be synchronized and do not need
    "! translation. CAUTION: If you have already synchronized and translated the texts of these objects, this action deletes
    "! the translations.</li></ul><br/>
    "! The translation relevance is set at package level and applies to all objects in the package.<br/><br/>
    "! If set_translation_relevance is called on an IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE the deploy mode is set for
    "! the package itself.<br/>
    "! If set_translation_relevance is called on an IF_CTS_HTA_OBJECT, the deploy mode is set for the package of the object.<br/>
    "! If set_translation_relevance is called on an IF_CTS_HTA_COMPONENT_LIST, the deploy mode is set for all contained packages
    "! in this list be it as package directly (IF_CTS_HTA_PACKAGE or IF_CTS_HTA_FULL_PACKAGE) or as package of an object (IF_CTS_HTA_OBJECT).<br/>
    "! <br/>
    "! Changes to translation relevance requires the whole package (R3TR HOTA) to be locked on a task of a transport request.<br/>
    "! For this the user will get a SAP GUI dialog to select/create a transport request/task. To prevent this dialog you can pass a transport
    "! request or task to be used (i_trkorr). If the package is already locked on a different transport request than provided, the dialog
    "! will be shown anyhow to be able to select the correct transport request or cancel the synchronization. If you do not want to have the SAP GUI dialogs
    "! shown in any case, you can use i_suppress_dialog. In this case the exception cx_cts_hta_wbo will be raised if the provided transport request/task
    "! can not be used.<br/><br/>
    "! Note: Translation relevance will only be changed if the provided Translation relevance is different than current setting.
    "!       Also the package is only locked on a transport request if Translation relevance was changed.
    "!
    "! @parameter i_translation_relevance | Translation relevance to be set.
    "! @parameter i_trkorr | Transport request to be used for change recording of the translation relevance change.
    "! @parameter i_suppress_dialog | If set to 'X', no dialogs will be shown, e.g. for transport request selection.<br/>
    "!                                In this case make sure package was already locked before or provide correct
    "!                                transport/request (i_trkorr).
    "! @raising cx_cts_hta_wbo | In case objects can not be locked on a task of a transport request
    "! @raising cx_cts_hta | Exception in case of other errors
    set_translation_relevance
      IMPORTING
        i_translation_relevance TYPE REF TO ce_cts_hta_translation
        i_trkorr                TYPE if_cts_hta_types=>ty_trkorr OPTIONAL
        i_suppress_dialog       TYPE c DEFAULT space
      RAISING
        cx_cts_hta_wbo
        cx_cts_hta,

    "! Returns the synchronization state of this IF_CTS_HTA_COMPONENT indicating whether the component is in sync between HTA(ABAP) repository and HANA or not
    "! and whether it can be synchronized or not.<br/>
    "!
    "! @parameter e_reasons_can_not_be_synced | Contains a list of exceptions with the reasons why packages/objects can not be synchronized.<br/>
    "!                                          Table contains only entries if r_result = CE_CTS_HTA_SYNC_STATE=&gt;CAN_NOT_BE_SYNCHRONIZED. Each exception
    "!                                          contains the package/object as attribute cts_hta_component for which it was raised.<br/>
    "!                                          If get_sync_state is called on an instance of IF_CTS_HTA_PACKAGE or IF_CTS_HTA_OBJECT then the list contains
    "!                                          one entry, in other cases it might contain several entries, one per package/object that can not be synchronized.<br/>
    "!                                          The exceptions returned here are the same as you would get when calling synchronize on the package/object/list/full package.<br/>
    "!                                          Example reasons: CX_CTS_HTA_WRONG_STATUS, CX_CTS_HTA_NAME_CONFLICT,...
    "! @parameter r_result | An instance of enumeration CE_CTS_HTA_SYNC_STATE.<br/>
    "!                       If the IF_CTS_HTA_COMPONENT instance is of type IF_CTS_HTA_PACKAGE and IF_CTS_HTA_OBJECT the returned sync state is for this package/object<br/>
    "!                       If the IF_CTS_HTA_COMPONENT instance is of type IF_CTS_HTA_FULL_PACKAGE or IF_CTS_HTA_COMPONENT_LIST the returned sync state is an aggregate over
    "!                       the sync states of the contained packages/objects/full packages and returned in the following way:
    "!                       <ul><li>CE_CTS_HTA_SYNC_STATE=&gt;IN_SYNC if ALL contained components have sync state CE_CTS_HTA_SYNC_STATE=&gt;IN_SYNC or if IF_CTS_HTA_COMPONENT_LIST/IF_CTS_HTA_FULL_PACKAGE is empty</li>
    "!                           <li>CE_CTS_HTA_SYNC_STATE=&gt;NOT_IN_SYNC if at least 1 contained IF_CTS_HTA_PACKAGE, IF_CTS_HTA_OBJECT of IF_CTS_HTA_FULL_PACKAGE has sync state CE_CTS_HTA_SYNC_STATE=&gt;NOT_IN_SYNC.
    "!                             All other components either also have CE_CTS_HTA_SYNC_STATE=&gt;NOT_IN_SYNC or CE_CTS_HTA_SYNC_STATE=&gt;IN_SYNC</li>
    "!                           <li>CE_CTS_HTA_SYNC_STATE=&gt;CAN_NOT_BE_SYNCHRONIZED if at least 1 contained component has sync state CE_CTS_HTA_SYNC_STATE=&gt;CAN_NOT_BE_SYNCHRONIZED</li></ul>
    "! @raising cx_cts_hta_no_hana_database | In case the current system is not running on HANA
    get_sync_state
      EXPORTING
        e_reasons_can_not_be_synced TYPE if_cts_hta_types=>ty_cx_cts_htas
      RETURNING
        VALUE(r_result)             TYPE REF TO ce_cts_hta_sync_state
      RAISING
        cx_cts_hta_no_hana_database.

ENDINTERFACE.