*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

"! Interface to RDDHANADEPLOYMENT to be able to mock the deployment.
INTERFACE lif_hana_deployer.
  "! Exeuctes HANA Deployment in RDDHANADEPLOYMENT
  METHODS execute_deployment
    IMPORTING
      i_packages            TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
      i_objects             TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
      i_abap_status         TYPE cts_hot_abap_status
    EXPORTING
      e_successful_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list
      e_successful_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list
      e_deploy_messages     TYPE if_cts_hta_types=>ty_deploy_messages
      e_max_severity        TYPE sprot_u-severity.

ENDINTERFACE.
"! Interface to HTA DB tables.
INTERFACE lif_db_access.
  METHODS:
    "! Updates the hot_status of passed package to 'I' if packaged was already deployed (hot_status = 'A')
    prepare_force_deploy_of_pkg
      IMPORTING
        i_package TYPE ref to cl_cts_hot_package,

    "! Updates the hot_status of passed object to 'I' if object was already deployed (hot_status = 'A')
    prepare_force_deploy_of_obj
      IMPORTING
        i_object TYPE ref to cl_cts_hot_object_v1.
ENDINTERFACE.