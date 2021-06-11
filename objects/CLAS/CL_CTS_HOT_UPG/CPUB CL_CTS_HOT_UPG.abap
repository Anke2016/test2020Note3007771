CLASS cl_cts_hot_upg DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(rr_instance) TYPE REF TO cl_cts_hot_upg.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check all HANA Repo Pkgs and Objs are same in HTA and HANA</p>
      "! Checks that all HANA Repository Packages and HANA Repository Objects known in HTA are deployed to HANA and whether they
      "! have still the same version (content) in HTA and HANA
      check_consistency
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Update status of HANA Repo Pkgs and Objs in HTA for revoke</p>
      "! Compare the HTA tables used in upgrade with HTA tables from before upgrade and prepare redeployment of HANA Repository Packages and
      "! HANA Repository Objects after revoke.<br/>
      "! <ul><li>All packages/objects that were created during upgrade need to be added as deletion to HTA tables</li>
      "! <li>All packages/objects that are different in HTA table before upgrade and HTA table after upgrade are reset to "imported" so that they get deployed.</li></ul>
      "! After prepare_redeployment the redeployment needs to be executed, e.g. with FUBA SNHI_DELIVERY_UNIT_MIGRATION.
      prepare_redeployment
        RETURNING
          VALUE(rv_success) TYPE abap_bool.
