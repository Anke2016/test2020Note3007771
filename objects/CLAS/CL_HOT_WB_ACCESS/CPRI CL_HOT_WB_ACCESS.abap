  PRIVATE SECTION.
    METHODS:
      activate_hdi_objects
        IMPORTING
          it_objects            TYPE cl_cts_hdi_object=>ty_t_hdi_objects
        EXPORTING
          et_successful_objects TYPE cl_cts_hdi_object=>ty_t_hdi_objects
          et_log                TYPE if_cts_hta_types=>ty_deploy_messages
          ev_max_severity       TYPE sprot_u-severity. "if_cts_hot_logger=>ty_t_sprot_u.