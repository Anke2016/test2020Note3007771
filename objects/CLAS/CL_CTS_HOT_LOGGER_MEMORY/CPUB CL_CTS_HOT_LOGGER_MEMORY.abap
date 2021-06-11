CLASS cl_cts_hot_logger_memory DEFINITION PUBLIC INHERITING FROM cl_cts_hot_logger_abstract FINAL CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      create_instance
        IMPORTING
          iv_msg_id          TYPE sprot_u-ag DEFAULT 'SCTS_HOT'
        RETURNING
          VALUE(rr_instance) TYPE REF TO if_cts_hot_logger.

    METHODS:
      display_log_sap_gui
        IMPORTING
          iv_title   TYPE syst_title OPTIONAL
          iv_heading TYPE logline OPTIONAL
          iv_level   TYPE protlevel DEFAULT if_cts_hot_logger=>co_level_2,

      get_log_messages
        RETURNING
          VALUE(rt_messages) TYPE if_cts_hot_logger=>ty_t_sprot_u,

      if_cts_hot_logger~flush REDEFINITION.
