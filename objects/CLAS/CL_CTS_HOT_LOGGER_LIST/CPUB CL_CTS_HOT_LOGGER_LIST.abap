CLASS cl_cts_hot_logger_list DEFINITION PUBLIC INHERITING FROM cl_cts_hot_logger_abstract FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      add_logger
        IMPORTING
          ir_logger TYPE REF TO if_cts_hot_logger,

      if_cts_hot_logger~flush REDEFINITION,
      if_cts_hot_logger~set_msg_id REDEFINITION.
