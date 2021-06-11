INTERFACE if_cts_hot_logger
  PUBLIC .
  CONSTANTS:
    co_level_1                     TYPE sprot_u-level VALUE 1,
    co_level_2                     TYPE sprot_u-level VALUE 2,
    co_level_3                     TYPE sprot_u-level VALUE 3,
    co_level_4                     TYPE sprot_u-level VALUE 4,
    co_severity_info               TYPE sprot_u-severity VALUE ' ',
    co_severity_warning            TYPE sprot_u-severity VALUE 'W',
    co_severity_post_processing    TYPE sprot_u-severity VALUE 'P',
    co_severity_error              TYPE sprot_u-severity VALUE 'E',
    co_severity_abnormal_terminatn TYPE sprot_u-severity VALUE 'A'.

  TYPES:
    ty_t_sprot_u TYPE STANDARD TABLE OF sprot_u WITH EMPTY KEY.

  METHODS:
    "! Logs the passed message on co_level_2 with co_severity_abnormal_terminatn
    abnormal_termination
      IMPORTING
        iv_msg_id TYPE sprot_u-ag OPTIONAL
        iv_msg_nr TYPE sprot_u-msgnr
        iv_var1   TYPE sprot_u-var1 OPTIONAL
        iv_var2   TYPE sprot_u-var2 OPTIONAL
        iv_var3   TYPE sprot_u-var3 OPTIONAL
        iv_var4   TYPE sprot_u-var4 OPTIONAL,

    "! Logs the passed exception on co_level_2 with co_severity_abnormal_terminatn.
    "! In addition all previous exceptions are logged as well with same level and severity.
    abnormal_termination_exception
      IMPORTING
        ix_exception TYPE REF TO cx_root,

    "! Logs an empty line on co_level_4 or on provided level and with co_severity_info or provided severity
    empty_line
      IMPORTING
        iv_level    TYPE sprot_u-level DEFAULT co_level_4
        iv_severity TYPE sprot_u-severity DEFAULT co_severity_info,

    "! Logs the passed message on co_level_2 with co_severity_error
    error
      IMPORTING
        iv_msg_id TYPE sprot_u-ag OPTIONAL
        iv_msg_nr TYPE sprot_u-msgnr
        iv_var1   TYPE sprot_u-var1 OPTIONAL
        iv_var2   TYPE sprot_u-var2 OPTIONAL
        iv_var3   TYPE sprot_u-var3 OPTIONAL
        iv_var4   TYPE sprot_u-var4 OPTIONAL,

    "! Logs the passed exception on co_level_2 with co_severity_error.<br/>
    "! In addition all previous exceptions are logged as well with same level and severity.
    error_exception
      IMPORTING
        ix_exception TYPE REF TO cx_root,

    warning_exception
      IMPORTING
        ix_exception TYPE REF TO cx_root,

    "! Persist the log to logfile/DB/...<br/>
    "! Make sure to clear the messages after they have been written. (clear mt_messages)
    flush,

    "! Returns the highest severity logged so far with this logger.<br/>
    "! Severities are ordered in the following way, lowest to highest, and with equivalent tp return code in brackets:
    "! <ol>
    "!  <li>space/I (0)</li>
    "!  <li>W (4)</li>
    "!  <li>P (6)</li>
    "!  <li>E (8)</li>
    "!  <li>A (12)</li>
    "! </ol>
    get_max_severity
      RETURNING
        VALUE(rv_max_severity) TYPE sprot_u-severity,

    "! Logs the passed message on co_level_3 with co_severity_info
    info_level_3
      IMPORTING
        iv_msg_id TYPE sprot_u-ag OPTIONAL
        iv_msg_nr TYPE sprot_u-msgnr
        iv_var1   TYPE sprot_u-var1 OPTIONAL
        iv_var2   TYPE sprot_u-var2 OPTIONAL
        iv_var3   TYPE sprot_u-var3 OPTIONAL
        iv_var4   TYPE sprot_u-var4 OPTIONAL,

    "! Logs the passed message on co_level_4 with co_severity_info
    info_level_4
      IMPORTING
        iv_msg_id TYPE sprot_u-ag OPTIONAL
        iv_msg_nr TYPE sprot_u-msgnr
        iv_var1   TYPE sprot_u-var1 OPTIONAL
        iv_var2   TYPE sprot_u-var2 OPTIONAL
        iv_var3   TYPE sprot_u-var3 OPTIONAL
        iv_var4   TYPE sprot_u-var4 OPTIONAL,

    "!
    "! @parameter iv_msg_nr | message of this message_nr must contain only &1&2&3&4 (plus additional spaces in front for SCTS_HOT but no spaces for SCTS_HDI.)<br/>
    "!                        For SCTS_HDI all long texts should be logged with msg_nr 100 and leading spaces to be part of iv_text.
    "! @parameter iv_level |
    "! @parameter iv_severity |
    "! @parameter iv_text |
    long_text
      IMPORTING
        iv_msg_id   TYPE sprot_u-ag DEFAULT 'SCTS_HDI'
        iv_msg_nr   TYPE sprot_u-msgnr DEFAULT '100'
        iv_level    TYPE sprot_u-level DEFAULT co_level_4
        iv_severity TYPE sprot_u-severity DEFAULT co_severity_info
        iv_text     TYPE string,

    "! Logs the passed message on passed level and severity
    message
      IMPORTING
        iv_msg_id   TYPE sprot_u-ag OPTIONAL
        iv_msg_nr   TYPE sprot_u-msgnr
        iv_level    TYPE sprot_u-level
        iv_severity TYPE sprot_u-severity DEFAULT co_severity_info
        iv_var1     TYPE sprot_u-var1 OPTIONAL
        iv_var2     TYPE sprot_u-var2 OPTIONAL
        iv_var3     TYPE sprot_u-var3 OPTIONAL
        iv_var4     TYPE sprot_u-var4 OPTIONAL,

    messages
      IMPORTING
        it_messages TYPE ty_t_sprot_u,

    "! Next log line will start as new section.
    new_log_section,

    "! Set global msg_id to be used for all messages.
    "! After setting a global msg_id, iv_msg_id becomes optional on the different logging methods
    set_msg_id
      IMPORTING
        iv_msg_id TYPE sprot_u-ag,

    "! Logs the passed message on co_level_3 with co_severity_warning
    warning
      IMPORTING
        iv_msg_id TYPE sprot_u-ag OPTIONAL
        iv_msg_nr TYPE sprot_u-msgnr
        iv_var1   TYPE sprot_u-var1 OPTIONAL
        iv_var2   TYPE sprot_u-var2 OPTIONAL
        iv_var3   TYPE sprot_u-var3 OPTIONAL
        iv_var4   TYPE sprot_u-var4 OPTIONAL.

ENDINTERFACE.