CLASS cl_cts_hot_logger_tr DEFINITION PUBLIC INHERITING FROM cl_cts_hot_logger_abstract FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    CLASS-METHODS:
      "! Creates logger instance for passed transport request and logname from trbat.
      "!
      "! @parameter iv_trkorr | transport request for which this logger should be created
      "! @parameter iv_logname |logname for the log file (defined by trbat)
      "! @parameter rr_instance | logger instance
      create_instance
        IMPORTING
          iv_trkorr          TYPE trkorr
          iv_logname         TYPE ctslogname
        RETURNING
          VALUE(rr_instance) TYPE REF TO cl_cts_hot_logger_tr,

      "! Creates logger instance for logging into a file in /transdir/log/&lt;iv_file_name&gt;
      "!
      "! @parameter iv_file_name | name of logfile (only the filename and suffix, no path)
      "! @parameter rr_instance  | logger instance
      create_instance_for_file
        IMPORTING
          iv_file_name       TYPE string
        RETURNING
          VALUE(rr_instance) TYPE REF TO cl_cts_hot_logger_tr,

      "! Creates the filename for the HDI logfile for the given logfile created from trbat.<br/>
      "! e.g.: iv_logfile = /usr/sap/trans/ABCC123456.EFG then rv_logfile = ABCC123456_HDI.EFG
      create_hdi_logfile_name
        IMPORTING
          iv_logfile        TYPE trfile
        RETURNING
          VALUE(rv_logfile) TYPE string.

    METHODS:
      "TODO: method to be removed
      get_logfile
        RETURNING
          VALUE(rv_file) TYPE trfile,

      if_cts_hot_logger~flush REDEFINITION.
