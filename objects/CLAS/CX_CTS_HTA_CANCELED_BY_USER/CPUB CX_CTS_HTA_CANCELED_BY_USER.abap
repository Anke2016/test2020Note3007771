"! HTA exception class for errors when the user canceled any action, e.g. dialogs shown by HTA
CLASS cx_cts_hta_canceled_by_user DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.