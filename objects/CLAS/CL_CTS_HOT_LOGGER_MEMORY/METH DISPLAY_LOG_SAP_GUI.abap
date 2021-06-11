  METHOD display_log_sap_gui.
    NEW cl_cts_hot_ext_call_intenal( )->if_cts_hot_ext_call_internal~display_log(
      EXPORTING
        i_log_messages = CONV #( mt_messages )
        i_title        = iv_title
        i_heading      = iv_heading
        i_level        = iv_level
    ).
  ENDMETHOD.