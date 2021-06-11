  METHOD dequeue_object.

    CALL FUNCTION 'DEQUEUE_ENQ_HOTO_HDI'
      EXPORTING
        mode_cts_hdi_object = 'E'
        abap_hdi_ns_cont    = iv_objname(40)
        abap_hdi_path       = iv_objname+40(70).
  ENDMETHOD.