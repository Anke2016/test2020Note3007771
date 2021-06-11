  METHOD enqueue_object.


    CALL FUNCTION 'ENQUEUE_ENQ_HOTO_HDI'
      EXPORTING
        mode_cts_hdi_object = 'E'
        abap_hdi_ns_cont    = iv_objname(40)
        abap_hdi_path       = iv_objname+40(70)
      EXCEPTIONS
        foreign_lock        = 1
        system_failure      = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.

      CASE sy-subrc.
        WHEN '1'." Object already locked by another user
          DATA(lv_user_name) = sy-msgv1.

          RAISE EXCEPTION TYPE cx_svrs_object_already_locked EXPORTING user = sy-uname.

        WHEN OTHERS.

          RAISE EXCEPTION TYPE cx_svrs_error_in_configclass
            EXPORTING
              textid = CONV #( 'Fehler bei der Sperrung des Objekts'(002) ).
      ENDCASE.

    ENDIF.
  ENDMETHOD.