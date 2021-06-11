  METHOD chk_prev_broken_object_exist.
    IF me->mv_broken_repo_exists_before = abap_false AND me->mv_broken_hdi_exists_before = abap_false.

      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '732'.

    ENDIF.
  ENDMETHOD.