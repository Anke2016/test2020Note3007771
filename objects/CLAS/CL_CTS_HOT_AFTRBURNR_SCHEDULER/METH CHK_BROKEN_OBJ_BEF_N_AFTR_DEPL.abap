  METHOD chk_broken_obj_bef_n_aftr_depl.
    IF ( me->mv_broken_repo_exists_before = abap_true AND
         me->mr_cts_hot_db_access->exist_broken_object_or_package( ) = abap_true AND
         me->mv_redeploy_mode CA 'XR'
       )
   OR ( me->mv_broken_hdi_exists_before = abap_true AND
        me->mr_hdi_object_db_access->exists_broken_object( ) = abap_true AND
        me->mv_redeploy_mode CA 'XH' ).
      "trigger redeployment
      rv_redpoly = abap_true.
    ELSE.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '733'.
    ENDIF.
  ENDMETHOD.