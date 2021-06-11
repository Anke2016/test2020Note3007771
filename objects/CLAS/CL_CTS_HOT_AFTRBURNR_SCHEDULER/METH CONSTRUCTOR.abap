  METHOD constructor.
    me->mr_cts_hot_db_access          = NEW cl_cts_hot_db_access( ).
    me->mr_hdi_object_db_access       = NEW cl_cts_hdi_object_db_access( ).
    me->mv_broken_repo_exists_before  = COND #( WHEN iv_broken_repo_exists_before IS SUPPLIED THEN iv_broken_repo_exists_before
                                                ELSE mr_cts_hot_db_access->exist_broken_object_or_package( ) ).
    me->mv_broken_hdi_exists_before   = COND #( WHEN iv_broken_hdi_exists_before IS SUPPLIED THEN iv_broken_hdi_exists_before
                                                ELSE mr_hdi_object_db_access->exists_broken_object( ) ).
  ENDMETHOD.