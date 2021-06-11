  METHOD update_error_pkg_in_hot.
    CASE i_cts_hot_package-hot_status.
      WHEN if_cts_hot_db_access=>co_hot_status_deploy_error
          OR if_cts_hot_db_access=>co_hot_status_delete_error.
        "nothing to do if package failed and was already on delete_error or deploy_error
      WHEN if_cts_hot_db_access=>co_hot_status_inactive.
        m_cts_hot_db_access->update_package_after_deploymnt( i_old_package = i_cts_hot_package
                                                             i_new_status = if_cts_hot_db_access=>co_hot_status_deploy_error ).
      WHEN if_cts_hot_db_access=>co_hot_status_to_be_deleted.
        m_cts_hot_db_access->update_package_after_deploymnt( i_old_package = i_cts_hot_package
                                                             i_new_status = if_cts_hot_db_access=>co_hot_status_delete_error ).
    ENDCASE.
  ENDMETHOD.