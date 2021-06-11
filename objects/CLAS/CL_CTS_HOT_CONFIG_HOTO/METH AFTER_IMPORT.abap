  METHOD after_import.

    DATA: ls_cts_hdi_object TYPE cts_hdi_object,
          ls_cts_hot_object TYPE cts_hot_object,
          lv_timestamp      TYPE timestampl,
          lr_hdi_object_dao TYPE REF TO if_cts_hdi_object_db_access,
          lv_trobj_name     TYPE  trobj_name.

    lr_hdi_object_dao = NEW cl_cts_hdi_object_db_access( ).
    GET TIME STAMP FIELD lv_timestamp. "  UTC time

    IF iv_objname CA '/'.
      DATA(lv_hdi_object) = 'X'.
    ENDIF.

    IF lv_hdi_object = 'X'.
      SELECT SINGLE abap_hdi_cont_namespace abap_hdi_obj_path_name_suffix abap_status
                    FROM cts_hdi_object INTO ls_cts_hdi_object
                         WHERE abap_hdi_cont_namespace       = iv_objname(40) AND
                               abap_hdi_obj_path_name_suffix = iv_objname+40(70) AND
                               abap_status                   = 'I'.
      IF sy-subrc = 0.  " Inactive version exists ==> object is either new or updated/modified
*      If an 'I' version is imported, ABAP_STATUS = 'M' must be deleted ( No info msg needed)
        lv_trobj_name = iv_objname.
        lr_hdi_object_dao->delete_hdi_object_abap_state( iv_transport_object_name = lv_trobj_name
                                                          iv_abap_status = if_cts_hdi_abap_types=>co_abap_status_modified ).
        RETURN.         " ==> we are in insert or modify case, but for sure not in deletion case!
      ENDIF.

      SELECT SINGLE * FROM cts_hdi_object INTO ls_cts_hdi_object
                           WHERE abap_hdi_cont_namespace       = iv_objname(40) AND
                                 abap_hdi_obj_path_name_suffix = iv_objname+40(70) AND
                                 abap_status                   = 'A'.
      IF sy-subrc <> 0.
        RETURN.           "Should not occur in scwb or snote context
      ENDIF.
*     When we come here only an active version exists (before implementation) and no inactive version.
*     This is only possible in deletion case.
      ls_cts_hdi_object-hot_status = 'D'.
      ls_cts_hdi_object-abap_status = 'I'.

*     NEW
      ls_cts_hdi_object-abap_changed_at = lv_timestamp.
      ls_cts_hdi_object-abap_changed_by = sy-uname.
      CLEAR: ls_cts_hdi_object-devenv_changed_at,
             ls_cts_hdi_object-abap_synced_at,
             ls_cts_hdi_object-abap_synced_by,
             ls_cts_hdi_object-abap_deployed_at,
             ls_cts_hdi_object-abap_deployed_by,
             ls_cts_hdi_object-hdi_content_bdata,
             ls_cts_hdi_object-hdi_content_hash,
             ls_cts_hdi_object-hdi_content_size,
*     Is there a possibility to fill ls_cts_hdi_object-abap_trkorr?
*     if not clear it
             ls_cts_hdi_object-abap_trkorr.

      MODIFY cts_hdi_object FROM ls_cts_hdi_object.
      IF sy-subrc = 0.

        lv_trobj_name = iv_objname.
        lr_hdi_object_dao->delete_hdi_object_abap_state( iv_transport_object_name = lv_trobj_name
                                                          iv_abap_status = if_cts_hdi_abap_types=>co_abap_status_modified ).
      ENDIF.

    ELSE.
      SELECT SINGLE abap_hana_package_id abap_hana_object_name_suffix abap_status
                    FROM cts_hot_object INTO ls_cts_hot_object
                         WHERE abap_hana_package_id         = iv_objname(40) AND
                               abap_hana_object_name_suffix = iv_objname+40(70) AND
                               abap_status                  = 'I'.
      IF sy-subrc = 0.  " Inactive version exists ==> object is either new or updated/modified
        RETURN.         " ==> we are in insert or modify case, but for sure not in deletion case!
      ENDIF.

      SELECT SINGLE * FROM cts_hot_object INTO ls_cts_hot_object
                           WHERE abap_hana_package_id         = iv_objname(40) AND
                                 abap_hana_object_name_suffix = iv_objname+40(70) AND
                                 abap_status                  = 'A'.
      IF sy-subrc <> 0.
        RETURN.           "Should not occur in scwb or snote context
      ENDIF.
*     When we come here only an active version exists (before implementation) and no inactive version.
*     This is only possible in deletion case.
      ls_cts_hot_object-hot_status = 'D'.
      ls_cts_hot_object-abap_status = 'I'.
      MODIFY cts_hot_object FROM ls_cts_hot_object.
    ENDIF.

  ENDMETHOD.