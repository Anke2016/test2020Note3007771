  METHOD get_object_header_from_repo.
    DATA: "ls_cts_hot_object TYPE cts_hot_object,
      ls_cts_hdi_object TYPE cts_hdi_object,
      ls_cts_hot_object TYPE cts_hot_object,
      lv_objname        TYPE versobjnam,
      lv_date_time      LIKE ls_cts_hdi_object-abap_synced_at.

    IF iv_objname CA '/'.
      DATA(lv_hdi_object) = 'X'.
    ENDIF.
    lv_objname = iv_objname.

    IF lv_hdi_object = 'X'.
      IF iv_state EQ 'I'.
        SELECT SINGLE * FROM cts_hdi_object INTO ls_cts_hdi_object
                                            WHERE abap_hdi_cont_namespace       = iv_objname(40) AND
                                                  abap_hdi_obj_path_name_suffix = iv_objname+40(70) AND
                                                  abap_status                   = iv_state." 'I'
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE cx_svrs_object_not_found
            EXPORTING
              objtype = me->av_objtype
              objname = lv_objname
              versno  = '99999'. "Inactive version
        ENDIF.

        IF ( ls_cts_hdi_object-hot_status = 'N' OR ls_cts_hdi_object-hot_status = 'D' ). "should not occur
*        should not occur
        ELSEIF ls_cts_hdi_object-hot_status = 'I'.                           " entry has been implemented via SNOTE/SCWB
          rs_header-uname    = ls_cts_hdi_object-abap_synced_by.
          CONVERT TIME STAMP ls_cts_hdi_object-abap_synced_at TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ENDIF.
      ELSE. " iv_state EQ 'A'
        SELECT SINGLE * FROM cts_hdi_object INTO ls_cts_hdi_object
                                            WHERE abap_hdi_cont_namespace       = iv_objname(40) AND
                                                  abap_hdi_obj_path_name_suffix = iv_objname+40(70) AND
                                                  abap_status                   = iv_state. "'A'.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE cx_svrs_object_not_found
            EXPORTING
              objtype = me->av_objtype
              objname = lv_objname
              versno  = '99998'. "Active version
        ENDIF.

        IF ls_cts_hdi_object-hot_status = 'N' OR                              " entry has been synced
           ls_cts_hdi_object-hot_status = 'A' OR                              " entry has been deployed/activated
           ( ls_cts_hdi_object-hot_status = 'I' AND                           " SNOTE or SCWB implementation and
             ls_cts_hdi_object-abap_import_timestamp = 0 ).                   " hot_status I because prework was not yet done
          rs_header-uname    = ls_cts_hdi_object-abap_synced_by.
          CONVERT TIME STAMP ls_cts_hdi_object-abap_synced_at TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ELSEIF ls_cts_hdi_object-hot_status = 'I' AND                         " R3trans import
           ls_cts_hdi_object-abap_import_timestamp NE 0.
          rs_header-uname = ls_cts_hdi_object-abap_synced_by.
          lv_date_time    = ls_cts_hdi_object-abap_import_timestamp(14).
          CONVERT TIME STAMP lv_date_time TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ELSEIF ( ls_cts_hdi_object-hot_status = 'D' ).
*         anything to do?
        ENDIF.

      ENDIF.

    ELSE.
      IF iv_state EQ 'I'.
        SELECT SINGLE * FROM cts_hot_object INTO ls_cts_hot_object
                                            WHERE abap_hana_package_id         = iv_objname(40) AND
                                                  abap_hana_object_name_suffix = iv_objname+40(70) AND
                                                  abap_status                  = iv_state. " 'I'
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE cx_svrs_object_not_found
            EXPORTING
              objtype = me->av_objtype
              objname = lv_objname
              versno  = '99999'. "Inactive version
        ENDIF.

        IF ( ls_cts_hot_object-hot_status = 'N' OR ls_cts_hot_object-hot_status = 'D' ). "should not occur
*        should not occur
        ELSEIF ls_cts_hot_object-hot_status = 'I'.                           " entry has been implemented via SNOTE/SCWB
          rs_header-uname    = ls_cts_hot_object-abap_synced_by.
          CONVERT TIME STAMP ls_cts_hot_object-abap_synced_at TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ENDIF.
      ELSE. " iv_state EQ 'A'
        SELECT SINGLE * FROM cts_hot_object INTO ls_cts_hot_object
                                            WHERE abap_hana_package_id         = iv_objname(40) AND
                                                  abap_hana_object_name_suffix = iv_objname+40(70) AND
                                                  abap_status                  = iv_state. "'A'.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE cx_svrs_object_not_found
            EXPORTING
              objtype = me->av_objtype
              objname = lv_objname
              versno  = '99998'. "Active version
        ENDIF.

        IF ls_cts_hot_object-hot_status = 'N' OR                              " entry has been synced
           ls_cts_hot_object-hot_status = 'A' OR                              " entry has been deployed/activated
           ( ls_cts_hot_object-hot_status = 'I' AND                           " SNOTE or SCWB implementation and
             ls_cts_hot_object-abap_import_timestamp = 0 ).                   " hot_status I because prework was not yet done
          rs_header-uname    = ls_cts_hot_object-abap_synced_by.
          CONVERT TIME STAMP ls_cts_hot_object-abap_synced_at TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ELSEIF ls_cts_hot_object-hot_status = 'I' AND                         " R3trans import
           ls_cts_hot_object-abap_import_timestamp NE 0.
          rs_header-uname    = ls_cts_hot_object-abap_synced_by.
          lv_date_time = ls_cts_hot_object-abap_import_timestamp(14).
          CONVERT TIME STAMP lv_date_time TIME ZONE sy-zonlo INTO DATE rs_header-udate TIME rs_header-utime.
        ELSEIF ( ls_cts_hot_object-hot_status = 'D' ).
*         anything to do?
        ENDIF.
      ENDIF.

    ENDIF.

    rs_header-objname = iv_objname.
    rs_header-objtype = me->av_objtype.
    CLEAR rs_header-ename. "necessary ?
    CLEAR rs_header-etime. "necessary ?
    CLEAR rs_header-edate. "necessary ?
  ENDMETHOD.