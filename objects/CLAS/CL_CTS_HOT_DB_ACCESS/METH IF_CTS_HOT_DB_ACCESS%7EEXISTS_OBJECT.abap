  METHOD if_cts_hot_db_access~exists_object.
    SELECT COUNT( DISTINCT abap_hana_package_id ) FROM cts_hot_object
                                                  WHERE abap_hana_package_id = @i_abap_hana_package_id
                                                          AND abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
                                                          AND abap_status = 'A'
                                                  INTO @DATA(cnt).
    IF cnt = 0.
      r_exists = abap_false.
    ELSE.
      r_exists = abap_true.
    ENDIF.
  ENDMETHOD.