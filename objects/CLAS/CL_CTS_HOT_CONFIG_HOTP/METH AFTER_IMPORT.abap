  METHOD after_import.

    DATA: ls_cts_hot_package TYPE cts_hot_package.

    SELECT SINGLE abap_hana_package_id  abap_status
                                        FROM cts_hot_package INTO ls_cts_hot_package
                                        WHERE abap_hana_package_id = iv_objname AND
                                              abap_status          = 'I'.
    IF sy-subrc = 0.  " Inactive version exists ==> object is either new or updated/modified
      RETURN.         " ==> we are in insert or modify case, but for sure not in deletion case!
    ENDIF.

    SELECT SINGLE * FROM cts_hot_package INTO ls_cts_hot_package
                    WHERE abap_hana_package_id = iv_objname(40) AND
                          abap_status          = 'A'.
    IF sy-subrc <> 0.
      RETURN.           "Should not occur in scwb or snote context
    ENDIF.

* When we come here only an active version exists (before implementation) and no inactive version.
* This is only possible in deletion case.
    ls_cts_hot_package-hot_status = 'D'.
    ls_cts_hot_package-abap_status = 'I'.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

*    MESSAGE ID 'SCTS_HOT' TYPE 'I' NUMBER '592'." DISPLAY LIKE 'I'.

  ENDMETHOD.