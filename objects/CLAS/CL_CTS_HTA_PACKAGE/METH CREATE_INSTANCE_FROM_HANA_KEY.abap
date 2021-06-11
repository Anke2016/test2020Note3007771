  METHOD create_instance_from_hana_key.
    DATA: lr_hot_package TYPE REF TO cl_cts_hot_package.
    TRY.
        lr_hot_package = cl_cts_hot_package=>create_instance( iv_hana_package_id = i_hana_package_name ).
      CATCH cx_hana_object_transport INTO DATA(lr_cx).
        IF lr_cx->if_t100_message~t100key = cx_hana_object_transport=>create_package_hash_error.
          RAISE EXCEPTION TYPE cx_cts_hta_hashing
            EXPORTING
              textid             = cx_cts_hta_hashing=>create_package_hash_error
              previous           = lr_cx
              message_variable_1 = CONV #( i_hana_package_name ).
        ELSE.
          RAISE EXCEPTION lr_cx.
        ENDIF.
    ENDTRY.
    r_result = NEW cl_cts_hta_package( lr_hot_package ).
  ENDMETHOD.