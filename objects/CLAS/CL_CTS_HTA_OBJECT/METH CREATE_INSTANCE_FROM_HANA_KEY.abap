  METHOD create_instance_from_hana_key.
    DATA: lr_hot_object TYPE REF TO cl_cts_hot_object_v1.

    TRY.
        " reuse old classes for now as they are used everywhere...
        lr_hot_object = cl_cts_hot_object_v1=>create_instance2(
                      io_cts_hot_package    = CAST cl_cts_hta_component( i_hta_package )->m_hot_package
                      iv_hana_object_name   = i_hana_object_name
                      iv_hana_object_suffix = i_hana_object_suffix ).
      CATCH cx_hana_object_transport INTO DATA(lr_cx).
        IF lr_cx->if_t100_message~t100key = cx_hana_object_transport=>create_object_name_hash_error.
          RAISE EXCEPTION TYPE cx_cts_hta_hashing
            EXPORTING
              textid             = cx_cts_hta_hashing=>create_object_name_hash_error
              previous           = lr_cx
              message_variable_1 = CONV #( i_hana_object_name )
              message_variable_2 = CONV #( i_hana_object_suffix ).
        ELSE.
          RAISE EXCEPTION lr_cx.
        ENDIF.
    ENDTRY.

    r_result = NEW cl_cts_hta_object( i_hta_package = i_hta_package
                                      i_hot_object = lr_hot_object ).
  ENDMETHOD.