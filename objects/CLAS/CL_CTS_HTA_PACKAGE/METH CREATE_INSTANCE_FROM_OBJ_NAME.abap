  METHOD create_instance_from_obj_name.
    DATA: lr_hot_package TYPE REF TO cl_cts_hot_package.
    lr_hot_package = cl_cts_hot_package=>create_instance_from_objname( iv_objname = i_transport_object_name(40) iv_abap_status = i_abap_status ).

    IF lr_hot_package->hana_package_id IS INITIAL.
      RAISE EXCEPTION TYPE cx_cts_hta_not_found
        EXPORTING
          textid             = cx_cts_hta_not_found=>package_not_found_in_hta
          message_variable_1 = CONV #( i_transport_object_name(40) ).
    ENDIF.
    r_result = NEW cl_cts_hta_package( i_hot_package = lr_hot_package
                                       i_abap_status = i_abap_status ).
  ENDMETHOD.