  METHOD create_instance_from_obj_name.
    DATA: lr_hot_object      TYPE REF TO cl_cts_hot_object_v1,
          lr_hta_api_factory TYPE REF TO cl_cts_hta_api_factory.

    lr_hot_object = cl_cts_hot_object_v1=>create_instance_from_objname( iv_objname = i_transport_object_name(110) iv_abap_status = i_abap_status ).

    IF lr_hot_object->hana_package_id IS INITIAL.
      RAISE EXCEPTION TYPE cx_cts_hta_not_found
        EXPORTING
          textid             = cx_cts_hta_not_found=>object_not_found_in_hta
          message_variable_1 = CONV #( i_transport_object_name(40) )
          message_variable_2 = CONV #( i_transport_object_name+40(70) ).
    ENDIF.
    r_result = NEW cl_cts_hta_object( i_hta_package = i_hta_package
                                      i_hot_object  = lr_hot_object
                                      i_abap_status = i_abap_status ).
  ENDMETHOD.