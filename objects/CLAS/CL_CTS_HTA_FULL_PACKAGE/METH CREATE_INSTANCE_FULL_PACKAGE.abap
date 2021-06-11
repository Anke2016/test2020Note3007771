  METHOD create_instance_full_package.
    r_result = NEW cl_cts_hta_full_package( i_cts_hta_package = i_cts_hta_package
                                            i_cts_hta_objects = i_cts_hta_objects ).
  ENDMETHOD.