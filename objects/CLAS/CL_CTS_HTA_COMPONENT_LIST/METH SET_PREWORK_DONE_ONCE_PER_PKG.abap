  METHOD set_prework_done_once_per_pkg.
    DATA: lr_component TYPE REF TO if_cts_hta_component.

    LOOP AT i_components INTO lr_component.
      IF line_exists( c_preworked_packages[ table_line = lr_component->transport_object_name(40) ] ).
        CONTINUE.
      ENDIF.
      lr_component->set_prework( i_prework_flag ).
      INSERT lr_component->transport_object_name(40) INTO TABLE c_preworked_packages.
    ENDLOOP.
  ENDMETHOD.