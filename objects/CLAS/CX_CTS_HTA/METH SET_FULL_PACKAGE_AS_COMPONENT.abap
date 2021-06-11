  METHOD set_full_package_as_component.
    IF cts_hta_component IS BOUND AND cts_hta_component->transport_object_name(40) = i_hta_full_package->transport_object_name(40).
      me->cts_hta_component = i_hta_full_package.
    ENDIF.
  ENDMETHOD.