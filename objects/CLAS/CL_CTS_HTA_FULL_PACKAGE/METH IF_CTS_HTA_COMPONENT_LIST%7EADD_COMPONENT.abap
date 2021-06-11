  METHOD if_cts_hta_component_list~add_component.
    "add_component not supported by full_package. It is fixed after instantiation. Data must be passed using create_instance methods.
    IF me->m_instantiation_finished = abap_false.
      super->add_component( i_cts_hta_component ).
    ENDIF.
  ENDMETHOD.