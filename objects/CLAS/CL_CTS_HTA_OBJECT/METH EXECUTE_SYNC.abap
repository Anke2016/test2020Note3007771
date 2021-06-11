  METHOD execute_sync.
    IF is_sync_required( i_force ) = abap_false.
      RETURN.
    ENDIF.

    create_hot_hana_connector( ).
    gr_hot_hana_connector->read_objects_from_hana_to_hot( VALUE #( ( me->m_hot_object ) ) ).
  ENDMETHOD.