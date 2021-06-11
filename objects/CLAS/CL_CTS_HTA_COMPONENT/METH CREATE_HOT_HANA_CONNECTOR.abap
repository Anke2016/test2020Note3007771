  METHOD create_hot_hana_connector.
    IF gr_hot_hana_connector IS NOT BOUND. "gr_hot_hana_connector will be bound in unit tests
      TRY.
          gr_hot_hana_connector = cl_cts_hot_hana_connector=>create_instance( ).
        CATCH cx_hana_object_transport INTO DATA(lr_exc).
          IF lr_exc->if_t100_message~t100key = cx_hana_object_transport=>no_hana_database.
            RAISE EXCEPTION TYPE cx_cts_hta_no_hana_database
              EXPORTING
                textid   = lr_exc->if_t100_message~t100key
                previous = lr_exc->previous.
          ELSE.
            RAISE EXCEPTION lr_exc.
          ENDIF.
      ENDTRY.
    ENDIF.
  ENDMETHOD.