  METHOD raise_exc_if_resp_is_not_bound.

    IF i_response IS NOT BOUND.
      RAISE EXCEPTION TYPE cx_hana_object_transport
        EXPORTING
          textid = cx_hana_object_transport=>response_is_null_error
          msgv1  = i_what && '->' && i_action.
    ENDIF.

  ENDMETHOD.