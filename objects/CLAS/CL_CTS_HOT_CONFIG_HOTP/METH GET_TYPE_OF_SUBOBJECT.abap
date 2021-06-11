  METHOD get_type_of_subobject.
    IF iv_ddic_struc EQ 'CTS_HOT_PACKAGE'.
*     no delta handling
      rv_type_of_subobject = if_cwb_dwb_object=>ac_subobj_type_wo_delta.
      RETURN.
    ENDIF.

    rv_type_of_subobject = if_cwb_dwb_object=>ac_subobj_type_tab_unordered.
  ENDMETHOD.