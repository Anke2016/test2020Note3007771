  METHOD if_cts_hot_ext_call_internal~get_akh_for_hota.
    DATA: lv_component   TYPE uffctr,
          lv_devclass    TYPE devclass,
          ls_package_akh TYPE ty_package_akh.

    IF i_hota_name IS INITIAL.
      RETURN.
    ENDIF.

    ls_package_akh = VALUE #( m_cache_package_akh[ package = i_hota_name ] OPTIONAL ).

    IF ls_package_akh IS INITIAL. "not found in cache
      ls_package_akh-package = i_hota_name.
      lv_devclass = me->if_cts_hot_ext_call_internal~get_devclass_for_hota( i_hota_name ).
      IF lv_devclass IS NOT INITIAL.
        SELECT SINGLE component FROM tdevc INTO lv_component WHERE devclass = lv_devclass.
        SELECT SINGLE ps_posid FROM df14l INTO ls_package_akh-akh WHERE fctr_id = lv_component AND as4local = 'A'.
      ENDIF.
      INSERT ls_package_akh INTO TABLE m_cache_package_akh.
    ENDIF.

    r_result = ls_package_akh-akh.
  ENDMETHOD.