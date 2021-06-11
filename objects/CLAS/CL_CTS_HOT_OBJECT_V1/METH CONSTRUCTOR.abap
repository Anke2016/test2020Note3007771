  METHOD constructor.

    me->abap_hana_package_id = iv_abap_hana_package_id.
    me->abap_hana_object_name_suffix = iv_abap_hana_object_name_suffx.

    me->hana_package_id = iv_hana_package_id.
    me->hana_object_name = iv_hana_object_name.
    me->hana_object_suffix = iv_hana_object_suffix.

    me->transport_object_name       = me->abap_hana_package_id.
    me->transport_object_name+40(70) = me->abap_hana_object_name_suffix.

  ENDMETHOD.