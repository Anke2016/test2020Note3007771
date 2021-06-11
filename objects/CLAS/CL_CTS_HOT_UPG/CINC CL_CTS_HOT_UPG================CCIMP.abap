*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_hot_logger IMPLEMENTATION.

  METHOD if_cts_hot_logger~flush.
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES
        xmsg = mt_messages.
    CLEAR mt_messages.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_logger IMPLEMENTATION.

  METHOD constructor.
    mr_logger = COND #( WHEN ir_logger IS BOUND THEN ir_logger ELSE NEW lcl_hot_logger( ) ).
  ENDMETHOD.

  METHOD error.
    mr_logger->error( iv_msg_nr ).
  ENDMETHOD.

  METHOD flush.
    mr_logger->flush( ).
  ENDMETHOD.

  METHOD message.
    mr_logger->message( iv_msg_nr = iv_msg_nr
                        iv_level = iv_level
                        iv_var1 = iv_var1
                        iv_var2 = iv_var2
                        iv_var3 = iv_var3
                        iv_var4 = iv_var4 ).
  ENDMETHOD.


  METHOD long_text.
    mr_logger->long_text( iv_msg_id   = iv_msg_id
                          iv_msg_nr   = iv_msg_nr
                          iv_level    = iv_level
                          iv_severity = iv_severity
                          iv_text     = iv_text ).
  ENDMETHOD.


  METHOD abnormal_termination_exception.
    mr_logger->abnormal_termination_exception( ir_exception ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD get_akh_for_hota.
    DATA: lv_component   TYPE uffctr,
          lv_devclass    TYPE devclass,
          ls_package_akh TYPE ty_package_akh.

    IF i_hota_name IS INITIAL.
      RETURN.
    ENDIF.

    ls_package_akh = VALUE #( m_cache_package_akh[ package = i_hota_name ] OPTIONAL ).

    IF ls_package_akh IS INITIAL. "not found in cache
      ls_package_akh-package = i_hota_name.
      lv_devclass = me->get_devclass_for_hota( i_hota_name ).
      IF lv_devclass IS NOT INITIAL.
        SELECT SINGLE component FROM tdevc INTO lv_component WHERE devclass = lv_devclass.
        SELECT SINGLE ps_posid FROM df14l INTO ls_package_akh-akh WHERE fctr_id = lv_component AND as4local = 'A'.
      ENDIF.
      INSERT ls_package_akh INTO TABLE m_cache_package_akh.
    ENDIF.

    r_result = ls_package_akh-akh.
  ENDMETHOD.


  METHOD get_devclass_for_hota.
    DATA ls_tadir TYPE tadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'    " Eingabe zum TADIR-Feld PGMID
        wi_tadir_object   = 'HOTA'    " Eingabe zum TADIR-Feld OBJECT
        wi_tadir_obj_name = iv_hota_name " Eingabe zum TADIR-Feld OBJ_NAME
        wi_read_only      = 'X'       " Lesen Objektkatalogeintrag
      IMPORTING
        new_tadir_entry   = ls_tadir  " Modifizierter TADIR-Eintrag
      EXCEPTIONS
        OTHERS            = 0. "ignore all exceptions...

    "ignore not existence of tadir entry
    rv_devclass = ls_tadir-devclass.
  ENDMETHOD.


  METHOD read_hana_object_data.
    CLEAR: ev_cdata, ev_bdata.

    DATA(lr_object_api) = cl_nhi_api=>create_instance( )->get_object( ).
    DATA(lr_nhi_object_id) = cl_nhi_object_id=>create_object_id(
                                tenant = ''
                                package = iv_package_id
                                name = iv_object_name
                                suffix = iv_object_suffix
                                version = cl_nhi_active_version=>create_active_version( )
                                metadata = cl_nhi_metadata_active_ver=>create_metadata(
                                  version_id = ''
                                  edit = '' )
                                ).

    DATA(lr_read_object_request) = lr_object_api->create_read_object_req(
                lang                 = ''
                object               = lr_nhi_object_id
                session              = cl_nhi_active_session=>create_active_session(  )
                version              = cl_nhi_active_version=>create_active_version(  )
    ).

    TRY.
        DATA(lr_read_object_response) = lr_object_api->read( request = lr_read_object_request ).
      CATCH cx_nhi_hana_repository INTO DATA(lr_nhi_exc).
        DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( lr_nhi_exc->get_text( ) ).
        RAISE EXCEPTION TYPE cx_hana_object_transport
          EXPORTING
            textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
            msgv1    = ls_split_text-chunk1
            msgv2    = ls_split_text-chunk2
            msgv3    = ls_split_text-chunk3
            msgv4    = ls_split_text-chunk4
            previous = lr_nhi_exc.
    ENDTRY.

    IF lr_read_object_response IS NOT BOUND.
      RAISE EXCEPTION TYPE cx_hana_object_transport
        EXPORTING
          textid = cx_hana_object_transport=>response_is_null_error
          msgv1  = cl_nhi_read_object_req=>co_what && '->' && cl_nhi_read_object_req=>co_action.
    ENDIF.

    IF lr_read_object_response->error_code IS NOT INITIAL.
      IF lr_read_object_response->error_code = '40112'. "40112 = object not existing
        "not existing object has no cdata/bdata.
      ELSE.
        RAISE EXCEPTION TYPE cx_hana_object_transport
          EXPORTING
            textid          = cx_hana_object_transport=>read_object_error
            msgv1           = iv_package_id && '.' && iv_object_name && '.' && iv_object_suffix
            hana_error_code = lr_read_object_response->error_code
            hana_error_msg  = lr_read_object_response->error_msg.
      ENDIF.
    ELSE.
      ev_bdata = lr_read_object_response->bdata.
      ev_cdata = lr_read_object_response->cdata.
    ENDIF.
  ENDMETHOD.

  METHOD read_hana_object_version.
    IF mr_hot_hana_connector IS NOT BOUND.
      mr_hot_hana_connector = cl_cts_hot_hana_connector=>create_instance( )..
    ENDIF.

    DATA(lr_object) = cl_cts_hot_object_v1=>create_instance( iv_hana_package_id = iv_package_id
                                                             iv_hana_object_name = iv_object_name
                                                             iv_hana_object_suffix = iv_object_suffix ).
    rv_version = mr_hot_hana_connector->read_object_data_from_hana( lr_object )-hana_object_version.
  ENDMETHOD.

ENDCLASS.

CLASS lcx_error_prepare_redeployment IMPLEMENTATION.

ENDCLASS.