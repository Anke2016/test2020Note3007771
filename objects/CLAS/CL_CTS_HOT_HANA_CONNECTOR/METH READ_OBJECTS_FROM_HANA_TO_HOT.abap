  METHOD read_objects_from_hana_to_hot.
    DATA: lo_cl_cts_hot_object    TYPE REF TO cl_cts_hot_object_v1,
          lo_nhi_object_id        TYPE REF TO cl_nhi_object_id,
          lo_read_object_request  TYPE REF TO cl_nhi_read_object_req,
          lo_read_object_response TYPE REF TO cl_nhi_read_object_res,
          ls_cts_hot_object       TYPE cts_hot_object,
          ls_hot_package          TYPE cts_hot_package,
          lv_object_metadata      TYPE REF TO cl_nhi_metadata_active_ver,
          lv_txt_obj_ref          TYPE string.

    LOOP AT i_objects INTO lo_cl_cts_hot_object.
      lo_nhi_object_id = cl_nhi_object_id=>create_object_id(
                                tenant = ''
                                package = lo_cl_cts_hot_object->hana_package_id
                                name = lo_cl_cts_hot_object->hana_object_name
                                suffix = lo_cl_cts_hot_object->hana_object_suffix
                                version = cl_nhi_active_version=>create_active_version( )
                                metadata = cl_nhi_metadata_active_ver=>create_metadata(
                                  version_id = ''
                                  activated_at = ''
                                  activated_by = ''
                                  edit = '' )
                                ).

      lo_read_object_request = me->m_nhi_object_api->create_read_object_req(
                  lang                 = ''
                  getoutgoingrefs      = abap_false
                  getincomingrefs      = abap_false
                  getreferencedobjects = abap_false
                  object               = lo_nhi_object_id
                  session              = cl_nhi_active_session=>create_active_session(  )
                  version              = cl_nhi_active_version=>create_active_version(  )
      ).

      TRY.
          lo_read_object_response = me->m_nhi_object_api->read( request = lo_read_object_request ).
        CATCH cx_nhi_hana_repository INTO DATA(nhi_exc).
          DATA(ls_split_text) = cl_cts_hot_utility=>split_text_50_chars( nhi_exc->get_text( ) ).
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
              msgv1    = ls_split_text-chunk1
              msgv2    = ls_split_text-chunk2
              msgv3    = ls_split_text-chunk3
              msgv4    = ls_split_text-chunk4
              previous = nhi_exc.
      ENDTRY.

      raise_exc_if_resp_is_not_bound( i_response = lo_read_object_response i_what = cl_nhi_read_object_req=>co_what i_action = cl_nhi_read_object_req=>co_action ).

      IF lo_read_object_response->error_code IS NOT INITIAL.
        IF lo_read_object_response->error_code = '40112'. "40112 = object not existing
          me->m_cts_hot_db_access->delete_cts_hot_object(
              i_abap_hana_package_id         = lo_cl_cts_hot_object->abap_hana_package_id
              i_abap_hana_object_name_suffix = lo_cl_cts_hot_object->abap_hana_object_name_suffix
          ).

          "also delete SMODI entries when object is deleted during sync
          me->delete_smodi_entries( i_obj_type = 'HOTO' i_obj_name = lo_cl_cts_hot_object->transport_object_name ).
        ELSE.
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid          = cx_hana_object_transport=>read_object_error
              msgv1           = lo_cl_cts_hot_object->hana_package_id && '.' && lo_cl_cts_hot_object->hana_object_name && '.' && lo_cl_cts_hot_object->hana_object_suffix
              hana_error_code = lo_read_object_response->error_code
              hana_error_msg  = lo_read_object_response->error_msg.
        ENDIF.
      ELSE.
        "all ok, data got
        "get metadata as active version metadata. cast should always work as we ask hana vor active version...
        lv_object_metadata ?= lo_read_object_response->metadata.

        CLEAR ls_cts_hot_object.
        CLEAR ls_hot_package.
        ls_cts_hot_object-abap_hana_package_id = lo_cl_cts_hot_object->abap_hana_package_id.
        ls_cts_hot_object-abap_hana_object_name_suffix = lo_cl_cts_hot_object->abap_hana_object_name_suffix.

        " set text reference only if texts were read and translation is enabled
        CLEAR ls_cts_hot_object-abap_object_reference.
        IF lo_read_object_response->texts IS NOT INITIAL OR lo_read_object_response->content_texts IS NOT INITIAL.
          "get package to check translation relevance
          ls_hot_package = me->m_cts_hot_db_access->read_cts_hot_package( lo_cl_cts_hot_object->abap_hana_package_id ).

          "Do only store texts if translation is switched on for this package.
          IF ls_hot_package-abap_no_translation = if_cts_hot_db_access=>co_hot_relevant_for_transl.
            lv_txt_obj_ref = |{ lo_cl_cts_hot_object->abap_hana_package_id };{ lo_cl_cts_hot_object->abap_hana_object_name_suffix }|.
            IF strlen( lv_txt_obj_ref ) > 70.
              TRY.
                  lv_txt_obj_ref = cl_cts_hot_utility=>string_to_hash_as_base32( lv_txt_obj_ref ).
                CATCH cx_abap_message_digest INTO DATA(lo_exc).
                  RAISE EXCEPTION TYPE cx_cts_hta_hashing
                    EXPORTING
                      textid             = cx_cts_hta_hashing=>create_object_ref_hash_error
                      previous           = lo_exc
                      message_variable_1 = CONV #( lv_txt_obj_ref ).
              ENDTRY.
            ENDIF.
            ls_cts_hot_object-abap_object_reference = lv_txt_obj_ref.
          ENDIF.
        ENDIF.
        ls_cts_hot_object-hana_package_id = lo_cl_cts_hot_object->hana_package_id.
        ls_cts_hot_object-hana_object_name = lo_cl_cts_hot_object->hana_object_name.
        ls_cts_hot_object-hana_object_suffix = lo_cl_cts_hot_object->hana_object_suffix.
        ls_cts_hot_object-hana_object_version = lo_read_object_response->metadata->version_id.
        ls_cts_hot_object-hana_source_object_version = lo_read_object_response->metadata->version_id.
        ls_cts_hot_object-abap_sync_system = sy-sysid.
        ls_cts_hot_object-hana_read_system = g_hana_sid.
        ls_cts_hot_object-hana_source_build_version = g_hana_build_version.
        "##TODO add correct status handling
        ls_cts_hot_object-abap_status = 'A'. "abap_status is always A in HOT, only set to I by SNOTE
        ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_new. "upload to HOT always ends with status NEW

        IF lo_read_object_response->bdata IS NOT INITIAL.
          ls_cts_hot_object-hana_content_bdata = lo_read_object_response->bdata.
        ENDIF.
        IF lo_read_object_response->cdata IS NOT INITIAL.
          ls_cts_hot_object-hana_content_cdata = lo_read_object_response->cdata.
        ENDIF.

        "##TODO check whether times are handled correctly this way
        ls_cts_hot_object-hana_activated_by = lv_object_metadata->activated_by.
        ls_cts_hot_object-hana_activated_at = conv_hana_actvted_at_to_timest( lv_object_metadata->activated_at ).

        "##TODO check whether times are handled correctly this way
        ls_cts_hot_object-abap_synced_by = sy-uname.
        GET TIME STAMP FIELD ls_cts_hot_object-abap_synced_at.

        me->m_cts_hot_db_access->modify_cts_hot_object( ls_cts_hot_object ).

*        handle master language text data if text is there and if translation is switched on for this package.
        IF ( lo_read_object_response->texts IS NOT INITIAL OR lo_read_object_response->content_texts IS NOT INITIAL )
           AND ls_hot_package-abap_no_translation = if_cts_hot_db_access=>co_hot_relevant_for_transl.

          " get master language from tadir
          DATA ls_tadir TYPE tadir.
          CALL FUNCTION 'TR_TADIR_INTERFACE'
            EXPORTING
              wi_tadir_pgmid    = 'R3TR'    " Eingabe zum TADIR-Feld PGMID
              wi_tadir_object   = 'HOTA'    " Eingabe zum TADIR-Feld OBJECT
              wi_tadir_obj_name = lo_cl_cts_hot_object->abap_hana_package_id    " Eingabe zum TADIR-Feld OBJ_NAME
              wi_read_only      = 'X'    " Lesen Objektkatalogeintrag
            IMPORTING
              new_tadir_entry   = ls_tadir    " Eintrag in der globalen TADIR
            EXCEPTIONS
              OTHERS            = 1.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_hana_object_transport
              EXPORTING
                textid = cx_hana_object_transport=>read_tadir_error
                msgv1  = CONV #( lo_cl_cts_hot_object->abap_hana_package_id )
                msgv2  = CONV #( sy-msgid )
                msgv3  = CONV #( sy-msgno )
                msgv4  = |msgv1={ sy-msgv1 } msgv2={ sy-msgv2 } msgv3={ sy-msgv3 } msgv4={ sy-msgv4 }|.
          ENDIF.

          "delete old text header entries so that we do not need to implement comparison which TextIDs form HANA we have in HTA and which are new or deleted in HANA
          DELETE FROM cts_hot_otexts_h WHERE abap_object_reference = ls_cts_hot_object-abap_object_reference.

          handle_texts( object_reference = ls_cts_hot_object-abap_object_reference
                        lang = ls_tadir-masterlang
                        text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object
                        texts = lo_read_object_response->texts ).
          handle_texts( object_reference = ls_cts_hot_object-abap_object_reference
                        lang = ls_tadir-masterlang
                        text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content
                        texts = lo_read_object_response->content_texts ).

          "delete old texts for removed text_references. (e.g. removed columns of a view)
          DELETE FROM cts_hot_otexts_s WHERE abap_object_reference = ls_cts_hot_object-abap_object_reference
                                         AND abap_text_reference NOT IN ( SELECT abap_text_reference FROM cts_hot_otexts_h WHERE abap_object_reference = ls_cts_hot_object-abap_object_reference AND abap_is_long_text <> 'X' ).
          DELETE FROM cts_hot_otexts_l WHERE abap_object_reference = ls_cts_hot_object-abap_object_reference
                                         AND abap_text_reference NOT IN ( SELECT abap_text_reference FROM cts_hot_otexts_h WHERE abap_object_reference = ls_cts_hot_object-abap_object_reference AND abap_is_long_text = 'X' ).
        ENDIF. "( lo_read_object_response->texts IS NOT INITIAL OR lo_read_object_response->content_texts IS NOT INITIAL ) AND ls_hot_package-abap_no_translation = if_cts_hot_db_access=>co_hot_relevant_for_transl.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.