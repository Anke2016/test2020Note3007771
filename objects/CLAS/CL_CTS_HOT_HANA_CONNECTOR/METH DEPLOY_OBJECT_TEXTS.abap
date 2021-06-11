  METHOD deploy_object_texts.
    DATA: lt_nhi_objects_with_text TYPE cl_nhi_object_and_texts=>ty_object_and_texts,
          lt_nhi_texts             TYPE cl_nhi_text=>ty_texts,
          lr_write_text_resp       TYPE REF TO cl_nhi_error,
          lv_hana_lang             TYPE string,
          lv_hot_status            TYPE cts_hot_object_status,
          lr_text_deploy_result    TYPE REF TO ty_text_deploy_result,
          lv_return_code           TYPE syst_subrc,
          lt_hot_object_texts      TYPE if_cts_hot_db_access=>ty_object_texts,
          lv_max_length            TYPE string.

    CLEAR: e_failed_text_deploy_result, e_not_active_objects, e_skipped_objects, e_success_text_deploy_result, e_unknown_objects.

    LOOP AT i_hot_objects_with_lang REFERENCE INTO DATA(lr_hot_object_with_lang).
*1. Check whether object exists in HTA and whether text deployment is allowed.
      "Deployment allowed for hot_status=active (usual deployment) and hot_status=new (language back transport with LANG HOTO)
      lv_hot_status = m_cts_hot_db_access->read_hot_status_for_object(
        EXPORTING
          i_abap_hana_package_id = lr_hot_object_with_lang->cts_hot_object->abap_hana_package_id
          i_abap_hana_object_name_suffix = lr_hot_object_with_lang->cts_hot_object->abap_hana_object_name_suffix
        IMPORTING
          e_return_code = lv_return_code
      ).

      IF lv_return_code <> 0.
        INSERT lr_hot_object_with_lang->cts_hot_object INTO TABLE e_unknown_objects.
        CONTINUE. "skip text deployment for current object because it is not existing in HTA anymore (maybe deleted already, e.g. during deployment before)
      ELSEIF lv_hot_status <> if_cts_hot_db_access=>co_hot_status_active
             AND  lv_hot_status <> if_cts_hot_db_access=>co_hot_status_new.
        INSERT lr_hot_object_with_lang->cts_hot_object INTO TABLE e_not_active_objects.
        CONTINUE. "skip text deployment for current object because deployment was not successful or not needed (prework missing, switch framework, ...)
      ENDIF.

*2. Select all texts and other required data for current object
      lt_hot_object_texts = m_cts_hot_db_access->read_deployable_texts_for_obj(
          i_abap_hana_package_id = lr_hot_object_with_lang->cts_hot_object->abap_hana_package_id
          i_abap_hana_object_name_suffix = lr_hot_object_with_lang->cts_hot_object->abap_hana_object_name_suffix
          i_languages = lr_hot_object_with_lang->abap_langs
      ).

      IF lt_hot_object_texts IS INITIAL.
        INSERT lr_hot_object_with_lang->cts_hot_object INTO TABLE e_skipped_objects.
        CONTINUE. "nothing to deploy for this object
      ENDIF.

*3. loop at all texts by text_type (C for content texts and O for object texts) and language. Convert to NHI API text objects and write to HANA per language and text_type (content or object text)
      LOOP AT lt_hot_object_texts REFERENCE INTO DATA(lr_hot_object_text) GROUP BY ( abap_object_reference = lr_hot_object_text->abap_object_reference
                                                                                     text_type = lr_hot_object_text->text_type
                                                                                     language = lr_hot_object_text->language ) REFERENCE INTO DATA(lr_hot_object_text_group).

        lv_hana_lang = cl_lxe_lang=>int_to_java( lr_hot_object_text_group->language ).
        IF lv_hana_lang IS INITIAL. "TODO change to get a warning in log for this language.
          CONTINUE.
        ENDIF.

        DO 2 TIMES. "to enable special handling for chinese texts, map 1 to zh_CN as well as zh
          IF sy-index = 2 AND lv_hana_lang = 'zh_CN'.
            lv_hana_lang = 'zh'.
          ELSEIF sy-index = 2.
            EXIT. "only do 2nd loop for zh_CN to write same texts also with language zh
          ENDIF.

          "first check whether other text_type of this object and lang was already processed and whether this failed or not.
          "   for HTA, HANA content and object texts are handled identically, therefore combine in 1 result structure
          CLEAR lr_text_deploy_result.
          lr_text_deploy_result = REF #( e_failed_text_deploy_result[ cts_hot_object = lr_hot_object_with_lang->cts_hot_object abap_lang = lr_hot_object_text_group->language hana_lang = lv_hana_lang ] DEFAULT lr_text_deploy_result ).

          IF lr_text_deploy_result IS BOUND.
            CONTINUE. "skip deployment of second text_type of the object if first text_type ended with error.
          ENDIF.

          lr_text_deploy_result = REF #( e_success_text_deploy_result[ cts_hot_object = lr_hot_object_with_lang->cts_hot_object abap_lang = lr_hot_object_text_group->language hana_lang = lv_hana_lang ] DEFAULT lr_text_deploy_result ).

          "convert all single texts to NHI text objects
          LOOP AT GROUP lr_hot_object_text_group REFERENCE INTO DATA(lr_hot_object_txt_group_member).
            lv_max_length = CONV #( lr_hot_object_txt_group_member->hana_text_max_length ). "e.g. results in 120 with a space at the end
            CONDENSE lv_max_length.
            APPEND cl_nhi_text_with_language=>create_text_with_language(
              EXPORTING
                lang       = lv_hana_lang
                text_id    = lr_hot_object_txt_group_member->hana_text_id
                text_type  = CONV #( lr_hot_object_txt_group_member->hana_text_type )
                max_length = lv_max_length
                content    = lr_hot_object_txt_group_member->hana_text_content
            ) TO lt_nhi_texts.
          ENDLOOP.

          " create NHI Object with Texts of language
          APPEND cl_nhi_object_and_texts=>create_object_and_texts(
              name   = lr_hot_object_with_lang->cts_hot_object->hana_object_name
              suffix = lr_hot_object_with_lang->cts_hot_object->hana_object_suffix
              texts  = lt_nhi_texts
          ) TO lt_nhi_objects_with_text.

          " Write text to HANA, either as object text or as content text
          IF lr_hot_object_text_group->text_type = if_cts_hot_db_access=>co_cts_hot_text_type_object.
            lr_write_text_resp = me->m_nhi_text_api->write_active_object( me->m_nhi_text_api->create_write_active_t_cont_req(
                        tenant  = ''
                        package = lr_hot_object_with_lang->cts_hot_object->hana_package_id
                        lang    = lv_hana_lang
                        objects = lt_nhi_objects_with_text
            ) ).
            raise_exc_if_resp_is_not_bound( i_response = lr_write_text_resp i_what = cl_nhi_write_active_t_cont_req=>co_what i_action = cl_nhi_write_active_t_cont_req=>co_action ).
          ELSEIF lr_hot_object_text_group->text_type = if_cts_hot_db_access=>co_cts_hot_text_type_content.
            lr_write_text_resp = me->m_nhi_text_api->write_active_object_content( me->m_nhi_text_api->create_write_active_ts_con_req(
                        tenant  = ''
                        package = lr_hot_object_with_lang->cts_hot_object->hana_package_id
                        lang    = lv_hana_lang
                        objects = lt_nhi_objects_with_text
            ) ).
            raise_exc_if_resp_is_not_bound( i_response = lr_write_text_resp i_what = cl_nhi_write_active_ts_con_req=>co_what i_action = cl_nhi_write_active_ts_con_req=>co_action ).
          ENDIF.

          IF lr_write_text_resp->error_code = '0'.
            IF lr_text_deploy_result IS INITIAL.
              CREATE DATA lr_text_deploy_result.
              lr_text_deploy_result->cts_hot_object = lr_hot_object_with_lang->cts_hot_object.
              lr_text_deploy_result->abap_lang = lr_hot_object_text_group->language.
              lr_text_deploy_result->hana_lang = lv_hana_lang.
              lr_text_deploy_result->imported_text_count = lines( lt_nhi_texts ).
              INSERT lr_text_deploy_result->* INTO TABLE e_success_text_deploy_result.
            ELSE.
              lr_text_deploy_result->imported_text_count = lr_text_deploy_result->imported_text_count + lines( lt_nhi_texts ).
            ENDIF.

            m_cts_hot_db_access->update_object_texts_after_dpl(
                i_abap_object_reference = lr_hot_object_text_group->abap_object_reference
                i_text_type = lr_hot_object_text_group->text_type
                i_language = lr_hot_object_text_group->language
            ).
          ELSE.
            IF lr_text_deploy_result IS INITIAL.
              CREATE DATA lr_text_deploy_result.
              lr_text_deploy_result->cts_hot_object = lr_hot_object_with_lang->cts_hot_object.
              lr_text_deploy_result->abap_lang = lr_hot_object_text_group->language.
              lr_text_deploy_result->hana_lang = lv_hana_lang.
              lr_text_deploy_result->hana_error_code = lr_write_text_resp->error_code.
              lr_text_deploy_result->hana_error_message = lr_write_text_resp->error_msg.
              INSERT lr_text_deploy_result->* INTO TABLE e_failed_text_deploy_result.
            ELSE.
              " Delete successful text deployment of other text_type
              DELETE TABLE e_success_text_deploy_result FROM lr_text_deploy_result->*.
              CREATE DATA lr_text_deploy_result.
              lr_text_deploy_result->cts_hot_object = lr_hot_object_with_lang->cts_hot_object.
              lr_text_deploy_result->abap_lang = lr_hot_object_text_group->language.
              lr_text_deploy_result->hana_lang = lv_hana_lang.
              lr_text_deploy_result->hana_error_code = lr_write_text_resp->error_code.
              lr_text_deploy_result->hana_error_message = lr_write_text_resp->error_msg.
              INSERT lr_text_deploy_result->* INTO TABLE e_failed_text_deploy_result.
            ENDIF.
          ENDIF.

          CLEAR: lt_nhi_texts, lt_nhi_objects_with_text.
        ENDDO.
      ENDLOOP.
      FREE lt_hot_object_texts. "free some memory
    ENDLOOP. "LOOP AT i_hot_objects_with_lang
  ENDMETHOD.