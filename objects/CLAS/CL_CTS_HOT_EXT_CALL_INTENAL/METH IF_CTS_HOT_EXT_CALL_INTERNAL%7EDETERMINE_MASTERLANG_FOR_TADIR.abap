  METHOD if_cts_hot_ext_call_internal~determine_masterlang_for_tadir.
    DATA: lv_popup_question          TYPE string,
          lv_popup_button1_quickinfo TYPE text132,
          lv_popup_answer            TYPE c LENGTH 1,
          ls_popup_param             TYPE spar,
          lt_popup_params            TYPE STANDARD TABLE OF spar.

    IF i_hana_original_language IS INITIAL.
      IF i_translation_relevance = ce_cts_hta_translation=>not_relevant_for_translation.
        r_result = sy-langu.
      ELSEIF i_suppress_dialog = 'X'.
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid       = cx_cts_hta_unknown_master_lang=>no_master_language
            hana_package = i_hana_package_name.
      ELSE.
        " if there is no language maintained in HANA, ask user about using current logon language or cancel.

        "Get logon language as 2 char string
        cl_i18n_languages=>sap1_to_sap2(
          EXPORTING
            im_lang_sap1  = sy-langu    " R/3-System, aktuelle Sprache
          RECEIVING
            re_lang_sap2  = DATA(lv_langu)    " 2 stelliger SAP Sprachschlüssel
            EXCEPTIONS
              no_assignment = 1
              OTHERS        = 2
        ).
        IF sy-subrc <> 0.
          cx_cts_hta=>raise_message_exception( ). "creates cx_cts_hta using sy-field values
        ENDIF.

        lv_popup_question = 'Möchten Sie Ihre Anmeldesprache &1 als Originalsprache (in ABAP) verwenden?'(002).
        lv_popup_button1_quickinfo = 'Ja, &1 verwenden'(003).

        REPLACE '&1' IN lv_popup_question WITH lv_langu.
        REPLACE '&1' IN lv_popup_button1_quickinfo WITH lv_langu.

        ls_popup_param-param = 'PACKAGE_ID'.
        ls_popup_param-value = i_hana_package_name.
        APPEND ls_popup_param TO lt_popup_params.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'ABAP Originalsprache kann nicht automatisch ermittelt werden'(004)    " Titel des Popup
            diagnose_object       = 'CTS_HTA_SYNC_UI_NO_LANG'    " Diagnosetext (Pflege über SE61)
            text_question         = lv_popup_question    " Fragetext im Popup
            text_button_1         = 'Ja'(005)    " Text auf der ersten Drucktaste - 12 Zeichen
            icon_button_1         = 'ICON_CHECKED'    " Ikone auf der ersten Drucktaste - 12 Zeichen
            text_button_2         = 'Abbrechen'(006)    " Text auf der zweiten Drucktaste
            icon_button_2         = 'ICON_CANCEL'    " Ikone auf der zweiten Drucktaste
            default_button        = '2'    " Cursorposition
            display_cancel_button = space    " Schalter,ob Abbrechen-Drucktaste angezeigt wird
            userdefined_f1_help   = 'CTS_HTA_SYNC_UI_NO_LANG_F1'    " Benutzerdefinierte F1-Hilfe
            iv_quickinfo_button_1 = lv_popup_button1_quickinfo   " Quickinfo auf der ersten Drucktaste
            iv_quickinfo_button_2 = 'Abbrechen und in SAP HANA ändern'(007)    " Quickinfo auf der zweiten Drucktaste
          IMPORTING
            answer                = lv_popup_answer   " Rückgabewerte: '1', '2', 'A'
          TABLES
            parameter             = lt_popup_params   " Übergabetabelle für Parameter im Text
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          cx_cts_hta=>raise_message_exception( ). "creates cx_cts_hta using sy-field values
        ENDIF.

        IF lv_popup_answer <> '1'.
          RAISE EXCEPTION TYPE cx_cts_hta_canceled_by_user.
        ENDIF.

        r_result = sy-langu.
      ENDIF.
    ELSE.
      "First: try to get language via translation tools
      r_result = cl_lxe_lang=>java_to_int( i_hana_original_language ).

      IF r_result IS INITIAL.
        "Second: try to get language via I18N
        IF strlen( i_hana_original_language ) = 2.
          "e.g.: de, DE, en, EN
          cl_i18n_languages=>sap2_to_sap1(
            EXPORTING
              im_lang_sap2      = CONV #( to_upper( i_hana_original_language ) )   " Sprache nach ISO 639
            RECEIVING
              re_lang_sap1      = r_result   " R/3-System, aktuelle Sprache
            EXCEPTIONS
              no_assignment     = 1
              no_representation = 2
              OTHERS            = 3
          ).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
              EXPORTING
                textid        = cx_cts_hta_unknown_master_lang=>unsupported_master_lang
                hana_language = i_hana_original_language
                hana_package  = i_hana_package_name.
          ENDIF.
        ELSE.
          "e.g.: de_DE, en_US
          cl_i18n_languages=>locale_to_sap1(
            EXPORTING
              iv_consider_inactive =  abap_false    " Gibt an, ob inaktive Sprachen verwendet werden.
              iv_localename        =  i_hana_original_language      " Locale Name
            IMPORTING
              ev_lang_sap1         =  r_result " Sprachkennzeichen
            EXCEPTIONS
              no_assignment        = 1
              OTHERS               = 2
          ).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
              EXPORTING
                textid        = cx_cts_hta_unknown_master_lang=>unsupported_master_lang
                hana_language = i_hana_original_language
                hana_package  = i_hana_package_name.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT cl_i18n_languages=>is_active_lang_key( r_result ).
        RAISE EXCEPTION TYPE cx_cts_hta_unknown_master_lang
          EXPORTING
            textid        = cx_cts_hta_unknown_master_lang=>unsupported_master_lang
            hana_language = i_hana_original_language
            hana_package  = i_hana_package_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.