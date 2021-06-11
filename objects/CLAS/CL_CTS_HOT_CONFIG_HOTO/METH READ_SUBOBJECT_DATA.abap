  METHOD read_subobject_data.

    FIELD-SYMBOLS:
      <fs_subobject_data> TYPE data.

    CLEAR: ev_content_as_string.

    CASE iv_subobject.
      WHEN 'CTS_HDI_OBJECT'.

        ASSIGN ir_data->* TO <fs_subobject_data>.
        IF sy-subrc = 0.

          ASSIGN COMPONENT 'HDI_CONTENT_BDATA' OF STRUCTURE <fs_subobject_data>
            TO FIELD-SYMBOL(<fs_object_content>).

          ev_content_as_string =  cl_abap_codepage=>convert_from( <fs_object_content> ).

        ENDIF.

      WHEN 'CTS_HOT_OBJECT'.
        ASSIGN ir_data->* TO <fs_subobject_data>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'HANA_CONTENT_CDATA' OF STRUCTURE <fs_subobject_data>
            TO <fs_object_content>.

          IF <fs_object_content> IS NOT INITIAL.
            ev_content_as_string = <fs_object_content>.
          ELSE.
            ASSIGN COMPONENT 'HANA_CONTENT_BDATA' OF STRUCTURE <fs_subobject_data>
              TO <fs_object_content>.

            IF <fs_object_content> IS NOT INITIAL.
              MESSAGE i100(scts_hdi) WITH text-005."'Binary Inhalt ist nicht unterstützt'
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_svrs_feature_not_supported.
    ENDCASE.

  ENDMETHOD.