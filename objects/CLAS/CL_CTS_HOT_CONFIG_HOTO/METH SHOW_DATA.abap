  METHOD show_data.

    DATA:
      lt_text        TYPE TABLE OF char255,
      lr_data_object TYPE REF TO data,
      lr_text_editor TYPE REF TO cl_gui_textedit,
      lr_container   TYPE REF TO cl_gui_container.

    FIELD-SYMBOLS :
      <ft_data_object> TYPE STANDARD TABLE.


    ASSIGN  ir_data->* TO <ft_data_object>.
    IF sy-subrc = 0.

      READ TABLE <ft_data_object> INDEX 1 REFERENCE INTO lr_data_object.
      IF sy-subrc = 0.

        TRY.
            me->read_subobject_data(
              EXPORTING
                iv_subobject = iv_subobject
                ir_data      = lr_data_object
              IMPORTING
                ev_content_as_string          = DATA(lv_content_string)
            ).

            me->fill_object_in_container(
              it_container     = it_container
              iv_content_as_string = lv_content_string ).

          CATCH cx_parameter_invalid_range
                cx_parameter_invalid_type
                cx_sy_codepage_converter_init
                cx_sy_conversion_codepage
                cx_cts_hta_hdi
              INTO DATA(lx_exception).
            "handle exception
            RAISE EXCEPTION TYPE cx_svrs_feature_not_supported
              EXPORTING
                previous = lx_exception.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.