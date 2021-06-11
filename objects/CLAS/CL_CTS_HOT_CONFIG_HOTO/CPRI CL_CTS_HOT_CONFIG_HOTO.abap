  PRIVATE SECTION.
    TYPES:
      ty_lt_text TYPE STANDARD TABLE OF char255 WITH DEFAULT KEY.
    "! Fill the GUI Container with content of Text Editor
    "! @parameter it_container | Instance of GUI Container
    "! @parameter iv_content_as_string | Content of the HOTO object as string
    METHODS fill_object_in_container
      IMPORTING
        it_container         TYPE cwb_container
        iv_content_as_string TYPE string
      RAISING
        cx_parameter_invalid_range
        cx_parameter_invalid_type
        cx_sy_codepage_converter_init
        cx_sy_conversion_codepage
        cx_cts_hta_hdi_not_found
        cx_cts_hta_hdi .
    "! Read the content of the HOTO oject and convert it into String
    "! @parameter iv_subobject | Distinguish subobject of the HOTO object: HDI or Hana Repo object
    "! @parameter ev_content_as_string | Content of the HOTO object as string
    METHODS read_subobject_data
      IMPORTING
        iv_subobject         TYPE tabname
        ir_data              TYPE REF TO data
      EXPORTING
        ev_content_as_string TYPE string
      RAISING
        cx_parameter_invalid_range
        cx_sy_conversion_codepage
        cx_sy_codepage_converter_init
        cx_svrs_feature_not_supported .
    "! SAP TextEdit Control to set properties of the text editor in the container
    "! @parameter it_text | Content of the HOTO object
    "! @parameter ir_text_editor | Instance of the TEXT EDITOR
    METHODS set_text_editor_properties
      IMPORTING
        it_text        TYPE cl_cts_hot_config_hoto=>ty_lt_text
        ir_text_editor TYPE REF TO cl_gui_textedit
      RAISING
        cx_cts_hta_hdi.
    METHODS get_source_system_id
      IMPORTING
        iv_abap_sync_system TYPE cts_hdi_object-abap_sync_system
      RETURNING
        value(rv_result)    TYPE char3.