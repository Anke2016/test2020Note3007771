  METHOD class_constructor.
    CREATE OBJECT directly EXPORTING i_value = 'A'.
    CREATE OBJECT prework_required EXPORTING i_value = 'P'.
  ENDMETHOD.