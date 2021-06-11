  METHOD class_constructor.
    CREATE OBJECT prework_done EXPORTING i_value = 'X'.
    CREATE OBJECT prework_not_done EXPORTING i_value = ' '.
  ENDMETHOD.