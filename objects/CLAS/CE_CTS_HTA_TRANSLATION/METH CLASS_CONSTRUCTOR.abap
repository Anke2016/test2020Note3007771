  METHOD class_constructor.
    CREATE OBJECT relevant_for_translation exporting i_value = ' '.
    CREATE OBJECT not_relevant_for_translation exporting i_value = 'X'.
  ENDMETHOD.