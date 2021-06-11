  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid = VALUE #( msgid = 'SCTS_HOT' msgno = '024' ).
  ENDMETHOD.