  METHOD if_cts_hot_ext_call_internal~get_devclass_for_hota.
    DATA ls_tadir TYPE tadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'    " Eingabe zum TADIR-Feld PGMID
        wi_tadir_object   = 'HOTA'    " Eingabe zum TADIR-Feld OBJECT
        wi_tadir_obj_name = iv_hota_name " Eingabe zum TADIR-Feld OBJ_NAME
        wi_read_only      = 'X'       " Lesen Objektkatalogeintrag
      IMPORTING
        new_tadir_entry   = ls_tadir  " Modifizierter TADIR-Eintrag
      EXCEPTIONS
        OTHERS            = 1. "ignore all exceptions...

    "ignore not existence of tadir entry
    rv_devclass = ls_tadir-devclass.
  ENDMETHOD.