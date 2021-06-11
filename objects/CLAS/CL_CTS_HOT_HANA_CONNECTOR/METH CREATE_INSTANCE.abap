  METHOD create_instance.
    DATA: l_nhi_api             TYPE REF TO if_nhi_api,
          l_nhi_api_user_string TYPE string,
          l_nhi_api_user_c      TYPE c LENGTH 100.

    IF i_nhi_api IS BOUND.
      l_nhi_api = i_nhi_api.
    ELSE.
      TRY.
          l_nhi_api = cl_nhi_api=>create_instance( ).
        CATCH cx_nhi_not_supported INTO DATA(nhi_exc).
          RAISE EXCEPTION TYPE cx_hana_object_transport
            EXPORTING
              textid   = cx_hana_object_transport=>no_hana_database
              previous = nhi_exc.
      ENDTRY.
    ENDIF.

    IF i_nhi_api_user IS NOT INITIAL.
      l_nhi_api_user_string = i_nhi_api_user.
    ELSE.
      CALL FUNCTION 'DB_DBUSER' IMPORTING dbuser = l_nhi_api_user_c.
      l_nhi_api_user_string = CONV #( l_nhi_api_user_c ).
    ENDIF.

    "##TODO if ABAP is not on HANA this will fail
    "       rethrow exception does not allow usage of this call and thus is not an option as we need to unit test on non HANA systems.
    "ideas: - catch exception and handle it here and set m_nhi_api to null and check in each method whether bound or not
    "       - set m_nhi_api on first usage if not set in all the methods
    "       - constructor with optional parameter nhi_api. If set, use this, if not, try to create instance (in error case rethrow exception)
    "       - create instance method with optional parameter?
    CREATE OBJECT r_result
      EXPORTING
        i_nhi_api      = l_nhi_api
        i_nhi_api_user = l_nhi_api_user_string.
  ENDMETHOD.