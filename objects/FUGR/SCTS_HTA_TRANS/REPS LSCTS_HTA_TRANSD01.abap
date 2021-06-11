CLASS lcl_transport_log DEFINITION.

  PUBLIC SECTION.
    METHODS:
      append
        IMPORTING
          i_severity       TYPE errortyp
          i_message_class  TYPE arbgb
          i_message_number TYPE msgnr
          i_new_object     TYPE protnewobj
          i_level          TYPE protlevel DEFAULT '2'
          i_var1           TYPE symsgv OPTIONAL
          i_var2           TYPE symsgv OPTIONAL
          i_var3           TYPE symsgv OPTIONAL
          i_var4           TYPE symsgv OPTIONAL.

  PROTECTED SECTION.
    METHODS:
      do_append
        IMPORTING
          log_message_tab TYPE sprot_u_tab.

ENDCLASS.

CLASS lcl_hta_deployment DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    CLASS-METHODS:
      create_instance
        IMPORTING
          i_transport_objects TYPE tr_objects
        RETURNING
          VALUE(r_result)     TYPE REF TO lcl_hta_deployment.

    METHODS:
      execute_deployment
        EXPORTING
          error_object_list TYPE e071_t.

  PRIVATE SECTION.
    DATA:
      m_transport_objects          TYPE tr_objects,
      m_transport_log              TYPE REF TO lcl_transport_log.

    METHODS:
      constructor
        IMPORTING i_transport_objects TYPE tr_objects,

      log_hdi_skipped_warning
        IMPORTING
          it_hdi_objects TYPE tr_objects.
ENDCLASS.