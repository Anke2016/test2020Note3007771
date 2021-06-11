CLASS cl_cts_hot_config_hoto DEFINITION
  PUBLIC

  INHERITING FROM cl_svrs_config_tlogo
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS active_to_inactive
        REDEFINITION .
    METHODS adjust_admin_data
        REDEFINITION .
    METHODS after_import " new with 740/SP12
        REDEFINITION.
    METHODS auth_check_for_modify
        REDEFINITION .
    METHODS dequeue_object
        REDEFINITION .
    METHODS display_object
        REDEFINITION .
    METHODS enqueue_object
        REDEFINITION .
    METHODS get_ignore_fields
        REDEFINITION .
    METHODS get_key_fields
        REDEFINITION .
    METHODS get_object_definition
        REDEFINITION .
    METHODS get_object_header_from_repo
        REDEFINITION .
    METHODS get_subobjects
        REDEFINITION .
    METHODS get_title
        REDEFINITION .
    METHODS get_type_of_subobject
        REDEFINITION .
    METHODS has_inactive_version
        REDEFINITION .
    METHODS is_deleted_at_activation
        REDEFINITION .
    METHODS show_data
        REDEFINITION.
