"! First version of exception class in HANA Transport for ABAP (previous name HANA Object Transport)
CLASS cx_hana_object_transport DEFINITION
  PUBLIC
  INHERITING FROM cx_cts_hta
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF cx_nhi_hana_repository_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'message_variable_1',
        attr2 TYPE scx_attrname VALUE 'message_variable_2',
        attr3 TYPE scx_attrname VALUE 'message_variable_3',
        attr4 TYPE scx_attrname VALUE 'message_variable_4',
      END OF cx_nhi_hana_repository_error .
    CONSTANTS:
      BEGIN OF read_object_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE.OBJNAME.SUFFIX', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF read_object_error .
    CONSTANTS:
      BEGIN OF read_package_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "package id in HANA Format (first 50 chars of package name)
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "package id in HANA Format (second 50 chars of package name in case of long package)
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF read_package_error .
    CONSTANTS:
      BEGIN OF response_is_null_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'message_variable_1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF response_is_null_error .
    CONSTANTS:
      BEGIN OF no_hana_database,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_hana_database .
    CONSTANTS:
      BEGIN OF get_objects_of_package_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "package id in HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF get_objects_of_package_error .
    CONSTANTS:
      BEGIN OF read_object_metadata_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE.OBJNAME.SUFFIX', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF read_object_metadata_error.
    CONSTANTS:
      "already moved to cx_cts_hta_not_existing
      BEGIN OF object_not_found_in_hot,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'ABAP_HANA_PACKAGE_ID.ABAP_HANA_OBJNAME.SUFFIX', "HOT Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF object_not_found_in_hot.
    CONSTANTS:
      "! Deployment of object not possible due to wrong status in HOT repository.
      BEGIN OF wrong_hot_status_for_obj_depl,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE.OBJNAME.SUFFIX', "HANA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', " HOT Status of Object
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF wrong_hot_status_for_obj_depl.
    CONSTANTS:
      "! Deployment of package not possible due to wrong status in HOT repository.
      BEGIN OF wrong_hot_status_for_pack_depl,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE', "HANA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', " HOT Status of Package
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF wrong_hot_status_for_pack_depl.
    CONSTANTS:
      BEGIN OF write_object_failed,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE.OBJNAME.SUFFIX', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF write_object_failed .
    CONSTANTS:
      BEGIN OF delete_object_failed,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE.OBJNAME.SUFFIX', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF delete_object_failed .
    CONSTANTS:
      "already moved to cx_cts_hta_not_existing
      BEGIN OF package_not_found_in_hot,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'ABAP_HANA_PACKAGE_ID, "HOT Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF package_not_found_in_hot.
    CONSTANTS:
      BEGIN OF create_package_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF create_package_error .
    CONSTANTS:
      "already moved to cx_cts_hta_hashing
      BEGIN OF create_package_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'PACKAGE', "HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_package_hash_error .
    CONSTANTS:
      "already moved to cx_cts_hta_hashing
      BEGIN OF create_object_name_hash_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "'OBJECT NAME', "HANA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "'OBJECT SUFFIX', "HANA Format
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF create_object_name_hash_error.
    CONSTANTS:
      BEGIN OF find_objects_of_package_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "package id in HANA Format
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "object_status
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF find_objects_of_package_error.
    CONSTANTS:
      BEGIN OF list_packages_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "package id in HANA Format
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE 'hana_error_code', "HANA error_code
        attr4 TYPE scx_attrname VALUE 'hana_error_msg', "HANA error_msg
      END OF list_packages_error.
    CONSTANTS:
      BEGIN OF read_tadir_error,
        msgid TYPE symsgid VALUE 'SCTS_HOT',
        msgno TYPE symsgno VALUE '020 ',
        attr1 TYPE scx_attrname VALUE 'message_variable_1', "package id in ABAP Format (tadir obj_name)
        attr2 TYPE scx_attrname VALUE 'message_variable_2', "arbg - sy-
        attr3 TYPE scx_attrname VALUE 'message_variable_3', "msgid - sy-
        attr4 TYPE scx_attrname VALUE 'message_variable_4', "msgvs  sy-msgv1 to sy-msgv4
      END OF read_tadir_error.

    DATA msgv1 TYPE symsgv READ-ONLY .
    DATA msgv2 TYPE symsgv READ-ONLY .
    DATA msgv3 TYPE symsgv READ-ONLY .
    DATA msgv4 TYPE symsgv READ-ONLY .
    DATA hana_error_code TYPE string READ-ONLY .
    DATA hana_error_msg TYPE string READ-ONLY .

    METHODS constructor
      IMPORTING
        !textid          LIKE if_t100_message=>t100key OPTIONAL
        !previous        LIKE previous OPTIONAL
        !msgv1           TYPE symsgv OPTIONAL
        !msgv2           TYPE symsgv OPTIONAL
        !msgv3           TYPE symsgv OPTIONAL
        !msgv4           TYPE symsgv OPTIONAL
        !hana_error_code TYPE string OPTIONAL
        !hana_error_msg  TYPE string OPTIONAL.