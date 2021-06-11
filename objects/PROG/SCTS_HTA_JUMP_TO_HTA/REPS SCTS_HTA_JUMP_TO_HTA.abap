*&---------------------------------------------------------------------*
*& Report scts_hta_jump_to_hta
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT scts_hta_jump_to_hta.
PARAMETERS:
  pv_pack  TYPE cts_hot_package-hana_package_id,
  pv_title TYPE sy-title.

DATA: lv_pack(110),
      lv_sidk(4).

lv_sidk = |{ sy-sysid }K|.  "<SID>K
lv_pack = pv_pack.

SET PARAMETER ID 'hta_pack' FIELD lv_pack ##EXISTS.

IF lv_pack CS '/'.
  CALL TRANSACTION 'SCTS_HDI_DEPLOY' WITHOUT AUTHORITY-CHECK. "#EC CI_CALLTA
ELSEIF pv_title CS lv_sidk.
  CALL TRANSACTION 'SCTS_HTA' WITHOUT AUTHORITY-CHECK. "#EC CI_CALLTA
ELSE.
  CALL TRANSACTION 'SCTS_HTA_DEPLOY' WITHOUT AUTHORITY-CHECK. "#EC CI_CALLTA
ENDIF.