*&---------------------------------------------------------------------*
*& Report scts_hta_redeploy_failed
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT scts_hta_redeploy_failed.

SELECTION-SCREEN COMMENT /1(50) TEXT-010.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_repo AS CHECKBOX DEFAULT 'X', "deploy HDI yes=X, no=all other
  p_hdi  AS CHECKBOX DEFAULT 'X'. "deploy HANA Repo yes=X, no=all other

INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '07' ID 'OBJTYPE' FIELD 'HOTA' ID 'OBJNAME' DUMMY ID 'DEVCLASS' DUMMY ID 'P_GROUP' DUMMY.

  IF sy-subrc <> 0.
    MESSAGE ID 'SCTS_HOT' TYPE 'I' NUMBER '031' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.

START-OF-SELECTION.
  DATA(lv_redeploy_mode) = 'X'.

  IF p_hdi = 'X' AND p_repo = 'X'.
    lv_redeploy_mode = 'X'.
  ELSEIF p_hdi = 'X' AND p_repo <> 'X'.
    lv_redeploy_mode = 'H'.
  ELSEIF p_hdi <> 'X' AND p_repo = 'X'.
    lv_redeploy_mode = 'R'.
  ELSEIF p_hdi <> 'X' AND p_repo <> 'X'.
    lv_redeploy_mode = space.
  ENDIF.

  SUBMIT rddhanadeployment AND RETURN WITH p_deplf = lv_redeploy_mode.

  WRITE:/ 'Das Redeployment fehlerhafter SAP-HANA-Objekte ist beendet.'(001).
  WRITE:/ 'Das Protokoll finden Sie im Unterverzeichnis "log" des Transportverzeichnisses unter dem dem Namen 5<yymmdd>.<SID>,'(002).
  WRITE: 'z.B.: /usr/sap/trans/log/5160122.QAS, wenn das Redeployment am 22.01.2016 gestartet wurde.'(003).
  WRITE:/ 'Zur Anzeige des Protokolls starten Sie den Report RSPUTPRT in der Transaktion SA38.'(004).
  WRITE: 'Geben Sie für das Feld ''Vollst. Dateiname (mit Dir.)'' den Wert des vollständigen Dateinamens an,'(005).
  WRITE: 'z.B.: /usr/sap/trans/log/5160122.QAS.'(006).
  WRITE:/ 'Wenn der Report RSPUTPRT nicht genutzt werden kann, nutzen Sie Transaktion AL11.'(007).