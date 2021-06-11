REPORT rddhanad MESSAGE-ID pu.
*----------------------------------------------------------------------------*
* frame program for deployment of HANA objects for HOTA, HOTO and HOTP objects
*----------------------------------------------------------------------------*

TABLES: trbat.
CONSTANTS: gc_func_c      LIKE trbat-function VALUE 'C',     "NICETODO: change all GC* in LC*
           gc_head_entry  LIKE trbat-trkorr   VALUE 'HEADER',
           gc_aborted     LIKE trbat-retcode  VALUE '8888',
           gc_new         LIKE trbat-retcode  VALUE '9999',
           gc_finished    LIKE trbat-retcode  VALUE 'F',
           gc_job_name(8) VALUE 'RDDHANAD'.                  "NICETODO: change all GV* in LV*
DATA: gv_jobcount LIKE tbtcm-jobcount, " job number
      gv_cntjobs  TYPE i,
      gv_subrc    LIKE sy-subrc,
      ls_trbat    TYPE trbat,
      lt_trbat    TYPE STANDARD TABLE OF trbat.
DATA: lv_timestamp LIKE trbat-timestmp,
      lv_field(4).
DATA: lv_trbat_argument TYPE trbat-argument.

*-----------------------find out own job number------------------------*
IF sy-batch = 'X'.
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobcount = gv_jobcount.
ELSE.
  gv_jobcount = space.
ENDIF.

*------------ synchronize SAP buffers by evaluation of DDLOG entries---*
CALL FUNCTION 'SBUF_SYNCHRONIZE'  "NICETODO: was wird hier gemacht?
  EXCEPTIONS
    OTHERS = 99.
IF sy-subrc = 0.
  MESSAGE s170(pu) WITH sy-host. "NICETODO: PU testen => Joblog
ELSE.
  MESSAGE s171(pu) WITH sy-host.
ENDIF.

*-------------- read transport requests from table trbat---------------*
SELECT * FROM trbat INTO TABLE lt_trbat
                    WHERE function = gc_func_c
                      AND trkorr   <> gc_head_entry
                      AND retcode  = gc_new
                    ORDER BY timestmp ASCENDING. "also necessary for merge deployment due to logging of info which version from which request finally was deployed.
* Decomment for testing in AT2
*ls_trbat-trkorr   = 'HEADER'.
*ls_trbat-function = 'C'.
*ls_trbat-retcode  = 'B'.
*append ls_trbat to lt_trbat.
*ls_trbat-trkorr   = 'AT1K900002'.
*ls_trbat-function = 'C'.
*ls_trbat-retcode  = '9999'.
*ls_trbat-logname  = ':D:T:S:log:F:AT1C900002.AT2'.
*append ls_trbat to lt_trbat.
*ls_trbat-trkorr   = 'AT1K900020'.
*ls_trbat-function = 'C'.
*ls_trbat-retcode  = '9999'.
*ls_trbat-logname  = ':D:T:S:log:F:AT1C900020.AT2'.
*append ls_trbat to lt_trbat.
*ls_trbat-trkorr   = 'AT1K900022'.
*ls_trbat-function = 'C'.
*ls_trbat-retcode  = '9999'.
*ls_trbat-logname  = ':D:T:S:log:F:AT1C900022.AT2'.
*append ls_trbat to lt_trbat.

*-------------- deploy HANA objects of all transport requests  ---------*
* For merge deployment decomment next line
PERFORM get_timestamp USING lv_timestamp.    "NICETODO: replace by class/method
**ANFANG DANIEL
DATA lt_trbat_depl TYPE STANDARD TABLE OF trbat. "kann man dies umgehen und direkt in r_trbat schreiben?
**ENDE DANIEL
LOOP AT lt_trbat INTO ls_trbat.
* lock TRBAT entry and assure that it is not already processed by
* another batch job: skip if already in progress
  SELECT SINGLE FOR UPDATE * FROM trbat
         WHERE trkorr    = ls_trbat-trkorr
           AND function  = ls_trbat-function. "gc_func_c.
  CHECK sy-subrc = 0.

* assure that it is not already processed by
* another batch job: skip if already in progress"
  IF trbat-retcode <> gc_new. "DANIEL müsste hier ls-trbat stehen?
    COMMIT WORK.
    CONTINUE.
  ENDIF.

* For merge deployment conment next line
  "  PERFORM get_timestamp USING lv_timestamp.
* set entry to '8888' (aborted/running)
  UPDATE trbat SET retcode  = gc_aborted
                   timestmp = lv_timestamp
                   argument = gv_jobcount
               WHERE trkorr   = ls_trbat-trkorr
                 AND function = gc_func_c.   "ls_trbat-function.
  COMMIT WORK.   " TRBAT is unlocked

**ANFANG DANIEL
  APPEND ls_trbat TO lt_trbat_depl.
**ENDE DANIEL

* For merge deployment decomment next line
ENDLOOP.

**ANFANG DANIEL
RANGES r_trbat FOR trbat.
r_trbat-sign = 'I'.
r_trbat-option = 'EQ'.

LOOP AT lt_trbat_depl INTO r_trbat-low.
  APPEND r_trbat.
ENDLOOP.
**ENDE DANIEL

SELECT SINGLE argument FROM trbat INTO lv_trbat_argument WHERE trkorr = gc_head_entry
                                                           AND function = gc_func_c.

* "NICETODO: (For merge deployment) hand over al tr. requests together
SUBMIT rddhanadeployment AND RETURN WITH so_trbat IN r_trbat with p_argum = lv_trbat_argument.

* Return-Code immer 0; tp findet den richtigen Return-Code
lv_field = '   0'.
* schreiben der Returncodes in TRBAT
PERFORM get_timestamp USING lv_timestamp.

* For merge deployment decomment next line DANIEL - oben wird teilweise übersprungen (IF trbat-retcode <> gc_new) und hier wird wieder über alle gelooped?
LOOP AT lt_trbat INTO ls_trbat.
  UPDATE trbat SET retcode  = lv_field
                   timestmp = lv_timestamp
               WHERE trkorr   = ls_trbat-trkorr
                 AND function = gc_func_c.
  COMMIT WORK.
* For merge deployment comment next line
ENDLOOP.

* order of actions:
* - DB lock on TRBAT header
* - delete own entry from TRJOB
* - check, if there are no further TRJOB entries
* - update TRBAT header
* - commit
SELECT SINGLE FOR UPDATE * FROM trbat
              WHERE trkorr   = gc_head_entry
                AND function = gc_func_c.
gv_subrc = sy-subrc.

* delete own entry from TRJOB, if program is running in background
IF gv_jobcount NE space.
  DELETE FROM trjob WHERE function = gc_func_c
                    AND   jobcount = gv_jobcount.
ENDIF.

IF gv_subrc EQ 0.
*--------modify header entry in TRBAT ----------------------------------
*--------only if there are no more running processes in TRJOB-----------
  CLEAR gv_cntjobs.
  SELECT COUNT(*) FROM trjob INTO gv_cntjobs
            WHERE function = gc_func_c.
  IF gv_cntjobs = 0. " set header entry in trbat to finished
    PERFORM get_timestamp USING trbat-timestmp.
    trbat-retcode = gc_finished.
    UPDATE trbat.
  ENDIF.
ELSE.
* TRBAT header was removed (manually?): better do nothing
ENDIF.
COMMIT WORK.

MESSAGE s130 WITH gc_func_c gc_job_name.

INCLUDE rddimpdi.                      "FORM GET_TIMESTAMP