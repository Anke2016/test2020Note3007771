FUNCTION SCTS_GET_AIM_TRACE_REMOTE.
*"--------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_WITH_PUTTB_SHD) TYPE  BOOLE_D DEFAULT SPACE
*"  TABLES
*"      ET_AIM_TRACE STRUCTURE  CTS_AIM_TRACE
*"      ET_AIM_TRC_OBJ STRUCTURE  CTS_AIM_TRC_OBJ
*"      ET_AIM_TRC_STM STRUCTURE  CTS_AIM_TRC_STM
*"      ET_PUTTB_SHD STRUCTURE  PUTTB_SHD
*"--------------------------------------------------------------------


  DATA lt_1 TYPE STANDARD TABLE OF CTS_AIM_TRACE.
  DATA lt_2 TYPE STANDARD TABLE OF CTS_AIM_TRC_OBJ.
  DATA lt_3 TYPE STANDARD TABLE OF CTS_AIM_TRC_STM.

  clear: ET_AIM_TRACE[],
         ET_AIM_TRC_OBJ[],
         ET_AIM_TRC_STM[],
         ET_PUTTB_SHD[].
  clear: ET_AIM_TRACE,
         ET_AIM_TRC_OBJ,
         ET_AIM_TRC_STM,
         ET_PUTTB_SHD.

  SELECT * from cts_aim_trace INto TABLE et_aim_trace[].

  if not et_aim_trace[] is initial.
    SELECT * from cts_aim_trc_obj INto TABLE ET_AIM_TRC_OBJ[]
           for all entries in et_aim_trace[]
           where timestamp = et_aim_trace-timestamp.


    SELECT * from cts_aim_trc_stm INto TABLE ET_AIM_TRC_STM[]
           for all entries in et_aim_trace[]
           where timestamp = et_aim_trace-timestamp.

   if iv_with_puttb_shd = 'X'.
      select * from puttb_shd into table et_puttb_shd[].
    endif.
  endif.

ENDFUNCTION.