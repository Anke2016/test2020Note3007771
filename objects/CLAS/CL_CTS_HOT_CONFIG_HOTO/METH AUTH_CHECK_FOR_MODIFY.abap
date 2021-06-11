  METHOD auth_check_for_modify.
* TODO TODO TODO TODO TODO TODO
* * for DDLS it looks like this ...
*  data: rc type sysubrc.
*
*  rv_is_allowed = abap_false.
*
*  call function 'DD_AUTH_CHECK'
*    exporting
*      action              = 'MODIFY'
*      objname             = iv_objname
*      objtype             = 'DDLS'
*    importing
*      rc                  = rc
*    exceptions
*      action_unsupported  = 1
*      objtype_unsupported = 2
*      param_error         = 3.
*  if sy-subrc > 0 or   "Internal error in function module
*     rc > 0.
*    raise exception type cx_svrs_error_in_configclass
*      exporting
*        textid  = cx_svrs_error_in_configclass=>cx_svrs_no_authorization
*        objtype = me->av_objtype
*        objname = iv_objname.
*  else.
*    rv_is_allowed = abap_true.
*  endif.
*
* probably we have to do some checks here?
* Same as we do for synchronization in hot_reader?
    rv_is_allowed = abap_true.

  ENDMETHOD.