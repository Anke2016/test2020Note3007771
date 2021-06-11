  METHOD if_cts_hta_component~set_prework.
    DATA: lr_component          TYPE REF TO if_cts_hta_component,
          lt_preworked_packages TYPE ty_preworked_packages. "performance, do only call set_prework once per package

    set_prework_done_once_per_pkg(
      EXPORTING i_components = me->m_hta_packages
                i_prework_flag = i_prework
      CHANGING  c_preworked_packages = lt_preworked_packages ).

    set_prework_done_once_per_pkg(
      EXPORTING i_components = me->m_hta_objects
                i_prework_flag = i_prework
      CHANGING  c_preworked_packages = lt_preworked_packages ).

    set_prework_done_once_per_pkg(
      EXPORTING i_components = me->m_hta_full_packages
                i_prework_flag = i_prework
      CHANGING  c_preworked_packages = lt_preworked_packages ).
  ENDMETHOD.