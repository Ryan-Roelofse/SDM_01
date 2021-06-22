*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_02
*&---------------------------------------------------------------------*

IF r_tabname = 'MEINH'.
  r_tabname = 'MARM'.
ELSEIF r_tabname = 'MPGD'.
  r_tabname = 'MPGD_V'.
ELSEIF r_tabname = 'MPOP'.
  r_tabname = 'MAPR'.
ELSEIF r_tabname = 'MWLI'.
  r_tabname = 'MAW1'.
ENDIF.
