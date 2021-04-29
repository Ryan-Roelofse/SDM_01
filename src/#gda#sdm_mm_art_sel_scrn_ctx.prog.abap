*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_SELECT_SCRN_CN
*&---------------------------------------------------------------------*

selection-screen begin of block b20 with frame title text-026.
select-options: s_werks  for marc-werks,
                s_mmsta  for marc-mmsta.
selection-screen end of block b20.

selection-screen begin of block b21 with frame title text-027.
select-options: s_lgort  for mard-lgort.
selection-screen end of block b21.

selection-screen begin of block b16 with frame title text-022.
select-options: s_vkorg  for mvke-vkorg,
                s_vtweg  for mvke-vtweg.
selection-screen end of block b16.


selection-screen begin of block b17 with frame title text-023.
select-options: s_infnr for eina-infnr,
                s_lifnr  for eina-lifnr,
                s_ekorg  for eine-ekorg.

selection-screen skip.
select-options:
            s_vdatu for eord-vdatu no intervals,
            s_bdatu for eord-bdatu no intervals.
selection-screen end of block b17.
