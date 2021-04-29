*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MAT_SELECT_SCREEN2
*&---------------------------------------------------------------------*

*selection-screen begin of block b15 with frame title text-021.
*select-options: s_werks  for gs_marc_sdm-werks,
*                s_lgort  for gs_mard_sdm-lgort.
*selection-screen skip.
*selection-screen end of block b15.
*
*selection-screen begin of block b16 with frame title text-022.
*select-options: s_vkorg  for gs_mvke_sdm-vkorg,
*                s_vtweg  for gs_mvke_sdm-vtweg.
*selection-screen end of block b16.
*selection-screen skip.
*selection-screen begin of block b17 with frame title text-023.
*selection-screen end of block b17.

selection-screen begin of block b20 with frame title text-026.
select-options: s_werks  for gs_marc_sdm-werks,
                s_mmsta  for gs_marc_sdm-mmsta.
selection-screen end of block b20.

selection-screen begin of block b21 with frame title text-027.
select-options: s_lgort  for gs_mard_sdm-lgort.
selection-screen end of block b21.

selection-screen begin of block b16 with frame title text-022.
select-options: s_vkorg  for gs_mvke_sdm-vkorg,
                s_vtweg  for gs_mvke_sdm-vtweg.
selection-screen end of block b16.


selection-screen begin of block b17 with frame title text-023.
select-options: s_infnr for gs_eina_sdm-infnr,
                s_lifnr  for gs_eina_sdm-lifnr,
                s_ekorg  for gs_eine_sdm-ekorg.

selection-screen skip.
select-options:
            s_vdatu for gs_eord_sdm-vdatu no intervals,
            s_bdatu for gs_eord_sdm-bdatu no intervals.
selection-screen end of block b17.
