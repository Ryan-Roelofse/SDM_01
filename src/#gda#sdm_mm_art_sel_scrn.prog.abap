*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SELECTIONS
*&---------------------------------------------------------------------*

*selection-screen begin of block b12 with frame title text-003.
selection-screen begin of block b14." with frame title text-020.

select-options: s_matnr  for mara-matnr,
                s_mtart  for mara-mtart,
                s_attyp  for mara-attyp,
                s_matkl  for mara-matkl,
                s_bwscl  for mara-bwscl,
                s_ersda  for mara-ersda,
                s_ernam  for mara-ernam,
                s_laeda  for mara-laeda,
                s_aenam  for mara-aenam.
selection-screen skip.

select-options: s_mstae  for mara-mstae.

selection-screen skip.
parameters:     p_struc   type char1 as checkbox user-command struc.
select-options: s_attyps  for mara-attyp modif id sc1.
selection-screen end of block b14.
*selection-screen end of block b12.
*selection-screen begin of block b15 with frame title text-021.
*select-options: s_werks  for marc-werks,
*                s_lgort  for mard-lgort.

*selection-screen end of block b12.
*selection-screen skip.

*selection-screen begin of block b16 with frame title text-022.
*select-options: s_vkorg  for mvke-vkorg,
*                s_vtweg  for mvke-vtweg.
*selection-screen end of block b16.
*
*
*selection-screen skip.
*selection-screen begin of block b17 with frame title text-023.
*select-options: s_infnr for eina-infnr,
*                s_lifnr  for eina-lifnr,
*                s_ekorg  for eine-ekorg.
*
*selection-screen skip.
*
*selection-screen skip.
*select-options:
*            s_vdatu for eord-vdatu no intervals,
*            s_bdatu for eord-bdatu no intervals.
*selection-screen end of block b17.
*selection-screen end of block b15.

*selection-screen end of block b12.
