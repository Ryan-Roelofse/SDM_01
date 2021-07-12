*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SELECTIONS
*&---------------------------------------------------------------------*

*selection-screen begin of block b12 with frame title text-003.
SELECTION-SCREEN BEGIN OF BLOCK b14." with frame title text-020.

SELECT-OPTIONS: s_matnr  FOR mara-matnr,
                s_mtart  FOR mara-mtart,
                s_attyp  FOR mara-attyp NO-DISPLAY,         "ST-1483
                s_matkl  FOR mara-matkl,
                s_bwscl  FOR mara-bwscl,
                s_ersda  FOR mara-ersda,
                s_ernam  FOR mara-ernam,
                s_laeda  FOR mara-laeda,
                s_aenam  FOR mara-aenam.
SELECTION-SCREEN SKIP.

SELECT-OPTIONS: s_mstae  FOR mara-mstae.

SELECTION-SCREEN SKIP.
PARAMETERS:     p_struc   TYPE char1 AS CHECKBOX USER-COMMAND struc.
SELECT-OPTIONS: s_attyps  FOR mara-attyp MODIF ID sc1.
SELECTION-SCREEN END OF BLOCK b14.
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
