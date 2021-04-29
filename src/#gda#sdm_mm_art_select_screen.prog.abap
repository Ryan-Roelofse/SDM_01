*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MAT_SELECTIONS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME." TITLE TEXT-003.
*SELECTION-SCREEN BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-020.

SELECT-OPTIONS: s_matnr  FOR gs_mara_sel-matnr,
                s_mtart  FOR gs_mara_sel-mtart,
                s_attyp  FOR gs_mara_sel-attyp," no-DISPLAY,
                s_matkl  FOR gs_mara_sel-matkl,
                s_bwscl  for gs_mara_sel-bwscl,
                s_ersda  FOR gs_mara_sel-ersda,
                s_ernam  FOR gs_mara_sel-ernam,
                s_laeda  FOR gs_mara_sel-laeda,
                s_aenam  FOR gs_mara_sel-aenam,
                s_mstae  FOR gs_mara_sel-mstae.
SELECTION-SCREEN SKIP.
*SELECTION-SCREEN END OF BLOCK b14.
*selection-screen skip.
parameters:     p_struc   type char1 as checkbox user-command struc.
select-options: s_attyps  for gs_mara-attyp modif id sc1.
SELECTION-SCREEN END OF BLOCK b12.

PARAMETERS: p_makt DEFAULT 'X' NO-DISPLAY, "Descriptions
            p_marc DEFAULT 'X' NO-DISPLAY, "Plant
            p_mard DEFAULT 'X' NO-DISPLAY, "Storage Location
            p_mvke DEFAULT 'X' NO-DISPLAY, "Sales
            p_mbew DEFAULT 'X' NO-DISPLAY, "Valuation
            p_mlgn DEFAULT 'X' NO-DISPLAY, "Warehousing
            p_mlgt DEFAULT 'X' NO-DISPLAY, "Storage Types
            p_mapr DEFAULT 'X' NO-DISPLAY, "Forecasting
            p_crvm DEFAULT 'X' NO-DISPLAY, "PRT
            p_mlan DEFAULT 'X' NO-DISPLAY, "Tax Data
            p_marm DEFAULT 'X' NO-DISPLAY, "Units of Measure
            p_mean DEFAULT 'X' NO-DISPLAY, "Additional EANs
            p_eord DEFAULT 'X' NO-DISPLAY, "Purchasing Source List
            p_eina DEFAULT 'X' NO-DISPLAY,
            p_eine DEFAULT 'X' NO-DISPLAY.
