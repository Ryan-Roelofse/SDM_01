*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_INCLUDE_MM_SCR1
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b11." WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN PUSHBUTTON /1(20) sel_all USER-COMMAND sel.
SELECTION-SCREEN PUSHBUTTON /1(20) dsel_all USER-COMMAND dsel.
INCLUDE /gda/sdm_include_pp.
SELECTION-SCREEN END OF BLOCK b11.
