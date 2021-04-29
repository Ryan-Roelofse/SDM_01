*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MAT_SCREENS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
INCLUDE /gda/sdm_include_mm_art_scr1.
SELECTION-SCREEN END OF SCREEN 101.

* SUBSCREEN 2 - Main ALV Selection
SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.
INCLUDE /gda/sdm_mm_art_select_screen.
SELECTION-SCREEN END OF SCREEN 102.

** SUBSCREEN 2 - Main ALV Selection
*SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.
* include /gda/sdm_mm_art_sel_scrn.
*SELECTION-SCREEN END OF SCREEN 102.

* SUBSCREEN 3 - Context Selection
SELECTION-SCREEN BEGIN OF SCREEN 103 AS SUBSCREEN.
INCLUDE /gda/sdm_mm_art_select_screen2.
SELECTION-SCREEN END OF SCREEN 103.

* SUBSCREEN 4 - Output
SELECTION-SCREEN BEGIN OF SCREEN 104 AS SUBSCREEN.
INCLUDE /gda/sdm_include_sdm_scr2.
INCLUDE /gda/sdm_include_sdm_scr3.
INCLUDE /gda/sdm_include_sdm_scr4.
SELECTION-SCREEN END OF SCREEN 104.

* STANDARD SELECTION SCREEN
SELECTION-SCREEN: BEGIN OF TABBED BLOCK maintab FOR 20 LINES,
                  TAB (20) button1 USER-COMMAND push1,
                  TAB (20) button2 USER-COMMAND push2,
                  TAB (20) button3 USER-COMMAND push3,
                  TAB (20) button4 USER-COMMAND push4,
                  END OF BLOCK maintab.
