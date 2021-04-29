**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_ARTICLE_EXC_SEL
**&---------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&  Include           ZGD_MM_MATERIAL_EXC_SELSCREEN
**&---------------------------------------------------------------------*
*
*************************************************************************
**
**                            TYPE-POOLS
**
*************************************************************************
*TYPE-POOLS: ICON.
*
*************************************************************************
**
**                            CONSTANTS
**
*************************************************************************
*CONSTANTS: GC_MAXDB_ENTRIES TYPE P_DBACC VALUE '5000'.
*
*************************************************************************
**
**                            SCALAR DATA
**
*************************************************************************
*DATA: GV_SELECTION_FIELDS_ENTERED TYPE ABAP_BOOL,
*      GV_EXECUTE_REPORT TYPE ABAP_BOOL.
*
*************************************************************************
**
**                            STRUCTURES
**
*************************************************************************
*DATA: GS_SSCRFIELDS TYPE SSCRFIELDS.
**DATA: GS_SELSCREEN TYPE ZCL_TEST_EXCEPTION=>TY_SELSCREEN.
*
***// Select-Options
*DATA: BEGIN OF GS_SO,
*    MATNR TYPE MARA-MATNR,
*    ERSDA TYPE MARA-ERSDA,
*    MTART TYPE MARA-MTART,
*    MSTAE TYPE MARA-MSTAE,
*    MATKL TYPE MARA-MATKL,
*    ATTYP TYPE MARA-ATTYP,
*    LVORM TYPE MARA-LVORM,
*    EKORG TYPE EINE-EKORG,
*    LIFNR TYPE EINA-LIFNR,
*    LTSNR TYPE EINA-LTSNR,
*    WERKS_D TYPE RMMW1-VZWRK,
*    WERKS_S TYPE RMMW1-FIWRK,
*    VKORG TYPE MVKE-VKORG,
*    VTWEG TYPE MVKE-VTWEG,
*    MSGNO TYPE T100-MSGNR,
*END OF GS_SO.
*
*************************************************************************
**
**                          SELECTION-SCREEN
**
*************************************************************************
*
*SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
*SELECTION-SCREEN BEGIN OF BLOCK 101 WITH FRAME TITLE TEXT-001.     "General Selection
*SELECT-OPTIONS: S_MATNR FOR GS_SO-MATNR,
*                S_ERSDA FOR GS_SO-ERSDA,
*                S_MATKL FOR GS_SO-MATKL,
*                S_MTART FOR GS_SO-MTART,
*                S_ATTYP FOR GS_SO-ATTYP,
*                S_LVORM FOR GS_SO-LVORM.
*SELECTION-SCREEN END OF BLOCK 101.
*
*SELECTION-SCREEN BEGIN OF BLOCK VIEW WITH FRAME TITLE TEXT-990.    "View Selection
*SELECTION-SCREEN PUSHBUTTON /1(20) SEL_ALL USER-COMMAND SEL.
*SELECTION-SCREEN PUSHBUTTON /1(20) DSEL_ALL USER-COMMAND DSEL.
*
*PARAMETERS: P_MAKT AS CHECKBOX DEFAULT 'X', "Descriptions
*            P_MARC_D AS CHECKBOX DEFAULT 'X', "Logistics: DC
*            P_MARC_S AS CHECKBOX DEFAULT 'X', "Logistics: Store
*            P_MVKE AS CHECKBOX DEFAULT 'X', "Sales
*            P_EINA AS CHECKBOX DEFAULT 'X', "Purchasing
*            P_WLK2 AS CHECKBOX DEFAULT 'X', "POS
*            P_MAW1 AS CHECKBOX DEFAULT 'X', "Listing
*            P_MBEW AS CHECKBOX DEFAULT 'X', "Valuation
*            P_MLGN AS CHECKBOX DEFAULT 'X', "Warehousing
*            P_MLGT AS CHECKBOX DEFAULT 'X', "Storage Types
*            P_MAPR AS CHECKBOX DEFAULT 'X', "Forecasting
*            P_CRVM AS CHECKBOX DEFAULT 'X', "PRT
*            P_MLAN TYPE C NO-DISPLAY, "Tax Data
*            P_MARM AS CHECKBOX DEFAULT 'X', "Units of Measure
*            P_MEAN AS CHECKBOX DEFAULT 'X', "Additional EANs
*            P_MLEA AS CHECKBOX DEFAULT 'X', "Vendor-Specific EANs
*            P_MALG AS CHECKBOX DEFAULT 'X', "Layout Modules
*            P_MYMS AS CHECKBOX DEFAULT 'X'. "LIFO-relevant materials
*
*
*PARAMETERS: P_MARD TYPE C LENGTH 1 DEFAULT 'X' NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK VIEW.
*
*SELECTION-SCREEN BEGIN OF BLOCK OPT WITH FRAME TITLE TEXT-991.    "Report Options
*
*SELECT-OPTIONS: S_MSGNO FOR GS_SO-MSGNO.
*
*PARAMETERS: P_ALV_L TYPE SLIS_VARI,
*            P_MAXNO TYPE P_DBACC DEFAULT '5000' NO-DISPLAY.
*PARAMETERS: P_ERR_O AS CHECKBOX DEFAULT 'X'.  "Errors Only
*
*SELECTION-SCREEN END OF BLOCK OPT.
*
*SELECTION-SCREEN BEGIN OF BLOCK STATS WITH FRAME TITLE TEXT-992.    "Statistics
*PARAMETERS: P_STATS AS CHECKBOX. "Record Statistics
*SELECTION-SCREEN END OF BLOCK STATS.
*
*SELECTION-SCREEN END OF SCREEN 101.
*
*SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.                 "Logistics
*SELECTION-SCREEN BEGIN OF BLOCK 102 WITH FRAME TITLE TEXT-002.
*
*SELECT-OPTIONS: S_WERKSD FOR GS_SO-WERKS_D,                        "Distribution Centre
*                S_WERKSS FOR GS_SO-WERKS_S,                        "Store
*                S_LVOWK FOR GS_SO-LVORM.
*
*SELECTION-SCREEN END OF BLOCK 102.
*SELECTION-SCREEN END OF SCREEN 102.
*
*SELECTION-SCREEN BEGIN OF SCREEN 103 AS SUBSCREEN.                 "Sales
*SELECTION-SCREEN BEGIN OF BLOCK 103 WITH FRAME TITLE TEXT-003.
*
*SELECT-OPTIONS: S_VKORG FOR GS_SO-VKORG,
*                S_VTWEG FOR GS_SO-VTWEG,
*                S_LVOVK FOR GS_SO-LVORM.
*
*SELECTION-SCREEN END OF BLOCK 103.
*SELECTION-SCREEN END OF SCREEN 103.
*
*
*SELECTION-SCREEN BEGIN OF SCREEN 104 AS SUBSCREEN.                 "Purchasing
*SELECTION-SCREEN BEGIN OF BLOCK 104 WITH FRAME TITLE TEXT-004.
*
*SELECT-OPTIONS: S_EKORG FOR GS_SO-EKORG,
*                S_LIFNR FOR GS_SO-LIFNR,
*                S_LTSNR FOR GS_SO-LTSNR.
*
*SELECTION-SCREEN END OF BLOCK 104.
*SELECTION-SCREEN END OF SCREEN 104.
*
*
***// Tabbed Block
*SELECTION-SCREEN BEGIN OF TABBED BLOCK BLCK FOR 35 LINES.
*
*SELECTION-SCREEN TAB (30) SCR_TAB1 USER-COMMAND UCOMM1
*DEFAULT SCREEN 101.
*
*SELECTION-SCREEN TAB (30) SCR_TAB2 USER-COMMAND UCOMM2
*DEFAULT SCREEN 102.
*
*SELECTION-SCREEN TAB (30) SCR_TAB3 USER-COMMAND UCOMM3
*DEFAULT SCREEN 103.
*
*SELECTION-SCREEN TAB (30) SCR_TAB4 USER-COMMAND UCOMM4
*DEFAULT SCREEN 104.
*
*SELECTION-SCREEN END OF BLOCK BLCK.
*
*************************************************************************
**
**                           INITIALIZATION
**
*************************************************************************
*INITIALIZATION.
*  PERFORM INIT.
*
*************************************************************************
**
**                         AT SELECTION-SCREEN
**
*************************************************************************
*AT SELECTION-SCREEN.
*
***// Select/Deselect All
*  CASE SY-UCOMM.
*    WHEN 'SEL'.
*      P_MAKT = P_MARC_D = P_MARC_S = P_MARD = ABAP_TRUE.
*      P_EINA = P_MAW1 = P_WLK2 = ABAP_TRUE.
*      P_MVKE = P_MBEW = P_MLGN = P_MLGT = ABAP_TRUE.
*      P_MAPR = P_CRVM = P_MLAN = P_MARM = P_MEAN = ABAP_TRUE.
*      P_MALG = P_MLEA = P_MYMS = ABAP_TRUE.
*    WHEN 'DSEL'.
*      CLEAR: P_MAKT, P_MARC_D, P_MARC_S, P_MARD,
*             P_EINA, P_MAW1, P_WLK2,
*             P_MVKE, P_MBEW, P_MLGN, P_MLGT,
*             P_MAPR, P_CRVM, P_MLAN, P_MARM, P_MEAN,
*             P_MALG, P_MLEA, P_MYMS.
*  ENDCASE.
*
***// Check Authorisation
**  IF P_STATS = 'X'.
**    IF GO_ARTICLE->CHECK_AUTHORISATION( IV_ACTVT = '40' ) = ABAP_FALSE. "Archive
**      WRITE: / 'Not Authorised to Create Statistics. Check SU53'.
**      MESSAGE 'Not Authorised to Create Statistics. Check SU53' TYPE 'E'.
**    ENDIF.
**  ELSE.
**    IF GO_ARTICLE->CHECK_AUTHORISATION( IV_ACTVT = '16' ) = ABAP_FALSE. "Execute
**      WRITE: / 'Not Authorised to Execute Report. Check SU53'.
**      MESSAGE 'Not Authorised to Execute Report. Check SU53' TYPE 'E'.
**    ENDIF.
**  ENDIF.
*
*  IF SY-UCOMM = SPACE.
*    GS_SSCRFIELDS-UCOMM = 'ONLI'.
*  ELSE.
*    GS_SSCRFIELDS-UCOMM = SY-UCOMM.
*  ENDIF.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ALV_L.
*  PERFORM F4_ALV_LAYOUT CHANGING P_ALV_L.
*
*************************************************************************
**
**                             SUBROUTINES
**
*************************************************************************
*FORM INIT.
**  IF GO_ARTICLE IS INITIAL.
**    GO_ARTICLE ?= ZCL_SDM_ARTICLE=>FACTORY( IV_OBJECT_TYPE = 'ARTICLE' IV_SOURCE = '1' IV_STATS = '1' ).
**  ENDIF.
*
*  CONCATENATE ICON_SELECT_ALL 'Select All'  INTO SEL_ALL SEPARATED BY SPACE.
*  CONCATENATE ICON_DESELECT_ALL 'Deselect All' INTO DSEL_ALL SEPARATED BY SPACE.
*
***// Tabbed Screens
*  SCR_TAB1 = TEXT-001.
*  SCR_TAB2 = TEXT-002.
*  SCR_TAB3 = TEXT-003.
*  SCR_TAB4 = TEXT-004.
*
***// Deletion Flags
*  S_LVORM-SIGN = 'I'.
*  S_LVORM-OPTION = 'NE'.
*  S_LVORM-LOW = 'X'.
*  APPEND S_LVORM.
*
*  S_LVOWK-SIGN = 'I'.
*  S_LVOWK-OPTION = 'NE'.
*  S_LVOWK-LOW = 'X'.
*  APPEND S_LVOWK.
*
**  s_lvolg-sign = 'I'.
**  s_lvolg-option = 'NE'.
**  s_lvolg-low = 'X'.
**  APPEND s_lvolg.
*
*  S_LVOVK-SIGN = 'I'.
*  S_LVOVK-OPTION = 'NE'.
*  S_LVOVK-LOW = 'X'.
*  APPEND S_LVOVK.
*
*ENDFORM.
*
*FORM F4_ALV_LAYOUT CHANGING EV_LAYOUT TYPE SLIS_VARI.
*
*  DATA: LS_LAYOUT_KEY TYPE SALV_S_LAYOUT_KEY,
*        LS_LAYOUT TYPE SALV_S_LAYOUT_INFO.
*
*  LS_LAYOUT_KEY-REPORT = SY-REPID.
*  LS_LAYOUT = CL_SALV_LAYOUT_SERVICE=>F4_LAYOUTS( S_KEY = LS_LAYOUT_KEY  ).
*  EV_LAYOUT = LS_LAYOUT-LAYOUT.
*
*ENDFORM.
*
*FORM SELECTION_SCREEN.
**
****// Populate Selection-Screen Structure
**  GS_SELSCREEN-MATNR = S_MATNR[].
**  GS_SELSCREEN-ERSDA = S_ERSDA[].
**  GS_SELSCREEN-MTART = S_MTART[].
**  GS_SELSCREEN-MATKL = S_MATKL[].
**  GS_SELSCREEN-ATTYP = S_ATTYP[].
**
**  GS_SELSCREEN-WERKS_D = S_WERKSD[].
**  GS_SELSCREEN-WERKS_S = S_WERKSS[].
***  gs_selscreen-lgort = s_lgort[].
**  GS_SELSCREEN-VKORG = S_VKORG[].
**  GS_SELSCREEN-VTWEG = S_VTWEG[].
**
**  GS_SELSCREEN-EKORG = S_EKORG[].
**  GS_SELSCREEN-LIFNR = S_LIFNR[].
**  GS_SELSCREEN-LTSNR = S_LTSNR[].
**
**  PERFORM CHECK_SELECTION_ENTRIES CHANGING GV_SELECTION_FIELDS_ENTERED.
**  PERFORM LIMIT_MAX_ENTRIES CHANGING P_MAXNO GV_EXECUTE_REPORT.
**  IF GV_EXECUTE_REPORT = ABAP_FALSE.
**    RETURN.
**  ENDIF.
**
***  GS_SELSCREEN-MAKT = P_MAKT.
***  GS_SELSCREEN-MARC_D = P_MARC_D.
***  GS_SELSCREEN-MARC_S = P_MARC_S.
***  GS_SELSCREEN-MARD = P_MARD.
***  GS_SELSCREEN-MVKE = P_MVKE.
***  GS_SELSCREEN-MBEW = P_MBEW.
***  GS_SELSCREEN-MLGN = P_MLGN.
***  GS_SELSCREEN-MLGT = P_MLGT.
***  GS_SELSCREEN-MAPR = P_MAPR.
***  GS_SELSCREEN-CRVM = P_CRVM.
***  GS_SELSCREEN-MLAN = P_MLAN.
***  GS_SELSCREEN-MARM = P_MARM.
***  GS_SELSCREEN-MEAN = P_MEAN.
***  GS_SELSCREEN-EINA = P_EINA.
***  GS_SELSCREEN-MAW1 = P_MAW1.
***  GS_SELSCREEN-WLK2 = P_WLK2.
***  GS_SELSCREEN-MALG = P_MALG.
***  GS_SELSCREEN-MLEA = P_MLEA.
***  GS_SELSCREEN-MYMS = P_MYMS.
***
***
***  GS_SELSCREEN-MARA_LVORM = S_LVORM[].
***  GS_SELSCREEN-MARC_LVORM = S_LVOWK[].
****  gs_selscreen-mard_lvorm = s_lvolg[].
***  GS_SELSCREEN-MVKE_LVORM = S_LVOVK[].
***
***  GS_SELSCREEN-MSGNO = S_MSGNO[].
***  GS_SELSCREEN-MAX_ROWS = P_MAXNO.
***  GS_SELSCREEN-ERRORS_ONLY = P_ERR_O.
***  GS_SELSCREEN-RECORD_STATISTICS = P_STATS.
**
**
**  DATA:
**    GS_SELSCREEN TYPE TABLE OF RSPARAMS.
**
**  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
**    EXPORTING
**      CURR_REPORT     = SY-REPID
**    TABLES
**      SELECTION_TABLE = GS_SELSCREEN[].
**
**  GO_ARTICLE->SET_SELSCREEN_NEW( IS_SELSCREEN = GS_SELSCREEN ).
**
***  GO_ARTICLE->SET_SELSCREEN( IS_SELSCREEN = GS_SELSCREEN ).
*ENDFORM.
*
*FORM LIMIT_MAX_ENTRIES CHANGING CV_MAX_NO TYPE P_DBACC
*                                CV_EXECUTE_REPORT TYPE ABAP_BOOL.
*  CV_MAX_NO = 0.
*  CV_EXECUTE_REPORT = ABAP_TRUE.
*
*  DATA: LV_QUESTION TYPE C LENGTH 200,
*        LV_ANSWER TYPE C LENGTH 1,
*        LV_MAX_ENTRIES TYPE STRING.
*
*  LV_MAX_ENTRIES = GC_MAXDB_ENTRIES.
*  SHIFT LV_MAX_ENTRIES LEFT DELETING LEADING '0'.
*
*  LV_QUESTION = |No data entered into Selection-screen. The number of database entries will be restricted to |
*                && LV_MAX_ENTRIES && |. Continue?|.
*
*  IF SY-BATCH = SPACE AND ( GS_SSCRFIELDS-UCOMM = 'ONLI' OR GS_SSCRFIELDS-UCOMM = 'PRIN' ) AND GV_SELECTION_FIELDS_ENTERED = ABAP_FALSE.
*    PERFORM POPUP_TO_CONFIRM USING LV_QUESTION CHANGING LV_ANSWER.
*    IF LV_ANSWER = '1'. "Yes
*      CV_MAX_NO = GC_MAXDB_ENTRIES.
*    ELSE.
*      CV_EXECUTE_REPORT = ABAP_FALSE.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*
*FORM POPUP_TO_CONFIRM USING IV_QUESTION TYPE C
*                      CHANGING CV_ANSWER TYPE FLAG.
*
*  DATA: LV_ANSWER TYPE C LENGTH 1.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
**     TITLEBAR       = ' '
**     DIAGNOSE_OBJECT             = ' '
*      TEXT_QUESTION  = IV_QUESTION
**     TEXT_BUTTON_1  = 'Ja'(001)
**     ICON_BUTTON_1  = ' '
**     TEXT_BUTTON_2  = 'Nein'(002)
**     ICON_BUTTON_2  = ' '
**     DEFAULT_BUTTON = '1'
**     DISPLAY_CANCEL_BUTTON       = 'X'
**     USERDEFINED_F1_HELP         = ' '
**     START_COLUMN   = 25
**     START_ROW      = 6
**     POPUP_TYPE     =
**     IV_QUICKINFO_BUTTON_1       = ' '
**     IV_QUICKINFO_BUTTON_2       = ' '
*    IMPORTING
*      ANSWER         = LV_ANSWER
** TABLES
**     PARAMETER      =
*    EXCEPTIONS
*      TEXT_NOT_FOUND = 1
*      OTHERS         = 2.
*
*  CV_ANSWER = LV_ANSWER.
*
*ENDFORM.
*
*FORM CHECK_SELECTION_ENTRIES CHANGING CV_SELECTION_FIELDS_ENTERED TYPE ABAP_BOOL.
*
**  IF GS_SELSCREEN IS INITIAL.
**    CV_SELECTION_FIELDS_ENTERED = ABAP_FALSE.
**  ELSE.
**    CV_SELECTION_FIELDS_ENTERED = ABAP_TRUE.
**  ENDIF.
*
*ENDFORM.
