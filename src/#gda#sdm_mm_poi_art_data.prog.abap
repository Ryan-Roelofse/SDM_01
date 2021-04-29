*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POI_ART_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

CONSTANTS:
  gc_object     TYPE /gda/sdm_de_object    VALUE 'ARTICLE',
  gc_object_id  TYPE /gda/sdm_de_object_id VALUE '01',
  gc_auth_rsr    TYPE xuobject              VALUE 'ZSDM_ART_R',
  gc_auth_e      TYPE xuobject              VALUE 'ZSDM_ART_E'.

*DATA:
*  go_article   TYPE REF TO /gda/sdm_cl_article,
*  go_selection TYPE REF TO /gda/sdm_cl_selections,
*  gs_selscreen,
*  gt_results   TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key.

DATA:
  go_article      TYPE REF TO /gda/sdm_cl_article ##NEEDED,
  go_selection    TYPE REF TO /gda/sdm_cl_art_selections,"zmat_selections_test,        "lcl_mm_material_exc,
  gt_sdm_article  TYPE STANDARD TABLE OF /gda/sdm_s_article,
*  gv_mvke_first,
*  gs_sdm_objects  TYPE /gda/sdm_s_article,
  gs_selscreen    TYPE /gda/sdm_cl_art_selections=>ty_selscreen, " lcl_mm_material_exc=>ty_selscreen,
  gt_results      TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
  gs_mara_sel     TYPE mara, "for selection screens
*  gt_mara         TYPE STANDARD TABLE OF /gda/sdm_s_mara_01,
*  gs_mg03steuer   type mg03steuer,"RD
*  gt_makt         TYPE STANDARD TABLE OF makt,
  gs_makt_temp    TYPE /gda/sdm_s_makt_01. "makt,
*  gt_ktex         type mat_ktext,
*  gt_mamt         TYPE STANDARD TABLE OF mamt,
*  gt_malg         TYPE STANDARD TABLE OF malg.
*  gs_mara         TYPE /gda/sdm_s_mara_01,
*  gt_steuer       like table of gs_mg03steuer,  "RD
*  gs_makt         TYPE makt.

FIELD-SYMBOLS:
  <results> type STANDARD TABLE, "LIKE gt_results,
  <field>   TYPE any.
*  <result>  type any. "LIKE LINE OF gt_results.

FIELD-SYMBOLS:
  <results_der>     TYPE STANDARD TABLE,
  <results_der_all> TYPE STANDARD TABLE.
