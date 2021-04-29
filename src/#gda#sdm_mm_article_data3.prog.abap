*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ARTICLE_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

TABLES:
  t100.

CONSTANTS:
  gc_object      TYPE /gda/sdm_de_object    VALUE 'ARTICLE',
  gc_object_id   TYPE /gda/sdm_de_object_id VALUE '01',
  gc_auth_rsr    TYPE xuobject              VALUE 'ZSDM_MAT_R',
  gc_auth_e      TYPE xuobject              VALUE 'ZSDM_MAT_E',
  gc_pir         TYPE /gda/sdm_de_type      VALUE '03',
  gc_pri         TYPE /gda/sdm_de_type      VALUE '04',
  gc_src         TYPE /gda/sdm_de_type      VALUE '05',
  gc_tar         TYPE /gda/sdm_de_type      VALUE '06'.


TYPES BEGIN OF objects.
TYPES: type   TYPE /gda/sdm_de_type.
TYPES: class  TYPE classname.
TYPES: include TYPE include.
TYPES: object TYPE REF TO /gda/sdm_cl_article.
TYPES: mapping TYPE REF TO /gda/sdm_cl_brf_mapping.
TYPES END OF objects.


DATA:
  go_article      TYPE REF TO /gda/sdm_cl_article ##NEEDED,
  go_selection    TYPE REF TO /gda/sdm_cl_art_selections,"zmat_selections_test,        "lcl_mm_material_exc,
  gt_sdm_article  TYPE STANDARD TABLE OF /gda/sdm_s_article,
  gv_mvke_first,
  gs_sdm_objects  TYPE /gda/sdm_s_article,
  gs_selscreen    TYPE /gda/sdm_cl_art_selections=>ty_selscreen, " lcl_mm_material_exc=>ty_selscreen,
  gt_results      TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
  gs_mara_sel     TYPE mara, "for selection screens
  gt_mara         TYPE STANDARD TABLE OF /gda/sdm_s_mara_01,
  gs_mg03steuer   type mg03steuer,"RD
  gt_makt         TYPE STANDARD TABLE OF makt,
  gs_makt_temp    TYPE /gda/sdm_s_makt_01, "makt,
*  gt_ktex         type mat_ktext,
  gt_mamt         TYPE STANDARD TABLE OF mamt,
  gt_malg         TYPE STANDARD TABLE OF malg,
  gs_mara         TYPE /gda/sdm_s_mara_01,
*  gt_steuer       like table of gs_mg03steuer,  "RD
  gs_makt         TYPE makt.

types begin of struc_rel.
types: matnr     type mara-matnr.
types: matnr_rel type mara-matnr.
types end of struc_rel.

DATA:
  gt_calcs1 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_message,
  gt_calcs2 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mtart,
  gt_calcs3 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mstae,
  gt_calcs4 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_matkl,
  gt_calcs5 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_attyp,
  gt_relations    type standard table of struc_rel. "RD

FIELD-SYMBOLS:
  <results> TYPE STANDARD TABLE, "LIKE gt_results,
  <field>   TYPE any.

*types begin of cond_header. "RD
*types: matnr type mara-matnr.
*types: knumh type konh-knumh.
*types end of cond_header.
