*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POI_ART_DATA
*&---------------------------------------------------------------------*
include /gda/sdm_data_core.

constants:
  gc_object    type /gda/sdm_de_object    value 'ARTICLE',
  gc_object_id type /gda/sdm_de_object_id value '01',
  gc_auth_rsr  type xuobject              value 'ZSDM_ART_R',
  gc_auth_e    type xuobject              value 'ZSDM_ART_E'.


data:
  go_article      type ref to /gda/sdm_cl_article ##NEEDED,
  go_selection    type ref to /gda/sdm_cl_art_selections,"zmat_selections_test,        "lcl_mm_material_exc,
  gt_sdm_article  type standard table of /gda/sdm_s_article,
  gs_selscreen    type /gda/sdm_cl_art_selections=>ty_selscreen, " lcl_mm_material_exc=>ty_selscreen,
  gt_results      type standard table of /gda/sdm_s_val_results_key,
  gs_mara_sel     type mara, "for selection screens
  gs_makt_temp    type /gda/sdm_s_makt_01. "makt,

field-symbols:
  <results>         type standard table, "LIKE gt_results,
  <field>           type any,
  <results_der>     type standard table,
  <results_der_all> type standard table.
