*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_DATA_DECL
*&---------------------------------------------------------------------*

tables:
  mara,
  marc,
  mard,
  mvke,
  eord,
  eina,
  eine,
  t100.

type-pools:
  abap,
  icon.


types begin of struc_rel.
types: matnr     type mara-matnr.
types: matnr_rel type mara-matnr.
types end of struc_rel.

types begin of struc_tax.
types: matnr     type mara-matnr.
types: mg03steuer type mg03steuer.
types end of struc_tax.

data:
  begin of gs_mard,
    matnr type marc-matnr,
    werks type mard-werks,
    lgort type mard-lgort,
    lgpbe type mard-lgpbe,
  end of gs_mard,

  begin of gs_stpo,
    idnrk type stpo-idnrk,
    stlty type stpo-stlty,
    stlnr type stpo-stlnr,
  end of gs_stpo,

  begin of gs_tpst,
    stlnr type tpst-stlnr,
    stlal type tpst-stlal,
    tplnr type tpst-tplnr,
    werks type tpst-werks,
  end of gs_tpst,

*  BEGIN OF gs_iflo,
*    tplnr TYPE iflo-tplnr,
*    pltxt TYPE iflo-pltxt,
*  END OF gs_iflo,

  begin of gs_stko,
    stlnr type stko-stlnr,
    stlal type stko-stlal,
    stlst type stko-stlst,
  end of gs_stko,

  begin of gs_eqst,
    stlnr type eqst-stlnr,
    stlal type eqst-stlal,
    equnr type eqst-equnr,
    werks type eqst-werks,
  end of gs_eqst,

  gs_mg03steuer type mg03steuer.

types begin of cond_header.
types: matnr type mara-matnr.
types: knumh type konh-knumh.
types end of cond_header.

data:
  gv_eina_first,
  gv_eord_first,
  gv_mvke_first,
  gt_sdm_articles type standard table of /gda/sdm_s_article,
  gs_sdm_objects  type /gda/sdm_s_article,
  gt_mara         type standard table of /gda/sdm_s_mara_01,
  gt_mara_struc   type standard table of /gda/sdm_s_mara_01,
  gt_relations    type standard table of struc_rel,
  gs_relations    type struc_rel,
  gs_mara         type /gda/sdm_s_mara_01,
  gt_makt         type standard table of /gda/sdm_s_makt_01,
  gs_makt         type /gda/sdm_s_makt_01, "makt,
  gt_marc         type standard table of marc,
  gt_mard         like table of gs_mard,
  gt_mbew         type standard table of /gda/sdm_s_mbew_01,
  gt_mvke         type standard table of mvke,
  gt_wlk1         type standard table of wlk1,
  gt_wlk2         type standard table of wlk2,
  gt_mean         type standard table of mean,
  gt_maw1         type standard table of maw1,
  gt_eina         like standard table of eina,
  gt_eine         type standard table of eine,
  gt_eord         type standard table of eord,
  gt_stpo         like table of gs_stpo,
  gt_mg03         type standard table of struc_tax,
  gs_mg03         type struc_tax,
  gt_stko         like table of gs_stko,
  gt_eqst         like table of gs_eqst,
  gt_mast         type standard table of mast,
  gt_steuer       like table of gs_mg03steuer,
  gt_konh         type standard table of konh,
  gt_cond_header  type sorted table of cond_header with non-unique key matnr knumh,
  gt_pricing      type /GDA/SDM_T_PRICING_01,
  gt_mlgn         type /gda/sdm_t_mlgn_01,
  gt_mlgt         type /gda/sdm_t_mlgt_01,
  gt_myms         type /gda/sdm_t_myms_01,
  gt_mwli         type /gda/sdm_t_mwli_01,
  gt_marm         type standard table of marm,
  gt_mamt         type /gda/sdm_t_mamt_01, "TYPE STANDARD TABLE OF mamt,
  gt_malg         type standard table of malg,
  gt_mapr         type standard table of mapr,
  gt_prop         type standard table of prop,
  gt_mlea         type standard table of mlea,
  gt_mpgd_v       type standard table of mpgd_v.

* FOR BRFPlus
data:
  gt_ktex       type mat_ktext,
  gs_rmmw1      type /gda/sdm_s_rmmw1_01, "RMMW1
  gt_basic_text type lvc_t_tlin,
  gt_calcs1     type standard table of /gda/sdm_s_calcs_message,
  gt_calcs2     type standard table of /gda/sdm_s_calcs_mtart,
  gt_calcs3     type standard table of /gda/sdm_s_calcs_mstae,
  gt_calcs4     type standard table of /gda/sdm_s_calcs_matkl,
  gt_calcs5     type standard table of /gda/sdm_s_calcs_attyp.

field-symbols:
  <mara_struc>     like line of gt_mara_struc.

constants:
  gc_pir           type /gda/sdm_de_type   value '03',
  gc_pri           type /gda/sdm_de_type   value '04',
  gc_src           type /gda/sdm_de_type   value '05',
  gc_tar           type /gda/sdm_de_type   value '06'.

* Enhance2-here
* define new table and temp table
