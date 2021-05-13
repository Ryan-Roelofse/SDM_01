*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_OBJ_DATA_REP
*&---------------------------------------------------------------------*

* Available for use in SDM config start***************************>>>
data:
  gt_mara_sdm     type /gda/sdm_t_mara_01,
  gt_makt_sdm     type /gda/sdm_t_makt_01,
  gt_marc_sdm     type /gda/sdm_t_marc_01,
  gt_mard_sdm     type /gda/sdm_t_mard_01,
  gt_mbew_sdm     type /gda/sdm_t_mbew_01,
  gt_mlgn_sdm     type /gda/sdm_t_mlgn_01,
  gt_mlgt_sdm     type /gda/sdm_t_mlgt_01,
  gt_mvke_sdm     type /gda/sdm_t_mvke_01,
  gt_mlea_sdm     type /gda/sdm_t_mlea_01,
  gs_makt_sdm     type /gda/sdm_s_makt_01,
  gt_eord_sdm     type /gda/sdm_t_eord_01,
  gt_mast_sdm     type /gda/sdm_t_mast_01,
  gt_meinh_sdm    type /gda/sdm_t_meinh_01,
  gt_mamt_sdm     type /gda/sdm_t_mamt_01,
  gt_malg_sdm     type /gda/sdm_t_malg_01,
  gt_myms_sdm     type /gda/sdm_t_myms_01,
  gt_maw1_sdm     type /gda/sdm_t_maw1_01,
  gt_mwli_sdm     type /gda/sdm_t_mwli_01,
  gt_eina_sdm     type /gda/sdm_t_eina_01,
  gt_eine_sdm     type /gda/sdm_t_eine_01,
  gt_mean_sdm     type /gda/sdm_t_mean_01,
  gt_konh_sdm     type /gda/sdm_t_konh_01,
  gt_wlk2_sdm     type /gda/sdm_t_wlk2_01,
  gt_wlk1_sdm     type /gda/sdm_t_wlk1_01,
  gt_mg03_sdm     type /gda/sdm_t_mlan_01,
  gt_maritc_sdm   type /gda/sdm_t_maritc_01,
  gt_tariff_sdm   type /gda/sdm_t_tariffs_01,
  gt_pricing_sdm  type /gda/sdm_t_pricing_01,
  gt_rmmw1_sdm    type /gda/sdm_t_rmmw1_01, "rmmw1,
*  gt_mg03_sdm_brf type /gda/sdm_t_mat_steuer_01,"mat_steuer,
  GT_MAT_STEUR_SDM TYPE /gda/sdm_t_mat_steuer_01,"mat_steuer,,
  gt_src_list_sdm type /gda/sdm_t_srclist_01,
  gt_marm_sdm     type /gda/sdm_t_marm_01,
  gt_mg03steumm_sdm type /gda/sdm_t_mg03steumm_01,
  gt_mpop_sdm     type /gda/sdm_t_mpop_01,
  gt_mpgd_sdm     type /gda/sdm_t_mpgd_01,
  gt_mlan_sdm     type /gda/sdm_t_mlan_01,
* Available for use in SDM config start***************************>>>
  gs_mara_sdm     type /gda/sdm_s_mara_01,
  gs_marc_sdm     type /gda/sdm_s_marc_01,
  gs_mard_sdm     type /gda/sdm_s_mard_01,
  gs_mvke_sdm     type /gda/sdm_s_mvke_01,
  gs_mbew_sdm     type /gda/sdm_s_mbew_01,
  gs_eina_sdm     type /gda/sdm_s_eina_01,
  gs_eine_sdm     type /gda/sdm_s_eine_01,
  gs_eord_sdm     type /gda/sdm_s_eord_01,
  gs_mlan_sdm     type /gda/sdm_s_mlan_01,

  gs_mg03_sdm     type /gda/sdm_mlan,
  gs_mg03_sdm_brf type /gda/sdm_s_mg03steuer_01,
  gs_mlgn_sdm     type mlgn,
  gs_maw1_sdm     type maw1,
  gs_syst_sdm     type syst.


ENHANCEMENT-POINT /GDA/SDM_MM_ART_EP3 SPOTS /GDA/SDM_MM_ART_ES3 STATIC INCLUDE BOUND .
