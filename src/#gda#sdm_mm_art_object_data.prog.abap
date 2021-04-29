*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_OBJECT_DATA
*&---------------------------------------------------------------------*
data:
* Tables
  gt_mara_sdm     type standard table of mara,
  gt_makt_sdm     type /gda/sdm_t_makt_01, "standard table of makt,
  gt_marc_sdm     type /gda/sdm_t_marc_01,
  gt_mard_sdm     type /gda/sdm_t_mard_01,
  gt_mvke_sdm     type /gda/sdm_t_mvke_01,
  gt_mbew_sdm     type /gda/sdm_t_mbew_01,
  gt_mlgn_sdm     type /gda/sdm_t_mlgn_01,
  gt_mlgt_sdm     type /gda/sdm_t_mlgt_01,
  gt_rmmw1_sdm    type /gda/sdm_t_rmmw1_01, "STANDARD TABLE OF rmmw1,
  gt_meinh_sdm    type /gda/sdm_t_meinh_01,
  gt_mlea_sdm     type /gda/sdm_t_mlea_01,
  gt_mg03_sdm     type /gda/sdm_t_mlan_01,
  gt_eord_sdm     type /gda/sdm_t_eord_01,
  gt_mast_sdm     type /gda/sdm_t_mast_01,
  gt_mamt_sdm     type /gda/sdm_t_mamt_01,
  gt_malg_sdm     type /gda/sdm_t_malg_01,
  gt_eina_sdm     type /gda/sdm_t_eina_01,
  gt_eine_sdm     type /gda/sdm_t_eine_01,
  gt_marm_sdm     type /gda/sdm_t_marm_01,
  gt_mpop_sdm     type /gda/sdm_t_mpop_01,
  gt_wlk2_sdm     type /gda/sdm_t_wlk2_01,
  gt_wlk1_sdm     type /gda/sdm_t_wlk1_01,
  gt_maw1_sdm     type /gda/sdm_t_maw1_01,
  gt_mwli_sdm     type /gda/sdm_t_mwli_01,
  gt_mean_sdm     type sorted table of mean with unique key mandt matnr meinh lfnum,
  gt_konh_sdm     type standard table of konh,
  gt_mg03_sdm_brf type mat_steuer,

* Structures
  gs_mara_sdm     type /gda/sdm_s_mara_01,
  gs_marc_sdm     type /gda/sdm_s_marc_01,
  gs_mard_sdm     type /gda/sdm_s_mard_01,
  gs_mvke_sdm     type /gda/sdm_s_mvke_01,
  gs_mbew_sdm     type /gda/sdm_s_mbew_01,
  gs_meinh_sdm    type /gda/sdm_s_meinh_01,
  gs_mg03_sdm     type /gda/sdm_s_mlan_01,
  gs_mlea_sdm     type /gda/sdm_s_mlea_01,
  gs_mlgn_sdm     type /gda/sdm_s_mlgn_01, "mlgn,
  gs_mlgt_sdm     type /gda/sdm_s_mlgt_01,
  gs_eina_sdm     type /gda/sdm_s_eina_01,
  gs_eine_sdm     type /gda/sdm_s_eine_01,
  gs_mamt_sdm     type /gda/sdm_s_mamt_01,
  gs_myms_sdm     type /gda/sdm_s_myms_01,
  gs_wlk2_sdm     type /gda/sdm_s_wlk2_01,
  gs_mwli_sdm     type /gda/sdm_s_mwli_01,
  gs_makt_sdm     type /gda/sdm_s_makt_01,"makt,
  gs_maw1_sdm     type /gda/sdm_s_maw1_01,
  gs_mpop_sdm     type /gda/sdm_s_mpop_01,
  gt_myms_sdm     type standard table of myms,
  gs_mg03_sdm_brf type mg03steuer,
  gs_syst_sdm     type syst.

* Enhancement Point: Declare non standard SDM tables and structures( Validations and Derivations)
ENHANCEMENT-POINT /GDA/SDM_MM_ART_EP1 SPOTS /GDA/SDM_MM_ART_ES1 INCLUDE BOUND .
