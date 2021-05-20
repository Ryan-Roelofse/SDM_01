*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_EXC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_mm_art_exc1 MESSAGE-ID /gda/sdm_pp1.

*INCLUDE /gda/sdm_mm_article_lcl.
PARAMETERS: p_vari   TYPE slis_vari NO-DISPLAY.

INCLUDE /gda/sdm_common_core_data.
INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_obj_data_rep.
INCLUDE /gda/sdm_mm_art_data_decl.
*include /GDA/SDM_MM_ARTICLE_DATA3.
INCLUDE /gda/sdm_mm_art_sel_scrn.
INCLUDE /gda/sdm_mm_art_sel_scrn_dum.
INCLUDE /gda/sdm_include_sdm_scr3.
INCLUDE /gda/sdm_include_sdm_scr4.
INCLUDE /gda/sdm_mm_art_rf01.
INCLUDE /gda/sdm_common_core.
INCLUDE /gda/sdm_common_article.
INCLUDE /gda/sdm_mm_art_exc1_screenf01.
*INCLUDE /gda/sdm_common_mm_article.
*include /GDA/SDM_MM_RSR_ARTICLE_F03.

INITIALIZATION.
  PERFORM auth_check USING gc_auth_e.
  PERFORM set_post_processing.
  PERFORM sdm_init_common USING gc_object
                       CHANGING gv_object gv_type gv_source go_selection.

AT SELECTION-SCREEN OUTPUT.
* SDM (common exception report)
  PERFORM selection_screen_output.
* SDM (Article report)
  PERFORM screen_output_art_exc.

AT SELECTION-SCREEN.
* SDM (common exception report)
  PERFORM at_sel_scrn_excep.

START-OF-SELECTION.
  PERFORM get_sdm_types USING gc_object gc_object_id gv_type p_user.
  PERFORM selection_screen.

  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.

  TRY.
      go_selection->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
        MESSAGE gv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.

  IF p_struc = abap_true.
    gt_relations[]   = go_selection->mt_mara_relations.
  ENDIF.

  PERFORM sdm_main_article.

END-OF-SELECTION.

  PERFORM display_results_excep_report USING gt_objects
                                             gt_sdm_articles
                                             gc_object.
