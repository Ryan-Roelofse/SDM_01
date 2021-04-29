*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_EXC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report /gda/sdm_mm_art_exc1 message-id /gda/sdm_pp1.

*INCLUDE /gda/sdm_mm_article_lcl.
parameters: p_vari   type slis_vari no-display.

include /gda/sdm_common_core_data.
include /gda/sdm_mm_poi_art_data.
include /gda/sdm_mm_art_obj_data_rep.
include /gda/sdm_mm_art_data_decl.
*include /GDA/SDM_MM_ARTICLE_DATA3.
include /gda/sdm_mm_art_sel_scrn.
include /gda/sdm_mm_art_sel_scrn_dum.
include /gda/sdm_include_sdm_scr3.
include /gda/sdm_include_sdm_scr4.
include /gda/sdm_mm_art_rf01.
include /gda/sdm_common_core.
include /gda/sdm_common_article.
include /gda/sdm_mm_art_exc1_screenf01.
*INCLUDE /gda/sdm_common_mm_article.
*include /GDA/SDM_MM_RSR_ARTICLE_F03.

initialization.
  perform auth_check using gc_auth_e.
  perform set_post_processing.
  PERFORM sdm_init_common USING gc_object
                       CHANGING gv_object gv_type gv_source go_selection.

at selection-screen output.
* SDM (common exception report)
  perform selection_screen_output.
* SDM (Article report)
  perform screen_output_art_exc.

at selection-screen.
* SDM (common exception report)
  perform at_sel_scrn_excep.

start-of-selection.
  perform get_sdm_types using gc_object gc_object_id gv_type p_user.
  perform selection_screen.

  if gv_execute_report = abap_false.
    return.
  endif.

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

  PERFORM sdm_main_article.

end-of-selection.

  perform display_results_excep_report using gt_objects
                                             gt_sdm_articles
                                             gc_object.
