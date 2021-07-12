
*&---------------------------------------------------------------------*
*& Report /GDA/SDM_ARTICLE_REC_STAT_REP1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report /gda/sdm_article_rec_stat_rep2  message-id /gda/sdm_pp1.

*class: lcl_event_rec definition deferred.
*
*data:
*  go_handler_local type ref to lcl_event_rec,
*  go_handler_top   type ref to lcl_event_rec.
*
*include /gda/fordata.
*include /gda/sdm_mm_poi_art_data.
*include /gda/sdm_common_core_data.
*include /gda/sdm_mm_art_obj_data_rep.
*include /gda/sdm_mm_art_data_decl.
*include /gda/sdm_mm_art_screens.
*include /gda/sdm_mm_art_lcl_class.
*include /gda/sdm_mm_rsr_art_rf01.
*include /gda/sdm_mm_art_mod.
*include /gda/sdm_common_core.
*include /gda/sdm_common_core_rsr.
*include /gda/sdm_common_article.
*
*initialization.
*  perform auth_check using gc_auth_rsr.
*  perform init       using gc_object gc_object_id.
*  perform init_art_rsr.
*  perform set_post_processing.
*  perform set_screen_data_rsr.
*
*at selection-screen output.
*  perform screen_output.
*  perform screen_output_art_rsr.
*
*at selection-screen.
*  perform at_selection_screen_rsr.
*
*at selection-screen on value-request for s_arbgb-low.
*  perform on_value_request.
*
*start-of-selection.
*  perform build_structure using gc_default
*                                gc_object
*                                p_struc.
*
*  perform build_dynamic_itab using gc_default
*                             changing ro_data.
*
*  perform get_sdm_types using gc_object gc_object_id gv_type p_user.
*
*  perform get_data.
*
*  perform set_alv_data_new.
*
*  if p_struc = abap_true.
*    perform set_up_relations.
*  endif.
*
**  PERFORM format_final.
*
*end-of-selection.
*  if <dyn_table>[] is not initial.
*    perform display_results  using gt_objects
*                                   gt_sdm_articles
*                                   gc_object.
*  else.
*    if gv_config_err = abap_true.
*      message s005.
*    else.
*      message s006.
*    endif.
*  endif.
