*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_EXC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_article_rec_stat_rep3 MESSAGE-ID /gda/sdm_pp1.

CLASS: lcl_event_rec DEFINITION DEFERRED.

DATA:
  go_handler_local            TYPE REF TO lcl_event_rec.

INCLUDE /gda/sdm_common_core_data.
INCLUDE /gda/fordata.
INCLUDE /gda/sdm_mm_article_data3."zmat_data_test."/gda/sdm_mm_mat_data.
INCLUDE /gda/sdm_mm_art_obj_data_rep."zmat_obj_data_test. "/gda/sdm_mm_mat_obj_data.
INCLUDE /gda/sdm_mm_art_screens3."/gda/sdm_mm_art_screens."zmat_screens_test."/gda/sdm_mm_mat_screens. " check
INCLUDE /gda/sdm_rsr_lcl_class. "/gda/sdm_mm_mat_lcl_class.
INCLUDE /gda/sdm_mm_rsr_article_f03."/gda/sdm_mm_rsr_mat_f03."zrsr_mat_f01_test. "/gda/sdm_mm_rsr_mat_f01.."
INCLUDE /gda/sdm_common_core.
INCLUDE /gda/sdm_common_core_rsr.
INCLUDE /gda/sdm_mod_core.
INCLUDE /gda/sdm_common_mm_article."zmat_test."gda/sdm_common_mm_mat.

INITIALIZATION.
  PERFORM auth_check USING gc_auth_rsr.
  PERFORM init       USING gc_object gc_object_id.
*  perform init_art_rsr.
  PERFORM set_post_processing.
  PERFORM sdm_init_common USING gc_object
                       CHANGING gv_object gv_type gv_source go_selection.

  PERFORM set_screen_data_rsr.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_output.
  PERFORM screen_output_art_rsr.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen_rsr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_arbgb-low.
  PERFORM on_value_request.

START-OF-SELECTION.

  p_rad1    = abap_true.
  gv_type   = /gda/sdm_cl_core=>mc_validation.
  gv_source = /gda/sdm_cl_core=>mc_rep.

  PERFORM selection_screen.
  CHECK gv_execute_report = abap_true.
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

  PERFORM sdm_logic.

  IF p_struc = abap_true.
    PERFORM set_up_relations.
  ENDIF.

END-OF-SELECTION.
  CHECK gv_execute_report = abap_true.
  IF <dyn_table>[] IS NOT INITIAL.
    PERFORM display_results USING gt_objects
                                  gt_sdm_article
                                  gc_object.
  ELSE.
    IF gv_config_err = abap_true.
      MESSAGE s005.
    ELSE.
      MESSAGE s006.
    ENDIF.
  ENDIF.
