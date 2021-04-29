*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_COMMON_ARTICLE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_POST_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_post_processing .

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_val.
  APPEND gs_sdm_type TO gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pir.
  APPEND gs_sdm_type TO gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pri.
  APPEND gs_sdm_type TO gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_src.
  APPEND gs_sdm_type TO gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_tar.
  APPEND gs_sdm_type TO gr_sdm_type_post_proc.

endform.                    " SET_POST_PROCESSING
form selection_screen.
  field-symbols:
   <range> type any table.

**// Populate Selection-Screen Structure
* Main Selection
  gs_selscreen-matnr = s_matnr[].
  gs_selscreen-mtart = s_mtart[].
  gs_selscreen-matkl = s_matkl[].
  gs_selscreen-ersda = s_ersda[].
  gs_selscreen-ernam = s_ernam[].
  gs_selscreen-laeda = s_laeda[].
  gs_selscreen-bwscl = s_bwscl[].
  gs_selscreen-attyp = s_attyp[].
  gs_selscreen-aenam = s_aenam[].
  gs_selscreen-mstae = s_mstae[].
  gs_selscreen-attyps = s_attyps[].

* Context Selection
  assign ('S_WERKS[]') to <range>.
  if <range> is assigned.
    gs_selscreen-werks = <range>.
  endif.
  unassign <range>.

  assign ('S_LGORT[]') to <range>.
  if <range> is assigned.
    gs_selscreen-lgort = <range>.
  endif.
  unassign <range>.

  assign ('S_VKORG[]') to <range>.
  if <range> is assigned.
    gs_selscreen-vkorg = <range>.
  endif.
  unassign <range>.

  assign ('S_VTWEG[]') to <range>.
  if <range> is assigned.
    gs_selscreen-vtweg = <range>.
  endif.
  unassign <range>.

  assign ('S_MMSTA[]') to <range>.
  if <range> is assigned.
    gs_selscreen-mmsta = <range>.
  endif.
  unassign <range>.

  assign ('S_INFNR[]') to <range>.
  if <range> is assigned.
    gs_selscreen-infnr = <range>.
  endif.
  unassign <range>.

    assign ('S_LIFNR[]') to <range>.
  if <range> is assigned.
    gs_selscreen-lifnr = <range>.
  endif.
  unassign <range>.

  perform check_selection_entries changing gv_selection_fields_entered.
  perform limit_max_entries changing p_max gv_execute_report.
  if gv_execute_report = abap_false.
    return.
  endif.

*  gs_selscreen-makt = p_makt.
*  gs_selscreen-marc = p_marc.
*  gs_selscreen-mard = p_mard.
*
*  gs_selscreen-mvke = p_mvke.
*  gs_selscreen-mbew = p_mbew.
*  gs_selscreen-mlgn = p_mlgn.
*  gs_selscreen-mlgt = p_mlgt.
*  gs_selscreen-mapr = p_mapr.
*  gs_selscreen-crvm = p_crvm.
*  gs_selscreen-mlan = p_mlan.
*  gs_selscreen-marm = p_marm.
*  gs_selscreen-mean = p_mean.
*  gs_selscreen-eord = p_eord.
*  gs_selscreen-eina = p_eina.
*  gs_selscreen-eine = p_eine.
  gs_selscreen-struc = p_struc.

  gs_selscreen-max_rows = p_max.
  go_selection->set_selscreen( is_selscreen = gs_selscreen ).
endform.
