*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_COMMON_MM_MAT
*&---------------------------------------------------------------------*
form set_post_processing .

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_val.
  append gs_sdm_type to gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pir.
  append gs_sdm_type to gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pri.
  append gs_sdm_type to gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_src.
  append gs_sdm_type to gr_sdm_type_post_proc.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_tar.
  append gs_sdm_type to gr_sdm_type_post_proc.

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

  gs_selscreen-makt = p_makt.
  gs_selscreen-marc = p_marc.
  gs_selscreen-mard = p_mard.

  gs_selscreen-mvke = p_mvke.
  gs_selscreen-mbew = p_mbew.
  gs_selscreen-mlgn = p_mlgn.
  gs_selscreen-mlgt = p_mlgt.
  gs_selscreen-mapr = p_mapr.
  gs_selscreen-crvm = p_crvm.
  gs_selscreen-mlan = p_mlan.
  gs_selscreen-marm = p_marm.
  gs_selscreen-mean = p_mean.
  gs_selscreen-eord = p_eord.
  gs_selscreen-eina = p_eina.
  gs_selscreen-eine = p_eine.
*  gs_selscreen-mpop = p_mpop.
  gs_selscreen-struc = p_struc.

  gs_selscreen-max_rows = p_max.
  go_selection->set_selscreen( is_selscreen = gs_selscreen ).
endform.

*&---------------------------------------------------------------------*
*&      Form  BRF_LOGIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0342   text
*      -->P_0343   text
*      -->P_0344   text
*      <--P_<OBJECTS>_OBJECT  text
*----------------------------------------------------------------------*
*form brf_logic  using x_type
*                      xo_mapping     type ref to /gda/sdm_cl_brf_mapping
*                      xo_stewardship type ref to /gda/sdm_cl_stwd_app_main
*             changing xo_object      type ref to /gda/sdm_cl_core."/gda/sdm_cl_material.
*
*  if xo_object is initial.
*    try.
*        xo_object ?= /gda/sdm_cl_core=>factory( iv_object_type = /gda/sdm_cl_material=>mc_object
*                                             iv_source         = gv_source
*                                             iv_type           = x_type
*                                             iv_stats          = p_stat1
*                                             iv_stats_brf      = p_stat2
*                                             iv_mapping        = xo_mapping
*                                             iv_stewardship    = xo_stewardship
*                                             iv_errors_only    = space ).
*      catch cx_fdt_input.
*
*        if xo_object is not initial.
*          xo_object->display_messages( ).
*          exit.
*        endif.
*    endtry.
*  endif.
*
*  check xo_object->mt_message[] is initial.
*
*  perform set_data using x_type "gv_type
*                   changing xo_object.
*
*  try.
*      xo_object->main( ).
*    catch /gda/cx_sdm_exception_handl into gx_sdm_root.
*      gv_message = gx_sdm_root->mv_text.
*      if sy-batch = abap_true.
*        write: / gv_message.
*      else.
*        message gv_message type 'W'.
*      endif.
*    catch cx_fdt_input into gx_fdt.
*      call method gx_fdt->if_message~get_longtext
*        receiving
*          result = gv_message.
*      if sy-batch = abap_true.
*        write: / gv_message.
*      else.
*        message gv_message type 'W'.
*      endif.
*  endtry.
*endform.

*form set_data using p_type
*              changing xo_object type ref to /gda/sdm_cl_core."/gda/sdm_cl_material.
*  data:
**    ls_mara       TYPE /gda/sdm_s_mara,
*    lt_attributes type standard table of xo_object->ty_brf_attributes.
*
*  field-symbols:
*    <attribute> like line of lt_attributes,
*    <data>      type any.
*
**  lt_attributes = xo_object->get_object_attributes( ).
*  lt_attributes = xo_object->get_object_attributes( iv_type = p_type ).
*
*
*  move-corresponding go_selection->ms_mara_spec to gs_mara_sdm.
*
*  loop at lt_attributes assigning <attribute>.
*    assign (<attribute>-abap_type) to <data>.
*    try.
*        xo_object->set_selection( iv_name = <attribute>-name  iv_data = <data> iv_type = p_type ).
*      catch /gda/cx_sdm_exception_handl into gx_sdm_root.
*        gv_message = gx_sdm_root->mv_text.
*        if sy-batch = abap_true.
*          write: / gv_message.
*        else.
*          message gv_message type 'W'.
*        endif.
*    endtry.
*  endloop.
*
*  if gv_message is not initial.
*    if sy-batch = abap_true.
*      write: / gv_message.
*    else.
*      message gv_message type 'I'.
*    endif.
*  endif.
*endform.
