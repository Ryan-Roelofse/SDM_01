*&---------------------------------------------------------------------*
*&  Include     : /GDA/SDM_MM_ART_DERIVATION                  [MWD-BRF+]
*&  Description : Call BRF+ MM Derivation Function, FN_DERIVE_ARTICLE,
*&                during execution of transactions MM42/MM41. Included
*&                in function module MATERIAL_REFERENCE_RT.  Parameter
*&                ID '/GDA/RMMG1' will contain original Sales Area selected
*&                and is populated in fnct MAIN_PARAMETER_SET_START_RET.
*&                Program will populate derived values back to screen.
*&---------------------------------------------------------------------*

include /gda/sdm_mm_poi_art_data.
include /gda/sdm_mm_art_object_data.

data:
  ls_screen_control type /gda/sdm_s_screen_control,
  lt_screen_control like hashed table of ls_screen_control
                         with unique key screen_name,
 ls_rmmw1        type rmmw1,
 ls_rmmw1_sdm    type /gda/sdm_s_rmmw1,
 ls_mg03_sdm_brf type mg03steuer,
 ls_mwli         type mwli.

field-symbols:
  <field>       type any,
  <table>       type any,
  <mlea>        like line of tmlea,
  <meinh>       like line of wmeinh,
  <tsteuertab>  like line of wsteuertab,
  <mamt>        like line of tmamt,
  <malg>        like line of tmalg.


free memory id 'GD_MM_BRF_SCREEN_CONTROL'.
check p_t130m-aktyp <> gc_display.

move-corresponding wmara to ls_rmmw1_sdm.
ls_rmmw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
append ls_rmmw1_sdm to gt_rmmw1_sdm.

* General
move-corresponding wmara to gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).

* Plant
move-corresponding wmarc to gs_marc_sdm.
gs_marc_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                         i_contents = gs_marc_sdm ).
append gs_marc_sdm to gt_marc_sdm.

* Storage Location
move-corresponding wmard to gs_marc_sdm.
gs_mard_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                         i_contents = gs_mard_sdm ).
append gs_mard_sdm to gt_mard_sdm.

* Material Master: Default Fields and Special Retail Fields
move-corresponding wmaw1 to gs_maw1_sdm.
gs_maw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                         i_contents = gs_maw1_sdm ).
append gs_maw1_sdm to gt_maw1_sdm.

* Material Descriptions
move-corresponding wmakt to gs_makt_sdm.
gs_makt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAKT'
                                                                         i_contents = gs_makt_sdm ).
append gs_makt_sdm to gt_makt_sdm.

* Sales Data for Material
move-corresponding wmvke to gs_mvke_sdm.
gs_mvke_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                         i_contents = gs_mvke_sdm ).

append gs_mvke_sdm to gt_mvke_sdm.

* Material Data for Each Warehouse Number
move-corresponding wmlgn to gs_mlgn_sdm.
gs_mlgn_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGN'
                                                                         i_contents = gs_mlgn_sdm ).
append gs_mlgn_sdm to gt_mlgn_sdm.

* Material Data for Each Storage Type
move-corresponding wmlgt to gs_mlgt_sdm.
gs_mlgt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGT'
                                                                         i_contents = gs_mlgt_sdm ).
append gs_mlgt_sdm to gt_mlgt_sdm.

* Forecast Parameters
move-corresponding wmpop to gs_mpop_sdm.
gs_mpop_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MPOP'
                                                                         i_contents = gs_mpop_sdm ).
append gs_mpop_sdm to gt_mpop_sdm.


*Change Document Structure for Material Master/Product Group
*move-corresponding wmpgd to gs_???_sdm.
*append gs_???_sdm to gt_???_sdm.


* Material Valuation
move-corresponding wmbew to gs_mbew_sdm.
gs_mbew_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                         i_contents = gs_mbew_sdm ).
append gs_mbew_sdm to gt_mbew_sdm.

* LIFO-relevant materials
if wmbew-xlifo = abap_true.
  move-corresponding wmyms to gs_myms_sdm.
  gs_myms_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                           i_contents = gs_myms_sdm ).
  append gs_myms_sdm to gt_myms_sdm.
endif.

* Purchasing Info Record: General Data
move-corresponding weina to gs_eina_sdm.
gs_eina_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                         i_contents = gs_eina_sdm ).

append gs_eina_sdm to gt_eina_sdm.

* Purchasing Info Record: Purchasing Organization Data
move-corresponding weine to gs_eine_sdm.
gs_eine_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                         i_contents = gs_eine_sdm ).

append gs_eine_sdm to gt_eine_sdm.

* Material Master: Default Fields and Special Retail Fields
move-corresponding wmaw1 to   gs_maw1_sdm.
gs_maw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                         i_contents = gs_maw1_sdm ).

append gs_maw1_sdm to gt_maw1_sdm.

* Article Master Data SAP Retail / Part POS Control Data
move-corresponding wwlk2 to gs_wlk2_sdm. "ls_wlk2.
gs_wlk2_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WLK2'
                                                                         i_contents = gs_wlk2_sdm ).

append gs_wlk2_sdm to gt_wlk2_sdm.

* Listing (Retail)
call function 'MWLI_GET_BILD'
  exporting
    matnr = p_rmmg1-matnr
    vkorg = p_rmmg1-vkorg
    vtweg = p_rmmg1-vtweg
    wmvke = wmvke
*   omvke = *mvke
    wmaw1 = wmaw1
*   omaw1 = *maw1
  importing
    wmwli = ls_mwli.
*    xmwli = *mwli.

if ls_mwli-matnr is not initial.
  move-corresponding ls_mwli to gs_mwli_sdm.
  gs_mwli_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                           i_contents = gs_mara_sdm ).

  append gs_mwli_sdm to gt_mwli_sdm.
endif.

* Handle tables
*1WKTEXT
*4WSTEUMMTAB
*6WMALG


*2 Unit of Measure for Display
loop at wmeinh assigning <meinh>.
  move-corresponding <meinh> to gs_meinh_sdm.
  gs_meinh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                          i_contents = gs_mara_sdm ).

  append gs_meinh_sdm to gt_meinh_sdm.
  clear:
   gs_meinh_sdm.
endloop.

*3 Table for Taxes
loop at wsteuertab assigning <tsteuertab>.
  move-corresponding <tsteuertab> to ls_mg03_sdm_brf.
*gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
*                                                                         i_contents = gs_mara_sdm ).

  append ls_mg03_sdm_brf to gt_mg03_sdm_brf.
endloop.

*5 Material Master Texts per Unit of Measure and Text ID
loop at tmamt assigning <mamt>.
  move-corresponding <mamt> to gs_mamt_sdm.
  gs_mamt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAMT'
                                                                           i_contents = gs_mamt_sdm ).

  append gs_mamt_sdm to gt_mamt_sdm.
endloop.

* Assignment of Layout Modules to Materials
*loop at tmalg assigning <malg>.
*  move-corresponding <malg> to gs_???_sdm.
*  append gs_???_sdm to gt_???_sdm.
*endloop.

* Vendor-Specific EANs
loop at tmlea assigning <mlea>.
  move-corresponding <mlea> to gs_mlea_sdm.
  gs_mlea_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                           i_contents = gs_mlea_sdm ).

  append gs_mlea_sdm to gt_mlea_sdm.
  clear:
   gs_mlea_sdm.
endloop.


* Set default SDM Type and include any custom SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_object_type_id = gc_object_id
                                                     x_sdm_type       = gc_der
                                                     x_sdm_source     = gc_poe ).

* Build a list of all the relevant SDM objects for Article - Drivations
select * from /gda/sdm_setup6 into corresponding fields of table gt_objects
  where  sdm_object  = gc_object
   and   type        in gr_sdm_type
   and   active      = abap_true.

check gt_objects[] is not initial.

loop at gt_objects assigning <objects>.
  try.
      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                                     iv_source      = gc_poe
                                                     iv_type        = <objects>-type
                                                     iv_stats       = space ).
    catch cx_fdt_input into gx_fdt.

      if <objects>-object is not initial.
        <objects>-object->display_messages( ).
        exit.
      endif.
  endtry.

  check <objects> is assigned.
  check <objects>-object is bound.
  check <objects>-object->is_active( ) = abap_true
    and <objects>-object->mt_message[] is initial.

  gt_attributes = <objects>-object->get_object_attributes(  iv_type = <objects>-type  ).

  loop at gt_attributes assigning <attribute>.
    assign (<attribute>-abap_type) to <set_data>.
    <objects>-object->set_selection( iv_name = <attribute>-name
                                     iv_data = <set_data>
                                     iv_type = <attribute>-type  ).
  endloop.

  try.
      <objects>-object->main( ).
    catch /gda/cx_sdm_exception_handl into gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      if sy-batch = abap_true.
        write: / gv_message.
      else.
        message gv_message type 'I'.
      endif.
      return.
  endtry.

  gr_data = <objects>-object->return_brf_result( ).
  assign gr_data->* to <results_der>.

  if <results_der> is not assigned.
    return.
  endif.
* collect all the results..
  if <results_der_all> is not assigned.
    if <objects>-object is bound.
      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
      assign gr_data_empty->* to <results_der_all>.
    endif.
  endif.
  append lines of <results_der> to <results_der_all>.
endloop.

check <results_der_all> is assigned.
loop at <results_der_all> into gs_result_der.
  if gs_result_der-skip_derivation = space.
    assign (gs_result_der-table) to <table>.
    if sy-subrc <> 0.
      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
      message gv_message type 'W'.
      return.
    endif.

    assign component gs_result_der-field of structure <table> to <field>.
    if sy-subrc = 0.
      <field> = gs_result_der-value.
    else.
      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
      message gv_message type 'W'.
    endif.
  endif.

*Fill the screen control internal table
  move-corresponding gs_result_der to ls_screen_control.

  clear ls_screen_control-screen_name.
  concatenate gs_result_der-table '-' gs_result_der-field into ls_screen_control-screen_name.
  if ls_screen_control-screen_name is not initial.
    insert ls_screen_control into table lt_screen_control.
  endif.
endloop.

if lt_screen_control is not initial.
  export screen_control = lt_screen_control to memory id 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR
endif.
