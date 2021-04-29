*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_VALIDATION
*&---------------------------------------------------------------------*
include /gda/sdm_mm_poi_art_data.
include /gda/sdm_mm_art_object_data.

data:
  ls_marc         type /gda/sdm_s_marc_01,
  ls_t130m        type t130m,
  lt_malg         type malg_tty,
  lt_mat_ktext    type mat_ktext,
  lt_mat_steuer   type mat_steuer,
  lt_mat_steumm   type mat_steumm,
  lt_mean         type mean_tab,
  ls_syst         type syst,
  ls_mg03_sdm_brf type mg03steuer,
  ls_eina         type eina,
  ls_eine         type eine,
  ls_merrdat      type merrdat,
  ls_rmmw1        type rmmw1,
  lv_count        type i.

field-symbols:
  <marc>        like  line of gt_marc_sdm,
  <mlea>        like line of tmlea,
  <meinh>       like line of tmeinh,
  <tsteuertab>  like line of tsteuertab,
  <mamt>        like line of tmamt.

* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_object_type_id = '01'
                                                     x_sdm_type       = gc_val
                                                     x_sdm_source     = gc_poe ).

* Build a list of all the relevant SDM objects
gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = gc_object
                                                       xt_sdm_types = gr_sdm_type ).

check gt_objects[] is not initial.

import rmmw1 to ls_rmmw1 from memory id 'GD_MM_ARTICLE_VAL_RMMW1'.
append ls_rmmw1 to gt_rmmw1_sdm.

*IMPORT basic_text TO lt_basic_text FROM MEMORY ID 'GD_MM_ARTICLE_VAL_TEXT'.

* General
move-corresponding wmara to gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
* Plant
move-corresponding wmarc to ls_marc.
move-corresponding wmarc to gs_marc_sdm.
gs_marc_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                         i_contents = gs_marc_sdm ).
*append gs_marc_sdm to gt_marc_sdm.

* Storage Location
move-corresponding wmard to gs_marc_sdm.
gs_mard_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                        i_contents = gs_mard_sdm ).
append gs_mard_sdm to gt_mard_sdm.


* PIR General and Purch Org
import eina to ls_eina eine to ls_eine from memory id 'GD_MM_ARTICLE_VAL_EINA_EINE'.
move-corresponding ls_eina to gs_eina_sdm.
move-corresponding ls_eine to gs_eine_sdm.

gs_eina_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                         i_contents = gs_eina_sdm ).

gs_eine_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                        i_contents = gs_eine_sdm ).

append gs_eina_sdm to gt_eina_sdm.
append gs_eine_sdm to gt_eine_sdm.

* Material Valuation
move-corresponding wmbew to gs_mbew_sdm.
gs_mbew_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                        i_contents = gs_mbew_sdm ).
append gs_mbew_sdm to gt_mbew_sdm.

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

* Material Master Texts per Unit of Measure and Text ID
loop at tmamt assigning <mamt>.
  move-corresponding <mamt> to gs_mamt_sdm.
  gs_mamt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAMT'
                                                                          i_contents = gs_mamt_sdm ).
  append gs_mamt_sdm to gt_mamt_sdm.
endloop.

* Sales Data for Material
move-corresponding wmvke to gs_mvke_sdm.
gs_mvke_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                        i_contents = gs_mvke_sdm ).
append gs_mvke_sdm to gt_mvke_sdm.

* LIFO-relevant materials
if wmbew-xlifo = abap_true.
  move-corresponding wmyms to gs_myms_sdm.
  gs_myms_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                          i_contents = gs_myms_sdm ).
  append gs_myms_sdm to gt_myms_sdm.
endif.

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
move-corresponding wmwli to gs_mwli_sdm. "ls_mwli.
gs_mwli_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                        i_contents = gs_mara_sdm ).
append gs_mwli_sdm to gt_mwli_sdm.

*ls_t130m = wstat.
*
*lt_malg = tmalg[].

* Vendor-Specific EANs
loop at tmlea assigning <mlea>.
  move-corresponding <mlea> to gs_mlea_sdm.
  gs_mlea_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                       i_contents = gs_mlea_sdm ).

  append gs_mlea_sdm to gt_mlea_sdm.
  clear:
   gs_mlea_sdm.
endloop.

*lt_mlea = tmlea[].

*lt_mamt = tmamt[].

*delete ttext where spras <> sy-langu.  "Delete other languages
*lt_mat_ktext = ttext[].


* Unit of Measure for Display
loop at tmeinh assigning <meinh>.
  move-corresponding <meinh> to gs_meinh_sdm.
  gs_meinh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEINH'
                                                                       i_contents = gs_meinh_sdm ).

  append gs_meinh_sdm to gt_meinh_sdm.
  clear:
   gs_meinh_sdm.
endloop.

lt_mat_steuer = tsteuertab[].


loop at tsteuertab assigning <tsteuertab>.
  move-corresponding <tsteuertab> to ls_mg03_sdm_brf.
*  gs_mg03_sdm-matnr = gs_mara_sdm-matnr.
  append ls_mg03_sdm_brf to gt_mg03_sdm_brf.
endloop.

*  LOOP AT gt_mg03 ASSIGNING <mg03> WHERE matnr = x_mara-matnr.
*    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm.
*    gs_mg03_sdm-matnr = <mg03>-matnr.
*    APPEND gs_mg03_sdm TO gt_mg03_sdm.
*
*    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm_brf.
*    APPEND gs_mg03_sdm_brf TO gt_mg03_sdm_brf.
*
*    CLEAR:
*      gs_mg03_sdm,
*      gs_mg03_sdm_brf.
*  ENDLOOP.

lt_mat_steumm = tsteummtab[].

lt_mean       = tmean_me_tab[].

ls_syst = sy.

* Enhancement Point: Populate non standard SDM tables and structures
ENHANCEMENT-POINT /gda/sdm_mm_art_ep2 SPOTS /gda/sdm_mm_art_ep2 .


* Only process SDM once!
if ls_rmmw1-fiwrk ne space.
  add 1 to lv_count.
endif.

if ls_rmmw1-vzwrk ne space.
  add 1 to lv_count.
endif.

* Store
if ( ls_marc-werks = ls_rmmw1-fiwrk ) and
   ( ls_marc-werks = wrmmg1-werks )    and
     ls_marc-werks is not initial.
  import gt_marc_sdm = gt_marc_sdm  from memory id ls_marc-matnr.
  read table gt_marc_sdm assigning <marc> with key matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  if sy-subrc <> 0.
    append ls_marc to gt_marc_sdm.
    export gt_marc_sdm = gt_marc_sdm  to memory id ls_marc-matnr.
  endif.
endif.

* DC
if ( ls_marc-werks = ls_rmmw1-vzwrk  ) and
   ( ls_marc-werks = wrmmg1-werks )     and
     ls_marc-werks is not initial.
  import gt_marc_sdm = gt_marc_sdm  from memory id ls_marc-matnr.
  read table gt_marc_sdm assigning <marc> with key matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  if sy-subrc <> 0.
    append ls_marc to gt_marc_sdm.
    export gt_marc_sdm = gt_marc_sdm  to memory id ls_marc-matnr.
  endif.
endif.

import gt_marc_sdm = gt_marc_sdm  from memory id ls_marc-matnr.

loop at gt_marc_sdm assigning field-symbol(<marc_sdm>).
  <marc_sdm>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                          i_contents = <marc_sdm> ).
endloop.

describe table gt_marc_sdm.

if sy-tfill = lv_count.
  loop at gt_objects assigning <objects>.
    try.
        <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                                       iv_source      = gc_poe
                                                       iv_type        = <objects>-type
                                                       iv_stats       = abap_true ).
      catch cx_fdt_input into gx_fdt.

        if <objects>-object is not initial.
          <objects>-object->display_messages( ).
          exit.
        endif.
    endtry.


*    IF <objects>-object IS NOT BOUND.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.

    check <objects> is assigned.
    check <objects>-object is bound.
    check <objects>-object->is_active( ) = abap_true
      and <objects>-object->mt_message[] is initial.

*    CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.
*    IF <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS NOT INITIAL.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.
    gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

    loop at gt_attributes assigning <attribute>.
      assign (<attribute>-abap_type) to <set_data>.
      check sy-subrc = 0.
      <objects>-object->set_selection( iv_name = <attribute>-name
                                       iv_data = <set_data>
                                       iv_type = <attribute>-type ).
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
      catch cx_fdt_input into gx_fdt.
        call method gx_fdt->if_message~get_longtext
          receiving
            result = gv_message.
        if sy-batch = abap_true.
          write: / gv_message.
        else.
          message gv_message type 'I'.
        endif.
        return.

    endtry.

    gr_data = <objects>-object->return_brf_result( ).
    assign gr_data->* to <results_val>.

    if <results_val> is not assigned.
      return.
    endif.

* collect all the results..
    if <results_val_all> is not assigned.
      if <objects>-object is bound.
        gr_data_empty  = <objects>-object->return_brf_result_structure( ).
        assign gr_data_empty->* to <results_val_all>.
      endif.
    endif.

    append lines of <results_val> to <results_val_all>.
  endloop.

  refresh:
   gt_marc_sdm.
  export gt_marc_sdm = gt_marc_sdm  to memory id ls_marc-matnr.
endif.

check <results_val_all> is assigned.
sort <results_val_all> by id number.
delete adjacent duplicates from <results_val_all>.

****// Process Message
loop at <results_val_all> assigning <result_val> where type ca 'EAX'.
  if <result_val>-id is initial.
    <result_val>-id = '/GDA/SDM1'.
  endif.

  if <result_val>-number is initial.
    <result_val>-number = '002'.
  endif.

  if <result_val>-message is not initial.
*/ Output only message
    ls_merrdat-tranc = ls_merrdat-tranc + 1.
    ls_merrdat-matnr = wmara-matnr.
    ls_merrdat-msgid = <result_val>-id.
    ls_merrdat-msgty = <result_val>-type.
    ls_merrdat-msgno = <result_val>-number.
    ls_merrdat-msgv1 = <result_val>-message(50).
    ls_merrdat-msgv2 = <result_val>-message+50(50).
    ls_merrdat-msgv3 = <result_val>-message+100(50).
    ls_merrdat-msgv4 = <result_val>-message+150(50).
    append ls_merrdat to rt_errdat.
  else.

*/ Output Variable parts
    ls_merrdat-tranc = ls_merrdat-tranc + 1.
    ls_merrdat-matnr = wmara-matnr.
    ls_merrdat-msgid = <result_val>-id.
    ls_merrdat-msgty = <result_val>-type.
    ls_merrdat-msgno = <result_val>-number.
    ls_merrdat-msgv1 = <result_val>-message_v1.
    ls_merrdat-msgv2 = <result_val>-message_v2.
    ls_merrdat-msgv3 = <result_val>-message_v3.
    ls_merrdat-msgv4 = <result_val>-message_v4.
    append ls_merrdat to rt_errdat.
  endif.
endloop.
*!
