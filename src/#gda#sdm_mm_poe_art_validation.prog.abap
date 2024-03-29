include /gda/sdm_mm_poi_art_data.
include /gda/sdm_mm_art_object_data.

data:
  ls_mg03_sdm_brf         type mg03steuer,
  ls_wrpl                 type wrpl,
  ls_merrdat              type merrdat,
  ls_rmmw1                type rmmw1,
  matnr_ranges            type range of mara-matnr,
  matnr_range             like line of matnr_ranges,
  attyp_ranges            type range of mara-attyp,
  attyp_range             like line of attyp_ranges,
  validate                type boolean,
  material                type matnr,
  vendor                  type rmmw1-lifnr,
  purchase_org            type rmmw1-ekorg,
*  plant                   type rmmw1-ekwrk,
  sales_org               type rmmw1-vkorg,
  distribution_channel    type rmmw1-vtweg,
  store                   type rmmw1-fiwrk,
  store_location_dc       type rmmg1-lgort,
  store_location_st       type rmmg1-lgort,
  distribution_centre     type rmmw1-vzwrk,
  store_details           type /gda/sdm_s_marc_01,
  dist_centre_details     type /gda/sdm_s_marc_01,
  store_details_stl       type /gda/sdm_s_mard_01,
  dist_centre_details_stl type /gda/sdm_s_mard_01,
  weina                   type eina,
  wmgeine                 type mgeine,
  customer             type wrpl-kunnr,
  details_original_material type rmmg1,
  details_dc_material       type rmmw2.

field-symbols:
  <marc>        like  line of gt_marc_sdm,
  <mlea>        like line of tmlea,
  <meinh>       like line of tmeinh,
  <tsteuertab>  like line of tsteuertab,
  <mamt>        like line of tmamt.


import details_original_material = details_original_material from memory id 'DETAILS_ORIGINAL'.
import details_dc_material       = details_dc_material       from memory id 'DETAILS_DC'.

material             = details_original_material-matnr.
sales_org            = details_original_material-vkorg.
distribution_channel = details_original_material-vtweg.
store                = details_original_material-werks.
store_location_st    = details_original_material-lgort.


*plant               = details_dc_material-werks.
store_location_dc   = details_dc_material-lgort.
vendor              = details_dc_material-lifnr.
distribution_centre = details_dc_material-werks.
purchase_org        = details_dc_material-ekorg.

if sy-uname = 'PAVITHRANSS' or sy-uname = 'KKEMRAJ'.
  return.
endif.

if store is initial.
* then use reference store
  if wrmmw2-rmatn is not initial.
    select single marc~werks
      into  store
      from marc inner join t001w on ( marc~werks = t001w~werks )
      where marc~matnr = wrmmw2-rmatn
      and   t001w~vlfkz = 'A'.
  endif.
endif.

if distribution_centre is initial.
* then use reference DC
  if wrmmw2-rmatn is not initial.
    select single marc~werks
      into  distribution_centre
      from marc inner join t001w on ( marc~werks = t001w~werks )
      where marc~matnr = wrmmw2-rmatn
      and   t001w~vlfkz = 'B'.
  endif.
endif.

* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_object_type_id = '01'
                                                     x_sdm_type       = gc_val
                                                     x_sdm_source     = gc_poe ).

* Build a list of all the relevant SDM objects
gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = gc_object
                                                       xt_sdm_types = gr_sdm_type ).

check gt_objects[] is not initial.
* Current Sales Data
call function 'MVKE_GET_BILD'
  exporting
    matnr = material
    vkorg = sales_org
    vtweg = distribution_channel
  importing
    wmvke = wmvke.

* Current POS Data
call function 'WLK2_GET_BILD'
  exporting
    matnr = material
    vkorg = sales_org
    vtweg = distribution_channel
    werks = space
  importing
    wwlk2 = wwlk2.

call function 'MWLI_GET_BILD'
  exporting
    matnr = material
    vkorg = sales_org
    vtweg = distribution_channel
    wmvke = wmvke
    wmaw1 = wmaw1
  importing
    wmwli = wmwli.

customer = store.

call function 'WRPL_GET_BILD'
  exporting
    matnr = material
    kunnr = customer
  importing
    wwrpl = ls_wrpl.

call function 'EINA_E_GET_BILD'
  exporting
    matnr   = material
    lifnr   = vendor
    ekorg   = purchase_org
    werks   = space
  importing
    wmgeine = wmgeine
    weina   = weina.

call function 'MARC_GET_BILD'
  exporting
    matnr = material
    werks = store
  importing
    wmarc = wmarc.

move-corresponding wmarc to store_details.

select single vlfkz
        from t001w
        into store_details-vlfkz
       where werks = store_details-werks.

call function 'MARC_GET_BILD'
  exporting
    matnr = material
    werks = distribution_centre
  importing
    wmarc = wmarc.

move-corresponding wmarc to dist_centre_details.

select single vlfkz
        from t001w
        into dist_centre_details-vlfkz
       where werks = dist_centre_details-werks.

call function 'MARD_GET_BILD'
  exporting
    matnr = material
    werks = store
    lgort = store_location_st
  importing
    wmard = wmard.

move-corresponding wmard to store_details_stl.

select single vlfkz
        from t001w
        into store_details_stl-vlfkz
       where werks = store_details_stl-werks.

call function 'MARD_GET_BILD'
  exporting
    matnr = material
    werks = distribution_centre
    lgort = store_location_dc
  importing
    wmard = wmard.

move-corresponding wmard to dist_centre_details_stl.

select single vlfkz
        from t001w
        into dist_centre_details_stl-vlfkz
       where werks = dist_centre_details_stl-werks.

call function 'MPOP_GET_BILD'
  exporting
    matnr = material
    werks = distribution_centre
  importing
    wmpop = wmpop.

* General
move-corresponding wmara to gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                             i_contents = gs_mara_sdm ).
* Plants
store_details-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                               i_contents = store_details ).
insert store_details into table gt_marc_sdm.

dist_centre_details-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                                     i_contents = dist_centre_details ).
insert dist_centre_details into table gt_marc_sdm.

* Storage Location - store
*move-corresponding store_details_stl to gs_mard_sdm.
store_details_stl-matnr = gs_marc_sdm-matnr.
store_details_stl-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                                   i_contents = store_details_stl ).
insert store_details_stl into table gt_mard_sdm.

* Storage Location - dc
*move-corresponding dist_centre_details to gs_mard_sdm.
dist_centre_details_stl-matnr = gs_marc_sdm-matnr.
dist_centre_details_stl-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                                     i_contents = dist_centre_details_stl ).
insert dist_centre_details_stl into table gt_mard_sdm.


*Change Document Structure for Material Master/Product Group
move-corresponding wmpgd to gs_mpgd_sdm.

if gs_mpgd_sdm-werks = distribution_centre.
  gs_mpgd_sdm-vlfkz = dist_centre_details_stl-vlfkz.
elseif gs_mpgd_sdm-werks = store.
  gs_mpgd_sdm-vlfkz = store_details_stl-vlfkz.
endif.

gs_mpgd_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
append gs_mpgd_sdm to gt_mpgd_sdm.


move-corresponding weina   to gs_eina_sdm.
move-corresponding wmgeine to gs_eine_sdm.
gs_eina_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                         i_contents = gs_eina_sdm ).
gs_eine_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                        i_contents = gs_eine_sdm ).
append gs_eina_sdm to gt_eina_sdm.
append gs_eine_sdm to gt_eine_sdm.

* Material Valuation
move-corresponding wmbew to gs_mbew_sdm.
if gs_mbew_sdm-bwkey = distribution_centre.
  gs_mbew_sdm-vlfkz = dist_centre_details_stl-vlfkz.
elseif gs_mbew_sdm-bwkey = store.
  gs_mbew_sdm-vlfkz = store_details_stl-vlfkz.
endif.
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

* Material Master: Default Fields and Special Retail Fields
move-corresponding wmaw1 to gs_maw1_sdm.
gs_maw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                         i_contents = gs_maw1_sdm ).
append gs_maw1_sdm to gt_maw1_sdm.

* LIFO-relevant materials
if wmbew-xlifo = abap_true.
  move-corresponding wmyms to gs_myms_sdm.
  gs_myms_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                          i_contents = gs_myms_sdm ).
  append gs_myms_sdm to gt_myms_sdm.
endif.

* Forecast Parameters
move-corresponding wmpop to gs_mpop_sdm.
gs_mpop_sdm-vlfkz = dist_centre_details_stl-vlfkz.
gs_mpop_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MPOP'
                                                                         i_contents = gs_mpop_sdm ).
append gs_mpop_sdm to gt_mpop_sdm.

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


if ls_wrpl-matnr is not initial.
  move-corresponding ls_wrpl to gs_wrpl_sdm.
  gs_wrpl_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                           i_contents = gs_mara_sdm ).
  append gs_wrpl_sdm to gt_wrpl_sdm.
endif.

* Vendor-Specific EANs
loop at tmlea assigning <mlea>.
  move-corresponding <mlea> to gs_mlea_sdm.
  gs_mlea_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                       i_contents = gs_mlea_sdm ).
  append gs_mlea_sdm to gt_mlea_sdm.
  clear:
   gs_mlea_sdm.
endloop.

* Unit of Measure for Display
loop at tmeinh assigning <meinh>.
  move-corresponding <meinh> to gs_meinh_sdm.
  gs_meinh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEINH'
                                                                       i_contents = gs_meinh_sdm ).

  append gs_meinh_sdm to gt_meinh_sdm.
  clear:
   gs_meinh_sdm.
endloop.

loop at tsteuertab assigning <tsteuertab>.
  move-corresponding <tsteuertab> to ls_mg03_sdm_brf.
  append ls_mg03_sdm_brf to gt_mg03_sdm_brf.
endloop.

create object go_selection.
go_selection->mv_object = gs_mara_sdm-matnr.

matnr_range-sign   = 'I'.
matnr_range-option = 'EQ'.
matnr_range-low    = gs_mara_sdm-matnr.
append matnr_range to matnr_ranges.

attyp_range-sign   = 'I'.
attyp_range-option = 'EQ'.
attyp_range-low    = gs_mara_sdm-attyp.
append attyp_range to attyp_ranges.

gs_selscreen-matnr[]  = matnr_ranges[].
gs_selscreen-attyps[] = attyp_ranges[].
gs_selscreen-eord     = abap_true.
go_selection->set_selscreen( is_selscreen = gs_selscreen ).
go_selection->main( ).
go_selection->build_spec( ).
gt_eord_sdm[] = go_selection->mt_eord_spec[].

loop at gt_eord_sdm assigning field-symbol(<eord_sdm>).
  <eord_sdm>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EORD'
                                                                              i_contents = <eord_sdm> ).
endloop.

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

  check <objects> is assigned.
  check <objects>-object is bound.
  check <objects>-object->is_active( ) = abap_true
    and <objects>-object->mt_message[] is initial.
  try.
      gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

      loop at gt_attributes assigning <attribute>.
        assign (<attribute>-abap_type) to <set_data>.
        check sy-subrc = 0.
        <objects>-object->set_selection( iv_name = <attribute>-name
                                         iv_data = <set_data>
                                         iv_type = <attribute>-type ).

      endloop.

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
* For Testing
    ls_merrdat-msgv1 = <result_val>-sdm_tabkey.
    if <result_val>-extra_v1 cs 'EINA'.
      ls_merrdat-msgv2 = weina-lifnr.
      ls_merrdat-msgv3 = wmgeine-ekorg.
    endif.
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
* For Testing
    ls_merrdat-msgv1 = <result_val>-sdm_tabkey.
    if <result_val>-extra_v1 cs 'EINA' or <result_val>-extra_v1 cs 'EINE'.
      ls_merrdat-msgv2 = weina-lifnr.
      ls_merrdat-msgv3 = wmgeine-ekorg.
    endif.
    if <result_val>-extra_v1 cs 'MPOP'.
      ls_merrdat-msgv1 = wmpop-werks.
    endif.
    if <result_val>-extra_v1 cs 'MARD'.
      loop at gt_mard_sdm into gs_mard_sdm.
        if <result_val>-sdm_tabkey cs  gs_mard_sdm-werks
        and <result_val>-sdm_tabkey cs  gs_mard_sdm-lgort.
          ls_merrdat-msgv1 = gs_mard_sdm-werks.
          ls_merrdat-msgv2 = gs_mard_sdm-lgort.
        endif.
      endloop.
    endif.
    if <result_val>-extra_v1 cs 'MARC'.
      loop at gt_marc_sdm into gs_marc_sdm.
        if <result_val>-sdm_tabkey cs gs_marc_sdm-werks.
          ls_merrdat-msgv1 = gs_marc_sdm-werks.
        endif.
      endloop.
    endif.

    if <result_val>-extra_v1 cs 'MLGN'.
      loop at gt_mlgn_sdm into gs_mlgn_sdm.
        if <result_val>-sdm_tabkey cs gs_mlgn_sdm-lgnum.
          ls_merrdat-msgv1 = gs_mlgn_sdm-lgnum.
        endif.
      endloop.
    endif.

    if <result_val>-extra_v1 cs 'MWLI'.
      if wmwli-matnr is initial.
        wmwli-matnr = wmara-matnr.
      endif.
      ls_merrdat-msgv1 = wmwli-matnr.
    endif.

    if <result_val>-extra_v1 cs 'MBEW'.
      ls_merrdat-msgv1 = wmara-matnr.
    endif.
    if <result_val>-extra_v1 cs 'MYMS'.
      ls_merrdat-msgv1 = wmyms-matnr.
    endif.
    if <result_val>-extra_v1 cs 'WLK2'.
      ls_merrdat-msgv1 = wwlk2-vkorg.
      ls_merrdat-msgv2 = wwlk2-vtweg.
    endif.
    if <result_val>-extra_v1 cs 'MAW1'.
      ls_merrdat-msgv1 = wmaw1-matnr.
    endif.
    if <result_val>-extra_v1 cs 'MAMT'.
      loop at gt_mamt_sdm into gs_mamt_sdm.
        if <result_val>-sdm_tabkey cs  gs_mamt_sdm-matnr.
          ls_merrdat-msgv1 = gs_mamt_sdm-matnr.
        endif.
      endloop.
    endif.
    if <result_val>-extra_v1 cs 'MPGD'.
      ls_merrdat-msgv1 = wmpgd-werks.
    endif.

    if <result_val>-extra_v1 cs 'EORD'.
      loop at gt_eord_sdm assigning <eord_sdm>.
        if <result_val>-sdm_tabkey cs  <eord_sdm>-werks.
          ls_merrdat-msgv1 = <eord_sdm>-werks.
        endif.
      endloop.
    endif.

    if <result_val>-extra_v1 cs 'MVKE'.
      loop at gt_mvke_sdm into gs_mvke_sdm.
        if gs_mvke_sdm-vkorg = space or gs_mvke_sdm-vtweg = space.
          ls_merrdat-msgv1 = ' ref '.
          ls_merrdat-msgv2 = ' ref '.
          continue.
        endif.

        if <result_val>-sdm_tabkey cs  gs_mvke_sdm-vkorg and
           <result_val>-sdm_tabkey cs  gs_mvke_sdm-vtweg.
          ls_merrdat-msgv1 = gs_mvke_sdm-vkorg.
          ls_merrdat-msgv2 = gs_mvke_sdm-vtweg.
        endif.
      endloop.
    endif.

    if <result_val>-extra_v1 cs 'MEINH'.
      loop at gt_meinh_sdm into gs_meinh_sdm.
        if <result_val>-sdm_tabkey cs  gs_meinh_sdm-matnr.
          ls_merrdat-msgv1 = gs_meinh_sdm-matnr.
        endif.
      endloop.
    endif.

    if <result_val>-extra_v1 cs 'MLEA'.
      loop at gt_mlea_sdm into gs_mlea_sdm.
        if <result_val>-sdm_tabkey cs  gs_mlea_sdm-lifnr.
          ls_merrdat-msgv1 = gs_mlea_sdm-matnr.
        endif.
      endloop.
    endif.

    append ls_merrdat to rt_errdat.
  endif.
endloop.
