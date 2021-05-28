*&---------------------------------------------------------------------*
*&  Include     : /GDA/SDM_MM_ART_DERIVATION                  [MWD-BRF+]
*&  Description : Call BRF+ MM Derivation Function, FN_DERIVE_ARTICLE,
*&                during execution of transactions MM42/MM41. Included
*&                in function module MATERIAL_REFERENCE_RT.  Parameter
*&                ID '/GDA/RMMG1' will contain original Sales Area selected
*&                and is populated in fnct MAIN_PARAMETER_SET_START_RET.
*&                Program will populate derived values back to screen.
*&---------------------------------------------------------------------*

INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_object_data.

DATA:
  ls_screen_control TYPE /gda/sdm_s_screen_control,
  lt_screen_control LIKE HASHED TABLE OF ls_screen_control
                         WITH UNIQUE KEY screen_name,
 ls_rmmw1        TYPE rmmw1,
 ls_rmmw1_sdm    TYPE /gda/sdm_s_rmmw1,
 ls_mg03_sdm_brf TYPE mg03steuer,
 ls_wrpl         TYPE wrpl,
 ls_mwli         TYPE mwli,
 lv_tabname      TYPE tabname.

FIELD-SYMBOLS:
*  <field>       type any,
  <table>       TYPE any,
  <mlea>        LIKE LINE OF tmlea,
  <meinh>       LIKE LINE OF wmeinh,
  <tsteuertab>  LIKE LINE OF wsteuertab,
  <mamt>        LIKE LINE OF tmamt,
  <malg>        LIKE LINE OF tmalg.


FREE MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.
CHECK p_t130m-aktyp <> gc_display.

MOVE-CORRESPONDING wmara TO ls_rmmw1_sdm.
ls_rmmw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
APPEND ls_rmmw1_sdm TO gt_rmmw1_sdm.

* General
MOVE-CORRESPONDING wmara TO gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).

* Plant
MOVE-CORRESPONDING wmarc TO gs_marc_sdm.
gs_marc_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                         i_contents = gs_marc_sdm ).
APPEND gs_marc_sdm TO gt_marc_sdm.

* Storage Location
MOVE-CORRESPONDING wmard TO gs_mard_sdm.
gs_mard_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                         i_contents = gs_mard_sdm ).
APPEND gs_mard_sdm TO gt_mard_sdm.

* Material Master: Default Fields and Special Retail Fields
MOVE-CORRESPONDING wmaw1 TO gs_maw1_sdm.
gs_maw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                         i_contents = gs_maw1_sdm ).
APPEND gs_maw1_sdm TO gt_maw1_sdm.

* Material Descriptions
MOVE-CORRESPONDING wmakt TO gs_makt_sdm.
gs_makt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAKT'
                                                                         i_contents = gs_makt_sdm ).
APPEND gs_makt_sdm TO gt_makt_sdm.

* Sales Data for Material
MOVE-CORRESPONDING wmvke TO gs_mvke_sdm.
gs_mvke_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                         i_contents = gs_mvke_sdm ).

APPEND gs_mvke_sdm TO gt_mvke_sdm.

* Material Data for Each Warehouse Number
MOVE-CORRESPONDING wmlgn TO gs_mlgn_sdm.
gs_mlgn_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGN'
                                                                         i_contents = gs_mlgn_sdm ).
APPEND gs_mlgn_sdm TO gt_mlgn_sdm.

* Material Data for Each Storage Type
MOVE-CORRESPONDING wmlgt TO gs_mlgt_sdm.
gs_mlgt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGT'
                                                                         i_contents = gs_mlgt_sdm ).
APPEND gs_mlgt_sdm TO gt_mlgt_sdm.

* Forecast Parameters
MOVE-CORRESPONDING wmpop TO gs_mpop_sdm.
gs_mpop_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MPOP'
                                                                         i_contents = gs_mpop_sdm ).
APPEND gs_mpop_sdm TO gt_mpop_sdm.

*Change Document Structure for Material Master/Product Group
MOVE-CORRESPONDING wmpgd TO gs_mpgd_sdm.
gs_mpgd_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
APPEND gs_mpgd_sdm TO gt_mpgd_sdm.

* Material Valuation
MOVE-CORRESPONDING wmbew TO gs_mbew_sdm.
gs_mbew_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                         i_contents = gs_mbew_sdm ).
APPEND gs_mbew_sdm TO gt_mbew_sdm.

* LIFO-relevant materials
IF wmbew-xlifo = abap_true.
  MOVE-CORRESPONDING wmyms TO gs_myms_sdm.
  gs_myms_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                           i_contents = gs_myms_sdm ).
  APPEND gs_myms_sdm TO gt_myms_sdm.
ENDIF.

* Purchasing Info Record: General Data
MOVE-CORRESPONDING weina TO gs_eina_sdm.
gs_eina_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                         i_contents = gs_eina_sdm ).

APPEND gs_eina_sdm TO gt_eina_sdm.

* Purchasing Info Record: Purchasing Organization Data
MOVE-CORRESPONDING weine TO gs_eine_sdm.
gs_eine_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                         i_contents = gs_eine_sdm ).

APPEND gs_eine_sdm TO gt_eine_sdm.

* Material Master: Default Fields and Special Retail Fields
MOVE-CORRESPONDING wmaw1 TO   gs_maw1_sdm.
gs_maw1_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                         i_contents = gs_maw1_sdm ).

APPEND gs_maw1_sdm TO gt_maw1_sdm.

* Article Master Data SAP Retail / Part POS Control Data
MOVE-CORRESPONDING wwlk2 TO gs_wlk2_sdm. "ls_wlk2.
gs_wlk2_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WLK2'
                                                                         i_contents = gs_wlk2_sdm ).

APPEND gs_wlk2_sdm TO gt_wlk2_sdm.

* Listing (Retail)
CALL FUNCTION 'MWLI_GET_BILD'
  EXPORTING
    matnr = p_rmmg1-matnr
    vkorg = p_rmmg1-vkorg
    vtweg = p_rmmg1-vtweg
    wmvke = wmvke
*   omvke = *mvke
    wmaw1 = wmaw1
*   omaw1 = *maw1
  IMPORTING
    wmwli = ls_mwli.
*    xmwli = *mwli.

IF ls_mwli-matnr IS NOT INITIAL.
  MOVE-CORRESPONDING ls_mwli TO gs_mwli_sdm.
  gs_mwli_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                           i_contents = gs_mara_sdm ).

  APPEND gs_mwli_sdm TO gt_mwli_sdm.
ENDIF.

* Replenishment: quantities per customer/material
CALL FUNCTION 'WRPL_GET_BILD'
  EXPORTING
    matnr = p_rmmg1-matnr
    kunnr = p_rmmg1-kunnr
  IMPORTING
    wwrpl = ls_wrpl.

IF ls_wrpl-matnr IS NOT INITIAL.
  MOVE-CORRESPONDING ls_wrpl TO gs_wrpl_sdm.
  gs_wrpl_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                           i_contents = gs_mara_sdm ).

  APPEND gs_wrpl_sdm TO gt_wrpl_sdm.
ENDIF.
* Handle tables
*1WKTEXT
*4WSTEUMMTAB
*6WMALG


*2 Unit of Measure for Display
LOOP AT wmeinh ASSIGNING <meinh>.
  MOVE-CORRESPONDING <meinh> TO gs_meinh_sdm.
  gs_meinh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                          i_contents = gs_mara_sdm ).

  APPEND gs_meinh_sdm TO gt_meinh_sdm.
  CLEAR:
   gs_meinh_sdm.
ENDLOOP.

*3 Table for Taxes
LOOP AT wsteuertab ASSIGNING <tsteuertab>.
  MOVE-CORRESPONDING <tsteuertab> TO ls_mg03_sdm_brf.
*gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
*                                                                         i_contents = gs_mara_sdm ).

  APPEND ls_mg03_sdm_brf TO gt_mg03_sdm_brf.
ENDLOOP.

*5 Material Master Texts per Unit of Measure and Text ID
LOOP AT tmamt ASSIGNING <mamt>.
  MOVE-CORRESPONDING <mamt> TO gs_mamt_sdm.
  gs_mamt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAMT'
                                                                           i_contents = gs_mamt_sdm ).

  APPEND gs_mamt_sdm TO gt_mamt_sdm.
ENDLOOP.

* Assignment of Layout Modules to Materials
*loop at tmalg assigning <malg>.
*  move-corresponding <malg> to gs_???_sdm.
*  append gs_???_sdm to gt_???_sdm.
*endloop.

* Vendor-Specific EANs
LOOP AT tmlea ASSIGNING <mlea>.
  MOVE-CORRESPONDING <mlea> TO gs_mlea_sdm.
  gs_mlea_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                           i_contents = gs_mlea_sdm ).

  APPEND gs_mlea_sdm TO gt_mlea_sdm.
  CLEAR:
   gs_mlea_sdm.
ENDLOOP.


* Set default SDM Type and include any custom SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_object_type_id = gc_object_id
                                                     x_sdm_type       = gc_der
                                                     x_sdm_source     = gc_poe ).

* Build a list of all the relevant SDM objects for Article - Drivations
SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
  WHERE  sdm_object  = gc_object
   AND   type        IN gr_sdm_type
   AND   active      = abap_true.

CHECK gt_objects[] IS NOT INITIAL.

LOOP AT gt_objects ASSIGNING <objects>.
  TRY.
      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                                     iv_source      = gc_poe
                                                     iv_type        = <objects>-type
                                                     iv_stats       = space ).
    CATCH cx_fdt_input INTO gx_fdt.

      IF <objects>-object IS NOT INITIAL.
        <objects>-object->display_messages( ).
        EXIT.
      ENDIF.
  ENDTRY.

  CHECK <objects> IS ASSIGNED.
  CHECK <objects>-object IS BOUND.
  CHECK <objects>-object->is_active( ) = abap_true
    AND <objects>-object->mt_message[] IS INITIAL.

  gt_attributes = <objects>-object->get_object_attributes(  iv_type = <objects>-type  ).

  LOOP AT gt_attributes ASSIGNING <attribute>.
    ASSIGN (<attribute>-abap_type) TO <set_data>.
    <objects>-object->set_selection( iv_name = <attribute>-name
                                     iv_data = <set_data>
                                     iv_type = <attribute>-type  ).
  ENDLOOP.

  TRY.
      <objects>-object->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
        MESSAGE gv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.

  gr_data = <objects>-object->return_brf_result( ).
  ASSIGN gr_data->* TO <results_der>.

  IF <results_der> IS NOT ASSIGNED.
    RETURN.
  ENDIF.
* collect all the results..
  IF <results_der_all> IS NOT ASSIGNED.
    IF <objects>-object IS BOUND.
      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
      ASSIGN gr_data_empty->* TO <results_der_all>.
    ENDIF.
  ENDIF.
  APPEND LINES OF <results_der> TO <results_der_all>.
ENDLOOP.

CHECK <results_der_all> IS ASSIGNED.
LOOP AT <results_der_all> INTO gs_result_der.
  IF gs_result_der-skip_derivation = space.
    CLEAR lv_tabname.
    IF gs_result_der-table = 'MWLI'.
      MOVE 'MAW1' TO lv_tabname.
    ELSE.
      MOVE gs_result_der-table TO lv_tabname.
    ENDIF.

    lv_tabname = |W{ lv_tabname }|.
    ASSIGN (lv_tabname) TO <table>.
    IF sy-subrc <> 0.
      ASSIGN (gs_result_der-table) TO <table>.
      IF sy-subrc <> 0.
        gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
        MESSAGE gv_message TYPE 'W'.
        RETURN.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT gs_result_der-field OF STRUCTURE <table> TO <field>.
    IF sy-subrc = 0.
      <field> = gs_result_der-value.
    ELSE.
      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
      MESSAGE gv_message TYPE 'W'.
    ENDIF.
  ENDIF.

*Fill the screen control internal table
  MOVE-CORRESPONDING gs_result_der TO ls_screen_control.

  CLEAR ls_screen_control-screen_name.
  CONCATENATE gs_result_der-table '-' gs_result_der-field INTO ls_screen_control-screen_name.
  IF ls_screen_control-screen_name IS NOT INITIAL.
    INSERT ls_screen_control INTO TABLE lt_screen_control.
  ENDIF.

** Special Case. " ST-1397
  IF ls_screen_control-screen_name = 'MLGN-LHMG1'.
    ls_screen_control-screen_name = '*MLGN-LHMG1'.
    INSERT ls_screen_control INTO TABLE lt_screen_control.
  ENDIF.
  IF ls_screen_control-screen_name = 'MLGN-LHME1'.
    ls_screen_control-screen_name = '*MLGN-LHME1'.
    INSERT ls_screen_control INTO TABLE lt_screen_control.
  ENDIF.
  IF ls_screen_control-screen_name = 'MLGN-LETY1'.
    ls_screen_control-screen_name = '*MLGN-LETY1'.
    INSERT ls_screen_control INTO TABLE lt_screen_control.
  ENDIF.
ENDLOOP.

IF lt_screen_control IS NOT INITIAL.
  EXPORT screen_control = lt_screen_control TO MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR
ENDIF.
