*&---------------------------------------------------------------------*
*&      Form  SET_VIEW_OUTPUT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COLUMN  text
*      -->P_<STATUS>  text
*----------------------------------------------------------------------*
form set_view_output_tree using x_column x_status.
  data:
    ls_hier_hdr type treev_hhdr,
    ls_variant  type disvariant,
    lt_keys     type lvc_t_nkey.
*    lt_result   TYPE STANDARD TABLE OF /gda/sdm_s_val_results. " Empty

  field-symbols:
    <material>    type any,
    <description> type any.

  perform build_structure    using x_column
                                   gc_object
                                   p_struc.
  perform build_dynamic_itab using x_column
                             changing ro_data.

  refresh:
   <dyn_table_view>.

* Set key fields..
  move-corresponding <dyn_wa> to <dyn_wa_view>.
  assign component 'KEY_MATNR' of structure <dyn_wa> to <material>.

*  CLEAR: GS_MARA.
  read table gt_mara into gs_mara with key matnr = <material>.
  move-corresponding gs_mara to <dyn_wa_view>.

  clear:
   gs_makt.
  if gt_makt is not initial.
    assign component 'MAKTX' of structure <dyn_wa_view> to <description>.
    read table gt_makt into gs_makt with key matnr = <material>.
    if <description> is assigned.
      <description> = gs_makt-maktx.
    endif.
  endif.

* Get BRF+ results for Article..
** Only if in Error..
  if x_status = icon_red_light or x_status = icon_green_light  or x_status = icon_yellow_light.
    if go_alv is bound.
      go_alv->free( ).
      free go_alv.
    endif.

    if go_tree is bound.
      go_tree->free( ).
      free go_tree.
    endif.

    if go_tree is initial.
* create tree control
      create object go_tree
        exporting
          parent                      = go_parent2
          node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
          item_selection              = 'X'
          no_html_header              = 'X'
          no_toolbar                  = ''
        exceptions
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          illegal_node_selection_mode = 5
          failed                      = 6
          illegal_column_name         = 7.
      if sy-subrc <> 0.
        message x208(00) with 'ERROR'.                      "#EC NOTEXT
      endif.

    endif.

    perform build_hierarchy_header changing ls_hier_hdr.

    ls_variant-report = sy-repid.
    ls_variant-variant = '/DEFAULT'.

    call method go_tree->set_table_for_first_display
      exporting
        is_variant          = ls_variant
        i_save              = 'A'
        i_default           = 'X'
        i_structure_name    = '/GDA/SDM_S_VAL_RETURN_GUI' "'/GDA/SDM_S_VAL_RESULTS'
        is_hierarchy_header = ls_hier_hdr
      changing
        it_outtab           = gt_result.

    if x_status = icon_red_light or x_status = icon_yellow_light.
* Create hierachy -
* Folders - BRF Errors All, Context
      perform create_hierarchy using
                                <material>
                                x_column
                                lt_keys.
    endif.
* Send data to frontend.
    call method go_tree->expand_nodes( it_node_key = lt_keys ).
    call method go_tree->frontend_update.
  endif.


endform.

form build_hierarchy_header changing
                               p_hierarchy_header type treev_hhdr.

  p_hierarchy_header-heading = text-010.
  p_hierarchy_header-tooltip = text-011.
  p_hierarchy_header-width = 75.
  p_hierarchy_header-width_pix = ' '.

endform.
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM init .
*  DATA:
*    lt_view_key TYPE STANDARD TABLE OF struc2,
*    ls_view_key TYPE struc2,
*    lt_sequence TYPE STANDARD TABLE OF /gda/sdm_setup5.
**    ls_sequence TYPE /gda/sdm_setup5.
*
*  CONCATENATE icon_select_all   TEXT-005 INTO sel_all  SEPARATED BY space.
*  CONCATENATE icon_deselect_all TEXT-006 INTO dsel_all SEPARATED BY space.
*
*  SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
*    WHERE object_type = gc_article
*      AND status      = abap_true
*      ORDER BY ord.
*
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_pp_main_gen
*    WHERE object_type  = gc_article
*      AND outp         = abap_true.
*
** Set up Default Fields Heading
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_default_fields
*    WHERE object_type  = gc_article
*      AND object_view  = gc_default
*      AND outp         = abap_true
*      AND default_all  = abap_true.
*
*
*  SELECT * FROM /gda/sdm_setup5 INTO TABLE gt_pp_output
*    WHERE object_type  = gc_article .
*
*  LOOP AT gt_pp_main_gen ASSIGNING <main_gen>.
*    APPEND <main_gen>-tabname TO gt_view_tables_all.
*  ENDLOOP.
*
*  SORT gt_view_tables_all.
*
*  DELETE ADJACENT DUPLICATES FROM gt_view_tables_all.
*
** Build secondary key table for Views
*  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*    LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = <main_setup>-object_view
*                                                  AND default_all = abap_true.
*
*      ls_view_key-tabname = <main_gen>-tabname.
*      ls_view_key-field   = <main_gen>-field.
*      APPEND ls_view_key TO lt_view_key.
*      CLEAR:
*        ls_view_key.
*    ENDLOOP.
*    IF lt_view_key[] IS INITIAL.
** get from default
*      LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = 'DEFAULT'
*                                                    AND default_all = abap_true.
*
*        ls_view_key-tabname = <main_gen>-tabname.
*        CONCATENATE 'KEY_' <main_gen>-field INTO ls_view_key-field.
*        APPEND ls_view_key TO lt_view_key.
*        CLEAR:
*          ls_view_key.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT gt_pp_output ASSIGNING <main_output> WHERE object_view = <main_setup>-object_view.
*      APPEND <main_output> TO lt_sequence.
*    ENDLOOP.
*
*    <main_setup>-sequence[]  = lt_sequence[].
*    <main_setup>-view_keys[] = lt_view_key[].
*    REFRESH:
*     lt_view_key[],
*     lt_sequence[].
*  ENDLOOP.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_val.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_pir.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_pri.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_src.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  TRY.
*      GET BADI sdm_handle
*        FILTERS
*          sdm_type_main = gc_val.
*
*    CATCH cx_badi_not_implemented.
*      CLEAR sdm_handle.
*  ENDTRY.
*
*  IF NOT sdm_handle IS INITIAL.
*    CALL BADI sdm_handle->add_sdm_type
*      EXPORTING
*        x_source          = gc_rep
*      CHANGING
*        xt_sdm_type       = gr_sdm_type
*      EXCEPTIONS
*        application_error = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0.
**          MESSAGE e() RAISING application_error.
*    ENDIF.
*  ENDIF.
*
*  SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*    WHERE  sdm_object  = gc_article
*     AND   active      = abap_true
*     AND   type        IN gr_sdm_type.
*
*  gv_source = /gda/sdm_cl_core=>mc_rep.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM screen_output.
*data:
* lv_text(80).
*
*  FIELD-SYMBOLS:
*    <parameter> TYPE any.
*
*  LOOP AT SCREEN.
*    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*      lv_text = '%_&&_%_APP_%-TEXT'.
*      REPLACE ALL OCCURRENCES OF '&&' IN lv_text WITH <main_setup>-object_view_o.
*      IF screen-name = lv_text.
*        ASSIGN (screen-name) TO <name>.
*        <name> = <main_setup>-object_view_d.
*      ENDIF.
*    ENDLOOP.
**    ENDIF.
*  ENDLOOP.
*
*  WRITE icon_red_light     AS ICON TO icon1.
*  WRITE icon_yellow_light  AS ICON TO icon2.
*  WRITE icon_green_light   AS ICON TO icon3.
*  WRITE icon_green_light   AS ICON TO icon6.
*  WRITE icon_complete      AS ICON TO icon4.
*  WRITE icon_message_error AS ICON TO icon5.
*
*  CASE ok_code.
*    WHEN 'COMP'.
*      IF p_com = abap_true. " OR p_fai = abap_true.
*        p_amb = abap_false.
*        p_suc = abap_false.
*        p_red = abap_false.
*        p_fai = abap_false.
*        p_gre = abap_false.
*      ENDIF.
*
*      IF p_com = abap_true.
** set to true..
*        LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*          ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*          <parameter> = abap_true.
*        ENDLOOP.
*
*        LOOP AT SCREEN.
*          IF screen-name  = 'SEL_ALL' OR screen-name  = 'DSEL_ALL'." or  screen-name CS 'P_'.
*            screen-input = 0.
*            MODIFY SCREEN.
*          ELSE.
*            READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view_o = screen-name.
*            IF sy-subrc = 0.
*              screen-input = 0.
*              MODIFY SCREEN.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    WHEN 'FAIL'.
*      IF p_fai = abap_true.
*        p_amb = abap_false.
*        p_suc = abap_false.
*        p_red = abap_false.
*        p_com = abap_false.
*        p_gre = abap_false.
*      ENDIF.
*
*    WHEN 'SUCCESS'.
*      IF p_suc = abap_true.
*        p_amb = abap_false.
*        p_red = abap_false.
*        p_gre = abap_false.
*        p_com = abap_false.
*      ENDIF.
*
*    WHEN 'IND'.
*      IF p_amb = abap_true OR p_red = abap_true OR p_gre = abap_true.
*        p_com = abap_false.
*        p_fai = abap_false.
*        p_suc = abap_false.
*      ENDIF.
*
*    WHEN OTHERS.
*
*
*  ENDCASE.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
*FORM build_structure USING x_view TYPE lvc_s_col.
*  DATA:
*    lo_ref_table_des TYPE REF TO cl_abap_structdescr,
*    lt_view_fields   TYPE STANDARD TABLE OF /gda/sdm_setup4,
*    lt_details       TYPE abap_compdescr_tab,
*    lv_lines         TYPE i,
*    ls_field         TYPE field.
*
*  FIELD-SYMBOLS:
*    <general>        LIKE LINE OF lt_view_fields,
*    <default_fields> LIKE LINE OF lt_view_fields,
*    <details>        LIKE LINE OF lt_details,
*    <view_struc>     LIKE LINE OF gt_view_struc,
*    <parameter>      TYPE any.
*
*  REFRESH:
*   gt_view_tables.
*
** Retrieve fields for specific view
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE lt_view_fields
*    WHERE object_type  = gc_article
*      AND object_view  = x_view
*      AND outp         = abap_true.
*
** Ensure that default field are displayed in all views.
*  LOOP AT gt_default_fields ASSIGNING <default_fields>.
*    READ TABLE lt_view_fields ASSIGNING <general>
*    WITH KEY field   = <default_fields>-field
*             tabname = <default_fields>-tabname.
*    IF sy-subrc <> 0.
*      CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
*      APPEND <default_fields> TO lt_view_fields.
*    ELSE.
*      IF x_view <> gc_default.
*        CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
*        <default_fields>-object_view = x_view.
*        APPEND <default_fields> TO lt_view_fields.
*      ELSE.
*        CONCATENATE 'KEY_' <general>-field  INTO <general>-field.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  SORT lt_view_fields BY default_all DESCENDING ord.
** Build a table of related tables for the view
*  LOOP AT lt_view_fields ASSIGNING <general>.
*    gs_table = <general>-tabname.
*    APPEND gs_table TO gt_view_tables.
*  ENDLOOP.
*
*  SORT gt_view_tables.
*  DELETE ADJACENT DUPLICATES FROM gt_view_tables.
*
*  LOOP AT gt_view_tables INTO gs_table.
*    lo_ref_table_des ?=
*        cl_abap_typedescr=>describe_by_name( gs_table ).
*
*    lt_details[] = lo_ref_table_des->components[].
*
*    LOOP AT lt_view_fields ASSIGNING <general> WHERE tabname = gs_table.
*      IF <general>-field CS 'KEY_'.
*        ls_field = <general>-field+4(6).
*        READ TABLE lt_details ASSIGNING <details> WITH KEY name = ls_field.
*      ELSE.
*        READ TABLE lt_details ASSIGNING <details> WITH KEY name = <general>-field.
*      ENDIF.
*      CHECK sy-subrc = 0.
*      CLEAR: gs_view_struc.
*      gs_view_struc-fieldname = <general>-field.
*      gs_view_struc-tabname   = <general>-tabname.
*      gs_view_struc-datatype  = <details>-type_kind.
*      CASE <details>-type_kind.
*        WHEN 'C'.
*          gs_view_struc-datatype = 'CHAR'.
*        WHEN 'N'.
*          gs_view_struc-datatype = 'NUMC'.
*        WHEN 'D'.
*          gs_view_struc-datatype = 'DATE'.
*        WHEN 'P'.
*          gs_view_struc-datatype = 'PACK'.
*        WHEN OTHERS.
*          gs_view_struc-datatype = <details>-type_kind.
*      ENDCASE.
*
*      IF <general>-field CS 'KEY_'.
*        gs_view_struc-key      = abap_true.
*      ENDIF.
*
*      gs_view_struc-inttype  = <details>-type_kind.
*      gs_view_struc-intlen   = <details>-length.
*      gs_view_struc-decimals = <details>-decimals.
*      gs_view_struc-col_pos  = <general>-ord.
*
*      SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
*             INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
*            WHERE tabname    = <general>-tabname
*              AND fieldname  = <general>-field
*              AND ddlanguage = sy-langu.
*
*      IF sy-subrc <> 0.
*        SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
*               INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
*              WHERE tabname    = <general>-tabname
*                AND fieldname  = <general>-field+4
*                AND ddlanguage = sy-langu.
*      ENDIF.
*
*      APPEND gs_view_struc TO gt_view_struc.
*    ENDLOOP.
*  ENDLOOP.
*
*  CLEAR:
*    gs_view_struc,
*    lv_lines.
*
*  SORT gt_view_struc BY key DESCENDING col_pos.
*
*  LOOP AT gt_view_struc ASSIGNING <view_struc>.
*    lv_lines = lv_lines + 1.
*    <view_struc>-col_pos =  lv_lines.
*  ENDLOOP.
*
*  DESCRIBE TABLE gt_view_struc LINES lv_lines.
*
*  IF x_view  = gc_default.
** Add an additional hidden linkage field to the end for DEFAULT View. But only if sructured articles is selected
*    lv_lines = lv_lines + 1.
*    gs_view_struc-fieldname  = 'LINKAGE'.
*    IF p_struc = abap_false.
*      gs_view_struc-no_out = abap_true.
*    ENDIF.
*    gs_view_struc-datatype   = 'CHAR'.
*    gs_view_struc-inttype    = 'C'.
*    gs_view_struc-intlen     = 000018.
*    gs_view_struc-outputlen  = 000018.
*    gs_view_struc-col_pos    = 1.
*    gs_view_struc-scrtext_l  = TEXT-012." 'Structured Article'.
*    gs_view_struc-scrtext_m  = TEXT-012." 'Structured Article'.
*    gs_view_struc-scrtext_s  = TEXT-012." 'Structured Article'.
*    APPEND gs_view_struc TO gt_view_struc.
*    CLEAR:
*     gs_view_struc.
*
*
*    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*      IF <main_setup>-object_view NE gc_default.
*        lv_lines = lv_lines + 1.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        IF <parameter> = abap_true.
*          gs_view_struc-fieldname  = <main_setup>-object_view.
*          gs_view_struc-datatype   = 'CHAR'.
*          gs_view_struc-inttype    = 'C'.
*          gs_view_struc-intlen     = 000004.
*          gs_view_struc-outputlen  = 000004.
*          gs_view_struc-icon       = abap_true.
*          gs_view_struc-col_pos    = lv_lines.
*          gs_view_struc-hotspot    =  abap_true.
*          APPEND gs_view_struc TO gt_view_struc.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ELSE.
** BRF Plus Fields..
*    lv_lines = lv_lines + 1.
*    gs_view_struc-fieldname  = 'MESSAGE'.
*    gs_view_struc-datatype   = 'CHAR'.
*    gs_view_struc-inttype    = 'C'.
*    gs_view_struc-intlen     = 000220.
*    gs_view_struc-outputlen  = 000100.
*    gs_view_struc-col_pos    = lv_lines.
*    APPEND gs_view_struc TO gt_view_struc.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_DYNAMIC_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_DEFAULT  text
*----------------------------------------------------------------------*
*FORM build_dynamic_itab USING x_view TYPE lvc_s_col.
*
*  DATA:
*    ro_table TYPE REF TO data,
*    ro_line  TYPE REF TO data.
*
*  FIELD-SYMBOLS:
*     <main_setup> LIKE LINE OF gt_pp_main_setup.
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = x_view.
*  IF sy-subrc <> 0 OR <main_setup>-ro_table IS INITIAL.
** Create dynamic internal table and assign to FS
*    CALL METHOD cl_alv_table_create=>create_dynamic_table
*      EXPORTING
*        it_fieldcatalog  = gt_view_struc
*        i_length_in_byte = 'X'
*      IMPORTING
*        ep_table         = ro_table.
*
*    IF x_view = gc_default.
*      ASSIGN ro_table->* TO <dyn_table>.
**      ASSIGN ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
**       CREATE DATA ro_line LIKE LINE OF <dyn_table_final>.
*      ASSIGN ro_line->* TO <dyn_wa>.
*    ELSE.
*      ASSIGN ro_table->* TO <dyn_table_view>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
*      ASSIGN ro_line->* TO <dyn_wa_view>.
*    ENDIF.
*
*    <main_setup>-tabname    = x_view.
*    <main_setup>-ro_table   = ro_table.
*    <main_setup>-tabstruc[] = gt_view_struc[].
*  ELSE.
*    IF x_view = gc_default.
*      ASSIGN <main_setup>-ro_table->* TO <dyn_table>.
**      ASSIGN <main_setup>-ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
*      ASSIGN ro_line->* TO <dyn_wa>.
*    ELSE.
*      ASSIGN <main_setup>-ro_table->* TO <dyn_table_view>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
*      ASSIGN ro_line->* TO <dyn_wa_view>.
*    ENDIF.
*  ENDIF.
*
*  REFRESH:
*    gt_view_struc[].
*ENDFORM.                    " BUILD_DYNAMIC_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .

  data:
    ls_pricing     type /gda/sdm_st_pricing,
    ls_cond_header type cond_header,
    ls_mwli        type /gda/sdm_s_mwli_01.

  field-symbols:
    <mara>   like line of gt_mara,
    <steuer> like line of  gt_steuer.

  if s_matnr is initial and
     s_ersda is initial and
     s_ernam is initial and
     s_laeda is initial and
     s_aenam is initial and
     s_mtart is initial and
     s_matkl is initial and
     s_mstae is initial.

    perform limit_max_entries changing p_max gv_execute_report.
  else.
    gv_execute_report = abap_true.
  endif.
  if gv_execute_report = abap_false.
    return.
  endif.

*Get the data from the table Mara and makt (for the description of mataerial)
  refresh:
    gt_mara.

  perform progress_bar using text-016.

  perform determine_selection.

  if p_struc = abap_false.
    perform material.
  else.
    perform material_structured.
  endif.

  refresh:
   gt_makt,gt_marc,gt_mard,gt_maw1,gt_mean,gt_eina,gt_eine,gt_eord,gt_mbew,gt_mvke,gt_wlk1,
   gt_wlk2,gt_stpo,gt_stko,gt_eqst,gt_mast,gt_mg03,gt_steuer,gt_mlgn,gt_mlgt,gt_myms,
   gt_mwli,gt_marm,gt_mamt,gt_malg,gt_mapr,gt_prop,gt_mlea.

  if gt_mara[] is not initial.
    select * from makt
             into corresponding fields of table gt_makt
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr
             and spras        =  'E'.
  endif.

  if gt_mara[] is not initial.
    select * from marc
             into corresponding fields of table gt_marc
             for all entries in gt_mara
                  where matnr      =  gt_mara-matnr
                    and werks      in s_werks
                    and mmsta      in s_mmsta.

    select * from mard into corresponding fields of table gt_mard
             for all entries in gt_mara
             where matnr     =  gt_mara-matnr
               and werks      in s_werks
               and lgort      in s_lgort.

    select * from mlgn into corresponding fields of table gt_mlgn
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from mlgt into corresponding fields of table gt_mlgt
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from maw1 into corresponding fields of table gt_maw1
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from mean into corresponding fields of table gt_mean
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from myms into corresponding fields of table gt_myms
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from marm into corresponding fields of table gt_marm
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from mamt into corresponding fields of table gt_mamt
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from malg into corresponding fields of table gt_malg
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

* Forecast Parameters
    select * from mapr into table gt_mapr
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    if gt_mapr is not initial.
      select * from prop into table gt_prop
               for all entries in gt_mapr
               where pnum1      =  gt_mapr-pnum1.
    endif.

* Vendor-Specific EANs
    select * from mlea into table gt_mlea
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

* Planning Data
    select * from mpgd_v into table gt_mpgd_v
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

* LIFO-relevant materials
    select * from myms into table gt_myms
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.


*    SELECT * FROM MWLI INTO CORRESPONDING FIELDS OF TABLE gt_mwli
*             FOR ALL ENTRIES IN gt_mara
*             WHERE matnr      =  gt_mara-matnr.

    if gt_eina is initial.
      select * from eina into corresponding fields of table gt_eina
               for all entries in gt_mara
               where matnr      =  gt_mara-matnr
                 and matkl      in s_matkl
                 and lifnr      in s_lifnr
                 and infnr      in s_infnr.

      select * from eine into table gt_eine
               for all entries in gt_eina
               where infnr = gt_eina-infnr
               and ekorg in s_ekorg
               and loekz = space.
    endif.

    if gt_eord is initial.
      select * from eord into corresponding fields of table gt_eord
               for all entries in gt_mara
               where matnr      =  gt_mara-matnr
                 and werks      in s_werks
                 and lifnr      in s_lifnr
                 and vdatu      in s_vdatu
                 and bdatu      in s_bdatu.
    endif.

    select * from mvke into corresponding fields of table gt_mvke
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr
               and vkorg      in s_vkorg
               and vtweg      in s_vtweg.

    select * from mbew into corresponding fields of table gt_mbew
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr.

    select * from wlk1 into corresponding fields of table gt_wlk1
             for all entries in gt_mara
             where artnr      =  gt_mara-matnr.

    select * from wlk2 into corresponding fields of table gt_wlk2
             for all entries in gt_mara
             where matnr      =  gt_mara-matnr
               and vkorg      in s_vkorg
               and vtweg      in s_vtweg.

    select * from stpo into corresponding fields of table gt_stpo
             for all entries in gt_mara
             where idnrk      =  gt_mara-matnr.


  endif.

  if gt_stpo is not initial.
*    SELECT * FROM tpst INTO CORRESPONDING FIELDS OF TABLE gt_tpst
*             FOR ALL ENTRIES IN gt_stpo
*             WHERE werks      IN s_werks
*             AND stlnr      =  gt_stpo-stlnr.

    select * from stko into corresponding fields of table gt_stko
             for all entries in gt_stpo
             where stlnr      =  gt_stpo-stlnr.

    select * from eqst into corresponding fields of table gt_eqst
             for all entries in gt_stpo
             where stlnr    =  gt_stpo-stlnr
             and werks      in s_werks.

*    select * from mast into corresponding fields of table gt_mast
*             for all entries in gt_stpo
*             where matnr    =  gt_stpo-idnrk
*             and werks      in s_werks.

    select * from mast into corresponding fields of table gt_mast
             for all entries in gt_stpo
             where werks    in s_werks
             and stlnr      =  gt_stpo-stlnr.

  endif.

*  IF gt_tpst IS NOT INITIAL.
*    SELECT * FROM iflo INTO CORRESPONDING FIELDS OF TABLE gt_iflo
*                       FOR ALL ENTRIES IN gt_tpst
*                       WHERE tplnr      =  gt_tpst-tplnr.
*  ENDIF.


  loop at gt_mara assigning <mara_struc>.
    call function 'STEUERTAB_READ'
      exporting
*       KZRFB           = ' '
        matnr           = <mara_struc>-matnr
      tables
        steuertab       = gt_steuer
      exceptions
        wrong_call      = 1
        steuertab_empty = 2
        others          = 3.
    if sy-subrc = 0.
      loop at gt_steuer assigning <steuer>.
        gs_mg03-matnr      = <mara_struc>-matnr.
        gs_mg03-mg03steuer = <steuer>.
        append gs_mg03 to gt_mg03.
        clear:
          gs_mg03.
      endloop.
    endif.

* KONH Records..
    call function '/GDA/SDM_PP_BRF_PRICING1'
      exporting
        x_matnr  = <mara_struc>-matnr
      importing
        y_result = ls_pricing.

    ls_pricing-matnr = <mara_struc>-matnr.
    append ls_pricing to gt_pricing.

    ls_cond_header-matnr = <mara_struc>-matnr.
    ls_cond_header-knumh = ls_pricing-knumh.
    insert ls_cond_header into table gt_cond_header.
*    APPEND ls_cond_header TO gt_cond_header.

    clear:
     ls_cond_header,
     ls_pricing.

** Source List Logic
*    CALL FUNCTION '/GDA/SDM_PP_BRF_SRC_LIST2'
*      EXPORTING
*        x_matnr  = <mara_struc>-matnr
*      IMPORTING
*        y_result = ls_src_lisr.

*    ls_cond_header-matnr = <mara_struc>-matnr.
*    ls_cond_header-knumh = ls_pricing-knumh.
*    INSERT ls_cond_header INTO TABLE gt_cond_header.

  endloop.

  if gt_cond_header is not initial.
*    SORT gt_cond_header ASCENDING.
    select * from konh into table gt_konh
             for all entries in gt_cond_header
             where knumh    =  gt_cond_header-knumh.
  endif.

* Listing
  loop at gt_maw1 assigning field-symbol(<maw1>).
* Build MWLI from MAW1
    move-corresponding <maw1> to ls_mwli.
    append ls_mwli to gt_mwli.
  endloop.

enhancement-point /gda/sdm_mm_art_ep4 spots /gda/sdm_mm_art_es4 .

  perform progress_bar using text-017.

endform.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUPUT_FIELDS  text
*      <--P_LT_FIELDS  text
*----------------------------------------------------------------------*
form get_fields using internal_table type /gda/sdm_t_setup4
                changing fields type table.

  field-symbols:
   <line> type /gda/sdm_setup4.

  loop at internal_table assigning <line>.
    gv_field =  <line>-field.
    append gv_field to fields.
    clear gv_field.
  endloop.

  sort fields.
  delete adjacent duplicates from fields.

endform.
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_alv_data_new .
  data:
    ro_data       type ref to data,
    ro_data_empty type ref to data,
    lt_general    type standard table of /gda/sdm_setup4,
    lv_count      type p,
    lv_pur        type p,
    lv_text       type string,
    lv_pur_text   type string.
data gt_default_fields        type standard table of /gda/sdm_setup4.
  field-symbols:
    <icon>            type any,
*    <results>         type standard table,
    <results_temp>    type standard table,
    <results_pp1>     type standard table,
    <result>          type any,
    <field>           type any,
    <type>            type any,
    <number>          type any,
    <brf_key4>        type any,
    <brf_key5>        type any,
    <brf_key6>        type any,
    <field_alv>       type any,
    <general>         like line of lt_general,
*    <general_default> like line of gt_default_fields,
*    <general_default> like line of gt_alvtop_key_fields,
    <view_table>      like line of gt_view_tables,
    <maw1>            like line of gt_maw1,
    <mean>            like line of gt_mean,
    <eine>            like line of gt_eine,
    <eina>            like line of gt_eina,
    <sdm_articles>    like line of gt_sdm_articles,
    <instances>       like line of <sdm_articles>-sdm_instances,
    <objects>         like line of gt_objects,
    <maktx>           type any.

  perform progress_bar using text-018.

  describe table gt_mara lines lv_count.
  perform progress_bar using text-018.


  loop at gt_mara into gs_mara.
    clear:
      lv_pur_text,
      lv_pur,
      lv_text.

    refresh:
     gt_mara_sdm,gt_marc_sdm,gt_mard_sdm,gt_mbew_sdm,gt_mlgn_sdm,
     gt_mlgt_sdm,gt_mvke_sdm,gt_myms_sdm,gt_maw1_sdm,gt_mean_sdm,
     gt_mwli_sdm,gt_meinh_sdm,gt_mamt_sdm,gt_malg_sdm,gt_ktex,
     gt_basic_text,gt_eine_sdm,gt_eina_sdm,gt_makt_sdm,gt_rmmw1_sdm,
     gt_konh_sdm,gt_wlk2_sdm,gt_wlk1_sdm,gt_mast_sdm,gt_mg03_sdm,
     gt_eord_sdm,gt_mg03_sdm_brf,gt_tariff_sdm, gt_src_list_sdm,gt_pricing_sdm,
     gt_mpop_sdm,gt_mpgd_sdm.


    lv_pur = ( sy-tabix / lv_count ) * 100.
    lv_pur_text = lv_pur.
    concatenate 'BRF Rules '(917) ':'  lv_pur_text '%' into lv_text separated by space.
    perform progress_bar using lv_text.

* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
    perform prep_data using gs_mara.

* For each Article process the BRF Functions
    loop at gt_objects assigning <objects>.
      clear:
       <objects>-object.
      perform brf_logic1  using <objects>-type
                               <objects>-mapping
                               <objects>-stewardship
                         changing <objects>-object.

      if <objects>-object is not bound or  <objects>-object->mt_message is not initial.
        gv_config_err = abap_true.
        exit.
      endif.

      if <results> is not assigned.
        if <objects>-object is bound.
          ro_data_empty  = <objects>-object->return_brf_result_structure( ).
          assign ro_data_empty->* to <results>.
          refresh:
           <results>.
        endif.
      endif.

      if <objects>-object is bound.
        ro_data  = <objects>-object->return_brf_result( ).
        assign ro_data->* to <results_pp1>.
        if sy-subrc = 0 and <results_pp1> is not initial.
          append lines of <results_pp1> to <results>.
        endif.
      endif.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      append gs_instance to gs_sdm_objects-sdm_instances.
    endloop.

    check gv_config_err = abap_false.

    gs_sdm_objects-article = gs_mara-matnr.
    gs_sdm_objects-mara[]  = gt_mara_sdm[].
    gs_sdm_objects-makt[]  = gt_makt_sdm[].
*ENHANCEMENT-POINT /gda/sdm_mm_art_ep6 SPOTS /gda/sdm_mm_art_es6 .

    append gs_sdm_objects to gt_sdm_articles.
    clear gs_sdm_objects.

    unassign:
*     <results>,
     <results_pp1>.
  endloop.

  sort gt_sdm_articles.

*    sort <results>.
*    delete adjacent duplicates from <results>.

*    REFRESH lt_icons.
*    LOOP AT <results> ASSIGNING <result>.
*      ASSIGN COMPONENT 'TYPE'     OF STRUCTURE <result> TO <type>.
*      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
*      IF <field> IS ASSIGNED.
*        SPLIT <field> AT '-' INTO lv_table lv_field.
*        ls_icons-field = lv_field.
*      ENDIF.
** Get key object...
*      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE <result> TO <brf_key4>.
*      IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
*        ls_icons-brf_key = <brf_key4>.
*      ENDIF.
*
*      IF ls_icons-brf_key IS INITIAL.
*        ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key5>.
*        IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
*          ls_icons-brf_key = <brf_key5>.
*        ENDIF.
*      ENDIF.
*
*      ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
*      IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
*        CLEAR: ls_icons-brf_key.
*        CONCATENATE <brf_key5> '/' <brf_key6> INTO ls_icons-brf_key.
*      ENDIF.
*
*      IF ls_icons-brf_key IS INITIAL.
*        ls_icons-brf_key = <sdm_articles>-article.
*      ENDIF.
*
*      CASE <type>.
*        WHEN 'E'.
*          ls_icons-icon = icon_red_light.
*        WHEN 'W'.
*          ls_icons-icon = icon_yellow_light.
*        WHEN OTHERS.
*      ENDCASE.
*
*      APPEND ls_icons TO lt_icons.
*      CLEAR ls_icons.
*    ENDLOOP.
*    <sdm_articles>-icons[] = lt_icons[].
*    REFRESH:
*     lt_icons[].
*  endloop.

  perform progress_bar using text-019.

*  loop at gt_sdm_articles assigning <sdm_objects>.
*    assign component 'SDM_INSTANCES' of structure <sdm_objects> to <sdm_instances>.
*    loop at <sdm_instances> assigning <sdm_instance>.
*      if <sdm_instance>-object is initial.
*        continue.
*      endif.
*      if <results_collated> is not assigned.
*        lo_data_empty  = <sdm_instance>-object->return_brf_result_structure( ).
*        assign lo_data_empty->* to <results_collated>.
*        refresh:
*         <results_collated>.
*      endif.
*
*      ro_data        = <sdm_instance>-object->return_brf_result( ).
*      assign ro_data->* to <results_temp>.
*
*      if <results_temp> is assigned and <results_temp> is not initial.
*        append lines of <results_temp> to <results>.
*      endif.
*    endloop.
*  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  PREP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form prep_data using x_mara type /gda/sdm_s_mara_01.
  data:
    ls_makt_sdm like line of gt_makt_sdm,
    ls_marc_sdm like line of gt_marc_sdm,
    ls_mard_sdm like line of gt_mard_sdm,
    ls_mvke_sdm like line of gt_mvke_sdm,
    ls_mlgn_sdm  like line of gt_mlgn_sdm,
    ls_mlgt_sdm like line of  gt_mlgt_sdm,
    ls_myms_sdm like line of  gt_myms_sdm,
    ls_mwli_sdm like line of  gt_mwli_sdm,
    ls_mbew_sdm like line of gt_mbew_sdm,
    ls_meinh_sdm like line of gt_meinh_sdm, " /gda/sdm_s_meinh,
    ls_mamt_sdm like line of gt_mamt_sdm,
    ls_marm_sdm like line of gt_marm_sdm,
    ls_malg_sdm like line of gt_malg_sdm,
    ls_maw1_sdm like line of gt_maw1_sdm,
    ls_mean_sdm like line of gt_mean_sdm,
    ls_eord_sdm like line of gt_eord_sdm,
    ls_wlk2_sdm like line of gt_wlk2_sdm,
    ls_wlk1_sdm like line of gt_wlk1_sdm,
    ls_mast_sdm like line of gt_mast_sdm,
    ls_eina_sdm like line of gt_eina_sdm,
    ls_eine_sdm like line of gt_eine_sdm,
    ls_konh_sdm like line of gt_konh_sdm,
    ls_src_list like line of gt_src_list_sdm, "type /gda/sdm_st_srclist,
    ls_rmmw1_sdm type /gda/sdm_s_rmmw1,
    ls_mpop_sdm like line of gt_mpop_sdm,
    ls_mlea_sdm like line of gt_mlea_sdm,
    ls_mpgd_sdm like line of gt_mpgd_sdm.

  field-symbols:
    <makt>         like line of gt_makt,
    <ttext>        like line of gt_ktex,
    <marc>         like line of gt_marc,
    <mard>         like line of gt_mard,
    <mvke>         like line of gt_mvke,
    <mbew>         like line of gt_mbew,
    <mlgn>         like line of gt_mlgn,
    <mlgt>         like line of gt_mlgt,
    <mamt>         like line of gt_mamt,
    <malg>         like line of gt_malg,
    <marm>         like line of gt_marm,
    <maw1>         like line of gt_maw1,
    <eord>         like line of gt_eord,
    <eord_sdm>     like line of gt_eord_sdm,
    <mean>         like line of gt_mean,
    <eina>         like line of gt_eina,
    <eina_sdm>     like line of gt_eina_sdm,
    <eine>         like line of gt_eine,
    <eine_sdm>     like line of gt_eine_sdm,
    <marc_sdm>     like line of gt_marc_sdm,
    <mvke_sdm>     like line of gt_mvke_sdm,
    <konh>         like line of gt_konh,
    <cond_header>  like line of gt_cond_header,
    <pricing>      like line of gt_pricing,
    <wlk2>         like line of gt_wlk2,
    <wlk1>         like line of gt_wlk1,
    <mast>         like line of gt_mast,
    <myms>         like line of gt_myms,
    <mwli>         like line of gt_mwli,
    <mg03>         like line of gt_mg03,
    <mapr>         like line of gt_mapr,
    <prop>         like line of gt_prop,
    <mlea>         like line of gt_mlea,
    <mpgd>         like line of gt_mpgd_v,
    <src_list_sdm> like line of gt_src_list_sdm.

  move-corresponding x_mara to ls_rmmw1_sdm.
  append ls_rmmw1_sdm to gt_rmmw1_sdm.

* MAKT
  loop at gt_makt assigning <makt> where matnr = x_mara-matnr.
    move-corresponding <makt> to ls_makt_sdm.

    perform build_string_from_key using 'MAKT'
                                        ls_makt_sdm
                                  changing ls_makt_sdm-sdm_tabkey.


    append ls_makt_sdm to gt_makt_sdm.
  endloop.

  read table gt_makt
    into gs_makt_sdm
    with key matnr = x_mara-matnr
             spras = sy-langu.

  if sy-subrc = 0.
    append initial line to gt_ktex assigning <ttext>.
    <ttext>-maktx = gs_makt_sdm-maktx.
    <ttext>-spras = sy-langu.
  endif.

  append x_mara to gt_mara_sdm.
* MARC
  loop at gt_marc assigning <marc> where matnr = x_mara-matnr.
    move-corresponding <marc> to ls_marc_sdm.

    perform build_string_from_key using 'MARC'
                                        ls_marc_sdm
                                  changing ls_marc_sdm-sdm_tabkey.

    append ls_marc_sdm to gt_marc_sdm.
    clear ls_marc_sdm-sdm_tabkey.
  endloop.

* MARD
  loop at gt_mard assigning <mard> where matnr = x_mara-matnr.
    move-corresponding <mard> to ls_mard_sdm.
    perform build_string_from_key using 'MARD'
                                        ls_mard_sdm
                                  changing ls_mard_sdm-sdm_tabkey.

    append ls_mard_sdm to gt_mard_sdm.
    clear ls_mard_sdm-sdm_tabkey.
  endloop.

* MVKE
  loop at gt_mvke assigning <mvke> where matnr = x_mara-matnr.
    move-corresponding <mvke> to ls_mvke_sdm.

    perform build_string_from_key using 'MVKE'
                                        ls_mvke_sdm
                                  changing ls_mvke_sdm-sdm_tabkey.

    insert ls_mvke_sdm into table gt_mvke_sdm.
    clear:
     ls_mvke_sdm-sdm_tabkey.
  endloop.

* MBEW
  loop at gt_mbew assigning <mbew> where matnr = x_mara-matnr.
    move-corresponding <mbew> to ls_mbew_sdm.
    perform build_string_from_key using 'MBEW'
                                        ls_mbew_sdm
                                  changing ls_mbew_sdm-sdm_tabkey.

    append ls_mbew_sdm to gt_mbew_sdm.
  endloop.

* MLGN
  loop at gt_mlgn assigning <mlgn> where matnr = x_mara-matnr.
    move-corresponding <mlgn> to ls_mlgn_sdm.
    perform build_string_from_key using 'MLGN'
                                        ls_mlgn_sdm
                                  changing ls_mlgn_sdm-sdm_tabkey.

    append ls_mlgn_sdm to gt_mlgn_sdm.

    clear:
     ls_mlgn_sdm-sdm_tabkey.

  endloop.

* MLGT
  loop at gt_mlgt assigning <mlgt> where matnr = x_mara-matnr.
    move-corresponding <mlgt> to ls_mlgt_sdm.
    perform build_string_from_key using 'MLGT'
                                        ls_mlgt_sdm
                                  changing ls_mlgt_sdm-sdm_tabkey.

    append ls_mlgt_sdm to gt_mlgt_sdm.

    clear:
     ls_mlgt_sdm-sdm_tabkey.
  endloop.

* MYMS
  loop at gt_myms assigning <myms> where matnr = x_mara-matnr.
    move-corresponding <myms> to ls_myms_sdm.
    perform build_string_from_key using 'MYMS'
                                        ls_myms_sdm
                                  changing ls_myms_sdm-sdm_tabkey.

    append ls_myms_sdm to gt_myms_sdm.

    clear:
     ls_myms_sdm-sdm_tabkey.
  endloop.

* MWLI
  loop at gt_mwli assigning <mwli> where matnr = x_mara-matnr.
    move-corresponding <mwli> to ls_mwli_sdm.
    perform build_string_from_key using 'MARA'
                                        x_mara
                                  changing ls_mwli_sdm-sdm_tabkey.

    append ls_mwli_sdm to gt_mwli_sdm.

    clear:
     ls_mwli_sdm-sdm_tabkey.
  endloop.

* MARM
  loop at gt_marm assigning <marm> where matnr = x_mara-matnr.
    move-corresponding <marm> to ls_meinh_sdm.
    ls_meinh_sdm-ntgew = x_mara-ntgew.

    perform build_string_from_key using 'MARM'
                                        <marm>
                                  changing ls_meinh_sdm-sdm_tabkey.

    append ls_meinh_sdm to gt_meinh_sdm.

    clear:
     ls_meinh_sdm-sdm_tabkey.
  endloop.

* MAMT
  loop at gt_mamt assigning <mamt> where matnr = x_mara-matnr.
    move-corresponding <mamt> to ls_mamt_sdm.

    perform build_string_from_key using 'MAMT'
                                        ls_mamt_sdm
                                  changing ls_mamt_sdm-sdm_tabkey.

    append ls_mamt_sdm to gt_mamt_sdm.

    clear:
     ls_mamt_sdm-sdm_tabkey.
  endloop.

* MARM
  loop at gt_marm assigning <marm> where matnr = x_mara-matnr.
    move-corresponding <marm> to ls_marm_sdm.

    perform build_string_from_key using 'MARM'
                                        ls_marm_sdm
                                  changing ls_marm_sdm-sdm_tabkey.

    append ls_marm_sdm to gt_marm_sdm.

    clear:
     ls_marm_sdm-sdm_tabkey.
  endloop.


* MALG
  loop at gt_malg assigning <malg> where matnr = x_mara-matnr.
    move-corresponding <malg> to ls_malg_sdm.

    perform build_string_from_key using 'MALG'
                                        ls_malg_sdm
                                  changing ls_malg_sdm-sdm_tabkey.

    append ls_malg_sdm to gt_malg_sdm.

    clear:
     ls_malg_sdm-sdm_tabkey.
  endloop.

* MEIN - POOL Table
  loop at gt_mean assigning <mean> where matnr = x_mara-matnr.
    move-corresponding <mean> to ls_mean_sdm.

    perform build_string_from_key using 'MEAN'
                                        ls_mean_sdm
                                  changing ls_mean_sdm-sdm_tabkey.

    append ls_mean_sdm to gt_mean_sdm.

    clear:
     ls_mean_sdm-sdm_tabkey.

  endloop.

* MAW1
  loop at gt_maw1 assigning <maw1> where matnr = x_mara-matnr.
    move-corresponding <maw1> to ls_maw1_sdm.

    perform build_string_from_key using 'MAW1'
                                        ls_maw1_sdm
                                  changing ls_maw1_sdm-sdm_tabkey.

    append ls_maw1_sdm to gt_maw1_sdm.

    clear:
     ls_maw1_sdm-sdm_tabkey.

  endloop.

* EORD
  loop at gt_eord assigning <eord> where matnr = x_mara-matnr.
    move-corresponding <eord> to ls_eord_sdm.

    perform build_string_from_key using 'EORD'
                                        ls_eord_sdm
                                  changing ls_eord_sdm-sdm_tabkey.

    append ls_eord_sdm to gt_eord_sdm.

    clear:
     ls_eord_sdm-sdm_tabkey.

  endloop.


* WLK2
  loop at gt_wlk2 assigning <wlk2> where matnr = x_mara-matnr.
    move-corresponding <wlk2> to ls_wlk2_sdm.

    perform build_string_from_key using 'WLK2'
                                        ls_wlk2_sdm
                                  changing ls_wlk2_sdm-sdm_tabkey.

    append ls_wlk2_sdm to gt_wlk2_sdm.
    clear:
     ls_wlk2_sdm-sdm_tabkey.

*    append <wlk2> to gt_wlk2_sdm.
  endloop.

* WLK1
  loop at gt_wlk1 assigning <wlk1> where artnr = x_mara-matnr.

    move-corresponding <wlk1> to ls_wlk1_sdm.

    perform build_string_from_key using 'WLK1'
                                        ls_wlk1_sdm
                                  changing ls_wlk1_sdm-sdm_tabkey.

    append ls_wlk1_sdm to gt_wlk1_sdm.
    clear:
     ls_wlk1_sdm-sdm_tabkey.
  endloop.

* MAST
  loop at gt_mast assigning <mast> where matnr = x_mara-matnr.
    move-corresponding <mast> to ls_mast_sdm.

    perform build_string_from_key using 'MAST'
                                        ls_mast_sdm
                                  changing ls_mast_sdm-sdm_tabkey.

    append ls_mast_sdm to gt_mast_sdm.
    clear:
     ls_mast_sdm-sdm_tabkey.
  endloop.

* TAX
  loop at gt_mg03 assigning <mg03> where matnr = x_mara-matnr.
    move-corresponding <mg03>-mg03steuer to gs_mg03_sdm.
    gs_mg03_sdm-matnr = <mg03>-matnr.

    perform build_string_from_key using '/GDA/SDM_MLAN'
                                        gs_mg03_sdm
                                  changing gs_mg03_sdm-sdm_tabkey.

    append gs_mg03_sdm to gt_mg03_sdm.

    move-corresponding <mg03>-mg03steuer to gs_mg03_sdm_brf.
    append gs_mg03_sdm_brf to gt_mg03_sdm_brf.

    clear:
      gs_mg03_sdm,
      gs_mg03_sdm_brf.
  endloop.

  sort gt_eina by matnr.
  sort gt_eine by infnr.

* EINE AND EINA
  read table gt_eina transporting no fields
   with key matnr = x_mara-matnr binary search.
  if sy-subrc = 0.
    loop at gt_eina assigning <eina> from sy-tabix.
      move-corresponding <eina> to ls_eina_sdm.
      if <eina>-matnr <> x_mara-matnr.
        exit.
      else.

        perform build_string_from_key using 'EINA'
                                            ls_eina_sdm
                                      changing ls_eina_sdm-sdm_tabkey.


        append  ls_eina_sdm  to gt_eina_sdm.
        clear:
         ls_eina_sdm.

*        APPEND <eina> TO gt_eina_sdm.
* EINE
        loop at gt_eine assigning <eine>
          where mandt = sy-mandt and infnr = <eina>-infnr.
          move-corresponding <eine> to ls_eine_sdm.

          perform build_string_from_key using 'EINE'
                                              ls_eine_sdm
                                        changing ls_eine_sdm-sdm_tabkey.

          append ls_eine_sdm to gt_eine_sdm.
          clear ls_eine_sdm-sdm_tabkey.

*          APPEND <eine> TO gt_eine_sdm.
        endloop.
      endif.
    endloop.
  endif.

* KONH
  read table gt_cond_header assigning <cond_header>
   with key matnr = x_mara-matnr binary search.
  if sy-subrc = 0.
    loop at gt_konh assigning <konh> where knumh = <cond_header>-knumh.
      move-corresponding <konh> to ls_konh_sdm.

      perform build_string_from_key using 'KONH'
                                          ls_konh_sdm
                                    changing ls_konh_sdm-sdm_tabkey.

      append ls_konh_sdm to gt_konh_sdm.
    endloop.
  endif.

* Pricing
  read table gt_pricing assigning <pricing>
   with key matnr = x_mara-matnr. " BINARY SEARCH.
  if sy-subrc = 0.
    perform build_string_from_key using 'MARA'
                                        x_mara
                                  changing <pricing>-sdm_tabkey.


    append <pricing> to gt_pricing_sdm.
  endif.

* Tariff
  if not gt_mvke_sdm is initial.
    call function '/GDA/SDM_PP_BRF_TARIFF1'
      exporting
        x_matnr  = x_mara-matnr
        xt_mvke  = gt_mvke_sdm
      importing
        y_result = gt_tariff_sdm.
  endif.

* MPOP
  loop at gt_mapr assigning <mapr> where matnr = x_mara-matnr.
    read table gt_prop assigning <prop> with key pnum1 = <mapr>-pnum1.
    check sy-subrc = 0.

    move-corresponding <mapr> to ls_mpop_sdm.
    move-corresponding <prop> to ls_mpop_sdm.


    perform build_string_from_key using 'MAPR'
                                        <mapr>
                                  changing ls_mpop_sdm-sdm_tabkey.

    append ls_mpop_sdm to gt_mpop_sdm.
    clear:
     ls_mpop_sdm-sdm_tabkey.
  endloop.

* MLEA
  loop at gt_mlea assigning <mlea> where matnr = x_mara-matnr.

    move-corresponding <mlea> to ls_mlea_sdm.
    perform build_string_from_key using 'MLEA'
                                        ls_mlea_sdm
                                  changing ls_mlea_sdm-sdm_tabkey.

    append ls_mlea_sdm to gt_mlea_sdm.
    clear:
     ls_mlea_sdm-sdm_tabkey.
  endloop.

* MPGD
  loop at gt_mpgd_v assigning <mpgd> where matnr = x_mara-matnr.

    move-corresponding <mpgd> to ls_mpgd_sdm.
    perform build_string_from_key using 'MARA'
                                        x_mara
                                  changing ls_mlea_sdm-sdm_tabkey.

    append ls_mpgd_sdm to gt_mpgd_sdm.
    clear:
     ls_mpgd_sdm-sdm_tabkey.
  endloop.


* populate stores
  loop at gt_marc_sdm assigning <marc_sdm>.
    select single werks from t001w
                        into gs_rmmw1-fiwrk
                      where werks = <marc_sdm>-werks
                       and vlfkz = 'A'.
    check sy-subrc = 0.
    append gs_rmmw1 to gt_rmmw1_sdm.
  endloop.

* populate DC
  clear: gs_rmmw1.
  loop at gt_marc_sdm assigning <marc_sdm>.
    select single werks from t001w
                        into gs_rmmw1-vzwrk
                      where werks = <marc_sdm>-werks
                        and vlfkz = 'B'.
    check sy-subrc = 0.
    append gs_rmmw1 to gt_rmmw1_sdm.
  endloop.

* populate Sales org
  clear: gs_rmmw1.
  loop at gt_mvke_sdm assigning <mvke_sdm>.
    gs_rmmw1-vkorg = <mvke_sdm>-vkorg.
    gs_rmmw1-vtweg = <mvke_sdm>-vtweg.
    collect gs_rmmw1 into gt_rmmw1_sdm.
  endloop.

* populate Vendor
  clear: gs_rmmw1.
  loop at gt_eina_sdm assigning <eina_sdm>.
    read table gt_eine_sdm assigning <eine_sdm> with key infnr = <eina_sdm>-infnr.
    gs_rmmw1-lifnr = <eina_sdm>-lifnr.
    if <eine_sdm> is assigned.
      gs_rmmw1-ekorg = <eine_sdm>-ekorg.
    endif.
    append gs_rmmw1 to gt_rmmw1_sdm.
  endloop.

  loop at gt_eord_sdm assigning <eord_sdm>.
    if ( <eord_sdm>-vdatu <= sy-datum and <eord_sdm>-bdatu >= sy-datum ).
* Valid Record
      ls_src_list-id    = '0'.
      ls_src_list-zeord = <eord_sdm>-zeord.
    else.
      ls_src_list-id   = 'W'.
      ls_src_list-zeord = <eord_sdm>-zeord.
    endif.

    read table gt_rmmw1_sdm with key fiwrk = <eord_sdm>-werks transporting no fields.
    if sy-subrc = 0.
* then assigned to store.
      ls_src_list-vlfkz = 'A'.
    else.
      read table gt_rmmw1_sdm with key vzwrk = <eord_sdm>-werks transporting no fields.
      if sy-subrc = 0.
* then assigned to DC.
        ls_src_list-vlfkz = 'B'.
      endif.
    endif.

    ls_src_list-werks = <eord_sdm>-werks.

    perform build_string_from_key using 'MARA'
                                        x_mara
                                  changing ls_src_list-sdm_tabkey.


    append ls_src_list to gt_src_list_sdm.
    clear:
     ls_src_list.
  endloop.

  loop at gt_src_list_sdm assigning <src_list_sdm>.
    case <src_list_sdm>-vlfkz.
      when 'A'.
        read table gt_src_list_sdm with key vlfkz = 'B' transporting no fields.
        if sy-subrc = 0.
          <src_list_sdm> =  abap_true.
        endif.
      when 'B'.
        read table gt_src_list_sdm with key vlfkz = 'A' transporting no fields.
        if sy-subrc = 0.
          <src_list_sdm> =  abap_true.
        endif.

      when others.
    endcase.
  endloop.


  if gt_eord_sdm is initial.
* No Record
    ls_src_list = 'E'.

    perform build_string_from_key using 'MARA'
                                        x_mara
                                  changing ls_src_list-sdm_tabkey.

    append ls_src_list to gt_src_list_sdm.
    clear:
     ls_src_list.
  endif.

  loop at gt_rmmw1_sdm assigning field-symbol(<rmmw1>).
    if <rmmw1>-lifnr is not initial.
      concatenate x_mara-matnr <rmmw1>-lifnr into <rmmw1>-sdm_tabkey.
    endif.

    if <rmmw1>-fiwrk is not initial.
      concatenate x_mara-matnr <rmmw1>-fiwrk into <rmmw1>-sdm_tabkey.
    endif.

    if <rmmw1>-vzwrk is not initial.
      concatenate x_mara-matnr <rmmw1>-vzwrk into <rmmw1>-sdm_tabkey.
    endif.

    if <rmmw1>-lifnr is initial and <rmmw1>-fiwrk is initial and <rmmw1>-vzwrk is initial.
      <rmmw1>-sdm_tabkey = x_mara-matnr.
    endif.
  endloop.
*  perform additional_data2 using x_mara. ##NEEDED.
endform.                    " BRF_LOGIC

form set_data_art using p_type        type /gda/sdm_de_type
              changing xo_article type ref to /gda/sdm_cl_core. "/gda/sdm_cl_article.
  data:
*    ls_mara       type /gda/sdm_s_mara_01,
    lt_attributes type standard table of xo_article->ty_brf_attributes.

  field-symbols:
    <attribute> like line of lt_attributes,
    <data>      type any.

  lt_attributes = xo_article->get_object_attributes( iv_type = p_type ).

*  MOVE-CORRESPONDING gs_mara TO ls_mara.
  move-corresponding gs_mara to gs_mara_sdm.
  perform build_string_from_key using 'MARA'
                                      gs_mara_sdm
                                changing gs_mara_sdm-sdm_tabkey.


  loop at lt_attributes assigning <attribute>.
    assign (<attribute>-abap_type) to <data>.
    try.
        xo_article->set_selection( iv_name = <attribute>-name  iv_data = <data> iv_type = p_type ).
      catch /gda/cx_sdm_exception_handl ##NO_HANDLER.
    endtry.
  endloop.

  if gv_message is not initial.
    if sy-batch = abap_true.
      write: / gv_message.
    else.
      message gv_message type 'I'.
    endif.
  endif.
endform.

form brf_logic1  using x_type
                      xo_mapping     type ref to /gda/sdm_cl_brf_mapping
                      xo_stewardship type ref to /gda/sdm_cl_stwd_app_main
                changing xo_article     type ref to /gda/sdm_cl_core. "/gda/sdm_cl_article.
*                         XO_ARTICLE_PP1 TYPE REF TO ZCL_SDM_ARTICLE2.

  data:
    lx_fdt type ref to cx_fdt.

  if xo_article is initial.
    try.
        xo_article ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                             iv_source      = gv_source
                                             iv_type        = x_type
                                             iv_stats       = p_stat1  "abap_false
                                             iv_stats_brf   = p_stat2  "abap_false
                                             iv_errors_only = space
                                             iv_mapping     = xo_mapping
                                             iv_stewardship = xo_stewardship ).
      catch cx_fdt_input into lx_fdt.

        if xo_article is not initial.
          xo_article->display_messages( ).
          exit.
        endif.
    endtry.
  endif.

*  IF xo_article IS NOT INITIAL.
*    xo_article->display_messages( ).
**    EXIT.
*  ELSE.
*    EXIT.
*  ENDIF.

  check xo_article is not initial.
  check xo_article->mt_message[] is initial.

  perform set_data_art using x_type "gv_type
                   changing xo_article.

  try.
      xo_article->main( ).
    catch /gda/cx_sdm_exception_handl into gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      if sy-batch = abap_true.
        write: / gv_message.
      else.
*        MESSAGE gv_message TYPE 'W'.
      endif.
    catch cx_fdt_input into gx_fdt.
      call method gx_fdt->if_message~get_longtext
        receiving
          result = gv_message.
      if sy-batch = abap_true.
        write: / gv_message.
      else.
*        MESSAGE gv_message TYPE 'W'.
      endif.
  endtry.
endform.                    " BRF_LOGIC
*&---------------------------------------------------------------------*
*&      Form  SET_UP_RELATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_up_relations .
  data:
    lt_relations       type standard table of struc_rel.
*    lt_relations_extra type standard table of struc_rel.
*    lv_matnr           type mara-matnr.

  field-symbols:
    <matnr>          type any,
    <linkage>        type any,
    <matnr_new>      type any,
    <attyp>          type any,
    <relations>      like line of gt_relations,
    <relations_copy> like line of lt_relations.

  loop at <dyn_table> assigning <dyn_wa>.
    assign component 'KEY_MATNR' of structure <dyn_wa> to <matnr>.
    check sy-subrc = 0.
    assign component 'KEY_ATTYP' of structure <dyn_wa> to <attyp>.
    check sy-subrc = 0.
    assign component 'LINKAGE' of structure <dyn_wa> to <linkage>.

    check sy-subrc = 0.
    read table gt_relations assigning <relations> with key matnr_rel = <matnr>.
    if sy-subrc = 0.
      <linkage> = <relations>-matnr.
    else.
      if <attyp> = '11' or <attyp> =  '01' or <attyp> = '10'  or <attyp> = '12'.
        <linkage> = <matnr>.
      endif.
    endif.
    check <relations> is assigned.
    delete gt_relations where matnr     = <relations>-matnr
                          and matnr_rel = <relations>-matnr_rel.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  FORMAT_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM format_final .
*  <dyn_table_final>[] = <dyn_table>[].
*ENDFORM.

form display_results_new.
  call screen 0100.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM set_display_top.
*  DATA:
**   lo_sort  TYPE REF TO cl_salv_sorts,
*    ls_txt_l TYPE scrtext_l,
*    ls_txt_m TYPE scrtext_m,
*    ls_txt_s TYPE scrtext_s,
*    ls_col   TYPE lvc_fname.
*
*  FIELD-SYMBOLS:
*   <parameter> TYPE any.
*
** Create Instance
*  TRY.
*      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*          r_container  = go_parent1
*        IMPORTING
*          r_salv_table = go_table_top
*        CHANGING
*          t_table      = <dyn_table>[]. "<dyn_table_final>[].
*    CATCH cx_salv_msg INTO gx_root.
*      gv_message = gx_root->get_text( ).
*  ENDTRY.
*
*  IF gv_message IS NOT INITIAL.
*    MESSAGE e000 WITH gv_message.
*  ENDIF.
**  ENDIF.
*
** LINKAGE
** Get Layout
*  go_layout_top      = go_table_top->get_layout( ).
*  go_layout_top->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  go_layout_top->set_default( if_salv_c_bool_sap=>true ).
*
** Get Functions
*  go_func_top = go_table_top->get_functions( ).
*  go_func_top->set_all( ).
*  go_columns_top = go_table_top->get_columns( ).
*  go_columns_top->set_optimize( 'X' ).
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = 'DEFAULT'.
*
*  IF <main_setup> IS ASSIGNED.
*    LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
*      IF <tabstruc>-fieldname = 'MESSAGE'.
*        CONTINUE.
*      ENDIF.
*      TRY.
*          go_column_top ?= go_columns_top->get_column( <tabstruc>-fieldname ).
*        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
*      ENDTRY.
*
*      IF <tabstruc>-key = abap_true.
*        TRY.
*            go_column_top->set_key( value  = if_salv_c_bool_sap=>true ).
*          CATCH cx_salv_data_error.                     "#EC NO_HANDLER
*        ENDTRY.
*      ENDIF.
*
*      TRY.
*          go_column_top->set_alignment( value  = if_salv_c_alignment=>left ).
*        CATCH cx_salv_data_error .                      "#EC NO_HANDLER
*      ENDTRY.
*
*      go_column_top->set_long_text( <tabstruc>-scrtext_l ).
*      go_column_top->set_medium_text( <tabstruc>-scrtext_m ).
*      go_column_top->set_short_text( <tabstruc>-scrtext_s ).
*      go_column_top->set_icon( abap_true ).
*      go_column_top->set_visible( abap_true ).
*
**      IF <tabstruc>-fieldname CS 'MATNR'.
**        TRY.
**            CALL METHOD go_column_top->set_cell_type( value = if_salv_c_cell_type=>hotspot ).
**          CATCH cx_salv_data_error .
**        ENDTRY.
**      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
** Set view/icon cells as hotspots
*  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*    IF <main_setup>-object_view NE 'DEFAULT'.
*      ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*      IF <parameter> = abap_true.
*
*        TRY.
*            ls_col = <main_setup>-object_view.
*            go_column_top ?= go_columns_top->get_column( ls_col ).
*          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
*        ENDTRY.
*
*        ls_txt_l  = <main_setup>-object_view_d.
*        ls_txt_m  = <main_setup>-object_view_d.
*        ls_txt_s  = <main_setup>-object_view_d.
*
*        go_column_top->set_long_text( ls_txt_l  ).
*        go_column_top->set_medium_text( ls_txt_m ).
*        go_column_top->set_short_text( ls_txt_s ).
*        go_column_top->set_icon( abap_true ).
*        go_column_top->set_visible( abap_true ).
*
*        TRY.
*            CALL METHOD go_column_top->set_cell_type
*              EXPORTING
*                value = if_salv_c_cell_type=>hotspot.
*          CATCH cx_salv_data_error.                     "#EC NO_HANDLER
*        ENDTRY.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
*
**  go_events_top = go_table_top->get_event( ).
*
**  Display Table
*  go_table_top->display( ).
*ENDFORM.

form get_layout .
  data: ls_vari type disvariant.

  ls_vari-report    = sy-repid.
  ls_vari-username  = sy-uname.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = ls_vari
      i_save        = 'A'
    importing
      es_variant    = ls_vari
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if sy-subrc eq 0.
    p_vari = ls_vari-variant.
  endif.
endform.

*FORM at_selection_screen.
*  FIELD-SYMBOLS:
*    <parameter>.
***// Select/Deselect All
*  CASE sy-ucomm.
*    WHEN 'SEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        <parameter> = abap_true.
*      ENDLOOP.
*
*    WHEN 'DSEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        <parameter> = abap_false.
*      ENDLOOP.
*  ENDCASE.
*
*  ok_code = sy-ucomm.
*
*ENDFORM.                    " AT_SELECTION_SCREEN

form create_hierarchy using x_matnr
                            x_column
                            xt_keys  type lvc_t_nkey.

  data:
    lv_folder_1      type lvc_nkey,
    lv_folder_2      type lvc_nkey,
    lv_leaf_1        type lvc_nkey,
    lv_leaf_context  type lvc_nkey,
    lv_key_context   type lvc_nkey,
    lv_mess_id(6)    type c,
    lv_field         type field,
    lv_table         type struc1,
    lv_tabname       type tabname,
    lv_context_added type boolean,
    lv_image         type tv_image,
    lv_image2        type tv_image,
    ls_result        type /gda/sdm_s_val_results,
    lv_key_attribute type string,
    ro_data          type ref to data,
    ro_data_empty    type ref to data.

  field-symbols:
    <sdm_object>      like line of gt_sdm_articles,
*    <struc>           LIKE LINE OF gt_view_struc,
    <main_output_02>  like line of gt_pp_output,
    <instances>       like line of <sdm_object>-sdm_instances,
    <results>         type standard table,
    <results_temp>    type standard table,
    <result>          type any,
    <field>           type any,
*    <brf_key>         TYPE any,
    <line_primary>    type any,
    <line_02>         type any,
    <key_field_main>  type any,
    <key_field_attr>  type any,
    <context_field>   type any,
    <key_field>       type any,
    <table_primary>   type any table,
    <table_secondary> type any table.

  read table gt_sdm_articles assigning <sdm_object> with key article = x_matnr.

* Collate results tab
  loop at <sdm_object>-sdm_instances assigning <instances>.
    if <instances>-object is initial.
      continue.
    endif.
    if <results> is not assigned.
      ro_data_empty  = <instances>-object->return_brf_result_structure( ).
      assign ro_data_empty->* to <results>.
      refresh:
       <results>.
    endif.

    ro_data        = <instances>-object->return_brf_result( ).
    assign ro_data->* to <results_temp>.

    if <results_temp> is assigned  and <results_temp> is not initial.
      append lines of <results_temp> to <results>.
    endif.
  endloop.

  sort <results>.
  delete adjacent duplicates from <results>.

* Create folder - 'BRF Errors All'
  perform add_folder using "LV_MESS_ID
                           ''
                           text-007
                           '1'
                    changing lv_folder_1.

  append lv_folder_1 to xt_keys.


  loop at <results> assigning <result>.
*  Determine if error is related to the selected view

    clear:
     lv_table,
     lv_field.

    read table gt_pp_main_setup assigning <main_setup> with key object_view = x_column.

* EXTRA_V1 contains table-field
    assign component 'EXTRA_V1' of structure <result> to <field>.
    if <field> is assigned.
      split <field> at '-' into lv_table-table lv_field.
      read table <main_setup>-tabstruc  with key fieldname = lv_field
                                                 tabname = lv_table-table transporting no fields.
      if sy-subrc <> 0.
        continue.
      endif.
    endif.

    move-corresponding <result> to ls_result.

    lv_mess_id = ls_result-number.

* Add BRF+ result to 'BRF Errors All' folder
    perform add_id using lv_folder_1
                         ls_result
                changing lv_leaf_1.
  endloop.

* Create folder - 'Context'
  perform add_folder using ''
                           text-008
                           '1'
                  changing lv_folder_1.

  loop at <main_setup>-sequence assigning <main_output> where seq = '1'.
    lv_tabname = '<SDM_OBJECT>-&&&&'.
    replace all occurrences of '&&&&' in lv_tabname with <main_output>-tabname.

    assign (lv_tabname) to <table_primary>.
* Loop through primary table
* Extract fields from primary table that are specified in config
    check <table_primary> is assigned.
    loop at <table_primary> assigning <line_primary>.
* Get key for this table entry...
      assign component <main_output>-node_level  of structure <line_primary> to <key_field_main>.

* Special Case!
      if <main_output>-node_level = 'VKORG'.
        assign component 'VTWEG' of structure <line_primary> to <key_field_attr>.
        if <key_field_attr> is assigned.
          lv_key_attribute = <key_field_attr>.
        endif.
      endif.

      if <key_field_main> is assigned and sy-subrc = 0.
* Add entry to : Create folder - 'Context'
        clear:
         lv_image,
         lv_image2.

* Determine if this context folder contains an error, if it does then mark with an error icon..
        loop at <results> assigning <result>.
          perform determine_icon using x_matnr
                                       <main_output>-node_level
                                       <key_field_main>
                                       lv_key_attribute
*                                       lv_folder_2
*                                       ' '
                                       <result>
                                       <main_setup>
                                  changing lv_image
                                           lv_image2.

          if lv_image = icon_failure.
            exit.
          endif.
        endloop.

        perform add_context_key using lv_folder_1
                                      <key_field_main>
                                      lv_key_attribute
                                      <main_output>-node_level
                                      <main_output>-tabname
                                      lv_image
                                      lv_image2
                             changing lv_key_context.

* Create Folder - 'BRF Errors'
        if lv_image = icon_failure.
          perform add_folder using lv_key_context
                                   text-009
                                   '1'
                          changing lv_folder_2.

* Now add the relevant errors..START
          loop at <results> assigning <result>.

            perform add_context_errors using x_matnr
                                             <key_field_main>
                                             lv_key_attribute
                                             lv_folder_2
                                             ' '
                                             <result>
                                             <main_setup>
                                    changing lv_mess_id
                                             lv_leaf_1.

          endloop.
        endif.
      endif.

* Include Context fields for main table on ALV Tree
      loop at <main_setup>-tabstruc assigning <tabstruc>.
        if <tabstruc>-fieldname = 'MESSAGE'.
          continue.
        endif.

        unassign:
         <context_field>,
         <key_field>.

        if <tabstruc>-fieldname cs 'KEY_'.
          continue.
        else.
          assign component <tabstruc>-fieldname  of structure <line_primary> to <context_field>.
        endif.

* is the context field found in the Primary table?
* Yes - then do logic
* No  - then find in seconday tables
        if <context_field> is assigned.

          perform add_context_value
             using "LV_MESS_ID
                   lv_key_context
                   <context_field>
                   <tabstruc>-fieldname
                   <tabstruc>-tabname
                   <key_field_main>
                   lv_key_attribute
                   <sdm_object>
*                   LV_SHOW_ICON
          changing lv_leaf_context.
* Assume the value exists in a secondary table..
        else.

          lv_context_added = abap_false.

          loop at <main_setup>-sequence assigning <main_output_02> where seq = '2'.
            if lv_context_added = abap_true.
              continue.
            endif.

            unassign:
             <context_field>,
             <table_secondary>.

            if <main_output_02> is assigned.
              lv_tabname = '<SDM_OBJECT>-&&&&'.
              replace all occurrences of '&&&&' in lv_tabname with <main_output_02>-tabname.
              assign (lv_tabname) to <table_secondary>.

              check <table_secondary> is assigned.
              loop at <table_secondary> assigning <line_02>.
                unassign <context_field>.
                assign component <main_output>-node_level of structure <line_02> to <context_field>.

                check <context_field> is assigned.
                check <key_field_main> eq <context_field>.

                assign component <main_output>-node_level  of structure <line_02> to <key_field>.
                if <key_field> is assigned and <key_field> <> <key_field_main>.
                  continue.
                endif.

                if <tabstruc>-fieldname cs 'KEY_'.
*                    ASSIGN COMPONENT <TABSTRUC>-FIELDNAME+4  OF STRUCTURE <LINE_02> TO <VALUE_02>.
                else.
                  unassign <context_field>. "RR 17.01.2019
                  assign component <tabstruc>-fieldname  of structure <line_02> to <context_field>.
                endif.

                if <context_field> is assigned.

                  perform add_context_value using lv_key_context
                                                  <context_field>
                                                  <tabstruc>-fieldname
                                                  <tabstruc>-tabname
                                                  <key_field>
                                                  lv_key_attribute
                                                  <sdm_object>
                                         changing lv_leaf_context.
                  lv_context_added = abap_true.
                  continue.
                endif.
              endloop.
            endif.
          endloop.
        endif.
      endloop.
    endloop.
  endloop.
endform.                    " CREATE_HIERARCHY

form add_context_errors using p_matnr
                              p_value_01
                              p_value_02
                              p_header_subkey
                              p_no_leaf
                              p_result
                              p_main_setup type /gda/sdm_s_main
                     changing p_mess_id
                              p_id_key.

  data:
    lv_field           type field,
    lv_table           type struc1,
    ls_result          type /gda/sdm_s_val_results,
    lv_mess_id_last(6) type c.

  field-symbols:
    <field>    type any,
    <brf_key>  type any,
    <brf_key6> type any.

* EXTRA_V1 contains table-field
  assign component 'EXTRA_V1' of structure p_result to <field>.
  if <field> is assigned.
    split <field> at '-' into lv_table-table lv_field.
    read table p_main_setup-tabstruc with key fieldname = lv_field
                                              tabname   = lv_table-table transporting no fields.
    if sy-subrc <> 0.
*        CONTINUE.
      exit.
    endif.
  endif.
*
  assign component 'EXTRA_V5' of structure p_result to <brf_key>.
  if <brf_key> = space.
    assign component 'EXTRA_V4' of structure p_result to <brf_key>.
  endif.

  assign component 'EXTRA_V6' of structure p_result to <brf_key6>.


*  IF lv_table = 'MARA'.
  if <brf_key> is initial.
    <brf_key> = p_matnr.
  endif.

  if p_value_01 = <brf_key> and p_value_02 = <brf_key6>.

    move-corresponding p_result to ls_result.

    p_mess_id = ls_result-number.

    if p_no_leaf = space.
      if p_mess_id <> lv_mess_id_last.
        lv_mess_id_last = p_mess_id.

        perform add_id using p_header_subkey
                             ls_result
                    changing p_id_key.

      endif.
    endif.
  endif.
endform.                    " ADD_CONTEXT_ERRORS

form add_context_key using p_relat_key type lvc_nkey
                           p_value     type any     "Key Field
                           p_value2    type any     "Key Field Attribute
                           p_name      type any
                           p_tabname   type any
                           p_image
                           p_image2
                  changing p_node_key  type lvc_nkey.

  data:
    lv_node_text   type lvc_value,
    lv_value       type string,
    lv_value2      type string,
    lv_ddtext      type dd04t-ddtext,
    ls_node_layout type lvc_s_layn,
    lt_item_layout type lvc_t_layi,
    ls_item_layout type lvc_s_layi,
    lv_rollname    type dd03l-rollname.

  lv_value  = p_value.
  lv_value2 = p_value2.

* get rollname..
  select single rollname from dd03l
                        into lv_rollname
                        where tabname   = p_tabname
                          and fieldname = p_name.
  if sy-subrc <> 0.
    lv_rollname = p_name.
  endif.

  select single ddtext from dd04t
                        into lv_ddtext
                        where rollname   = lv_rollname
                          and ddlanguage = sy-langu.
  if sy-subrc <> 0.
    lv_ddtext = p_name.
  endif.

  if p_value2 is initial.
    concatenate lv_ddtext ' : ' lv_value into lv_node_text.
  else.
    concatenate lv_ddtext ' : ' lv_value '/' lv_value2 into lv_node_text.
  endif.
  ls_node_layout-n_image   = p_image.
  ls_node_layout-exp_image = p_image.

* Test
  if p_image2 is not initial.
    ls_item_layout-t_image = p_image2. "ICON_DISTRIBUTION.
    ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.

    append ls_item_layout to lt_item_layout.
  endif.
* Test
  call method go_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_node_layout   = ls_node_layout
      it_item_layout   = lt_item_layout
    importing
      e_new_node_key   = p_node_key.

endform.                    " ADD_ID

form add_context_value using p_relat_key    type lvc_nkey
                             p_value        type any
                             p_name         type any
                             p_table        type any
                             p_keyfield     type any
                             p_keyattr      type any
                             p_object       type /gda/sdm_s_article
                    changing p_leaf_context type lvc_nkey.

  data:
    lv_node_text type lvc_value,
    lv_value     type char30, "string,
    lv_layout    type lvc_s_layn,
    lv_ddtext    type dd04t-ddtext,
*    lt_item_layout TYPE lvc_t_layi,
*    ls_item_layout TYPE lvc_s_layi,
    lv_line      type /gda/sdm_s_val_results,
    lv_key_combo type string.

  field-symbols:
     <icons> like line of p_object-icons.

  lv_value = p_value.

  select single ddtext from dd03m
                        into lv_ddtext
                        where tabname    = p_table
                          and fieldname  = p_name
                          and ddlanguage = sy-langu.

  if sy-subrc = 0.
    lv_node_text = lv_ddtext.
  else.
    lv_node_text = p_name.
  endif.

  lv_line-extra_v5 = lv_value.

  read table p_object-icons assigning <icons>  with key field   = p_name
                                     brf_key = p_keyfield.
  if sy-subrc = 0.
    lv_layout-n_image = <icons>-icon.
  else.
    concatenate p_keyfield '/' p_keyattr into lv_key_combo.
    read table p_object-icons assigning <icons>  with key field   = p_name
                                       brf_key = lv_key_combo.
    if sy-subrc = 0.
      lv_layout-n_image = <icons>-icon.
    endif.
  endif.

  call method go_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_node_layout   = lv_layout
*     it_item_layout   = lt_item_layout
      is_outtab_line   = lv_line
    importing
      e_new_node_key   = p_leaf_context.

endform.                    " ADD_ID

form add_folder  using p_relat_key type lvc_nkey
                       p_text      type lvc_value
                       p_type      type c
              changing p_node_key  type lvc_nkey.

  data:
    lv_node_text type lvc_value,
    ls_result    type /gda/sdm_s_val_results,
    lv_rel       type int4.

  lv_node_text = p_text.

  case p_type.
    when '1'.
      lv_rel = cl_gui_column_tree=>relat_last_child.
    when '2'.
      lv_rel = cl_gui_column_tree=>relat_last_sibling.
    when others.
      lv_rel = cl_gui_column_tree=>relat_last_child.
  endcase.


  call method go_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = lv_rel "CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      i_node_text      = lv_node_text
      is_outtab_line   = ls_result
*     is_node_layout   = ls_node_layout
    importing
      e_new_node_key   = p_node_key.

endform.                    " ADD_ID

form add_id  using    p_relat_key type lvc_nkey
                      p_result    type /gda/sdm_s_val_results "ZCA_BRF_VAL_RETURN_GUI
            changing  p_node_key  type lvc_nkey.

  data:
   l_node_text type lvc_value.

  l_node_text = p_result-number.

  call method go_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = p_result
    importing
      e_new_node_key   = p_node_key.

endform.                    " ADD_ID


*FORM set_display_top2.
*  DATA:
**   lo_sort  TYPE REF TO cl_salv_sorts,
*    ls_txt_l  TYPE scrtext_l,
*    ls_txt_m  TYPE scrtext_m,
*    ls_txt_s  TYPE scrtext_s,
**    ls_col    TYPE lvc_fname,
*    ls_layout TYPE lvc_s_layo.
*
*
*  FIELD-SYMBOLS:
*    <parameter>  TYPE any,
*    <view_setup> LIKE LINE OF gt_pp_main_setup.
*
** Create Instance
*  TRY.
*      CREATE OBJECT go_alv_top
*        EXPORTING
*          i_parent = go_parent1.
*
*    CATCH cx_salv_msg INTO gx_root.
*      gv_message = gx_root->get_text( ).
*  ENDTRY.
*
*  IF gv_message IS NOT INITIAL.
*    MESSAGE e000 WITH gv_message.
*  ENDIF.
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = 'DEFAULT'.
*
*  IF <main_setup> IS ASSIGNED.
*    LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
*      IF <tabstruc>-fieldname = 'MESSAGE'.
*        CONTINUE.
*      ENDIF.
**      TRY.
**          go_column_top ?= go_columns_top->get_column( <tabstruc>-fieldname ).
**        CATCH cx_salv_not_found.
**      ENDTRY.
**
*      IF <tabstruc>-key = abap_true.
**        TRY.
**            go_column_top->set_key( value  = if_salv_c_bool_sap=>true ).
**          CATCH cx_salv_data_error .
**        ENDTRY.
*      ENDIF.
*
*      READ TABLE gt_pp_main_setup ASSIGNING <view_setup> WITH KEY object_view = <tabstruc>-fieldname.
*      IF sy-subrc = 0.
*        ASSIGN (<view_setup>-object_view_o) TO <parameter>.
*        IF <parameter> = abap_true.
*          ls_txt_l  = <view_setup>-object_view_d.
*          ls_txt_m  = <view_setup>-object_view_d.
*          ls_txt_s  = <view_setup>-object_view_d.
*
*          <tabstruc>-scrtext_l = ls_txt_l.
*          <tabstruc>-scrtext_m = ls_txt_m.
*          <tabstruc>-scrtext_s = ls_txt_s.
*        ENDIF.
*      ENDIF.
*      IF <tabstruc>-fieldname CS 'MATNR'.
*        <tabstruc>-hotspot = abap_true.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  ls_layout-cwidth_opt = abap_true.
*
*  CREATE OBJECT go_handler_top.
*  SET HANDLER go_handler_top->on_hotspot_click    FOR go_alv_top.
*  SET HANDLER go_handler_top->handle_context_menu FOR go_alv_top.
*  SET HANDLER go_handler_top->handle_user_command FOR go_alv_top.
*  SET HANDLER go_handler_top->toolbar             FOR go_alv_top.
*
** Display Table
*  CALL METHOD go_alv_top->set_table_for_first_display
*    EXPORTING
*      is_layout       = ls_layout
*    CHANGING
*      it_fieldcatalog = <main_setup>-tabstruc[]
*      it_outtab       = <dyn_table>[]. "<dyn_table_final>[].
*
*
*ENDFORM.

form determine_icon using p_matnr
                          p_node_level
                          p_value_01
*                          p_header_subkey
                          p_value_02
*                          p_no_leaf
                          p_result
                          p_main_setup type /gda/sdm_s_main
                 changing p_image
                          p_image2.

  data:
    lv_field type field,
    lv_table type struc1.
*    ls_result          TYPE /gda/sdm_s_val_results,
*    lv_mess_id_last(6) TYPE c.

  field-symbols:
    <field>    type any,
    <brf_key>  type any,
    <brf_key6> type any.

* EXTRA_V1 contains table-field
  assign component 'EXTRA_V1' of structure p_result to <field>.
*  IF <field>  EQ '/GDA/SDM_TARIFF-LAND1'.
*    BREAK-POINT.
*  endif.
  if <field> is assigned.
    split <field> at '-' into lv_table-table lv_field.
    read table p_main_setup-tabstruc with key fieldname = lv_field
                                              tabname   = lv_table-table transporting no fields.
*    IF sy-subrc <> 0.
*      EXIT.
  endif.
*  ENDIF.
*

  if sy-subrc = 0.
    assign component 'EXTRA_V5' of structure p_result to <brf_key>.
    if <brf_key> = space.
      assign component 'EXTRA_V4' of structure p_result to <brf_key>.
    endif.

* Additional key field data
    assign component 'EXTRA_V6' of structure p_result to <brf_key6>.
    if <brf_key6> is assigned and <brf_key6> is not initial.

    endif.

*  IF lv_table = 'MARA'.
    if <brf_key> is initial.
      <brf_key> = p_matnr.
    endif.

    if p_value_02 is initial.
      if p_value_01 = <brf_key>.
        p_image = icon_failure.
      else.
        p_image = icon_positive.
      endif.
    else.
      if p_value_01 = <brf_key> and p_value_02 = <brf_key6>.
        p_image = icon_failure.
      else.
        p_image = icon_positive.
      endif.

    endif.
  endif.

* Special condition! - Consider an exit for this..
  if p_node_level = 'WERKS'.
    data:
      ls_t001w type t001w.

    select single * from t001w
                        into ls_t001w
                      where werks = p_value_01.
*                       AND vlfkz = 'A'.

    if ls_t001w-vlfkz = 'A'.
      p_image2 = icon_store_location. "ICON_DISTRIBUTION
    else.
      p_image2 = icon_distribution. "ICON_DISTRIBUTION
    endif.
  endif.
endform.                    " ADD_CONTEXT_ERRORS
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form determine_selection .
  if ( s_infnr is not initial or s_ekorg is not initial or s_lifnr is not initial )." AND s_matnr IS INITIAL.
    gv_eina_first = abap_true.
  endif.

  if ( s_vdatu is not initial or s_bdatu  is not initial ).
    gv_eord_first = abap_true.
  endif.

  if s_vkorg is not initial or s_vtweg is not initial.
    gv_mvke_first = abap_true.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_STRUCTURED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form material_structured .
  data:
    lt_mara_variants type standard table of /gda/sdm_s_mara_01,
    lt_mast          type standard table of mast,
    lt_stpo          type standard table of stpo,
    lt_relations     type standard table of struc_rel,
    lt_mara          type standard table of /gda/sdm_s_mara_01.

  field-symbols:
    <relations1>    like line of gt_relations,
    <relations2>    like line of lt_relations,
    <mara>          like line of gt_mara,
    <mara_variants> like line of lt_mara_variants.

  ranges: r_attyp for mara-attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '00'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '01'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '02'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '10'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '11'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '12'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '21'.
  append  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '22'.
  append  r_attyp.

  select * from mara
           into corresponding fields of table gt_mara
           where matnr in s_matnr
             and ersda in s_ersda
             and ernam in s_ernam
             and laeda in s_laeda
             and aenam in s_aenam
             and mtart in s_mtart
             and matkl in s_matkl
             and mstae in s_mstae
             and attyp in r_attyp.

* Cater for Structured Articles.
  if p_struc = abap_true.
    refresh:
     lt_mara_variants.

* Strip out any items which are not top level!
    delete gt_mara where attyp = '00'.

    loop at gt_mara assigning <mara_struc>. " WHERE attyp <> '00'.

      case <mara_struc>-attyp.
* Generics & Variants:
        when '01'.
          select * from mara
                   into corresponding fields of table lt_mara_variants
                   where satnr eq <mara_struc>-matnr.

          loop at lt_mara_variants assigning <mara_variants>.
            gs_relations-matnr     = <mara_struc>-matnr.
            gs_relations-matnr_rel = <mara_variants>-matnr.
            append gs_relations to gt_relations.
            clear   gs_relations.
          endloop.

          append lines of lt_mara_variants to gt_mara.

* Pre-Pack, Sales Sets, Display Articles etc
        when others.

          select matnr stlnr from mast
                             into corresponding fields of table lt_mast
                             where matnr = <mara_struc>-matnr.

          if lt_mast is not initial.
            select idnrk from stpo
                         into corresponding fields of table lt_stpo
                         for all entries in lt_mast
                         where stlnr =  lt_mast-stlnr.
* Now get these entries in the MARA struc
            if lt_stpo is not initial.
              select * from mara
                       into corresponding fields of table lt_mara_variants
                       for all entries in lt_stpo
                       where matnr eq lt_stpo-idnrk.

              loop at lt_mara_variants assigning <mara_variants>.
                gs_relations-matnr     = <mara_struc>-matnr.
                gs_relations-matnr_rel = <mara_variants>-matnr.
                append gs_relations to gt_relations.
                clear   gs_relations.
              endloop.

              append lines of lt_mara_variants to gt_mara.
            endif.

          endif.
      endcase.

    endloop.

* GS_RELATIONS
*    APPEND LINES OF GT_MARA_VARIANTS TO GT_MARA.
    sort gt_mara.
    delete adjacent duplicates from gt_mara.

    lt_relations[] = gt_relations[].

    loop at gt_relations assigning <relations1>.
      loop at lt_relations assigning <relations2> where matnr <> <relations1>-matnr
                                                    and matnr_rel = <relations1>-matnr_rel.
        read table gt_mara assigning <mara> with key matnr = <relations1>-matnr_rel.
        append <mara> to lt_mara.
        exit.
* this entry should be added back
      endloop.
    endloop.
    sort lt_mara.
    delete adjacent duplicates from lt_mara.
    append lines of lt_mara to gt_mara.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form material .
  data:
   lt_matnr    type table_matnr,
   lv_function type string.

  field-symbols:
    <eina> like line of gt_eina,
    <eord> like line of gt_eord,
    <mvke> like line of gt_mvke.

  if sy-saprl >= gc_sap_version.
    lv_function = '/GDA/SDM_MM_MARA_GET_NEW'.
  else.
    lv_function = '/GDA/SDM_MM_MARA_GET_OLD'.
  endif.

  if gv_eina_first = abap_true.
* Note -- Possibly include a join on material to ensure we have an article..
    select * from eina into corresponding fields of table gt_eina
               where matnr      in s_matnr
               and   matkl      in s_matkl
               and   lifnr      in s_lifnr
               and   infnr      in s_infnr.
    if sy-subrc = 0.
      loop at gt_eina assigning <eina>.
        collect <eina>-matnr into lt_matnr.
      endloop.
    endif.
  endif.

  if gv_eord_first = abap_true.
    select * from eord into corresponding fields of table gt_eord
             where matnr      in s_matnr
               and werks      in s_werks
               and lifnr      in s_lifnr
               and vdatu      in s_vdatu
               and bdatu      in s_bdatu.
    if sy-subrc = 0.
      loop at gt_eord assigning <eord>.
        collect <eord>-matnr into lt_matnr.
      endloop.
    endif.
  endif.

  if gv_mvke_first = abap_true.
    select * from mvke into corresponding fields of table gt_mvke
             where matnr      in s_matnr
               and vkorg      in s_vkorg
               and vtweg      in s_vtweg.
    if sy-subrc = 0.
      loop at gt_mvke assigning <mvke>.
        collect <mvke>-matnr into lt_matnr.
      endloop.
    endif.
  endif.

  if gv_eina_first = abap_true or gv_eord_first = abap_true or gv_mvke_first = abap_true.
    if lt_matnr is not initial.
      call function lv_function
        exporting
          x_max_rows   = p_max
          xt_materials = lt_matnr
        importing
          xt_mara      = gt_mara[]
        tables
          xt_matnr     = s_matnr
          xt_ersda     = s_ersda
          xt_ernam     = s_ernam
          xt_laeda     = s_laeda
          xt_aenam     = s_aenam
          xt_mtart     = s_mtart
          xt_matkl     = s_matkl
          xt_mstae     = s_mstae
          xt_bwscl     = s_bwscl
          xt_attyp     = s_attyp
          xt_werks     = s_werks
          xt_mmsta     = s_mmsta
          xt_lgort     = s_lgort.
    endif.

  else.
    call function lv_function
      exporting
        x_max_rows = p_max
      importing
        xt_mara    = gt_mara[]
      tables
        xt_matnr   = s_matnr
        xt_ersda   = s_ersda
        xt_ernam   = s_ernam
        xt_laeda   = s_laeda
        xt_aenam   = s_aenam
        xt_mtart   = s_mtart
        xt_matkl   = s_matkl
        xt_mstae   = s_mstae
        xt_bwscl   = s_bwscl
        xt_attyp   = s_attyp
        xt_werks   = s_werks
        xt_mmsta   = s_mmsta
        xt_lgort   = s_lgort.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sdm_main_article.
  data:
    percent           type p,
    percent_text      type string,
    message           type string.

  perform progress_bar using text-018.

  describe table go_selection->mt_mara lines data(count).
  perform progress_bar using text-018.

  loop at go_selection->mt_mara into go_selection->ms_mara_spec.
    clear:
      percent,
      percent_text,
      message.

    percent = ( sy-tabix / count ) * 100.
    percent_text = percent.
    concatenate 'BRF Rules '(917)  percent_text '%' into message.
    perform progress_bar using message.

* BRF+ Logic
    go_selection->refresh( ).
    go_selection->mv_object = go_selection->ms_mara_spec-matnr.
    go_selection->build_spec( ).

    gs_mara_sdm     = go_selection->ms_mara_spec.
    gt_marc_sdm[]   = go_selection->mt_marc_spec[].
    gt_mast_sdm[]   = go_selection->mt_mast_spec[].
    gt_mard_sdm[]   = go_selection->mt_mard_spec[].
    gt_maw1_sdm[]   = go_selection->mt_maw1_spec[].
    gt_mwli_sdm[]   = go_selection->mt_mwli_spec[].
    gt_mbew_sdm[]   = go_selection->mt_mbew_spec[].
    gt_meinh_sdm[]  = go_selection->mt_meinh_spec[].
*    gt_mfhm_sdm[]   = go_selection->mt_mfhm_spec[].
    gt_mlgn_sdm[]   = go_selection->mt_mlgn_spec[].
    gt_mlgt_sdm[]   = go_selection->mt_mlgt_spec[].
    gt_mvke_sdm[]   = go_selection->mt_mvke_spec[].
    gt_wlk1_sdm[]   = go_selection->mt_wlk1_spec[].
    gt_wlk2_sdm[]   = go_selection->mt_wlk2_spec[].
    gt_mean_sdm[]   = go_selection->mt_mean_spec[].
    gt_mpop_sdm[]   = go_selection->mt_mpop_spec[].
    gt_marm_sdm[]   = go_selection->mt_marm_spec[].
    gt_mlan_sdm[]   = go_selection->mt_mlan_spec[].
    gt_malg_sdm[]   = go_selection->mt_malg_spec[].
    gt_mamt_sdm[]   = go_selection->mt_mamt_spec[].
    gt_myms_sdm[]   = go_selection->mt_myms_spec[].
*    gt_steuer_sdm[] = go_selection->mt_steuertab_spec[].
*    gt_steumm_sdm[] = go_selection->mt_steummtab_spec[].
    gt_eord_sdm[]   = go_selection->mt_eord_spec[].
    gt_eina_sdm[]   = go_selection->mt_eina_spec[].
    gt_eine_sdm[]   = go_selection->mt_eine_spec[].
    gt_konh_sdm[]   = go_selection->mt_konh_spec[].
    gt_tariff_sdm   = go_selection->mt_tariff_spec[].
    gs_syst_sdm     = syst.
    append go_selection->ms_makt_spec to gt_makt_sdm[].

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP3 SPOTS /GDA/SDM_MM_MAT_ES4 .
**$*$-Start: /GDA/SDM_MM_MAT_EP3-----------------------------------------------------------------$*$*
*ENHANCEMENT 1  ZSDM_MM_MAT_ES4.    "active version
*   GT_CUSTOM_TEST_SDM[] = go_selection->MT_CUSTOM_TEST_spec[].
*ENDENHANCEMENT.
*$*$-End:   /GDA/SDM_MM_MAT_EP3-----------------------------------------------------------------$*$*

* For derivations..
    read table go_selection->mt_marc_spec[] into gs_marc_sdm index 1.
*    read table go_selection->mt_mlgt_spec[] into gs_mlgt_sdm index 1.
    read table go_selection->mt_mvke_spec[] into gs_mvke_sdm index 1.


* For each Material process the BRF Functions
    loop at gt_objects assigning <objects>.
      clear:
       <objects>-object.
      perform brf_logic  using <objects>-type
                               <objects>-mapping
                               <objects>-stewardship
                         changing <objects>-object.

      if <objects>-object is not bound or  <objects>-object->mt_message is not initial.
        gv_config_err = abap_true.
        exit.
      endif.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      append gs_instance to gs_sdm_objects-sdm_instances.
    endloop.

    check gv_config_err = abap_false.

    gs_sdm_objects-article = go_selection->ms_mara_spec-matnr.
    append gs_sdm_objects to gt_sdm_articles.

    clear:
     gs_sdm_objects.
    refresh:
     gt_mara_sdm,gt_marc_sdm,gt_mard_sdm,gt_mbew_sdm,gt_mlgn_sdm,gt_makt_sdm,gt_eord_sdm,
     gt_mlan_sdm,gt_malg_sdm,gt_mamt_sdm,gt_maw1_sdm,gt_wlk1_sdm,gt_wlk2_sdm,gt_mwli_sdm,
     gt_konh_sdm.
  endloop.

  perform progress_bar using text-019.
endform.
