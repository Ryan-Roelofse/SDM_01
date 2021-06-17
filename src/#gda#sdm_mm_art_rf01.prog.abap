*&---------------------------------------------------------------------*
*&      Form  SET_VIEW_OUTPUT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COLUMN  text
*      -->P_<STATUS>  text
*----------------------------------------------------------------------*
FORM set_view_output_tree USING x_column x_status.
  DATA:
    ls_hier_hdr TYPE treev_hhdr,
    ls_variant  TYPE disvariant,
    lt_keys     TYPE lvc_t_nkey.
*    lt_result   TYPE STANDARD TABLE OF /gda/sdm_s_val_results. " Empty

  FIELD-SYMBOLS:
    <material>    TYPE any,
    <description> TYPE any.

  PERFORM build_structure    USING x_column
                                   gc_object
                                   p_struc.
  PERFORM build_dynamic_itab USING x_column
                             CHANGING ro_data.

  REFRESH:
   <dyn_table_view>.

* Set key fields..
  MOVE-CORRESPONDING <dyn_wa> TO <dyn_wa_view>.
  ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <material>.

*  CLEAR: GS_MARA.
  READ TABLE gt_mara INTO gs_mara WITH KEY matnr = <material>.
  MOVE-CORRESPONDING gs_mara TO <dyn_wa_view>.

  CLEAR:
   gs_makt.
  IF gt_makt IS NOT INITIAL.
    ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <dyn_wa_view> TO <description>.
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = <material>.
    IF <description> IS ASSIGNED.
      <description> = gs_makt-maktx.
    ENDIF.
  ENDIF.

* Get BRF+ results for Article..
** Only if in Error..
  IF x_status = icon_red_light OR x_status = icon_green_light  OR x_status = icon_yellow_light.
    IF go_alv IS BOUND.
      go_alv->free( ).
      FREE go_alv.
    ENDIF.

    IF go_tree IS BOUND.
      go_tree->free( ).
      FREE go_tree.
    ENDIF.

    IF go_tree IS INITIAL.
* create tree control
      CREATE OBJECT go_tree
        EXPORTING
          parent                      = go_parent2
          node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
          item_selection              = 'X'
          no_html_header              = 'X'
          no_toolbar                  = ''
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          illegal_node_selection_mode = 5
          failed                      = 6
          illegal_column_name         = 7.
      IF sy-subrc <> 0.
        MESSAGE x208(00) WITH 'ERROR'.                      "#EC NOTEXT
      ENDIF.

    ENDIF.

    PERFORM build_hierarchy_header CHANGING ls_hier_hdr.

    ls_variant-report = sy-repid.
    ls_variant-variant = '/DEFAULT'.

    CALL METHOD go_tree->set_table_for_first_display
      EXPORTING
        is_variant          = ls_variant
        i_save              = 'A'
        i_default           = 'X'
        i_structure_name    = '/GDA/SDM_S_VAL_RETURN_GUI' "'/GDA/SDM_S_VAL_RESULTS'
        is_hierarchy_header = ls_hier_hdr
      CHANGING
        it_outtab           = gt_result.

    IF x_status = icon_red_light OR x_status = icon_yellow_light.
* Create hierachy -
* Folders - BRF Errors All, Context
      PERFORM create_hierarchy USING
                                <material>
                                x_column
                                lt_keys.
    ENDIF.
* Send data to frontend.
    CALL METHOD go_tree->expand_nodes( it_node_key = lt_keys ).
    CALL METHOD go_tree->frontend_update.
  ENDIF.


ENDFORM.

FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = text-010.
  p_hierarchy_header-tooltip = text-011.
  p_hierarchy_header-width = 75.
  p_hierarchy_header-width_pix = ' '.

ENDFORM.
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
FORM get_data .

  DATA:
    ls_pricing     TYPE /gda/sdm_st_pricing,
    ls_cond_header TYPE cond_header,
    ls_mwli        TYPE /gda/sdm_s_mwli_01.

  FIELD-SYMBOLS:
    <mara>   LIKE LINE OF gt_mara,
    <steuer> LIKE LINE OF  gt_steuer.

  IF s_matnr IS INITIAL AND
     s_ersda IS INITIAL AND
     s_ernam IS INITIAL AND
     s_laeda IS INITIAL AND
     s_aenam IS INITIAL AND
     s_mtart IS INITIAL AND
     s_matkl IS INITIAL AND
     s_mstae IS INITIAL.

    PERFORM limit_max_entries CHANGING p_max gv_execute_report.
  ELSE.
    gv_execute_report = abap_true.
  ENDIF.
  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.

*Get the data from the table Mara and makt (for the description of mataerial)
  REFRESH:
    gt_mara.

  PERFORM progress_bar USING text-016.

  PERFORM determine_selection.

  IF p_struc = abap_false.
    PERFORM material.
  ELSE.
    PERFORM material_structured.
  ENDIF.

  REFRESH:
   gt_makt,gt_marc,gt_mard,gt_maw1,gt_mean,gt_eina,gt_eine,gt_eord,gt_mbew,gt_mvke,gt_wlk1,
   gt_wlk2,gt_stpo,gt_stko,gt_eqst,gt_mast,gt_mg03,gt_steuer,gt_mlgn,gt_mlgt,gt_myms,
   gt_mwli,gt_marm,gt_mamt,gt_malg,gt_mapr,gt_prop,gt_mlea.

  IF gt_mara[] IS NOT INITIAL.
    SELECT * FROM makt
             INTO CORRESPONDING FIELDS OF TABLE gt_makt
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr
             AND spras        =  'E'.
  ENDIF.

  IF gt_mara[] IS NOT INITIAL.
    SELECT * FROM marc
             INTO CORRESPONDING FIELDS OF TABLE gt_marc
             FOR ALL ENTRIES IN gt_mara
                  WHERE matnr      =  gt_mara-matnr
                    AND werks      IN s_werks
                    AND mmsta      IN s_mmsta.

    SELECT * FROM mard INTO CORRESPONDING FIELDS OF TABLE gt_mard
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr     =  gt_mara-matnr
               AND werks      IN s_werks
               AND lgort      IN s_lgort.

    SELECT * FROM mlgn INTO CORRESPONDING FIELDS OF TABLE gt_mlgn
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM mlgt INTO CORRESPONDING FIELDS OF TABLE gt_mlgt
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM maw1 INTO CORRESPONDING FIELDS OF TABLE gt_maw1
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM mean INTO CORRESPONDING FIELDS OF TABLE gt_mean
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM myms INTO CORRESPONDING FIELDS OF TABLE gt_myms
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM marm INTO CORRESPONDING FIELDS OF TABLE gt_marm
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM mamt INTO CORRESPONDING FIELDS OF TABLE gt_mamt
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM malg INTO CORRESPONDING FIELDS OF TABLE gt_malg
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

* Forecast Parameters
    SELECT * FROM mapr INTO TABLE gt_mapr
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    IF gt_mapr IS NOT INITIAL.
      SELECT * FROM prop INTO TABLE gt_prop
               FOR ALL ENTRIES IN gt_mapr
               WHERE pnum1      =  gt_mapr-pnum1.
    ENDIF.

* Vendor-Specific EANs
    SELECT * FROM mlea INTO TABLE gt_mlea
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

* Planning Data
    SELECT * FROM mpgd_v INTO TABLE gt_mpgd_v
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

* LIFO-relevant materials
    SELECT * FROM myms INTO TABLE gt_myms
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.


*    SELECT * FROM MWLI INTO CORRESPONDING FIELDS OF TABLE gt_mwli
*             FOR ALL ENTRIES IN gt_mara
*             WHERE matnr      =  gt_mara-matnr.

    IF gt_eina IS INITIAL.
      SELECT * FROM eina INTO CORRESPONDING FIELDS OF TABLE gt_eina
               FOR ALL ENTRIES IN gt_mara
               WHERE matnr      =  gt_mara-matnr
                 AND matkl      IN s_matkl
                 AND lifnr      IN s_lifnr
                 AND infnr      IN s_infnr.

      SELECT * FROM eine INTO TABLE gt_eine
               FOR ALL ENTRIES IN gt_eina
               WHERE infnr = gt_eina-infnr
               AND ekorg IN s_ekorg
               AND loekz = space.
    ENDIF.

    IF gt_eord IS INITIAL.
      SELECT * FROM eord INTO CORRESPONDING FIELDS OF TABLE gt_eord
               FOR ALL ENTRIES IN gt_mara
               WHERE matnr      =  gt_mara-matnr
                 AND werks      IN s_werks
                 AND lifnr      IN s_lifnr
                 AND vdatu      IN s_vdatu
                 AND bdatu      IN s_bdatu.
    ENDIF.

    SELECT * FROM mvke INTO CORRESPONDING FIELDS OF TABLE gt_mvke
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr
               AND vkorg      IN s_vkorg
               AND vtweg      IN s_vtweg.

    SELECT * FROM mbew INTO CORRESPONDING FIELDS OF TABLE gt_mbew
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr.

    SELECT * FROM wlk1 INTO CORRESPONDING FIELDS OF TABLE gt_wlk1
             FOR ALL ENTRIES IN gt_mara
             WHERE artnr      =  gt_mara-matnr.

    SELECT * FROM wlk2 INTO CORRESPONDING FIELDS OF TABLE gt_wlk2
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr      =  gt_mara-matnr
               AND vkorg      IN s_vkorg
               AND vtweg      IN s_vtweg.

    SELECT * FROM stpo INTO CORRESPONDING FIELDS OF TABLE gt_stpo
             FOR ALL ENTRIES IN gt_mara
             WHERE idnrk      =  gt_mara-matnr.


  ENDIF.

  IF gt_stpo IS NOT INITIAL.
*    SELECT * FROM tpst INTO CORRESPONDING FIELDS OF TABLE gt_tpst
*             FOR ALL ENTRIES IN gt_stpo
*             WHERE werks      IN s_werks
*             AND stlnr      =  gt_stpo-stlnr.

    SELECT * FROM stko INTO CORRESPONDING FIELDS OF TABLE gt_stko
             FOR ALL ENTRIES IN gt_stpo
             WHERE stlnr      =  gt_stpo-stlnr.

    SELECT * FROM eqst INTO CORRESPONDING FIELDS OF TABLE gt_eqst
             FOR ALL ENTRIES IN gt_stpo
             WHERE stlnr    =  gt_stpo-stlnr
             AND werks      IN s_werks.

*    select * from mast into corresponding fields of table gt_mast
*             for all entries in gt_stpo
*             where matnr    =  gt_stpo-idnrk
*             and werks      in s_werks.

    SELECT * FROM mast INTO CORRESPONDING FIELDS OF TABLE gt_mast
             FOR ALL ENTRIES IN gt_stpo
             WHERE werks    IN s_werks
             AND stlnr      =  gt_stpo-stlnr.

  ENDIF.

*  IF gt_tpst IS NOT INITIAL.
*    SELECT * FROM iflo INTO CORRESPONDING FIELDS OF TABLE gt_iflo
*                       FOR ALL ENTRIES IN gt_tpst
*                       WHERE tplnr      =  gt_tpst-tplnr.
*  ENDIF.


  LOOP AT gt_mara ASSIGNING <mara_struc>.
    CALL FUNCTION 'STEUERTAB_READ'
      EXPORTING
*       KZRFB           = ' '
        matnr           = <mara_struc>-matnr
      TABLES
        steuertab       = gt_steuer
      EXCEPTIONS
        wrong_call      = 1
        steuertab_empty = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      LOOP AT gt_steuer ASSIGNING <steuer>.
        gs_mg03-matnr      = <mara_struc>-matnr.
        gs_mg03-mg03steuer = <steuer>.
        APPEND gs_mg03 TO gt_mg03.
        CLEAR:
          gs_mg03.
      ENDLOOP.
    ENDIF.

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

  ENDLOOP.

  IF gt_cond_header IS NOT INITIAL.
*    SORT gt_cond_header ASCENDING.
    SELECT * FROM konh INTO TABLE gt_konh
             FOR ALL ENTRIES IN gt_cond_header
             WHERE knumh    =  gt_cond_header-knumh.
  ENDIF.

* Listing
  LOOP AT gt_maw1 ASSIGNING FIELD-SYMBOL(<maw1>).
* Build MWLI from MAW1
    MOVE-CORRESPONDING <maw1> TO ls_mwli.
    APPEND ls_mwli TO gt_mwli.
  ENDLOOP.

ENHANCEMENT-POINT /gda/sdm_mm_art_ep4 SPOTS /gda/sdm_mm_art_es4 .

  PERFORM progress_bar USING text-017.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUPUT_FIELDS  text
*      <--P_LT_FIELDS  text
*----------------------------------------------------------------------*
FORM get_fields USING internal_table TYPE /gda/sdm_t_setup4
                CHANGING fields TYPE table.

  FIELD-SYMBOLS:
   <line> TYPE /gda/sdm_setup4.

  LOOP AT internal_table ASSIGNING <line>.
    gv_field =  <line>-field.
    APPEND gv_field TO fields.
    CLEAR gv_field.
  ENDLOOP.

  SORT fields.
  DELETE ADJACENT DUPLICATES FROM fields.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_alv_data_new .
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data,
    lt_general    TYPE STANDARD TABLE OF /gda/sdm_setup4,
    lv_count      TYPE p,
    lv_pur        TYPE p,
    lv_text       TYPE string,
    lv_pur_text   TYPE string.
  DATA gt_default_fields        TYPE STANDARD TABLE OF /gda/sdm_setup4.
  FIELD-SYMBOLS:
    <icon>            TYPE any,
*    <results>         type standard table,
    <results_temp>    TYPE STANDARD TABLE,
    <results_pp1>     TYPE STANDARD TABLE,
    <result>          TYPE any,
    <field>           TYPE any,
    <type>            TYPE any,
    <number>          TYPE any,
    <brf_key4>        TYPE any,
    <brf_key5>        TYPE any,
    <brf_key6>        TYPE any,
    <field_alv>       TYPE any,
    <general>         LIKE LINE OF lt_general,
*    <general_default> like line of gt_default_fields,
*    <general_default> like line of gt_alvtop_key_fields,
    <view_table>      LIKE LINE OF gt_view_tables,
    <maw1>            LIKE LINE OF gt_maw1,
    <mean>            LIKE LINE OF gt_mean,
    <eine>            LIKE LINE OF gt_eine,
    <eina>            LIKE LINE OF gt_eina,
    <sdm_articles>    LIKE LINE OF gt_sdm_articles,
    <instances>       LIKE LINE OF <sdm_articles>-sdm_instances,
    <objects>         LIKE LINE OF gt_objects,
    <maktx>           TYPE any.

  PERFORM progress_bar USING text-018.

  DESCRIBE TABLE gt_mara LINES lv_count.
  PERFORM progress_bar USING text-018.


  LOOP AT gt_mara INTO gs_mara.
    CLEAR:
      lv_pur_text,
      lv_pur,
      lv_text.

    REFRESH:
     gt_mara_sdm,gt_marc_sdm,gt_mard_sdm,gt_mbew_sdm,gt_mlgn_sdm,
     gt_mlgt_sdm,gt_mvke_sdm,gt_myms_sdm,gt_maw1_sdm,gt_mean_sdm,
     gt_mwli_sdm,gt_meinh_sdm,gt_mamt_sdm,gt_malg_sdm,gt_ktex,
     gt_basic_text,gt_eine_sdm,gt_eina_sdm,gt_makt_sdm,gt_rmmw1_sdm,
     gt_konh_sdm,gt_wlk2_sdm,gt_wlk1_sdm,gt_mast_sdm,gt_mg03_sdm,
     gt_eord_sdm,gt_mat_steur_sdm,gt_tariff_sdm, gt_src_list_sdm,gt_pricing_sdm,
     gt_mpop_sdm,gt_mpgd_sdm.


    lv_pur = ( sy-tabix / lv_count ) * 100.
    lv_pur_text = lv_pur.
    CONCATENATE 'BRF Rules '(917) ':'  lv_pur_text '%' INTO lv_text SEPARATED BY space.
    PERFORM progress_bar USING lv_text.

* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
    PERFORM prep_data USING gs_mara.

* For each Article process the BRF Functions
    LOOP AT gt_objects ASSIGNING <objects>.
      CLEAR:
       <objects>-object.
      PERFORM brf_logic1  USING <objects>-type
                               <objects>-mapping
                               <objects>-stewardship
                         CHANGING <objects>-object.

      IF <objects>-object IS NOT BOUND OR  <objects>-object->mt_message IS NOT INITIAL.
        gv_config_err = abap_true.
        EXIT.
      ENDIF.

      IF <results> IS NOT ASSIGNED.
        IF <objects>-object IS BOUND.
          ro_data_empty  = <objects>-object->return_brf_result_structure( ).
          ASSIGN ro_data_empty->* TO <results>.
          REFRESH:
           <results>.
        ENDIF.
      ENDIF.

      IF <objects>-object IS BOUND.
        ro_data  = <objects>-object->return_brf_result( ).
        ASSIGN ro_data->* TO <results_pp1>.
        IF sy-subrc = 0 AND <results_pp1> IS NOT INITIAL.
          APPEND LINES OF <results_pp1> TO <results>.
        ENDIF.
      ENDIF.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      APPEND gs_instance TO gs_sdm_objects-sdm_instances.
    ENDLOOP.

    CHECK gv_config_err = abap_false.

    gs_sdm_objects-article = gs_mara-matnr.
    gs_sdm_objects-mara[]  = gt_mara_sdm[].
    gs_sdm_objects-makt[]  = gt_makt_sdm[].
*ENHANCEMENT-POINT /gda/sdm_mm_art_ep6 SPOTS /gda/sdm_mm_art_es6 .

    APPEND gs_sdm_objects TO gt_sdm_articles.
    CLEAR gs_sdm_objects.

    UNASSIGN:
*     <results>,
     <results_pp1>.
  ENDLOOP.

  SORT gt_sdm_articles.

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

  PERFORM progress_bar USING text-019.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prep_data USING x_mara TYPE /gda/sdm_s_mara_01.
  DATA:
    ls_makt_sdm LIKE LINE OF gt_makt_sdm,
    ls_marc_sdm LIKE LINE OF gt_marc_sdm,
    ls_mard_sdm LIKE LINE OF gt_mard_sdm,
    ls_mvke_sdm LIKE LINE OF gt_mvke_sdm,
    ls_mlgn_sdm  LIKE LINE OF gt_mlgn_sdm,
    ls_mlgt_sdm LIKE LINE OF  gt_mlgt_sdm,
    ls_myms_sdm LIKE LINE OF  gt_myms_sdm,
    ls_mwli_sdm LIKE LINE OF  gt_mwli_sdm,
    ls_mbew_sdm LIKE LINE OF gt_mbew_sdm,
    ls_meinh_sdm LIKE LINE OF gt_meinh_sdm, " /gda/sdm_s_meinh,
    ls_mamt_sdm LIKE LINE OF gt_mamt_sdm,
    ls_marm_sdm LIKE LINE OF gt_marm_sdm,
    ls_malg_sdm LIKE LINE OF gt_malg_sdm,
    ls_maw1_sdm LIKE LINE OF gt_maw1_sdm,
    ls_mean_sdm LIKE LINE OF gt_mean_sdm,
    ls_eord_sdm LIKE LINE OF gt_eord_sdm,
    ls_wlk2_sdm LIKE LINE OF gt_wlk2_sdm,
    ls_wlk1_sdm LIKE LINE OF gt_wlk1_sdm,
    ls_mast_sdm LIKE LINE OF gt_mast_sdm,
    ls_eina_sdm LIKE LINE OF gt_eina_sdm,
    ls_eine_sdm LIKE LINE OF gt_eine_sdm,
    ls_konh_sdm LIKE LINE OF gt_konh_sdm,
    ls_src_list LIKE LINE OF gt_src_list_sdm, "type /gda/sdm_st_srclist,
    ls_rmmw1_sdm TYPE /gda/sdm_s_rmmw1,
    ls_mpop_sdm LIKE LINE OF gt_mpop_sdm,
    ls_mlea_sdm LIKE LINE OF gt_mlea_sdm,
    ls_mpgd_sdm LIKE LINE OF gt_mpgd_sdm.

  FIELD-SYMBOLS:
    <makt>         LIKE LINE OF gt_makt,
    <ttext>        LIKE LINE OF gt_ktex,
    <marc>         LIKE LINE OF gt_marc,
    <mard>         LIKE LINE OF gt_mard,
    <mvke>         LIKE LINE OF gt_mvke,
    <mbew>         LIKE LINE OF gt_mbew,
    <mlgn>         LIKE LINE OF gt_mlgn,
    <mlgt>         LIKE LINE OF gt_mlgt,
    <mamt>         LIKE LINE OF gt_mamt,
    <malg>         LIKE LINE OF gt_malg,
    <marm>         LIKE LINE OF gt_marm,
    <maw1>         LIKE LINE OF gt_maw1,
    <eord>         LIKE LINE OF gt_eord,
    <eord_sdm>     LIKE LINE OF gt_eord_sdm,
    <mean>         LIKE LINE OF gt_mean,
    <eina>         LIKE LINE OF gt_eina,
    <eina_sdm>     LIKE LINE OF gt_eina_sdm,
    <eine>         LIKE LINE OF gt_eine,
    <eine_sdm>     LIKE LINE OF gt_eine_sdm,
    <marc_sdm>     LIKE LINE OF gt_marc_sdm,
    <mvke_sdm>     LIKE LINE OF gt_mvke_sdm,
    <konh>         LIKE LINE OF gt_konh,
    <cond_header>  LIKE LINE OF gt_cond_header,
    <pricing>      LIKE LINE OF gt_pricing,
    <wlk2>         LIKE LINE OF gt_wlk2,
    <wlk1>         LIKE LINE OF gt_wlk1,
    <mast>         LIKE LINE OF gt_mast,
    <myms>         LIKE LINE OF gt_myms,
    <mwli>         LIKE LINE OF gt_mwli,
    <mg03>         LIKE LINE OF gt_mg03,
    <mapr>         LIKE LINE OF gt_mapr,
    <prop>         LIKE LINE OF gt_prop,
    <mlea>         LIKE LINE OF gt_mlea,
    <mpgd>         LIKE LINE OF gt_mpgd_v,
    <src_list_sdm> LIKE LINE OF gt_src_list_sdm.

  MOVE-CORRESPONDING x_mara TO ls_rmmw1_sdm.
  APPEND ls_rmmw1_sdm TO gt_rmmw1_sdm.

* MAKT
  LOOP AT gt_makt ASSIGNING <makt> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <makt> TO ls_makt_sdm.

    PERFORM build_string_from_key USING 'MAKT'
                                        ls_makt_sdm
                                  CHANGING ls_makt_sdm-sdm_tabkey.


    APPEND ls_makt_sdm TO gt_makt_sdm.
  ENDLOOP.

  READ TABLE gt_makt
    INTO gs_makt_sdm
    WITH KEY matnr = x_mara-matnr
             spras = sy-langu.

  IF sy-subrc = 0.
    APPEND INITIAL LINE TO gt_ktex ASSIGNING <ttext>.
    <ttext>-maktx = gs_makt_sdm-maktx.
    <ttext>-spras = sy-langu.
  ENDIF.

  APPEND x_mara TO gt_mara_sdm.
* MARC
  LOOP AT gt_marc ASSIGNING <marc> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <marc> TO ls_marc_sdm.

    PERFORM build_string_from_key USING 'MARC'
                                        ls_marc_sdm
                                  CHANGING ls_marc_sdm-sdm_tabkey.

    APPEND ls_marc_sdm TO gt_marc_sdm.
    CLEAR ls_marc_sdm-sdm_tabkey.
  ENDLOOP.

* MARD
  LOOP AT gt_mard ASSIGNING <mard> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mard> TO ls_mard_sdm.
    PERFORM build_string_from_key USING 'MARD'
                                        ls_mard_sdm
                                  CHANGING ls_mard_sdm-sdm_tabkey.

    APPEND ls_mard_sdm TO gt_mard_sdm.
    CLEAR ls_mard_sdm-sdm_tabkey.
  ENDLOOP.

* MVKE
  LOOP AT gt_mvke ASSIGNING <mvke> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mvke> TO ls_mvke_sdm.

    PERFORM build_string_from_key USING 'MVKE'
                                        ls_mvke_sdm
                                  CHANGING ls_mvke_sdm-sdm_tabkey.

    INSERT ls_mvke_sdm INTO TABLE gt_mvke_sdm.
    CLEAR:
     ls_mvke_sdm-sdm_tabkey.
  ENDLOOP.

* MBEW
  LOOP AT gt_mbew ASSIGNING <mbew> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mbew> TO ls_mbew_sdm.
    PERFORM build_string_from_key USING 'MBEW'
                                        ls_mbew_sdm
                                  CHANGING ls_mbew_sdm-sdm_tabkey.

    APPEND ls_mbew_sdm TO gt_mbew_sdm.
  ENDLOOP.

* MLGN
  LOOP AT gt_mlgn ASSIGNING <mlgn> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mlgn> TO ls_mlgn_sdm.
    PERFORM build_string_from_key USING 'MLGN'
                                        ls_mlgn_sdm
                                  CHANGING ls_mlgn_sdm-sdm_tabkey.

    APPEND ls_mlgn_sdm TO gt_mlgn_sdm.

    CLEAR:
     ls_mlgn_sdm-sdm_tabkey.

  ENDLOOP.

* MLGT
  LOOP AT gt_mlgt ASSIGNING <mlgt> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mlgt> TO ls_mlgt_sdm.
    PERFORM build_string_from_key USING 'MLGT'
                                        ls_mlgt_sdm
                                  CHANGING ls_mlgt_sdm-sdm_tabkey.

    APPEND ls_mlgt_sdm TO gt_mlgt_sdm.

    CLEAR:
     ls_mlgt_sdm-sdm_tabkey.
  ENDLOOP.

* MYMS
  LOOP AT gt_myms ASSIGNING <myms> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <myms> TO ls_myms_sdm.
    PERFORM build_string_from_key USING 'MYMS'
                                        ls_myms_sdm
                                  CHANGING ls_myms_sdm-sdm_tabkey.

    APPEND ls_myms_sdm TO gt_myms_sdm.

    CLEAR:
     ls_myms_sdm-sdm_tabkey.
  ENDLOOP.

* MWLI
  LOOP AT gt_mwli ASSIGNING <mwli> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mwli> TO ls_mwli_sdm.
    PERFORM build_string_from_key USING 'MARA'
                                        x_mara
                                  CHANGING ls_mwli_sdm-sdm_tabkey.

    APPEND ls_mwli_sdm TO gt_mwli_sdm.

    CLEAR:
     ls_mwli_sdm-sdm_tabkey.
  ENDLOOP.

* MARM
  LOOP AT gt_marm ASSIGNING <marm> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <marm> TO ls_meinh_sdm.
    ls_meinh_sdm-ntgew = x_mara-ntgew.

    PERFORM build_string_from_key USING 'MARM'
                                        <marm>
                                  CHANGING ls_meinh_sdm-sdm_tabkey.

    APPEND ls_meinh_sdm TO gt_meinh_sdm.

    CLEAR:
     ls_meinh_sdm-sdm_tabkey.
  ENDLOOP.

* MAMT
  LOOP AT gt_mamt ASSIGNING <mamt> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mamt> TO ls_mamt_sdm.

    PERFORM build_string_from_key USING 'MAMT'
                                        ls_mamt_sdm
                                  CHANGING ls_mamt_sdm-sdm_tabkey.

    APPEND ls_mamt_sdm TO gt_mamt_sdm.

    CLEAR:
     ls_mamt_sdm-sdm_tabkey.
  ENDLOOP.

* MARM
  LOOP AT gt_marm ASSIGNING <marm> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <marm> TO ls_marm_sdm.

    PERFORM build_string_from_key USING 'MARM'
                                        ls_marm_sdm
                                  CHANGING ls_marm_sdm-sdm_tabkey.

    APPEND ls_marm_sdm TO gt_marm_sdm.

    CLEAR:
     ls_marm_sdm-sdm_tabkey.
  ENDLOOP.


* MALG
  LOOP AT gt_malg ASSIGNING <malg> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <malg> TO ls_malg_sdm.

    PERFORM build_string_from_key USING 'MALG'
                                        ls_malg_sdm
                                  CHANGING ls_malg_sdm-sdm_tabkey.

    APPEND ls_malg_sdm TO gt_malg_sdm.

    CLEAR:
     ls_malg_sdm-sdm_tabkey.
  ENDLOOP.

* MEIN - POOL Table
  LOOP AT gt_mean ASSIGNING <mean> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mean> TO ls_mean_sdm.

    PERFORM build_string_from_key USING 'MEAN'
                                        ls_mean_sdm
                                  CHANGING ls_mean_sdm-sdm_tabkey.

    APPEND ls_mean_sdm TO gt_mean_sdm.

    CLEAR:
     ls_mean_sdm-sdm_tabkey.

  ENDLOOP.

* MAW1
  LOOP AT gt_maw1 ASSIGNING <maw1> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <maw1> TO ls_maw1_sdm.

    PERFORM build_string_from_key USING 'MAW1'
                                        ls_maw1_sdm
                                  CHANGING ls_maw1_sdm-sdm_tabkey.

    APPEND ls_maw1_sdm TO gt_maw1_sdm.

    CLEAR:
     ls_maw1_sdm-sdm_tabkey.

  ENDLOOP.

* EORD
  LOOP AT gt_eord ASSIGNING <eord> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <eord> TO ls_eord_sdm.

    PERFORM build_string_from_key USING 'EORD'
                                        ls_eord_sdm
                                  CHANGING ls_eord_sdm-sdm_tabkey.

    APPEND ls_eord_sdm TO gt_eord_sdm.

    CLEAR:
     ls_eord_sdm-sdm_tabkey.

  ENDLOOP.


* WLK2
  LOOP AT gt_wlk2 ASSIGNING <wlk2> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <wlk2> TO ls_wlk2_sdm.

    PERFORM build_string_from_key USING 'WLK2'
                                        ls_wlk2_sdm
                                  CHANGING ls_wlk2_sdm-sdm_tabkey.

    APPEND ls_wlk2_sdm TO gt_wlk2_sdm.
    CLEAR:
     ls_wlk2_sdm-sdm_tabkey.

*    append <wlk2> to gt_wlk2_sdm.
  ENDLOOP.

* WLK1
  LOOP AT gt_wlk1 ASSIGNING <wlk1> WHERE artnr = x_mara-matnr.

    MOVE-CORRESPONDING <wlk1> TO ls_wlk1_sdm.

    PERFORM build_string_from_key USING 'WLK1'
                                        ls_wlk1_sdm
                                  CHANGING ls_wlk1_sdm-sdm_tabkey.

    APPEND ls_wlk1_sdm TO gt_wlk1_sdm.
    CLEAR:
     ls_wlk1_sdm-sdm_tabkey.
  ENDLOOP.

* MAST
  LOOP AT gt_mast ASSIGNING <mast> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mast> TO ls_mast_sdm.

    PERFORM build_string_from_key USING 'MAST'
                                        ls_mast_sdm
                                  CHANGING ls_mast_sdm-sdm_tabkey.

    APPEND ls_mast_sdm TO gt_mast_sdm.
    CLEAR:
     ls_mast_sdm-sdm_tabkey.
  ENDLOOP.

* TAX
  LOOP AT gt_mg03 ASSIGNING <mg03> WHERE matnr = x_mara-matnr.
    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm.
    gs_mg03_sdm-matnr = <mg03>-matnr.

    PERFORM build_string_from_key USING '/GDA/SDM_MLAN'
                                        gs_mg03_sdm
                                  CHANGING gs_mg03_sdm-sdm_tabkey.

    APPEND gs_mg03_sdm TO gt_mg03_sdm.

    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm_brf.
    APPEND gs_mg03_sdm_brf TO gt_mat_steur_sdm."gt_mg03_sdm_brf.

    CLEAR:
      gs_mg03_sdm,
      gs_mg03_sdm_brf.
  ENDLOOP.

  SORT gt_eina BY matnr.
  SORT gt_eine BY infnr.

* EINE AND EINA
  READ TABLE gt_eina TRANSPORTING NO FIELDS
   WITH KEY matnr = x_mara-matnr BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT gt_eina ASSIGNING <eina> FROM sy-tabix.
      MOVE-CORRESPONDING <eina> TO ls_eina_sdm.
      IF <eina>-matnr <> x_mara-matnr.
        EXIT.
      ELSE.

        PERFORM build_string_from_key USING 'EINA'
                                            ls_eina_sdm
                                      CHANGING ls_eina_sdm-sdm_tabkey.


        APPEND  ls_eina_sdm  TO gt_eina_sdm.
        CLEAR:
         ls_eina_sdm.

*        APPEND <eina> TO gt_eina_sdm.
* EINE
        LOOP AT gt_eine ASSIGNING <eine>
          WHERE mandt = sy-mandt AND infnr = <eina>-infnr.
          MOVE-CORRESPONDING <eine> TO ls_eine_sdm.

          PERFORM build_string_from_key USING 'EINE'
                                              ls_eine_sdm
                                        CHANGING ls_eine_sdm-sdm_tabkey.

          APPEND ls_eine_sdm TO gt_eine_sdm.
          CLEAR ls_eine_sdm-sdm_tabkey.

*          APPEND <eine> TO gt_eine_sdm.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

* KONH
  READ TABLE gt_cond_header ASSIGNING <cond_header>
   WITH KEY matnr = x_mara-matnr BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT gt_konh ASSIGNING <konh> WHERE knumh = <cond_header>-knumh.
      MOVE-CORRESPONDING <konh> TO ls_konh_sdm.

      PERFORM build_string_from_key USING 'KONH'
                                          ls_konh_sdm
                                    CHANGING ls_konh_sdm-sdm_tabkey.

      APPEND ls_konh_sdm TO gt_konh_sdm.
    ENDLOOP.
  ENDIF.

* Pricing
  READ TABLE gt_pricing ASSIGNING <pricing>
   WITH KEY matnr = x_mara-matnr. " BINARY SEARCH.
  IF sy-subrc = 0.
    PERFORM build_string_from_key USING 'MARA'
                                        x_mara
                                  CHANGING <pricing>-sdm_tabkey.


    APPEND <pricing> TO gt_pricing_sdm.
  ENDIF.

* Tariff
  IF NOT gt_mvke_sdm IS INITIAL.
    CALL FUNCTION '/GDA/SDM_PP_BRF_TARIFF1'
      EXPORTING
        x_matnr  = x_mara-matnr
        xt_mvke  = gt_mvke_sdm
      IMPORTING
        y_result = gt_tariff_sdm.
  ENDIF.

* MPOP
  LOOP AT gt_mapr ASSIGNING <mapr> WHERE matnr = x_mara-matnr.
    READ TABLE gt_prop ASSIGNING <prop> WITH KEY pnum1 = <mapr>-pnum1.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING <mapr> TO ls_mpop_sdm.
    MOVE-CORRESPONDING <prop> TO ls_mpop_sdm.


    PERFORM build_string_from_key USING 'MAPR'
                                        <mapr>
                                  CHANGING ls_mpop_sdm-sdm_tabkey.

    APPEND ls_mpop_sdm TO gt_mpop_sdm.
    CLEAR:
     ls_mpop_sdm-sdm_tabkey.
  ENDLOOP.

* MLEA
  LOOP AT gt_mlea ASSIGNING <mlea> WHERE matnr = x_mara-matnr.

    MOVE-CORRESPONDING <mlea> TO ls_mlea_sdm.
    PERFORM build_string_from_key USING 'MLEA'
                                        ls_mlea_sdm
                                  CHANGING ls_mlea_sdm-sdm_tabkey.

    APPEND ls_mlea_sdm TO gt_mlea_sdm.
    CLEAR:
     ls_mlea_sdm-sdm_tabkey.
  ENDLOOP.

* MPGD
  LOOP AT gt_mpgd_v ASSIGNING <mpgd> WHERE matnr = x_mara-matnr.

    MOVE-CORRESPONDING <mpgd> TO ls_mpgd_sdm.
    PERFORM build_string_from_key USING 'MARA'
                                        x_mara
                                  CHANGING ls_mlea_sdm-sdm_tabkey.

    APPEND ls_mpgd_sdm TO gt_mpgd_sdm.
    CLEAR:
     ls_mpgd_sdm-sdm_tabkey.
  ENDLOOP.


* populate stores
  LOOP AT gt_marc_sdm ASSIGNING <marc_sdm>.
    SELECT SINGLE werks FROM t001w
                        INTO gs_rmmw1-fiwrk
                      WHERE werks = <marc_sdm>-werks
                       AND vlfkz = 'A'.
    CHECK sy-subrc = 0.
    APPEND gs_rmmw1 TO gt_rmmw1_sdm.
  ENDLOOP.

* populate DC
  CLEAR: gs_rmmw1.
  LOOP AT gt_marc_sdm ASSIGNING <marc_sdm>.
    SELECT SINGLE werks FROM t001w
                        INTO gs_rmmw1-vzwrk
                      WHERE werks = <marc_sdm>-werks
                        AND vlfkz = 'B'.
    CHECK sy-subrc = 0.
    APPEND gs_rmmw1 TO gt_rmmw1_sdm.
  ENDLOOP.

* populate Sales org
  CLEAR: gs_rmmw1.
  LOOP AT gt_mvke_sdm ASSIGNING <mvke_sdm>.
    gs_rmmw1-vkorg = <mvke_sdm>-vkorg.
    gs_rmmw1-vtweg = <mvke_sdm>-vtweg.
    COLLECT gs_rmmw1 INTO gt_rmmw1_sdm.
  ENDLOOP.

* populate Vendor
  CLEAR: gs_rmmw1.
  LOOP AT gt_eina_sdm ASSIGNING <eina_sdm>.
    READ TABLE gt_eine_sdm ASSIGNING <eine_sdm> WITH KEY infnr = <eina_sdm>-infnr.
    gs_rmmw1-lifnr = <eina_sdm>-lifnr.
    IF <eine_sdm> IS ASSIGNED.
      gs_rmmw1-ekorg = <eine_sdm>-ekorg.
    ENDIF.
    APPEND gs_rmmw1 TO gt_rmmw1_sdm.
  ENDLOOP.

  LOOP AT gt_eord_sdm ASSIGNING <eord_sdm>.
    IF ( <eord_sdm>-vdatu <= sy-datum AND <eord_sdm>-bdatu >= sy-datum ).
* Valid Record
      ls_src_list-id    = '0'.
      ls_src_list-zeord = <eord_sdm>-zeord.
    ELSE.
      ls_src_list-id   = 'W'.
      ls_src_list-zeord = <eord_sdm>-zeord.
    ENDIF.

    READ TABLE gt_rmmw1_sdm WITH KEY fiwrk = <eord_sdm>-werks TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
* then assigned to store.
      ls_src_list-vlfkz = 'A'.
    ELSE.
      READ TABLE gt_rmmw1_sdm WITH KEY vzwrk = <eord_sdm>-werks TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* then assigned to DC.
        ls_src_list-vlfkz = 'B'.
      ENDIF.
    ENDIF.

    ls_src_list-werks = <eord_sdm>-werks.

    PERFORM build_string_from_key USING 'MARA'
                                        x_mara
                                  CHANGING ls_src_list-sdm_tabkey.


    APPEND ls_src_list TO gt_src_list_sdm.
    CLEAR:
     ls_src_list.
  ENDLOOP.

  LOOP AT gt_src_list_sdm ASSIGNING <src_list_sdm>.
    CASE <src_list_sdm>-vlfkz.
      WHEN 'A'.
        READ TABLE gt_src_list_sdm WITH KEY vlfkz = 'B' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <src_list_sdm> =  abap_true.
        ENDIF.
      WHEN 'B'.
        READ TABLE gt_src_list_sdm WITH KEY vlfkz = 'A' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <src_list_sdm> =  abap_true.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


  IF gt_eord_sdm IS INITIAL.
* No Record
    ls_src_list = 'E'.

    PERFORM build_string_from_key USING 'MARA'
                                        x_mara
                                  CHANGING ls_src_list-sdm_tabkey.

    APPEND ls_src_list TO gt_src_list_sdm.
    CLEAR:
     ls_src_list.
  ENDIF.

  LOOP AT gt_rmmw1_sdm ASSIGNING FIELD-SYMBOL(<rmmw1>).
    IF <rmmw1>-lifnr IS NOT INITIAL.
      CONCATENATE x_mara-matnr <rmmw1>-lifnr INTO <rmmw1>-sdm_tabkey.
    ENDIF.

    IF <rmmw1>-fiwrk IS NOT INITIAL.
      CONCATENATE x_mara-matnr <rmmw1>-fiwrk INTO <rmmw1>-sdm_tabkey.
    ENDIF.

    IF <rmmw1>-vzwrk IS NOT INITIAL.
      CONCATENATE x_mara-matnr <rmmw1>-vzwrk INTO <rmmw1>-sdm_tabkey.
    ENDIF.

    IF <rmmw1>-lifnr IS INITIAL AND <rmmw1>-fiwrk IS INITIAL AND <rmmw1>-vzwrk IS INITIAL.
      <rmmw1>-sdm_tabkey = x_mara-matnr.
    ENDIF.
  ENDLOOP.
*  perform additional_data2 using x_mara. ##NEEDED.
ENDFORM.                    " BRF_LOGIC

FORM set_data_art USING p_type        TYPE /gda/sdm_de_type
              CHANGING xo_article TYPE REF TO /gda/sdm_cl_core. "/gda/sdm_cl_article.
  DATA:
*    ls_mara       type /gda/sdm_s_mara_01,
    lt_attributes TYPE STANDARD TABLE OF xo_article->ty_brf_attributes.

  FIELD-SYMBOLS:
    <attribute> LIKE LINE OF lt_attributes,
    <data>      TYPE any.

  lt_attributes = xo_article->get_object_attributes( iv_type = p_type ).

*  MOVE-CORRESPONDING gs_mara TO ls_mara.
  MOVE-CORRESPONDING gs_mara TO gs_mara_sdm.
  PERFORM build_string_from_key USING 'MARA'
                                      gs_mara_sdm
                                CHANGING gs_mara_sdm-sdm_tabkey.


  LOOP AT lt_attributes ASSIGNING <attribute>.
    ASSIGN (<attribute>-abap_type) TO <data>.
    TRY.
        xo_article->set_selection( iv_name = <attribute>-name  iv_data = <data> iv_type = p_type ).
      CATCH /gda/cx_sdm_exception_handl ##NO_HANDLER.
    ENDTRY.
  ENDLOOP.

  IF gv_message IS NOT INITIAL.
    IF sy-batch = abap_true.
      WRITE: / gv_message.
    ELSE.
      MESSAGE gv_message TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM brf_logic1  USING x_type
                      xo_mapping     TYPE REF TO /gda/sdm_cl_brf_mapping
                      xo_stewardship TYPE REF TO /gda/sdm_cl_stwd_app_main
                CHANGING xo_article     TYPE REF TO /gda/sdm_cl_core. "/gda/sdm_cl_article.
*                         XO_ARTICLE_PP1 TYPE REF TO ZCL_SDM_ARTICLE2.

  DATA:
    lx_fdt TYPE REF TO cx_fdt.

  IF xo_article IS INITIAL.
    TRY.
        xo_article ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                             iv_source      = gv_source
                                             iv_type        = x_type
                                             iv_stats       = p_stat1  "abap_false
                                             iv_stats_brf   = p_stat2  "abap_false
                                             iv_errors_only = space
                                             iv_mapping     = xo_mapping
                                             iv_stewardship = xo_stewardship ).
      CATCH cx_fdt_input INTO lx_fdt.

        IF xo_article IS NOT INITIAL.
          xo_article->display_messages( ).
          EXIT.
        ENDIF.
    ENDTRY.
  ENDIF.

*  IF xo_article IS NOT INITIAL.
*    xo_article->display_messages( ).
**    EXIT.
*  ELSE.
*    EXIT.
*  ENDIF.

  CHECK xo_article IS NOT INITIAL.
  CHECK xo_article->mt_message[] IS INITIAL.

  PERFORM set_data_art USING x_type "gv_type
                   CHANGING xo_article.

  TRY.
      xo_article->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
*        MESSAGE gv_message TYPE 'W'.
      ENDIF.
    CATCH cx_fdt_input INTO gx_fdt.
      CALL METHOD gx_fdt->if_message~get_longtext
        RECEIVING
          result = gv_message.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
*        MESSAGE gv_message TYPE 'W'.
      ENDIF.
  ENDTRY.
ENDFORM.                    " BRF_LOGIC
*&---------------------------------------------------------------------*
*&      Form  SET_UP_RELATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_up_relations .
  DATA:
    lt_relations       TYPE STANDARD TABLE OF struc_rel.
*    lt_relations_extra type standard table of struc_rel.
*    lv_matnr           type mara-matnr.

  FIELD-SYMBOLS:
    <matnr>          TYPE any,
    <linkage>        TYPE any,
    <matnr_new>      TYPE any,
    <attyp>          TYPE any,
    <relations>      LIKE LINE OF gt_relations,
    <relations_copy> LIKE LINE OF lt_relations.

  LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
    ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <matnr>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_ATTYP' OF STRUCTURE <dyn_wa> TO <attyp>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'LINKAGE' OF STRUCTURE <dyn_wa> TO <linkage>.

    CHECK sy-subrc = 0.
    READ TABLE gt_relations ASSIGNING <relations> WITH KEY matnr_rel = <matnr>.
    IF sy-subrc = 0.
      <linkage> = <relations>-matnr.
    ELSE.
      IF <attyp> = '11' OR <attyp> =  '01' OR <attyp> = '10'  OR <attyp> = '12'.
        <linkage> = <matnr>.
      ENDIF.
    ENDIF.
    CHECK <relations> IS ASSIGNED.
    DELETE gt_relations WHERE matnr     = <relations>-matnr
                          AND matnr_rel = <relations>-matnr_rel.
  ENDLOOP.
ENDFORM.
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

FORM display_results_new.
  CALL SCREEN 0100.
ENDFORM.
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

FORM get_layout .
  DATA: ls_vari TYPE disvariant.

  ls_vari-report    = sy-repid.
  ls_vari-username  = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_vari
      i_save        = 'A'
    IMPORTING
      es_variant    = ls_vari
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    p_vari = ls_vari-variant.
  ENDIF.
ENDFORM.

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

FORM create_hierarchy USING x_matnr
                            x_column
                            xt_keys  TYPE lvc_t_nkey.

  DATA:
    lv_folder_1      TYPE lvc_nkey,
    lv_folder_2      TYPE lvc_nkey,
    lv_leaf_1        TYPE lvc_nkey,
    lv_leaf_context  TYPE lvc_nkey,
    lv_key_context   TYPE lvc_nkey,
    lv_mess_id(6)    TYPE c,
    lv_field         TYPE field,
    lv_table         TYPE struc1,
    lv_tabname       TYPE tabname,
    lv_context_added TYPE boolean,
    lv_image         TYPE tv_image,
    lv_image2        TYPE tv_image,
    ls_result        TYPE /gda/sdm_s_val_results,
    lv_key_attribute TYPE string,
    ro_data          TYPE REF TO data,
    ro_data_empty    TYPE REF TO data.

  FIELD-SYMBOLS:
    <sdm_object>      LIKE LINE OF gt_sdm_articles,
*    <struc>           LIKE LINE OF gt_view_struc,
    <main_output_02>  LIKE LINE OF gt_pp_output,
    <instances>       LIKE LINE OF <sdm_object>-sdm_instances,
    <results>         TYPE STANDARD TABLE,
    <results_temp>    TYPE STANDARD TABLE,
    <result>          TYPE any,
    <field>           TYPE any,
*    <brf_key>         TYPE any,
    <line_primary>    TYPE any,
    <line_02>         TYPE any,
    <key_field_main>  TYPE any,
    <key_field_attr>  TYPE any,
    <context_field>   TYPE any,
    <key_field>       TYPE any,
    <table_primary>   TYPE ANY TABLE,
    <table_secondary> TYPE ANY TABLE.

  READ TABLE gt_sdm_articles ASSIGNING <sdm_object> WITH KEY article = x_matnr.

* Collate results tab
  LOOP AT <sdm_object>-sdm_instances ASSIGNING <instances>.
    IF <instances>-object IS INITIAL.
      CONTINUE.
    ENDIF.
    IF <results> IS NOT ASSIGNED.
      ro_data_empty  = <instances>-object->return_brf_result_structure( ).
      ASSIGN ro_data_empty->* TO <results>.
      REFRESH:
       <results>.
    ENDIF.

    ro_data        = <instances>-object->return_brf_result( ).
    ASSIGN ro_data->* TO <results_temp>.

    IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
      APPEND LINES OF <results_temp> TO <results>.
    ENDIF.
  ENDLOOP.

  SORT <results>.
  DELETE ADJACENT DUPLICATES FROM <results>.

* Create folder - 'BRF Errors All'
  PERFORM add_folder USING "LV_MESS_ID
                           ''
                           text-007
                           '1'
                    CHANGING lv_folder_1.

  APPEND lv_folder_1 TO xt_keys.


  LOOP AT <results> ASSIGNING <result>.
*  Determine if error is related to the selected view

    CLEAR:
     lv_table,
     lv_field.

    READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = x_column.

* EXTRA_V1 contains table-field
    ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
    IF <field> IS ASSIGNED.
      SPLIT <field> AT '-' INTO lv_table-table lv_field.
      READ TABLE <main_setup>-tabstruc  WITH KEY fieldname = lv_field
                                                 tabname = lv_table-table TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING <result> TO ls_result.

    lv_mess_id = ls_result-number.

* Add BRF+ result to 'BRF Errors All' folder
    PERFORM add_id USING lv_folder_1
                         ls_result
                CHANGING lv_leaf_1.
  ENDLOOP.

* Create folder - 'Context'
  PERFORM add_folder USING ''
                           text-008
                           '1'
                  CHANGING lv_folder_1.

  LOOP AT <main_setup>-sequence ASSIGNING <main_output> WHERE seq = '1'.
    lv_tabname = '<SDM_OBJECT>-&&&&'.
    REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output>-tabname.

    ASSIGN (lv_tabname) TO <table_primary>.
* Loop through primary table
* Extract fields from primary table that are specified in config
    CHECK <table_primary> IS ASSIGNED.
    LOOP AT <table_primary> ASSIGNING <line_primary>.
* Get key for this table entry...
      ASSIGN COMPONENT <main_output>-node_level  OF STRUCTURE <line_primary> TO <key_field_main>.

* Special Case!
      IF <main_output>-node_level = 'VKORG'.
        ASSIGN COMPONENT 'VTWEG' OF STRUCTURE <line_primary> TO <key_field_attr>.
        IF <key_field_attr> IS ASSIGNED.
          lv_key_attribute = <key_field_attr>.
        ENDIF.
      ENDIF.

      IF <key_field_main> IS ASSIGNED AND sy-subrc = 0.
* Add entry to : Create folder - 'Context'
        CLEAR:
         lv_image,
         lv_image2.

* Determine if this context folder contains an error, if it does then mark with an error icon..
        LOOP AT <results> ASSIGNING <result>.
          PERFORM determine_icon USING x_matnr
                                       <main_output>-node_level
                                       <key_field_main>
                                       lv_key_attribute
*                                       lv_folder_2
*                                       ' '
                                       <result>
                                       <main_setup>
                                  CHANGING lv_image
                                           lv_image2.

          IF lv_image = icon_failure.
            EXIT.
          ENDIF.
        ENDLOOP.

        PERFORM add_context_key USING lv_folder_1
                                      <key_field_main>
                                      lv_key_attribute
                                      <main_output>-node_level
                                      <main_output>-tabname
                                      lv_image
                                      lv_image2
                             CHANGING lv_key_context.

* Create Folder - 'BRF Errors'
        IF lv_image = icon_failure.
          PERFORM add_folder USING lv_key_context
                                   text-009
                                   '1'
                          CHANGING lv_folder_2.

* Now add the relevant errors..START
          LOOP AT <results> ASSIGNING <result>.

            PERFORM add_context_errors USING x_matnr
                                             <key_field_main>
                                             lv_key_attribute
                                             lv_folder_2
                                             ' '
                                             <result>
                                             <main_setup>
                                    CHANGING lv_mess_id
                                             lv_leaf_1.

          ENDLOOP.
        ENDIF.
      ENDIF.

* Include Context fields for main table on ALV Tree
      LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
        IF <tabstruc>-fieldname = 'MESSAGE'.
          CONTINUE.
        ENDIF.

        UNASSIGN:
         <context_field>,
         <key_field>.

        IF <tabstruc>-fieldname CS 'KEY_'.
          CONTINUE.
        ELSE.
          ASSIGN COMPONENT <tabstruc>-fieldname  OF STRUCTURE <line_primary> TO <context_field>.
        ENDIF.

* is the context field found in the Primary table?
* Yes - then do logic
* No  - then find in seconday tables
        IF <context_field> IS ASSIGNED.

          PERFORM add_context_value
             USING "LV_MESS_ID
                   lv_key_context
                   <context_field>
                   <tabstruc>-fieldname
                   <tabstruc>-tabname
                   <key_field_main>
                   lv_key_attribute
                   <sdm_object>
*                   LV_SHOW_ICON
          CHANGING lv_leaf_context.
* Assume the value exists in a secondary table..
        ELSE.

          lv_context_added = abap_false.

          LOOP AT <main_setup>-sequence ASSIGNING <main_output_02> WHERE seq = '2'.
            IF lv_context_added = abap_true.
              CONTINUE.
            ENDIF.

            UNASSIGN:
             <context_field>,
             <table_secondary>.

            IF <main_output_02> IS ASSIGNED.
              lv_tabname = '<SDM_OBJECT>-&&&&'.
              REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output_02>-tabname.
              ASSIGN (lv_tabname) TO <table_secondary>.

              CHECK <table_secondary> IS ASSIGNED.
              LOOP AT <table_secondary> ASSIGNING <line_02>.
                UNASSIGN <context_field>.
                ASSIGN COMPONENT <main_output>-node_level OF STRUCTURE <line_02> TO <context_field>.

                CHECK <context_field> IS ASSIGNED.
                CHECK <key_field_main> EQ <context_field>.

                ASSIGN COMPONENT <main_output>-node_level  OF STRUCTURE <line_02> TO <key_field>.
                IF <key_field> IS ASSIGNED AND <key_field> <> <key_field_main>.
                  CONTINUE.
                ENDIF.

                IF <tabstruc>-fieldname CS 'KEY_'.
*                    ASSIGN COMPONENT <TABSTRUC>-FIELDNAME+4  OF STRUCTURE <LINE_02> TO <VALUE_02>.
                ELSE.
                  UNASSIGN <context_field>. "RR 17.01.2019
                  ASSIGN COMPONENT <tabstruc>-fieldname  OF STRUCTURE <line_02> TO <context_field>.
                ENDIF.

                IF <context_field> IS ASSIGNED.

                  PERFORM add_context_value USING lv_key_context
                                                  <context_field>
                                                  <tabstruc>-fieldname
                                                  <tabstruc>-tabname
                                                  <key_field>
                                                  lv_key_attribute
                                                  <sdm_object>
                                         CHANGING lv_leaf_context.
                  lv_context_added = abap_true.
                  CONTINUE.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " CREATE_HIERARCHY

FORM add_context_errors USING p_matnr
                              p_value_01
                              p_value_02
                              p_header_subkey
                              p_no_leaf
                              p_result
                              p_main_setup TYPE /gda/sdm_s_main
                     CHANGING p_mess_id
                              p_id_key.

  DATA:
    lv_field           TYPE field,
    lv_table           TYPE struc1,
    ls_result          TYPE /gda/sdm_s_val_results,
    lv_mess_id_last(6) TYPE c.

  FIELD-SYMBOLS:
    <field>    TYPE any,
    <brf_key>  TYPE any,
    <brf_key6> TYPE any.

* EXTRA_V1 contains table-field
  ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE p_result TO <field>.
  IF <field> IS ASSIGNED.
    SPLIT <field> AT '-' INTO lv_table-table lv_field.
    READ TABLE p_main_setup-tabstruc WITH KEY fieldname = lv_field
                                              tabname   = lv_table-table TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
*        CONTINUE.
      EXIT.
    ENDIF.
  ENDIF.
*
  ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE p_result TO <brf_key>.
  IF <brf_key> = space.
    ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE p_result TO <brf_key>.
  ENDIF.

  ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE p_result TO <brf_key6>.


*  IF lv_table = 'MARA'.
  IF <brf_key> IS INITIAL.
    <brf_key> = p_matnr.
  ENDIF.

  IF p_value_01 = <brf_key> AND p_value_02 = <brf_key6>.

    MOVE-CORRESPONDING p_result TO ls_result.

    p_mess_id = ls_result-number.

    IF p_no_leaf = space.
      IF p_mess_id <> lv_mess_id_last.
        lv_mess_id_last = p_mess_id.

        PERFORM add_id USING p_header_subkey
                             ls_result
                    CHANGING p_id_key.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " ADD_CONTEXT_ERRORS

FORM add_context_key USING p_relat_key TYPE lvc_nkey
                           p_value     TYPE any     "Key Field
                           p_value2    TYPE any     "Key Field Attribute
                           p_name      TYPE any
                           p_tabname   TYPE any
                           p_image
                           p_image2
                  CHANGING p_node_key  TYPE lvc_nkey.

  DATA:
    lv_node_text   TYPE lvc_value,
    lv_value       TYPE string,
    lv_value2      TYPE string,
    lv_ddtext      TYPE dd04t-ddtext,
    ls_node_layout TYPE lvc_s_layn,
    lt_item_layout TYPE lvc_t_layi,
    ls_item_layout TYPE lvc_s_layi,
    lv_rollname    TYPE dd03l-rollname.

  lv_value  = p_value.
  lv_value2 = p_value2.

* get rollname..
  SELECT SINGLE rollname FROM dd03l
                        INTO lv_rollname
                        WHERE tabname   = p_tabname
                          AND fieldname = p_name.
  IF sy-subrc <> 0.
    lv_rollname = p_name.
  ENDIF.

  SELECT SINGLE ddtext FROM dd04t
                        INTO lv_ddtext
                        WHERE rollname   = lv_rollname
                          AND ddlanguage = sy-langu.
  IF sy-subrc <> 0.
    lv_ddtext = p_name.
  ENDIF.

  IF p_value2 IS INITIAL.
    CONCATENATE lv_ddtext ' : ' lv_value INTO lv_node_text.
  ELSE.
    CONCATENATE lv_ddtext ' : ' lv_value '/' lv_value2 INTO lv_node_text.
  ENDIF.
  ls_node_layout-n_image   = p_image.
  ls_node_layout-exp_image = p_image.

* Test
  IF p_image2 IS NOT INITIAL.
    ls_item_layout-t_image = p_image2. "ICON_DISTRIBUTION.
    ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.

    APPEND ls_item_layout TO lt_item_layout.
  ENDIF.
* Test
  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_node_layout   = ls_node_layout
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " ADD_ID

FORM add_context_value USING p_relat_key    TYPE lvc_nkey
                             p_value        TYPE any
                             p_name         TYPE any
                             p_table        TYPE any
                             p_keyfield     TYPE any
                             p_keyattr      TYPE any
                             p_object       TYPE /gda/sdm_s_article
                    CHANGING p_leaf_context TYPE lvc_nkey.

  DATA:
    lv_node_text TYPE lvc_value,
    lv_value     TYPE char30, "string,
    lv_layout    TYPE lvc_s_layn,
    lv_ddtext    TYPE dd04t-ddtext,
*    lt_item_layout TYPE lvc_t_layi,
*    ls_item_layout TYPE lvc_s_layi,
    lv_line      TYPE /gda/sdm_s_val_results,
    lv_key_combo TYPE string.

  FIELD-SYMBOLS:
     <icons> LIKE LINE OF p_object-icons.

  lv_value = p_value.

  SELECT SINGLE ddtext FROM dd03m
                        INTO lv_ddtext
                        WHERE tabname    = p_table
                          AND fieldname  = p_name
                          AND ddlanguage = sy-langu.

  IF sy-subrc = 0.
    lv_node_text = lv_ddtext.
  ELSE.
    lv_node_text = p_name.
  ENDIF.

  lv_line-extra_v5 = lv_value.

  READ TABLE p_object-icons ASSIGNING <icons>  WITH KEY field   = p_name
                                     brf_key = p_keyfield.
  IF sy-subrc = 0.
    lv_layout-n_image = <icons>-icon.
  ELSE.
    CONCATENATE p_keyfield '/' p_keyattr INTO lv_key_combo.
    READ TABLE p_object-icons ASSIGNING <icons>  WITH KEY field   = p_name
                                       brf_key = lv_key_combo.
    IF sy-subrc = 0.
      lv_layout-n_image = <icons>-icon.
    ENDIF.
  ENDIF.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_node_layout   = lv_layout
*     it_item_layout   = lt_item_layout
      is_outtab_line   = lv_line
    IMPORTING
      e_new_node_key   = p_leaf_context.

ENDFORM.                    " ADD_ID

FORM add_folder  USING p_relat_key TYPE lvc_nkey
                       p_text      TYPE lvc_value
                       p_type      TYPE c
              CHANGING p_node_key  TYPE lvc_nkey.

  DATA:
    lv_node_text TYPE lvc_value,
    ls_result    TYPE /gda/sdm_s_val_results,
    lv_rel       TYPE int4.

  lv_node_text = p_text.

  CASE p_type.
    WHEN '1'.
      lv_rel = cl_gui_column_tree=>relat_last_child.
    WHEN '2'.
      lv_rel = cl_gui_column_tree=>relat_last_sibling.
    WHEN OTHERS.
      lv_rel = cl_gui_column_tree=>relat_last_child.
  ENDCASE.


  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = lv_rel "CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      i_node_text      = lv_node_text
      is_outtab_line   = ls_result
*     is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " ADD_ID

FORM add_id  USING    p_relat_key TYPE lvc_nkey
                      p_result    TYPE /gda/sdm_s_val_results "ZCA_BRF_VAL_RETURN_GUI
            CHANGING  p_node_key  TYPE lvc_nkey.

  DATA:
   l_node_text TYPE lvc_value.

  l_node_text = p_result-number.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = p_result
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " ADD_ID


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

FORM determine_icon USING p_matnr
                          p_node_level
                          p_value_01
*                          p_header_subkey
                          p_value_02
*                          p_no_leaf
                          p_result
                          p_main_setup TYPE /gda/sdm_s_main
                 CHANGING p_image
                          p_image2.

  DATA:
    lv_field TYPE field,
    lv_table TYPE struc1.
*    ls_result          TYPE /gda/sdm_s_val_results,
*    lv_mess_id_last(6) TYPE c.

  FIELD-SYMBOLS:
    <field>    TYPE any,
    <brf_key>  TYPE any,
    <brf_key6> TYPE any.

* EXTRA_V1 contains table-field
  ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE p_result TO <field>.
*  IF <field>  EQ '/GDA/SDM_TARIFF-LAND1'.
*    BREAK-POINT.
*  endif.
  IF <field> IS ASSIGNED.
    SPLIT <field> AT '-' INTO lv_table-table lv_field.
    READ TABLE p_main_setup-tabstruc WITH KEY fieldname = lv_field
                                              tabname   = lv_table-table TRANSPORTING NO FIELDS.
*    IF sy-subrc <> 0.
*      EXIT.
  ENDIF.
*  ENDIF.
*

  IF sy-subrc = 0.
    ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE p_result TO <brf_key>.
    IF <brf_key> = space.
      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE p_result TO <brf_key>.
    ENDIF.

* Additional key field data
    ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE p_result TO <brf_key6>.
    IF <brf_key6> IS ASSIGNED AND <brf_key6> IS NOT INITIAL.

    ENDIF.

*  IF lv_table = 'MARA'.
    IF <brf_key> IS INITIAL.
      <brf_key> = p_matnr.
    ENDIF.

    IF p_value_02 IS INITIAL.
      IF p_value_01 = <brf_key>.
        p_image = icon_failure.
      ELSE.
        p_image = icon_positive.
      ENDIF.
    ELSE.
      IF p_value_01 = <brf_key> AND p_value_02 = <brf_key6>.
        p_image = icon_failure.
      ELSE.
        p_image = icon_positive.
      ENDIF.

    ENDIF.
  ENDIF.

* Special condition! - Consider an exit for this..
  IF p_node_level = 'WERKS'.
    DATA:
      ls_t001w TYPE t001w.

    SELECT SINGLE * FROM t001w
                        INTO ls_t001w
                      WHERE werks = p_value_01.
*                       AND vlfkz = 'A'.

    IF ls_t001w-vlfkz = 'A'.
      p_image2 = icon_store_location. "ICON_DISTRIBUTION
    ELSE.
      p_image2 = icon_distribution. "ICON_DISTRIBUTION
    ENDIF.
  ENDIF.
ENDFORM.                    " ADD_CONTEXT_ERRORS
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM determine_selection .
  IF ( s_infnr IS NOT INITIAL OR s_ekorg IS NOT INITIAL OR s_lifnr IS NOT INITIAL )." AND s_matnr IS INITIAL.
    gv_eina_first = abap_true.
  ENDIF.

  IF ( s_vdatu IS NOT INITIAL OR s_bdatu  IS NOT INITIAL ).
    gv_eord_first = abap_true.
  ENDIF.

  IF s_vkorg IS NOT INITIAL OR s_vtweg IS NOT INITIAL.
    gv_mvke_first = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_STRUCTURED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material_structured .
  DATA:
    lt_mara_variants TYPE STANDARD TABLE OF /gda/sdm_s_mara_01,
    lt_mast          TYPE STANDARD TABLE OF mast,
    lt_stpo          TYPE STANDARD TABLE OF stpo,
    lt_relations     TYPE STANDARD TABLE OF struc_rel,
    lt_mara          TYPE STANDARD TABLE OF /gda/sdm_s_mara_01.

  FIELD-SYMBOLS:
    <relations1>    LIKE LINE OF gt_relations,
    <relations2>    LIKE LINE OF lt_relations,
    <mara>          LIKE LINE OF gt_mara,
    <mara_variants> LIKE LINE OF lt_mara_variants.

  RANGES: r_attyp FOR mara-attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '00'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '01'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '02'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '10'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '11'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '12'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '21'.
  APPEND  r_attyp.

  r_attyp-sign   = 'I'.
  r_attyp-option = 'EQ'.
  r_attyp-low    = '22'.
  APPEND  r_attyp.

  SELECT * FROM mara
           INTO CORRESPONDING FIELDS OF TABLE gt_mara
           WHERE matnr IN s_matnr
             AND ersda IN s_ersda
             AND ernam IN s_ernam
             AND laeda IN s_laeda
             AND aenam IN s_aenam
             AND mtart IN s_mtart
             AND matkl IN s_matkl
             AND mstae IN s_mstae
             AND attyp IN r_attyp.

* Cater for Structured Articles.
  IF p_struc = abap_true.
    REFRESH:
     lt_mara_variants.

* Strip out any items which are not top level!
    DELETE gt_mara WHERE attyp = '00'.

    LOOP AT gt_mara ASSIGNING <mara_struc>. " WHERE attyp <> '00'.

      CASE <mara_struc>-attyp.
* Generics & Variants:
        WHEN '01'.
          SELECT * FROM mara
                   INTO CORRESPONDING FIELDS OF TABLE lt_mara_variants
                   WHERE satnr EQ <mara_struc>-matnr.

          LOOP AT lt_mara_variants ASSIGNING <mara_variants>.
            gs_relations-matnr     = <mara_struc>-matnr.
            gs_relations-matnr_rel = <mara_variants>-matnr.
            APPEND gs_relations TO gt_relations.
            CLEAR   gs_relations.
          ENDLOOP.

          APPEND LINES OF lt_mara_variants TO gt_mara.

* Pre-Pack, Sales Sets, Display Articles etc
        WHEN OTHERS.

          SELECT matnr stlnr FROM mast
                             INTO CORRESPONDING FIELDS OF TABLE lt_mast
                             WHERE matnr = <mara_struc>-matnr.

          IF lt_mast IS NOT INITIAL.
            SELECT idnrk FROM stpo
                         INTO CORRESPONDING FIELDS OF TABLE lt_stpo
                         FOR ALL ENTRIES IN lt_mast
                         WHERE stlnr =  lt_mast-stlnr.
* Now get these entries in the MARA struc
            IF lt_stpo IS NOT INITIAL.
              SELECT * FROM mara
                       INTO CORRESPONDING FIELDS OF TABLE lt_mara_variants
                       FOR ALL ENTRIES IN lt_stpo
                       WHERE matnr EQ lt_stpo-idnrk.

              LOOP AT lt_mara_variants ASSIGNING <mara_variants>.
                gs_relations-matnr     = <mara_struc>-matnr.
                gs_relations-matnr_rel = <mara_variants>-matnr.
                APPEND gs_relations TO gt_relations.
                CLEAR   gs_relations.
              ENDLOOP.

              APPEND LINES OF lt_mara_variants TO gt_mara.
            ENDIF.

          ENDIF.
      ENDCASE.

    ENDLOOP.

* GS_RELATIONS
*    APPEND LINES OF GT_MARA_VARIANTS TO GT_MARA.
    SORT gt_mara.
    DELETE ADJACENT DUPLICATES FROM gt_mara.

    lt_relations[] = gt_relations[].

    LOOP AT gt_relations ASSIGNING <relations1>.
      LOOP AT lt_relations ASSIGNING <relations2> WHERE matnr <> <relations1>-matnr
                                                    AND matnr_rel = <relations1>-matnr_rel.
        READ TABLE gt_mara ASSIGNING <mara> WITH KEY matnr = <relations1>-matnr_rel.
        APPEND <mara> TO lt_mara.
        EXIT.
* this entry should be added back
      ENDLOOP.
    ENDLOOP.
    SORT lt_mara.
    DELETE ADJACENT DUPLICATES FROM lt_mara.
    APPEND LINES OF lt_mara TO gt_mara.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material .
  DATA:
   lt_matnr    TYPE table_matnr,
   lv_function TYPE string.

  FIELD-SYMBOLS:
    <eina> LIKE LINE OF gt_eina,
    <eord> LIKE LINE OF gt_eord,
    <mvke> LIKE LINE OF gt_mvke.

  IF sy-saprl >= gc_sap_version.
    lv_function = '/GDA/SDM_MM_MARA_GET_NEW'.
  ELSE.
    lv_function = '/GDA/SDM_MM_MARA_GET_OLD'.
  ENDIF.

  IF gv_eina_first = abap_true.
* Note -- Possibly include a join on material to ensure we have an article..
    SELECT * FROM eina INTO CORRESPONDING FIELDS OF TABLE gt_eina
               WHERE matnr      IN s_matnr
               AND   matkl      IN s_matkl
               AND   lifnr      IN s_lifnr
               AND   infnr      IN s_infnr.
    IF sy-subrc = 0.
      LOOP AT gt_eina ASSIGNING <eina>.
        COLLECT <eina>-matnr INTO lt_matnr.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gv_eord_first = abap_true.
    SELECT * FROM eord INTO CORRESPONDING FIELDS OF TABLE gt_eord
             WHERE matnr      IN s_matnr
               AND werks      IN s_werks
               AND lifnr      IN s_lifnr
               AND vdatu      IN s_vdatu
               AND bdatu      IN s_bdatu.
    IF sy-subrc = 0.
      LOOP AT gt_eord ASSIGNING <eord>.
        COLLECT <eord>-matnr INTO lt_matnr.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gv_mvke_first = abap_true.
    SELECT * FROM mvke INTO CORRESPONDING FIELDS OF TABLE gt_mvke
             WHERE matnr      IN s_matnr
               AND vkorg      IN s_vkorg
               AND vtweg      IN s_vtweg.
    IF sy-subrc = 0.
      LOOP AT gt_mvke ASSIGNING <mvke>.
        COLLECT <mvke>-matnr INTO lt_matnr.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gv_eina_first = abap_true OR gv_eord_first = abap_true OR gv_mvke_first = abap_true.
    IF lt_matnr IS NOT INITIAL.
      CALL FUNCTION lv_function
        EXPORTING
          x_max_rows   = p_max
          xt_materials = lt_matnr
        IMPORTING
          xt_mara      = gt_mara[]
        TABLES
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
    ENDIF.

  ELSE.
    CALL FUNCTION lv_function
      EXPORTING
        x_max_rows = p_max
      IMPORTING
        xt_mara    = gt_mara[]
      TABLES
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
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sdm_main_article.
  DATA:
    percent           TYPE p,
    percent_text      TYPE string,
    message           TYPE string.

  PERFORM progress_bar USING text-018.

  DESCRIBE TABLE go_selection->mt_mara LINES DATA(count).
  PERFORM progress_bar USING text-018.

  LOOP AT go_selection->mt_mara INTO go_selection->ms_mara_spec.
    CLEAR:
      percent,
      percent_text,
      message.

    percent = ( sy-tabix / count ) * 100.
    percent_text = percent.
    CONCATENATE 'BRF Rules '(917)  percent_text '%' INTO message.
    PERFORM progress_bar USING message.

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
    gt_mlea_sdm[]   = go_selection->mt_mlea_spec[].
    gt_malg_sdm[]   = go_selection->mt_malg_spec[].
    gt_mpgd_sdm[]   = go_selection->mt_mpgd_spec[].
    gt_wrpl_sdm[]   = go_selection->mt_wrpl_spec[].
    gt_mamt_sdm[]   = go_selection->mt_mamt_spec[].
    gt_myms_sdm[]   = go_selection->mt_myms_spec[].
    gt_rmmw1_sdm[]   = go_selection->mt_rmmw1_spec[].
*    gt_steuer_sdm[] = go_selection->mt_steuertab_spec[].
*    gt_steumm_sdm[] = go_selection->mt_steummtab_spec[].
    gt_eord_sdm[]   = go_selection->mt_eord_spec[].
    gt_eina_sdm[]   = go_selection->mt_eina_spec[].
    gt_eine_sdm[]   = go_selection->mt_eine_spec[].
    gt_konh_sdm[]   = go_selection->mt_konh_spec[].
    gt_tariff_sdm   = go_selection->mt_tariff_spec[].
    gt_mat_steur_sdm = go_selection->mt_mg03_spec[].
*    gt_mg03_sdm_brf[] = go_selection->mt_mg03_spec[].
    gs_syst_sdm     = syst.
    APPEND go_selection->ms_makt_spec TO gt_makt_sdm[].

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP3 SPOTS /GDA/SDM_MM_MAT_ES4 .
**$*$-Start: /GDA/SDM_MM_MAT_EP3-----------------------------------------------------------------$*$*
*ENHANCEMENT 1  ZSDM_MM_MAT_ES4.    "active version
*   GT_CUSTOM_TEST_SDM[] = go_selection->MT_CUSTOM_TEST_spec[].
*ENDENHANCEMENT.
*$*$-End:   /GDA/SDM_MM_MAT_EP3-----------------------------------------------------------------$*$*

* For derivations..
    READ TABLE go_selection->mt_marc_spec[] INTO gs_marc_sdm INDEX 1.
*    read table go_selection->mt_mlgt_spec[] into gs_mlgt_sdm index 1.
    READ TABLE go_selection->mt_mvke_spec[] INTO gs_mvke_sdm INDEX 1.


* For each Material process the BRF Functions
    LOOP AT gt_objects ASSIGNING <objects>.
      CLEAR:
       <objects>-object.
      PERFORM brf_logic  USING <objects>-type
                               <objects>-mapping
                               <objects>-stewardship
                         CHANGING <objects>-object.

      IF <objects>-object IS NOT BOUND OR  <objects>-object->mt_message IS NOT INITIAL.
        gv_config_err = abap_true.
        EXIT.
      ENDIF.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      APPEND gs_instance TO gs_sdm_objects-sdm_instances.
    ENDLOOP.

    CHECK gv_config_err = abap_false.

    gs_sdm_objects-article = go_selection->ms_mara_spec-matnr.
    gs_sdm_objects-attyp = go_selection->ms_mara_spec-attyp.
*****
    READ TABLE gt_relations ASSIGNING FIELD-SYMBOL(<relations>)
                            WITH KEY matnr_rel = go_selection->ms_mara_spec-matnr.
    IF sy-subrc = 0.
      gs_sdm_objects-linkage = <relations>-matnr.
    ELSE.
      IF go_selection->ms_mara_spec-attyp = '11' OR
         go_selection->ms_mara_spec-attyp =  '01' OR
        go_selection->ms_mara_spec-attyp = '10'  OR
        go_selection->ms_mara_spec-attyp = '12'.
        gs_sdm_objects-linkage = go_selection->ms_mara_spec-matnr.
      ENDIF.
    ENDIF.
    IF <relations> IS ASSIGNED.
      DELETE gt_relations WHERE matnr     = <relations>-matnr
                            AND matnr_rel = <relations>-matnr_rel.
    ENDIF.
****
    APPEND gs_sdm_objects TO gt_sdm_articles.

    CLEAR:
     gs_sdm_objects.
    REFRESH:
     gt_mara_sdm,gt_marc_sdm,gt_mard_sdm,gt_mbew_sdm,gt_mlgn_sdm,gt_makt_sdm,gt_eord_sdm,
     gt_mlan_sdm,gt_malg_sdm,gt_mamt_sdm,gt_maw1_sdm,gt_wlk1_sdm,gt_wlk2_sdm,gt_mwli_sdm,
     gt_konh_sdm,gt_rmmw1_sdm,gt_mlea_sdm,gt_mpgd_sdm,gt_wrpl_sdm.
  ENDLOOP.

  PERFORM progress_bar USING text-019.
ENDFORM.
