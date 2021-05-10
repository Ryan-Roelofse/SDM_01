
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sdm_logic .
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data,
    lv_field      TYPE fieldname,
    lv_count      TYPE p,
    lv_pur        TYPE p,
    lv_text       TYPE string,
    lv_pur_text   TYPE string,
    lv_exit.

  FIELD-SYMBOLS:
    <results>      TYPE STANDARD TABLE,
    <results_pp1>  TYPE STANDARD TABLE,
    <result>       TYPE any,
    <field_alv>    TYPE any,
    <objects>      LIKE LINE OF gt_objects.

*  PERFORM build_structure USING gc_default
*                                gc_object
*                                space.
  PERFORM build_structure USING gc_default
                                gc_object
                                p_struc.

  PERFORM build_dynamic_itab USING gc_default
                             CHANGING ro_data.

  PERFORM progress_bar USING text-018.

  DESCRIBE TABLE go_selection->mt_mara LINES lv_count.

  LOOP AT go_selection->mt_mara INTO go_selection->ms_mara_spec.
    CLEAR:
      lv_pur_text,
      lv_pur,
      lv_text.

    lv_pur = ( sy-tabix / lv_count ) * 100.
    lv_pur_text = lv_pur.
    CONCATENATE 'BRF Rules '(917)  lv_pur_text '%' INTO lv_text.
    PERFORM progress_bar USING lv_text.

* Set all Default Views to icon successful
    PERFORM default_view_icons.
* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
*    go_selection->refresh_spec( ).
    go_selection->refresh( ).
    go_selection->mv_object = go_selection->ms_mara_spec-matnr.
    go_selection->build_spec( ).

    APPEND go_selection->ms_mara_spec TO gt_mara_sdm[].
    APPEND go_selection->ms_makt_spec TO gt_makt_sdm[].
    gs_mara_sdm     = go_selection->ms_mara_spec.
    gt_marc_sdm[]   = go_selection->mt_marc_spec[].
    gt_mard_sdm[]   = go_selection->mt_mard_spec[].
    gt_mbew_sdm[]   = go_selection->mt_mbew_spec[].
    gt_meinh_sdm[]  = go_selection->mt_meinh_spec[].
*    gt_mfhm_sdm[]   = go_selection->mt_mfhm_spec[].
    gt_mlgn_sdm[]   = go_selection->mt_mlgn_spec[].
    gt_mlgt_sdm[]   = go_selection->mt_mlgt_spec[].
    gt_mvke_sdm[]   = go_selection->mt_mvke_spec[].
    gt_mean_sdm[]   = go_selection->mt_mean_spec[].
    gt_mpop_sdm[]   = go_selection->mt_mpop_spec[].
    gt_marm_sdm[]   = go_selection->mt_marm_spec[].

    gt_maw1_sdm[]   = go_selection->mt_maw1_spec[].
    gt_eord_sdm[]   = go_selection->mt_eord_spec[].
    gt_eina_sdm[]   = go_selection->mt_eina_spec[].
    gt_mwli_sdm[]   = go_selection->mt_mwli_spec[].
    gt_wlk1_sdm[]   = go_selection->mt_wlk1_spec[].
    gt_wlk2_sdm[]   = go_selection->mt_wlk2_spec[].
    gt_mlan_sdm[]   = go_selection->mt_mlan_spec[].
    gt_mamt_sdm[]   = go_selection->mt_mamt_spec[].
    gt_malg_sdm[]   = go_selection->mt_malg_spec[].
    gt_myms_sdm[]   = go_selection->mt_myms_spec[].
    gt_tariff_sdm   = go_selection->mt_tariff_spec[].

*    gt_mlan_sdm[]   = go_selection->mt_mlan_spec[].
*    gt_steuer_sdm[] = go_selection->mt_steuertab_spec[].
*    gt_steumm_sdm[] = go_selection->mt_steummtab_spec[].
    gt_eine_sdm[]   = go_selection->mt_eine_spec[].
*    gt_eina_sdm[]   = go_selection->mt_eina_spec[].

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP3 SPOTS /GDA/SDM_MM_MAT_ES5 .

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
        IF sy-subrc = 0.
          IF <results_pp1> IS NOT INITIAL.
            APPEND LINES OF <results_pp1> TO <results>.
          ENDIF.
        ENDIF.
      ENDIF.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      APPEND gs_instance TO gs_sdm_objects-sdm_instances.
    ENDLOOP.

    CHECK gv_config_err = abap_false.

    IF <results> IS ASSIGNED.
      SORT <results>.
      DELETE ADJACENT DUPLICATES FROM <results>.

      IF <results> IS NOT INITIAL.
        LOOP AT <results> ASSIGNING <result>.

          PERFORM message_filter USING    <result>
                                 CHANGING lv_exit.

          CHECK lv_exit = abap_false.

          PERFORM message_context_link USING <result>
                                             gc_object.
        ENDLOOP.
      ENDIF.
    ENDIF.

* Ensure Default/Key fields are populated..
* The key fields can can only equate to a key  equal to the object type..
* Table that can be used
* MARA
* MAKT
    READ TABLE gt_makt_sdm INTO gs_makt_temp WITH KEY matnr = gs_mara_sdm-matnr.

    LOOP AT gt_alvtop_key_fields ASSIGNING FIELD-SYMBOL(<alvtop_key>).
      CLEAR:
       lv_field.
      CONCATENATE 'KEY_' <alvtop_key>-field INTO lv_field.
      ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa> TO <field_alv>.
      ASSIGN COMPONENT <alvtop_key>-field OF STRUCTURE gs_mara_sdm TO <field>.
      IF sy-subrc = 0.
        CHECK <field_alv> IS ASSIGNED.
        <field_alv> = <field>.
      ELSE.
        ASSIGN COMPONENT <alvtop_key>-field OF STRUCTURE gs_makt_temp TO <field>.
        CHECK sy-subrc = 0.
        CHECK <field_alv> IS ASSIGNED.
        <field_alv> = <field>.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_alvtop_default_fields ASSIGNING FIELD-SYMBOL(<alv_top_default>).
      ASSIGN COMPONENT <alv_top_default>-field OF STRUCTURE <dyn_wa> TO <field_alv>.
      ASSIGN COMPONENT <alv_top_default>-field OF STRUCTURE gs_mara_sdm TO <field>.
      IF sy-subrc = 0.
        CHECK <field_alv> IS ASSIGNED.
        <field_alv> = <field>.
      ELSE.
        ASSIGN COMPONENT <alv_top_default>-field OF STRUCTURE gs_makt_temp TO <field>.
        CHECK sy-subrc = 0.
        CHECK <field_alv> IS ASSIGNED.
        <field_alv> = <field>.
      ENDIF.
    ENDLOOP.

    gs_sdm_objects-article = gs_mara_sdm-matnr.
    gs_sdm_objects-mara[]   = gt_mara_sdm[].
    gs_sdm_objects-makt[]   = gt_makt_sdm[].
    gs_sdm_objects-marc[]   = gt_marc_sdm[].
    gs_sdm_objects-mard[]   = gt_mard_sdm[].
    gs_sdm_objects-mbew[]   = gt_mbew_sdm[].
*    gs_sdm_objects-meinh[]  = gt_meinh_sdm[].
*    gs_sdm_objects-mfhm[]   = gt_mfhm_sdm[].
*    gs_sdm_objects-mlgn[]   = gt_mlgn_sdm[].
*    gs_sdm_objects-mlgt[]   = gt_mlgt_sdm[].
    gs_sdm_objects-mvke[]   = gt_mvke_sdm[].
    gs_sdm_objects-mean[]   = gt_mean_sdm[].
    gs_sdm_objects-mpop[]   = gt_mpop_sdm[].
*    gs_sdm_objects-marm[]   = gt_marm_sdm[].
*    gs_sdm_objects-mlan[]   = gt_mlan_sdm[].
    gs_sdm_objects-eine[]   = gt_eine_sdm[].
    gs_sdm_objects-eina[]   = gt_eina_sdm[].

    gs_sdm_objects-maw1   = gt_maw1_sdm[].
    gs_sdm_objects-eord   = gt_eord_sdm[].
    gs_sdm_objects-eina   = gt_eina_sdm[].
    gs_sdm_objects-mwli   = gt_mwli_sdm[].
    gs_sdm_objects-wlk1   = gt_wlk1_sdm[].
    gs_sdm_objects-wlk2   = gt_wlk2_sdm[].
    gs_sdm_objects-myms   = gt_myms_sdm[].
    gs_sdm_objects-/gda/sdm_mlan   = gt_mlan_sdm[].
*    gs_sdm_objects-mamt   = gt_mamt_sdm[].
*    gs_sdm_objects-malg   = gt_malg_sdm[].
    gs_sdm_objects-/gda/sdm_tariff   = gt_tariff_sdm[].

*    gs_sdm_objects-/gda/mg03steuer[] = gt_steuer_sdm[].
*    gs_sdm_objects-/gda/mg03steumm[]  = gt_steumm_sdm[].
    gs_syst_sdm              = syst.

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP4 SPOTS /GDA/SDM_MM_MAT_ES6 .

    PERFORM determine_output USING   gs_sdm_objects
                             CHANGING  gt_sdm_article.

    CLEAR gs_sdm_objects.

    REFRESH:
     gt_mara_sdm,
     gt_makt_sdm,
     gt_marc_sdm,
     gt_mard_sdm,
     gt_mbew_sdm,
     gt_meinh_sdm,
*     gt_mfhm_sdm,
     gt_mlgn_sdm,
     gt_mlgt_sdm,
     gt_mvke_sdm,
     gt_mean_sdm,
     gt_mpop_sdm,
     gt_marm_sdm,

    gt_maw1_sdm,
    gt_eord_sdm,
    gt_eina_sdm,
    gt_eine_sdm,
    gt_mwli_sdm,
    gt_wlk1_sdm,
    gt_wlk2_sdm,
    gt_mlan_sdm,
    gt_mamt_sdm,
    gt_malg_sdm,
    gt_tariff_sdm.
*     gt_mlan_sdm,
*     gt_steuer_sdm,
*     gt_steumm_sdm.



    UNASSIGN:
     <results>,
     <results_pp1>.
  ENDLOOP.

  PERFORM progress_bar USING text-019.
ENDFORM.

FORM set_view_output_new USING x_column TYPE lvc_s_col x_status.

  DATA:
    ls_layout           TYPE lvc_s_layo,
    ro_data             TYPE REF TO data,
    ro_data_empty       TYPE REF TO data,
    lv_view             TYPE /gda/sdm_de_view,
    lt_sequence_primary TYPE STANDARD TABLE OF /gda/sdm_setup5,
    lt_sequence_second  TYPE STANDARD TABLE OF /gda/sdm_setup5,
    lv_field            TYPE field,
    lv_table            TYPE tabname16,
    lv_key_node         TYPE  field,
    lv_key_att          TYPE  field.


  FIELD-SYMBOLS:
    <field>        TYPE any,
    <field_check>  TYPE any,
    <field_check2> TYPE any,
    <brf_key>      TYPE any,
    <brf_key6>     TYPE any,
    <message>      TYPE any,
    <article>      TYPE any,
    <result>       TYPE any,
    <field_alv>    TYPE any,
    <results>      TYPE table,
    <results_temp> TYPE  table,
*    <view_table>   like line of gt_view_tables,
    <sdm_object>   LIKE LINE OF gt_sdm_article,
    <instances>    LIKE LINE OF <sdm_object>-sdm_instances.

  FIELD-SYMBOLS:
    <setup>           LIKE LINE OF gt_pp_main_setup,
    <primary>         LIKE LINE OF lt_sequence_primary,
    <secondary>       LIKE LINE OF lt_sequence_second,
    <table_primary>   TYPE ANY TABLE,
    <table_secondary> TYPE ANY TABLE,
    <line_primary>    TYPE any,
    <line_secondary>  TYPE any.

  PERFORM build_structure USING x_column
                                gc_object
                                space.
  PERFORM build_dynamic_itab USING x_column
                             CHANGING ro_data.

  REFRESH:
   <dyn_table_view>.

  IF x_status <> '@08@'.
    lv_view = x_column.

    MOVE-CORRESPONDING <dyn_wa> TO <dyn_wa_view>.
    ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <article>.

    READ TABLE gt_sdm_article ASSIGNING <sdm_object> WITH KEY article = <article>.
    CHECK sy-subrc = 0.
    IF <sdm_object> IS ASSIGNED.
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

        IF <results_temp> IS ASSIGNED AND <results_temp> IS NOT INITIAL.
          APPEND LINES OF <results_temp> TO <results>.
        ENDIF.
      ENDLOOP.

      SORT <results>.
      DELETE ADJACENT DUPLICATES FROM <results>.

      READ TABLE gt_pp_main_setup ASSIGNING <setup> WITH KEY object_view = lv_view.

      CHECK sy-subrc = 0.
      lt_sequence_primary[] =  <setup>-sequence[].
      LOOP AT lt_sequence_primary ASSIGNING <primary> WHERE seq = '01'.
*        lv_key_table = <primary>-tabname.
        lv_key_node  = <primary>-node_level.

        ASSIGN COMPONENT <primary>-tabname OF STRUCTURE <sdm_object> TO <table_primary>.
        LOOP AT <table_primary> ASSIGNING <line_primary>.
          MOVE-CORRESPONDING <line_primary> TO <dyn_wa_view>.
* populate output with Secondary table values
          LOOP AT lt_sequence_second ASSIGNING <secondary> WHERE seq NE '01'.
            ASSIGN COMPONENT <secondary>-tabname OF STRUCTURE <sdm_object> TO <table_secondary>.
            LOOP AT <table_secondary> ASSIGNING <line_secondary>.
              MOVE-CORRESPONDING <line_secondary> TO <dyn_wa_view>.
            ENDLOOP.
          ENDLOOP.

          APPEND <dyn_wa_view> TO <dyn_table_view>.
        ENDLOOP.
      ENDLOOP.


      LOOP AT <dyn_table_view> ASSIGNING <dyn_wa_view>.

        LOOP AT <results> ASSIGNING <result>.
* we now have a valid error for the line
          ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
          IF <field> IS ASSIGNED.
            SPLIT <field> AT '-' INTO lv_table lv_field.
            READ TABLE <setup>-tabstruc  WITH KEY fieldname = lv_field
                                                  tabname   = lv_table TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.
          ENDIF.

* Potential Valid error found
* Key Field Value
          ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key>.
          IF <brf_key> IS ASSIGNED AND <brf_key> IS INITIAL.
* Then set to material
            <brf_key>     =  <article>.
            lv_key_node = 'KEY_MATNR'.
          ENDIF.

* get Key Field attribute Value
          ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
          IF <brf_key6> IS ASSIGNED AND <brf_key6> IS NOT INITIAL.
* Special Conditions - Get corresponding key field attribute value from context
            IF lv_key_node = 'VKORG'.
              lv_key_att = 'VTWEG'.
              ASSIGN COMPONENT lv_key_att OF STRUCTURE <dyn_wa_view> TO <field_check2>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT lv_key_node OF STRUCTURE <dyn_wa_view> TO <field_check>. "lv_field

          IF sy-subrc = 0 AND <brf_key> IS ASSIGNED AND <brf_key6> IS INITIAL.
            IF <brf_key> = <field_check>.
* Pass the BRF message to the screen
              ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <result> TO <message>.
              IF sy-subrc = 0 AND <message> IS ASSIGNED.
                ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <dyn_wa_view> TO <field_alv>.
                IF sy-subrc = 0 AND <field_alv> IS ASSIGNED.
                  <field_alv> = <message>.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSEIF sy-subrc = 0 AND <brf_key> IS ASSIGNED AND <brf_key6> IS NOT INITIAL.
            IF <brf_key> = <field_check> AND <brf_key6> = <field_check2>.
** Pass the BRF message to the screen
              ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <result> TO <message>.
              IF sy-subrc = 0 AND <message> IS ASSIGNED.
                ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <dyn_wa_view> TO <field_alv>.
                IF sy-subrc = 0 AND <field_alv> IS ASSIGNED.
                  <field_alv> = <message>.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

** Is this context field found in the current structure?
*        ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa_view> TO <field_context>. "lv_key_att
*        IF sy-subrc = 0 AND <field_context> IS ASSIGNED.
*
** get Key Field
*          ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key>.
*          IF <brf_key> IS ASSIGNED AND <brf_key> IS INITIAL.
** Then set to material
*            <brf_key>     =  <material>.
*            lv_key_node = 'KEY_MATNR'.
*          ENDIF.
*
*
** get Key Field attribute
*          ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
*          IF <brf_key6> IS ASSIGNED AND <brf_key6> IS NOT INITIAL.
** Special Conditions
*            IF lv_key_node = 'VKORG'.
*              lv_key_att = 'VTWEG'.
*              ASSIGN COMPONENT lv_key_att OF STRUCTURE <dyn_wa_view> TO <field_check2>.
*            ENDIF.
*          ENDIF.
*
*          IF sy-subrc = 0 AND <brf_key> IS ASSIGNED AND <brf_key6> IS INITIAL.
*            IF <brf_key> = <field_context>.
** Pass the BRF message to the screen
*              ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <result> TO <message>.
*              IF sy-subrc = 0 AND <message> IS ASSIGNED.
*                ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <dyn_wa_view> TO <field_alv>.
*                IF sy-subrc = 0 AND <field_alv> IS ASSIGNED.
*                  <field_alv> = <message>.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ELSEIF sy-subrc = 0 AND <brf_key> IS ASSIGNED AND <brf_key6> IS NOT INITIAL.
*            IF <brf_key> = <field_context> AND <brf_key6> = <field_check2>.
** Pass the BRF message to the screen
*              ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <result> TO <message>.
*              IF sy-subrc = 0 AND <message> IS ASSIGNED.
*                ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <dyn_wa_view> TO <field_alv>.
*                IF sy-subrc = 0 AND <field_alv> IS ASSIGNED.
*                  <field_alv> = <message>.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF go_alv IS BOUND.
    go_alv->free( ).
    FREE go_alv.
  ENDIF.

  IF go_tree IS BOUND.
    go_tree->free( ).
    FREE go_tree.
  ENDIF.

* create an instance of alv control
  CREATE OBJECT go_alv
    EXPORTING
      i_parent = go_parent2.

  ls_layout-cwidth_opt = abap_true.

  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = lv_view.

  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = <main_setup>-tabstruc[]
      it_outtab       = <dyn_table_view>.

ENDFORM.

FORM set_view_output_tree USING x_column x_status.
  DATA:
    ls_hier_hdr TYPE treev_hhdr,
    ls_variant  TYPE disvariant,
    lt_keys     TYPE lvc_t_nkey.
*    lt_result   TYPE STANDARD TABLE OF /gda/sdm_s_val_results. " Empty

  FIELD-SYMBOLS:
    <article>     TYPE any,
    <description> TYPE any.

  PERFORM build_structure    USING x_column
                                   gc_object
                                   space.
  PERFORM build_dynamic_itab USING x_column
                             CHANGING ro_data.

  REFRESH:
   <dyn_table_view>.

* Set key fields..
  MOVE-CORRESPONDING <dyn_wa> TO <dyn_wa_view>.
  ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <article>.

*  CLEAR: GS_MARA.
  READ TABLE gt_mara INTO gs_mara WITH KEY matnr = <article>.
  MOVE-CORRESPONDING gs_mara TO <dyn_wa_view>.

  CLEAR:
   gs_makt.
  IF gt_makt IS NOT INITIAL.
    ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <dyn_wa_view> TO <description>.
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = <article>.
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
                                <article>
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
    <sdm_object>      LIKE LINE OF gt_sdm_article,
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

  READ TABLE gt_sdm_article ASSIGNING <sdm_object> WITH KEY article = x_matnr.

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
                          AND ddlanguage = sy-langu
                          AND as4local   = 'A'.
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
    lv_key_combo TYPE char30. "string.

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

FORM mass_download.
  DATA:
    template TYPE sy-repid.

  PERFORM process_spreadsheet.
  EXPORT template TO MEMORY ID 'TEMPLATE'.

*Create SAP Document
  PERFORM create_sapdoc.

ENDFORM.


FORM pop_main.
*  DATA:
*   new,
*   tabix LIKE sy-tabix.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
    CONCATENATE '<dyn_wa>-' range_item-name INTO name.

    ASSIGN (name) TO <cell>.

    CHECK sy-subrc = 0.

    WRITE <cell> TO gs_tab LEFT-JUSTIFIED.

    APPEND gs_tab TO gt_tab.
  ENDLOOP.

ENDFORM.

FORM pop_main_details.
  DATA:
*    new,
*    tabix         LIKE sy-tabix,
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data.

  FIELD-SYMBOLS:
    <articles>     LIKE LINE OF gt_sdm_article,
    <instances>    LIKE LINE OF <articles>-sdm_instances,
    <results>      TYPE STANDARD TABLE,
    <results_temp> TYPE STANDARD TABLE,
    <result>       TYPE any.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT gt_sdm_article ASSIGNING <articles>.

* Test changes
    UNASSIGN <results>.

    LOOP AT <articles>-sdm_instances ASSIGNING <instances>.
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

    LOOP AT <results> ASSIGNING <result>.
      IF range_item-name = 'MATNR'.
        name = '<articles>-article'.
      ELSE.
        CONCATENATE '<result>-' range_item-name INTO name.
      ENDIF.

      ASSIGN (name) TO <cell>.

      CHECK sy-subrc = 0.

      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.

      APPEND gs_tab TO gt_tab.
    ENDLOOP.

  ENDLOOP.
ENDFORM.

FORM pop_context_details.
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data.

  FIELD-SYMBOLS:
    <articles>     LIKE LINE OF gt_sdm_article,
    <instances>    LIKE LINE OF <articles>-sdm_instances,
    <results>      TYPE STANDARD TABLE,
    <results_temp> TYPE STANDARD TABLE,
    <result>       TYPE any.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT gt_sdm_article ASSIGNING <articles>.

    UNASSIGN <results>.

    LOOP AT <articles>-sdm_instances ASSIGNING <instances>.
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
* RROELOFSE
*    LOOP AT <results> ASSIGNING <result>.
*      IF range_item-name = 'MATNR'.
*        name = '<articles>-article'.
*      ELSE.
*        CONCATENATE '<result>-' range_item-name INTO name.
*      ENDIF.
*
*      ASSIGN (name) TO <cell>.
*
*      CHECK sy-subrc = 0.
*
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*
*      APPEND gs_tab TO i_ztab.
*    ENDLOOP.

  ENDLOOP.

ENDFORM.

FORM pop_calcs_details.
  DATA:
    ro_data           TYPE REF TO data,
    ro_data_empty     TYPE REF TO data,
    ro_download_table TYPE REF TO data,
    ls_calcs1         TYPE /gda/sdm_s_calcs_message,
    ls_calcs2         TYPE /gda/sdm_s_calcs_mtart,
*    ls_calcs3         TYPE /gda/sdm_s_calcs_mstae,
    ls_calcs4         TYPE /gda/sdm_s_calcs_matkl,
*    ls_calcs5         TYPE /gda/sdm_s_calcs_attyp,
    ls_wgbez60        TYPE wgbez60,
    sort_field        TYPE tabname.


  FIELD-SYMBOLS:
    <articles>       LIKE LINE OF gt_sdm_article,
    <instances>      LIKE LINE OF <articles>-sdm_instances,
    <results>        TYPE STANDARD TABLE,
    <results_temp>   TYPE STANDARD TABLE,
    <result>         TYPE any,
*    <type>           type any,
    <id>             TYPE any,
    <number>         TYPE any,
*    <message>        type any,
    <calcs1>         LIKE LINE OF gt_calcs1,
    <calcs2>         LIKE LINE OF gt_calcs2,
    <calcs3>         LIKE LINE OF gt_calcs3,
    <calcs4>         LIKE LINE OF gt_calcs4,
    <calcs5>         LIKE LINE OF gt_calcs5,
    <field>          TYPE any,
     <check>         TYPE any,
    <table_download> TYPE STANDARD TABLE,
    <download>       TYPE any.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  IF gt_calcs1 IS INITIAL.
    LOOP AT gt_sdm_article ASSIGNING <articles>.

      UNASSIGN <results>.

      LOOP AT <articles>-sdm_instances ASSIGNING <instances>.
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

      LOOP AT <results> ASSIGNING <result>.
        ASSIGN COMPONENT 'ID'      OF STRUCTURE <result> TO <id>.
        ASSIGN COMPONENT 'NUMBER'  OF STRUCTURE <result> TO <number>.

        SELECT SINGLE text FROM t100
                           INTO ls_calcs1-message
                      WHERE sprsl = sy-langu
                        AND arbgb = <id>
                        AND msgnr = <number>.
        IF sy-subrc = 0.
          CONCATENATE <number> '-' ls_calcs1-message INTO ls_calcs1-message.
          ls_calcs1-count1  = '1'.
          COLLECT ls_calcs1 INTO gt_calcs1.
          CLEAR ls_calcs1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT gt_calcs1 BY count1 DESCENDING.
  ENDIF.

  CREATE DATA ro_download_table LIKE <dyn_table>.

  ASSIGN ro_download_table->* TO <table_download>.
  <table_download>[] = <dyn_table>[].


* Material Type
  IF gt_calcs2 IS INITIAL.
    READ TABLE <table_download> ASSIGNING <download> INDEX 1.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_MTART' OF STRUCTURE <download> TO <check>.
    IF sy-subrc = 0.
      sort_field = 'KEY_MTART'.
    ELSE.
      ASSIGN COMPONENT 'MTART' OF STRUCTURE <download> TO <check>.
      IF sy-subrc = 0.
        sort_field = 'MTART'.
      ENDIF.
    ENDIF.

    IF sort_field IS NOT INITIAL.
      SORT <table_download> BY (sort_field).
      LOOP AT <table_download> ASSIGNING <dyn_wa>.
        ASSIGN COMPONENT sort_field OF STRUCTURE <dyn_wa> TO <field>.
        ls_calcs2-count2  = '1'.
        ls_calcs2-key_mtart  = <field>.
        COLLECT ls_calcs2 INTO gt_calcs2.
        CLEAR ls_calcs2.
      ENDLOOP.
      SORT gt_calcs2 BY count2 DESCENDING.
    ENDIF.
    CLEAR:
     sort_field.
  ENDIF.

* Material Group
  IF gt_calcs4 IS INITIAL.
    READ TABLE <table_download> ASSIGNING <download> INDEX 1.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_MATKL' OF STRUCTURE <download> TO <check>.
    IF sy-subrc = 0.
      sort_field = 'KEY_MATKL'.
    ELSE.
      ASSIGN COMPONENT 'MATKL' OF STRUCTURE <download> TO <check>.
      IF sy-subrc = 0.
        sort_field = 'MATKL'.
      ENDIF.
    ENDIF.

    IF sort_field IS NOT INITIAL.
      SORT <table_download> BY (sort_field).
      LOOP AT <table_download> ASSIGNING <dyn_wa>.
        ASSIGN COMPONENT sort_field OF STRUCTURE <dyn_wa> TO <field>.
        ls_calcs4-count4  = '1'.
        ls_calcs4-key_matkl  = <field>.
        COLLECT ls_calcs4 INTO gt_calcs4.
        CLEAR ls_calcs4.
      ENDLOOP.

      LOOP AT gt_calcs4 ASSIGNING <calcs4>.
        SELECT SINGLE wgbez FROM t023t INTO ls_wgbez60
             WHERE spras = sy-langu
               AND matkl = <calcs4>-key_matkl.
        CHECK sy-subrc = 0.
        CONCATENATE <calcs4>-key_matkl '-' ls_wgbez60 INTO <calcs4>-key_matkl.
      ENDLOOP.

      SORT gt_calcs4 BY count4 DESCENDING.
      CLEAR:
       sort_field.
    ENDIF.

  ENDIF.

*  if gt_calcs5 is initial.
*    SORT <table_download> BY ('KEY_ATTYP').
*    LOOP AT <table_download> ASSIGNING <dyn_wa>.
*      ASSIGN COMPONENT 'KEY_ATTYP' OF STRUCTURE <dyn_wa> TO <field>.
*      ls_calcs5-count5  = '1'.
*      ls_calcs5-key_attyp  = <field>.
*      COLLECT ls_calcs5 INTO gt_calcs5.
*      CLEAR ls_calcs5.
*    ENDLOOP.

*    sort gt_calcs5 by count5 descending.
*  endif.


  IF range_item-name = 'MESSAGE' OR range_item-name = 'COUNT1'.
    LOOP AT gt_calcs1 ASSIGNING <calcs1>.
      CONCATENATE '<calcs1>-' range_item-name INTO name.
      ASSIGN (name) TO <cell>.
      CHECK sy-subrc = 0.
      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
      IF range_item-name CS 'COUNT1'.
        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
      ENDIF.
      APPEND gs_tab TO gt_tab.
    ENDLOOP.
  ENDIF.

  IF range_item-name = 'KEY_MTART' OR range_item-name = 'MTART' OR range_item-name = 'COUNT2'.
    LOOP AT gt_calcs2 ASSIGNING <calcs2>.
      CONCATENATE '<calcs2>-' range_item-name INTO name.
      ASSIGN (name) TO <cell>.
      CHECK sy-subrc = 0.
      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
      IF range_item-name CS 'COUNT2'.
        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
      ENDIF.
      APPEND gs_tab TO gt_tab.
    ENDLOOP.
  ENDIF.

  IF range_item-name = 'MSTAE' OR range_item-name = 'COUNT3'.
    LOOP AT gt_calcs3 ASSIGNING <calcs3>.
      CONCATENATE '<calcs3>-' range_item-name INTO name.
      ASSIGN (name) TO <cell>.
      CHECK sy-subrc = 0.
      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
      IF range_item-name CS 'COUNT3'.
        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
      ENDIF.
      APPEND gs_tab TO gt_tab.
    ENDLOOP.
  ENDIF.

  IF range_item-name = 'KEY_MATKL' OR range_item-name = 'MATKL' OR range_item-name = 'COUNT4'.
    LOOP AT gt_calcs4 ASSIGNING <calcs4>.
      CONCATENATE '<calcs4>-' range_item-name INTO name.
      ASSIGN (name) TO <cell>.
      CHECK sy-subrc = 0.
      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
      IF range_item-name CS 'COUNT4'.
        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
      ENDIF.
      APPEND gs_tab TO gt_tab.
    ENDLOOP.
  ENDIF.

  IF range_item-name = 'KEY_ATTYP' OR range_item-name = 'ATTYP' OR range_item-name = 'COUNT5'.
    LOOP AT gt_calcs5 ASSIGNING <calcs5>.
      CONCATENATE '<calcs5>-' range_item-name INTO name.
      ASSIGN (name) TO <cell>.
      CHECK sy-subrc = 0.
      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
      IF range_item-name CS 'COUNT5'.
        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
      ENDIF.
      APPEND gs_tab TO gt_tab.
    ENDLOOP.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPREADSHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_spreadsheet .
* Populate main sheet
  PERFORM pop_main_sheet.
* Populate main sheet
  PERFORM pop_details_sheet.
* Populate context sheet
  PERFORM pop_context_sheet.
* Populate Calcs sheet
  PERFORM pop_calcs_sheet.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POP_MAIN_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_main_sheet.
  CONSTANTS:
   extra(9) VALUE 'KEY_EXTRA'.

  DATA:
    fldcat   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
    lt_views TYPE STANDARD TABLE OF /gda/sdm_setup3,
    VALUE(1).

  FIELD-SYMBOLS:
    <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[],
    <fieldname>   TYPE any,
    <views>       LIKE LINE OF lt_views.

  MOVE 'DATA' TO v_sheet.

*Starting row
  MOVE '1' TO v_row.
*Starting column
  MOVE '0' TO v_col.

  LOOP AT <main_setup>-tabstruc ASSIGNING <fieldsymbol>.
    ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fieldsymbol> TO <fieldname>.
    IF <fieldname> = 'LINKAGE'.
      DATA(count) = sy-tabix - 1.
      DATA(remainder) = 10 - count.
      DO remainder TIMES.
        ADD 1 TO value.
        CONCATENATE extra value INTO fldcat-fieldname.
        APPEND fldcat.
        CLEAR fldcat.
      ENDDO.
    ENDIF.
    MOVE-CORRESPONDING <fieldsymbol> TO fldcat.
    APPEND fldcat.
    CLEAR fldcat.
    IF <fieldname> = 'LINKAGE'.
* Display All Views in Spreedsheet and hide ones not populated.
      EXIT.
    ENDIF.
  ENDLOOP.

  SELECT * FROM /gda/sdm_setup3 INTO TABLE lt_views
           WHERE object_type = <main_setup>-object_type
            AND  object_view <> 'DEFAULT'.
*            order by ord.

  SORT lt_views BY ord.

  LOOP AT lt_views ASSIGNING <views>.
    fldcat-fieldname = <views>-object_view.
    APPEND fldcat.
    CLEAR fldcat.
  ENDLOOP.

  LOOP AT lt_views ASSIGNING <views>.
    READ TABLE <main_setup>-tabstruc WITH KEY fieldname = <views>-object_view TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    hide_columns-sheet = 'DATA'.
    hide_columns-index = <views>-ord + 11.
    hide_columns-view  = <views>-object_view.
    APPEND hide_columns.
    CLEAR hide_columns.
  ENDLOOP.

  fieldcat[] = fldcat[].

  PERFORM load_fieldcat.

ENDFORM.

FORM pop_details_sheet .

  PERFORM build_partial_cat USING space
                                  space
                                  '/GDA/SDM_S_VAL_RESULTS_KEY'
                                  space.


*-----Main - 2nd sheet
  MOVE 'DETAILS' TO v_sheet.

*------Starting row
  MOVE '1' TO v_row.

*-----Starting column
  MOVE '0' TO v_col.

  fieldcat[] = gt_fldcat[].

  PERFORM load_fieldcat.

ENDFORM.

FORM build_partial_cat USING prog_name
                             tabname
                             struct
                             include.

  REFRESH gt_fldcat.
  CLEAR gt_fldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = prog_name
      i_internal_tabname     = tabname
      i_structure_name       = struct
      i_inclname             = include
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = gt_fldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " BUILD_PARTIAL_CAT

*FORM progress_bar USING p_progress_message.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
**     PERCENTAGE       = 0
*      text = p_progress_message.
*
*ENDFORM.

FORM pop_context_sheet.
  DATA:
   fldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  FIELD-SYMBOLS:
   <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[].

*-----Context - 3nd sheet
  MOVE 'CONTEXT' TO v_sheet.

*------Starting row
  MOVE '1' TO v_row.

*-----Starting column
  MOVE '0' TO v_col.

  LOOP AT <main_setup>-tabstruc ASSIGNING <fieldsymbol>.
    MOVE-CORRESPONDING <fieldsymbol> TO fldcat.
    APPEND fldcat.
    CLEAR fldcat.
  ENDLOOP.

  fieldcat[] = fldcat[].

  PERFORM load_fieldcat.

ENDFORM.

FORM pop_calcs_sheet.

*-----Context - 3nd sheet
  MOVE 'CALCS' TO v_sheet.

*------Starting row
  MOVE '1' TO v_row.

*-----Starting column
  MOVE '0' TO v_col.

  PERFORM build_partial_cat USING space
                                  space
                                  '/GDA/SDM_S_CALCS'
                                  space.

  fieldcat[] = gt_fldcat[].

  PERFORM load_fieldcat.

ENDFORM.
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
*    lt_relations_extra type standard table of struc_rel,
*    lv_matnr           type mara-matnr.

  FIELD-SYMBOLS:
    <matnr>          TYPE any,
    <linkage>        TYPE any,
*    <matnr_new>      type any,
    <attyp>          TYPE any,
    <relations>      LIKE LINE OF gt_relations.
*    <relations_copy> like line of lt_relations.

  gt_relations[]   = go_selection->mt_mara_relations.

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
*&      Form  AT_SELECTION_SCREEN_ART_RSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_output_art_rsr.
  DATA:
    lv_active.


  CASE ok_code.
    WHEN 'STRUC'.
      IF p_struc = abap_true.
        lv_active = 1.
      ELSE.
        lv_active = 0.
      ENDIF.
      LOOP AT SCREEN.
        IF screen-group1 = 'SC1'.
          screen-active = lv_active.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      IF p_struc = abap_true.
        lv_active = 1.
      ELSE.
        lv_active = 0.
      ENDIF.
      LOOP AT SCREEN.
        IF screen-group1 = 'SC1'.
          screen-active = lv_active.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ART_RSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_art_rsr .
  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '00'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '01'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '02'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '10'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '11'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '12'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '21'.
  APPEND  s_attyps.

  s_attyps-sign   = 'I'.
  s_attyps-option = 'EQ'.
  s_attyps-low    = '22'.
  APPEND  s_attyps.
ENDFORM.
