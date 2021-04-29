
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sdm_logic .
  data:
    ro_data       type ref to data,
    ro_data_empty type ref to data,
    lv_field      type fieldname,
    lv_count      type p,
    lv_pur        type p,
    lv_text       type string,
    lv_pur_text   type string,
    lv_exit.

  field-symbols:
    <results>      type standard table,
    <results_pp1>  type standard table,
    <result>       type any,
    <field_alv>    type any,
    <objects>      like line of gt_objects.

  perform build_structure using gc_default
                                gc_object
                                space.

  perform build_dynamic_itab using gc_default
                             changing ro_data.

  perform progress_bar using text-018.

  describe table go_selection->mt_mara lines lv_count.

  loop at go_selection->mt_mara into go_selection->ms_mara_spec.
    clear:
      lv_pur_text,
      lv_pur,
      lv_text.

    lv_pur = ( sy-tabix / lv_count ) * 100.
    lv_pur_text = lv_pur.
    concatenate 'BRF Rules '(917)  lv_pur_text '%' into lv_text.
    perform progress_bar using lv_text.

* Set all Default Views to icon successful
    perform default_view_icons.
* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
*    go_selection->refresh_spec( ).
    go_selection->refresh( ).
    go_selection->mv_object = go_selection->ms_mara_spec-matnr.
    go_selection->build_spec( ).

    append go_selection->ms_mara_spec to gt_mara_sdm[].
    append go_selection->ms_makt_spec to gt_makt_sdm[].
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
*    gt_mlan_sdm[]   = go_selection->mt_mlan_spec[].
*    gt_steuer_sdm[] = go_selection->mt_steuertab_spec[].
*    gt_steumm_sdm[] = go_selection->mt_steummtab_spec[].
    gt_eine_sdm[]   = go_selection->mt_eine_spec[].
    gt_eina_sdm[]   = go_selection->mt_eina_spec[].

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP3 SPOTS /GDA/SDM_MM_MAT_ES5 .

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
        if sy-subrc = 0.
          if <results_pp1> is not initial.
            append lines of <results_pp1> to <results>.
          endif.
        endif.
      endif.

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      append gs_instance to gs_sdm_objects-sdm_instances.
    endloop.

    check gv_config_err = abap_false.

    if <results> is assigned.
      sort <results>.
      delete adjacent duplicates from <results>.

      if <results> is not initial.
        loop at <results> assigning <result>.

          perform message_filter using    <result>
                                 changing lv_exit.

          check lv_exit = abap_false.

          perform message_context_link using <result>
                                             gc_object.
        endloop.
      endif.
    endif.

* Ensure Default/Key fields are populated..
* The key fields can can only equate to a key  equal to the object type..
* Table that can be used
* MARA
* MAKT
    read table gt_makt_sdm into gs_makt_temp with key matnr = gs_mara_sdm-matnr.

    loop at gt_alvtop_key_fields assigning field-symbol(<alvtop_key>).
      clear:
       lv_field.
      concatenate 'KEY_' <alvtop_key>-field into lv_field.
      assign component lv_field of structure <dyn_wa> to <field_alv>.
      assign component <alvtop_key>-field of structure gs_mara_sdm to <field>.
      if sy-subrc = 0.
        check <field_alv> is assigned.
        <field_alv> = <field>.
      else.
        assign component <alvtop_key>-field of structure gs_makt_temp to <field>.
        check sy-subrc = 0.
        check <field_alv> is assigned.
        <field_alv> = <field>.
      endif.
    endloop.

    loop at gt_alvtop_default_fields assigning field-symbol(<alv_top_default>).
      assign component <alv_top_default>-field of structure <dyn_wa> to <field_alv>.
      assign component <alv_top_default>-field of structure gs_mara_sdm to <field>.
      if sy-subrc = 0.
        check <field_alv> is assigned.
        <field_alv> = <field>.
      else.
        assign component <alv_top_default>-field of structure gs_makt_temp to <field>.
        check sy-subrc = 0.
        check <field_alv> is assigned.
        <field_alv> = <field>.
      endif.
    endloop.

    gs_sdm_objects-ARTICLE  = gs_mara_sdm-matnr.
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
*    gs_sdm_objects-mpop[]   = gt_mpop_sdm[].
*    gs_sdm_objects-marm[]   = gt_marm_sdm[].
*    gs_sdm_objects-mlan[]   = gt_mlan_sdm[].
    gs_sdm_objects-eine[]   = gt_eine_sdm[].
    gs_sdm_objects-eina[]   = gt_eina_sdm[].
*    gs_sdm_objects-/gda/mg03steuer[] = gt_steuer_sdm[].
*    gs_sdm_objects-/gda/mg03steumm[]  = gt_steumm_sdm[].
    gs_syst_sdm              = syst.

*ENHANCEMENT-POINT /GDA/SDM_MM_MAT_EP4 SPOTS /GDA/SDM_MM_MAT_ES6 .

    perform determine_output using   gs_sdm_objects
                             changing  gt_sdm_article.

    clear gs_sdm_objects.

    refresh:
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
     gt_marm_sdm.
*     gt_mlan_sdm,
*     gt_steuer_sdm,
*     gt_steumm_sdm.



    unassign:
     <results>,
     <results_pp1>.
  endloop.

  perform progress_bar using text-019.
endform.

form set_view_output_new using x_column type lvc_s_col x_status.

  data:
    ls_layout           type lvc_s_layo,
    ro_data             type ref to data,
    ro_data_empty       type ref to data,
    lv_view             type /gda/sdm_de_view,
    lt_sequence_primary type standard table of /gda/sdm_setup5,
    lt_sequence_second  type standard table of /gda/sdm_setup5,
    lv_field            type field,
    lv_table            type tabname16,
    lv_key_node         type  field,
    lv_key_att          type  field.


  field-symbols:
    <field>        type any,
    <field_check>  type any,
    <field_check2> type any,
    <brf_key>      type any,
    <brf_key6>     type any,
    <message>      type any,
    <material>     type any,
    <result>       type any,
    <field_alv>    type any,
    <results>      type table,
    <results_temp> type  table,
*    <view_table>   like line of gt_view_tables,
    <sdm_object>   like line of gt_sdm_article,
    <instances>    like line of <sdm_object>-sdm_instances.

  field-symbols:
    <setup>           like line of gt_pp_main_setup,
    <primary>         like line of lt_sequence_primary,
    <secondary>       like line of lt_sequence_second,
    <table_primary>   type any table,
    <table_secondary> type any table,
    <line_primary>    type any,
    <line_secondary>  type any.

  perform build_structure using x_column
                                gc_object
                                space.
  perform build_dynamic_itab using x_column
                             changing ro_data.

  refresh:
   <dyn_table_view>.

  if x_status <> '@08@'.
    lv_view = x_column.

    move-corresponding <dyn_wa> to <dyn_wa_view>.
    assign component 'KEY_MATNR' of structure <dyn_wa> to <material>.

    read table gt_sdm_material assigning <sdm_object> with key material = <material>.
    check sy-subrc = 0.
    if <sdm_object> is assigned.
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

        if <results_temp> is assigned and <results_temp> is not initial.
          append lines of <results_temp> to <results>.
        endif.
      endloop.

      sort <results>.
      delete adjacent duplicates from <results>.

      read table gt_pp_main_setup assigning <setup> with key object_view = lv_view.

      check sy-subrc = 0.
      lt_sequence_primary[] =  <setup>-sequence[].
      loop at lt_sequence_primary assigning <primary> where seq = '01'.
*        lv_key_table = <primary>-tabname.
        lv_key_node  = <primary>-node_level.

        assign component <primary>-tabname of structure <sdm_object> to <table_primary>.
        loop at <table_primary> assigning <line_primary>.
          move-corresponding <line_primary> to <dyn_wa_view>.
* populate output with Secondary table values
          loop at lt_sequence_second assigning <secondary> where seq ne '01'.
            assign component <secondary>-tabname of structure <sdm_object> to <table_secondary>.
            loop at <table_secondary> assigning <line_secondary>.
              move-corresponding <line_secondary> to <dyn_wa_view>.
            endloop.
          endloop.

          append <dyn_wa_view> to <dyn_table_view>.
        endloop.
      endloop.


      loop at <dyn_table_view> assigning <dyn_wa_view>.

        loop at <results> assigning <result>.
* we now have a valid error for the line
          assign component 'EXTRA_V1' of structure <result> to <field>.
          if <field> is assigned.
            split <field> at '-' into lv_table lv_field.
            read table <setup>-tabstruc  with key fieldname = lv_field
                                                  tabname   = lv_table transporting no fields.
            if sy-subrc <> 0.
              continue.
            endif.
          endif.

* Potential Valid error found
* Key Field Value
          assign component 'EXTRA_V5' of structure <result> to <brf_key>.
          if <brf_key> is assigned and <brf_key> is initial.
* Then set to material
            <brf_key>     =  <material>.
            lv_key_node = 'KEY_MATNR'.
          endif.

* get Key Field attribute Value
          assign component 'EXTRA_V6' of structure <result> to <brf_key6>.
          if <brf_key6> is assigned and <brf_key6> is not initial.
* Special Conditions - Get corresponding key field attribute value from context
            if lv_key_node = 'VKORG'.
              lv_key_att = 'VTWEG'.
              assign component lv_key_att of structure <dyn_wa_view> to <field_check2>.
            endif.
          endif.

          assign component lv_key_node of structure <dyn_wa_view> to <field_check>. "lv_field

          if sy-subrc = 0 and <brf_key> is assigned and <brf_key6> is initial.
            if <brf_key> = <field_check>.
* Pass the BRF message to the screen
              assign component 'MESSAGE' of structure <result> to <message>.
              if sy-subrc = 0 and <message> is assigned.
                assign component 'MESSAGE' of structure <dyn_wa_view> to <field_alv>.
                if sy-subrc = 0 and <field_alv> is assigned.
                  <field_alv> = <message>.
                  exit.
                endif.
              endif.
            endif.
          elseif sy-subrc = 0 and <brf_key> is assigned and <brf_key6> is not initial.
            if <brf_key> = <field_check> and <brf_key6> = <field_check2>.
** Pass the BRF message to the screen
              assign component 'MESSAGE' of structure <result> to <message>.
              if sy-subrc = 0 and <message> is assigned.
                assign component 'MESSAGE' of structure <dyn_wa_view> to <field_alv>.
                if sy-subrc = 0 and <field_alv> is assigned.
                  <field_alv> = <message>.
                  exit.
                endif.
              endif.
            endif.
          endif.

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
        endloop.
      endloop.
    endif.
  endif.

  if go_alv is bound.
    go_alv->free( ).
    free go_alv.
  endif.

  if go_tree is bound.
    go_tree->free( ).
    free go_tree.
  endif.

* create an instance of alv control
  create object go_alv
    exporting
      i_parent = go_parent2.

  ls_layout-cwidth_opt = abap_true.

  read table gt_pp_main_setup assigning <main_setup> with key object_view = lv_view.

  call method go_alv->set_table_for_first_display
    exporting
      is_layout       = ls_layout
    changing
      it_fieldcatalog = <main_setup>-tabstruc[]
      it_outtab       = <dyn_table_view>.

endform.

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
                                   space.
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
    <sdm_object>      like line of gt_sdm_material,
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

  read table gt_sdm_material assigning <sdm_object> with key material = x_matnr.

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
                          and ddlanguage = sy-langu
                          and as4local   = 'A'.
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
                             p_object       type /gda/sdm_s_material
                    changing p_leaf_context type lvc_nkey.

  data:
    lv_node_text type lvc_value,
    lv_value     type char30, "string,
    lv_layout    type lvc_s_layn,
    lv_ddtext    type dd04t-ddtext,
*    lt_item_layout TYPE lvc_t_layi,
*    ls_item_layout TYPE lvc_s_layi,
    lv_line      type /gda/sdm_s_val_results,
    lv_key_combo type char30. "string.

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

form mass_download.
  data:
    template type sy-repid.

  perform process_spreadsheet.
  export template to memory id 'TEMPLATE'.

*Create SAP Document
  perform create_sapdoc.

endform.


form pop_main.
*  DATA:
*   new,
*   tabix LIKE sy-tabix.

* Header
  write range_item-name to gs_tab left-justified.
  append gs_tab to gt_tab.

  loop at <dyn_table> assigning <dyn_wa>.
    concatenate '<dyn_wa>-' range_item-name into name.

    assign (name) to <cell>.

    check sy-subrc = 0.

    write <cell> to gs_tab left-justified.

    append gs_tab to gt_tab.
  endloop.

endform.

form pop_main_details.
  data:
*    new,
*    tabix         LIKE sy-tabix,
    ro_data       type ref to data,
    ro_data_empty type ref to data.

  field-symbols:
    <articles>     like line of gt_sdm_material,
    <instances>    like line of <articles>-sdm_instances,
    <results>      type standard table,
    <results_temp> type standard table,
    <result>       type any.

* Header
  write range_item-name to gs_tab left-justified.
  append gs_tab to gt_tab.

  loop at gt_sdm_material assigning <articles>.

* Test changes
    unassign <results>.

    loop at <articles>-sdm_instances assigning <instances>.
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

    loop at <results> assigning <result>.
      if range_item-name = 'MATNR'.
        name = '<articles>-material'.
      else.
        concatenate '<result>-' range_item-name into name.
      endif.

      assign (name) to <cell>.

      check sy-subrc = 0.

      write <cell> to gs_tab left-justified.

      append gs_tab to gt_tab.
    endloop.

  endloop.
endform.

form pop_context_details.
  data:
    ro_data       type ref to data,
    ro_data_empty type ref to data.

  field-symbols:
    <articles>     like line of gt_sdm_material,
    <instances>    like line of <articles>-sdm_instances,
    <results>      type standard table,
    <results_temp> type standard table,
    <result>       type any.

* Header
  write range_item-name to gs_tab left-justified.
  append gs_tab to gt_tab.

  loop at gt_sdm_material assigning <articles>.

    unassign <results>.

    loop at <articles>-sdm_instances assigning <instances>.
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

  endloop.

endform.

form pop_calcs_details.
  data:
    ro_data           type ref to data,
    ro_data_empty     type ref to data,
    ro_download_table type ref to data,
    ls_calcs1         type /gda/sdm_s_calcs_message,
    ls_calcs2         type /gda/sdm_s_calcs_mtart,
*    ls_calcs3         TYPE /gda/sdm_s_calcs_mstae,
    ls_calcs4         type /gda/sdm_s_calcs_matkl,
*    ls_calcs5         TYPE /gda/sdm_s_calcs_attyp,
    ls_wgbez60        type wgbez60,
    sort_field        type tabname.


  field-symbols:
    <articles>       like line of gt_sdm_material,
    <instances>      like line of <articles>-sdm_instances,
    <results>        type standard table,
    <results_temp>   type standard table,
    <result>         type any,
*    <type>           type any,
    <id>             type any,
    <number>         type any,
*    <message>        type any,
    <calcs1>         like line of gt_calcs1,
    <calcs2>         like line of gt_calcs2,
    <calcs3>         like line of gt_calcs3,
    <calcs4>         like line of gt_calcs4,
    <calcs5>         like line of gt_calcs5,
    <field>          type any,
     <check>         type any,
    <table_download> type standard table,
    <download>       type any.

* Header
  write range_item-name to gs_tab left-justified.
  append gs_tab to gt_tab.

  if gt_calcs1 is initial.
    loop at gt_sdm_material assigning <articles>.

      unassign <results>.

      loop at <articles>-sdm_instances assigning <instances>.
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

      loop at <results> assigning <result>.
        assign component 'ID'      of structure <result> to <id>.
        assign component 'NUMBER'  of structure <result> to <number>.

        select single text from t100
                           into ls_calcs1-message
                      where sprsl = sy-langu
                        and arbgb = <id>
                        and msgnr = <number>.
        if sy-subrc = 0.
          concatenate <number> '-' ls_calcs1-message into ls_calcs1-message.
          ls_calcs1-count1  = '1'.
          collect ls_calcs1 into gt_calcs1.
          clear ls_calcs1.
        endif.
      endloop.
    endloop.

    sort gt_calcs1 by count1 descending.
  endif.

  create data ro_download_table like <dyn_table>.

  assign ro_download_table->* to <table_download>.
  <table_download>[] = <dyn_table>[].


* Material Type
  if gt_calcs2 is initial.
    read table <table_download> assigning <download> index 1.
    check sy-subrc = 0.
    assign component 'KEY_MTART' of structure <download> to <check>.
    if sy-subrc = 0.
      sort_field = 'KEY_MTART'.
    else.
      assign component 'MTART' of structure <download> to <check>.
      if sy-subrc = 0.
        sort_field = 'MTART'.
      endif.
    endif.

    if sort_field is not initial.
      sort <table_download> by (sort_field).
      loop at <table_download> assigning <dyn_wa>.
        assign component sort_field of structure <dyn_wa> to <field>.
        ls_calcs2-count2  = '1'.
        ls_calcs2-key_mtart  = <field>.
        collect ls_calcs2 into gt_calcs2.
        clear ls_calcs2.
      endloop.
      sort gt_calcs2 by count2 descending.
    endif.
    clear:
     sort_field.
  endif.

* Material Group
  if gt_calcs4 is initial.
    read table <table_download> assigning <download> index 1.
    check sy-subrc = 0.
    assign component 'KEY_MATKL' of structure <download> to <check>.
    if sy-subrc = 0.
      sort_field = 'KEY_MATKL'.
    else.
      assign component 'MATKL' of structure <download> to <check>.
      if sy-subrc = 0.
        sort_field = 'MATKL'.
      endif.
    endif.

    if sort_field is not initial.
      sort <table_download> by (sort_field).
      loop at <table_download> assigning <dyn_wa>.
        assign component sort_field of structure <dyn_wa> to <field>.
        ls_calcs4-count4  = '1'.
        ls_calcs4-key_matkl  = <field>.
        collect ls_calcs4 into gt_calcs4.
        clear ls_calcs4.
      endloop.

      loop at gt_calcs4 assigning <calcs4>.
        select single wgbez from t023t into ls_wgbez60
             where spras = sy-langu
               and matkl = <calcs4>-key_matkl.
        check sy-subrc = 0.
        concatenate <calcs4>-key_matkl '-' ls_wgbez60 into <calcs4>-key_matkl.
      endloop.

      sort gt_calcs4 by count4 descending.
      clear:
       sort_field.
    endif.

  endif.

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


  if range_item-name = 'MESSAGE' or range_item-name = 'COUNT1'.
    loop at gt_calcs1 assigning <calcs1>.
      concatenate '<calcs1>-' range_item-name into name.
      assign (name) to <cell>.
      check sy-subrc = 0.
      write <cell> to gs_tab left-justified.
      if range_item-name cs 'COUNT1'.
        replace all occurrences of '.' in gs_tab with '' .
      endif.
      append gs_tab to gt_tab.
    endloop.
  endif.

  if range_item-name = 'KEY_MTART' or range_item-name = 'MTART' or range_item-name = 'COUNT2'.
    loop at gt_calcs2 assigning <calcs2>.
      concatenate '<calcs2>-' range_item-name into name.
      assign (name) to <cell>.
      check sy-subrc = 0.
      write <cell> to gs_tab left-justified.
      if range_item-name cs 'COUNT2'.
        replace all occurrences of '.' in gs_tab with '' .
      endif.
      append gs_tab to gt_tab.
    endloop.
  endif.

  if range_item-name = 'MSTAE' or range_item-name = 'COUNT3'.
    loop at gt_calcs3 assigning <calcs3>.
      concatenate '<calcs3>-' range_item-name into name.
      assign (name) to <cell>.
      check sy-subrc = 0.
      write <cell> to gs_tab left-justified.
      if range_item-name cs 'COUNT3'.
        replace all occurrences of '.' in gs_tab with '' .
      endif.
      append gs_tab to gt_tab.
    endloop.
  endif.

  if range_item-name = 'KEY_MATKL' or range_item-name = 'MATKL' or range_item-name = 'COUNT4'.
    loop at gt_calcs4 assigning <calcs4>.
      concatenate '<calcs4>-' range_item-name into name.
      assign (name) to <cell>.
      check sy-subrc = 0.
      write <cell> to gs_tab left-justified.
      if range_item-name cs 'COUNT4'.
        replace all occurrences of '.' in gs_tab with '' .
      endif.
      append gs_tab to gt_tab.
    endloop.
  endif.

  if range_item-name = 'KEY_ATTYP' or range_item-name = 'ATTYP' or range_item-name = 'COUNT5'.
    loop at gt_calcs5 assigning <calcs5>.
      concatenate '<calcs5>-' range_item-name into name.
      assign (name) to <cell>.
      check sy-subrc = 0.
      write <cell> to gs_tab left-justified.
      if range_item-name cs 'COUNT5'.
        replace all occurrences of '.' in gs_tab with '' .
      endif.
      append gs_tab to gt_tab.
    endloop.
  endif.

endform.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPREADSHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form process_spreadsheet .
* Populate main sheet
  perform pop_main_sheet.
* Populate main sheet
  perform pop_details_sheet.
* Populate context sheet
  perform pop_context_sheet.
* Populate Calcs sheet
  perform pop_calcs_sheet.
endform.
*&---------------------------------------------------------------------*
*&      Form  POP_MAIN_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pop_main_sheet.
  constants:
   extra(9) value 'KEY_EXTRA'.

  data:
    fldcat   type slis_t_fieldcat_alv with header line,
    lt_views type standard table of /gda/sdm_setup3,
    value(1).

  field-symbols:
    <fieldsymbol> like line of <main_setup>-tabstruc[],
    <fieldname>   type any,
    <views>       like line of lt_views.

  move 'DATA' to v_sheet.

*Starting row
  move '1' to v_row.
*Starting column
  move '0' to v_col.

  loop at <main_setup>-tabstruc assigning <fieldsymbol>.
    assign component 'FIELDNAME' of structure <fieldsymbol> to <fieldname>.
    if <fieldname> = 'LINKAGE'.
      data(count) = sy-tabix - 1.
      data(remainder) = 10 - count.
      do remainder times.
        add 1 to value.
        concatenate extra value into fldcat-fieldname.
        append fldcat.
        clear fldcat.
      enddo.
    endif.
    move-corresponding <fieldsymbol> to fldcat.
    append fldcat.
    clear fldcat.
    if <fieldname> = 'LINKAGE'.
* Display All Views in Spreedsheet and hide ones not populated.
      exit.
    endif.
  endloop.

  select * from /gda/sdm_setup3 into table lt_views
           where object_type = <main_setup>-object_type
            and  object_view <> 'DEFAULT'.
*            order by ord.

  sort lt_views by ord.

  loop at lt_views assigning <views>.
    fldcat-fieldname = <views>-object_view.
    append fldcat.
    clear fldcat.
  endloop.

  loop at lt_views assigning <views>.
    read table <main_setup>-tabstruc with key fieldname = <views>-object_view transporting no fields.
    check sy-subrc <> 0.
    hide_columns-sheet = 'DATA'.
    hide_columns-index = <views>-ord + 11.
    hide_columns-view  = <views>-object_view.
    append hide_columns.
    clear hide_columns.
  endloop.

  fieldcat[] = fldcat[].

  perform load_fieldcat.

endform.

form pop_details_sheet .

  perform build_partial_cat using space
                                  space
                                  '/GDA/SDM_S_VAL_RESULTS_KEY'
                                  space.


*-----Main - 2nd sheet
  move 'DETAILS' to v_sheet.

*------Starting row
  move '1' to v_row.

*-----Starting column
  move '0' to v_col.

  fieldcat[] = gt_fldcat[].

  perform load_fieldcat.

endform.

form build_partial_cat using prog_name
                             tabname
                             struct
                             include.

  refresh gt_fldcat.
  clear gt_fldcat.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = prog_name
      i_internal_tabname     = tabname
      i_structure_name       = struct
      i_inclname             = include
      i_client_never_display = 'X'
    changing
      ct_fieldcat            = gt_fldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " BUILD_PARTIAL_CAT

*FORM progress_bar USING p_progress_message.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
**     PERCENTAGE       = 0
*      text = p_progress_message.
*
*ENDFORM.

form pop_context_sheet.
  data:
   fldcat type slis_t_fieldcat_alv with header line.

  field-symbols:
   <fieldsymbol> like line of <main_setup>-tabstruc[].

*-----Context - 3nd sheet
  move 'CONTEXT' to v_sheet.

*------Starting row
  move '1' to v_row.

*-----Starting column
  move '0' to v_col.

  loop at <main_setup>-tabstruc assigning <fieldsymbol>.
    move-corresponding <fieldsymbol> to fldcat.
    append fldcat.
    clear fldcat.
  endloop.

  fieldcat[] = fldcat[].

  perform load_fieldcat.

endform.

form pop_calcs_sheet.

*-----Context - 3nd sheet
  move 'CALCS' to v_sheet.

*------Starting row
  move '1' to v_row.

*-----Starting column
  move '0' to v_col.

  perform build_partial_cat using space
                                  space
                                  '/GDA/SDM_S_CALCS'
                                  space.

  fieldcat[] = gt_fldcat[].

  perform load_fieldcat.

endform.
