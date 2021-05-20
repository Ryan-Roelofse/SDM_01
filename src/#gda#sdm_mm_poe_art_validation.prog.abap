*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_VALIDATION
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_object_data.

DATA:
  ls_marc         TYPE /gda/sdm_s_marc_01,
  ls_t130m        TYPE t130m,
  lt_malg         TYPE malg_tty,
  lt_mat_ktext    TYPE mat_ktext,
  lt_mat_steuer   TYPE mat_steuer,
  lt_mat_steumm   TYPE mat_steumm,
  lt_mean         TYPE mean_tab,
  ls_syst         TYPE syst,
  ls_mg03_sdm_brf TYPE mg03steuer,
  ls_eina         TYPE eina,
  ls_eine         TYPE eine,
  ls_merrdat      TYPE merrdat,
  ls_rmmw1        TYPE rmmw1,
  lv_count        TYPE i.

FIELD-SYMBOLS:
  <marc>        LIKE  LINE OF gt_marc_sdm,
  <mlea>        LIKE LINE OF tmlea,
  <meinh>       LIKE LINE OF tmeinh,
  <tsteuertab>  LIKE LINE OF tsteuertab,
  <mamt>        LIKE LINE OF tmamt.

* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_object_type_id = '01'
                                                     x_sdm_type       = gc_val
                                                     x_sdm_source     = gc_poe ).

* Build a list of all the relevant SDM objects
gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = gc_object
                                                       xt_sdm_types = gr_sdm_type ).

CHECK gt_objects[] IS NOT INITIAL.

IMPORT rmmw1 TO ls_rmmw1 FROM MEMORY ID 'GD_MM_ARTICLE_VAL_RMMW1'.
APPEND ls_rmmw1 TO gt_rmmw1_sdm.

*IMPORT basic_text TO lt_basic_text FROM MEMORY ID 'GD_MM_ARTICLE_VAL_TEXT'.

* General
MOVE-CORRESPONDING wmara TO gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).
* Plant
MOVE-CORRESPONDING wmarc TO ls_marc.
MOVE-CORRESPONDING wmarc TO gs_marc_sdm.
gs_marc_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                         i_contents = gs_marc_sdm ).
*append gs_marc_sdm to gt_marc_sdm.

* Storage Location
MOVE-CORRESPONDING wmard TO gs_marc_sdm.
gs_mard_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                        i_contents = gs_mard_sdm ).
APPEND gs_mard_sdm TO gt_mard_sdm.


* PIR General and Purch Org
IMPORT eina TO ls_eina eine TO ls_eine FROM MEMORY ID 'GD_MM_ARTICLE_VAL_EINA_EINE'.
MOVE-CORRESPONDING ls_eina TO gs_eina_sdm.
MOVE-CORRESPONDING ls_eine TO gs_eine_sdm.

gs_eina_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                         i_contents = gs_eina_sdm ).

gs_eine_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                        i_contents = gs_eine_sdm ).

APPEND gs_eina_sdm TO gt_eina_sdm.
APPEND gs_eine_sdm TO gt_eine_sdm.

* Material Valuation
MOVE-CORRESPONDING wmbew TO gs_mbew_sdm.
gs_mbew_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                        i_contents = gs_mbew_sdm ).
APPEND gs_mbew_sdm TO gt_mbew_sdm.

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

* Material Master Texts per Unit of Measure and Text ID
LOOP AT tmamt ASSIGNING <mamt>.
  MOVE-CORRESPONDING <mamt> TO gs_mamt_sdm.
  gs_mamt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAMT'
                                                                          i_contents = gs_mamt_sdm ).
  APPEND gs_mamt_sdm TO gt_mamt_sdm.
ENDLOOP.

* Sales Data for Material
MOVE-CORRESPONDING wmvke TO gs_mvke_sdm.
gs_mvke_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                        i_contents = gs_mvke_sdm ).
APPEND gs_mvke_sdm TO gt_mvke_sdm.

* LIFO-relevant materials
IF wmbew-xlifo = abap_true.
  MOVE-CORRESPONDING wmyms TO gs_myms_sdm.
  gs_myms_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                          i_contents = gs_myms_sdm ).
  APPEND gs_myms_sdm TO gt_myms_sdm.
ENDIF.

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
MOVE-CORRESPONDING wmwli TO gs_mwli_sdm. "ls_mwli.
gs_mwli_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                        i_contents = gs_mara_sdm ).
APPEND gs_mwli_sdm TO gt_mwli_sdm.

*ls_t130m = wstat.
*
*lt_malg = tmalg[].

* Vendor-Specific EANs
LOOP AT tmlea ASSIGNING <mlea>.
  MOVE-CORRESPONDING <mlea> TO gs_mlea_sdm.
  gs_mlea_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                       i_contents = gs_mlea_sdm ).

  APPEND gs_mlea_sdm TO gt_mlea_sdm.
  CLEAR:
   gs_mlea_sdm.
ENDLOOP.

*lt_mlea = tmlea[].

*lt_mamt = tmamt[].

*delete ttext where spras <> sy-langu.  "Delete other languages
*lt_mat_ktext = ttext[].


* Unit of Measure for Display
LOOP AT tmeinh ASSIGNING <meinh>.
  MOVE-CORRESPONDING <meinh> TO gs_meinh_sdm.
  gs_meinh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEINH'
                                                                       i_contents = gs_meinh_sdm ).

  APPEND gs_meinh_sdm TO gt_meinh_sdm.
  CLEAR:
   gs_meinh_sdm.
ENDLOOP.

lt_mat_steuer = tsteuertab[].


LOOP AT tsteuertab ASSIGNING <tsteuertab>.
  MOVE-CORRESPONDING <tsteuertab> TO ls_mg03_sdm_brf.
*  gs_mg03_sdm-matnr = gs_mara_sdm-matnr.
  APPEND ls_mg03_sdm_brf TO gt_mg03_sdm_brf.
ENDLOOP.

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

** Enhancement Point: Populate non standard SDM tables and structures
*ENHANCEMENT-POINT /gda/sdm_mm_art_ep2 SPOTS /gda/sdm_mm_art_ep2 .


* Only process SDM once!
IF ls_rmmw1-fiwrk NE space.
  ADD 1 TO lv_count.
ENDIF.

IF ls_rmmw1-vzwrk NE space.
  ADD 1 TO lv_count.
ENDIF.

* Store
IF ( ls_marc-werks = ls_rmmw1-fiwrk ) AND
   ( ls_marc-werks = wrmmg1-werks )    AND
     ls_marc-werks IS NOT INITIAL.
  IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.
  READ TABLE gt_marc_sdm ASSIGNING <marc> WITH KEY matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  IF sy-subrc <> 0.
    APPEND ls_marc TO gt_marc_sdm.
    EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
  ENDIF.
ENDIF.

* DC
IF ( ls_marc-werks = ls_rmmw1-vzwrk  ) AND
   ( ls_marc-werks = wrmmg1-werks )     AND
     ls_marc-werks IS NOT INITIAL.
  IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.
  READ TABLE gt_marc_sdm ASSIGNING <marc> WITH KEY matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  IF sy-subrc <> 0.
    APPEND ls_marc TO gt_marc_sdm.
    EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
  ENDIF.
ENDIF.

IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.

LOOP AT gt_marc_sdm ASSIGNING FIELD-SYMBOL(<marc_sdm>).
  <marc_sdm>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                          i_contents = <marc_sdm> ).
ENDLOOP.

DESCRIBE TABLE gt_marc_sdm.

IF sy-tfill = lv_count.
  LOOP AT gt_objects ASSIGNING <objects>.
    TRY.
        <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_object
                                                       iv_source      = gc_poe
                                                       iv_type        = <objects>-type
                                                       iv_stats       = abap_true ).
      CATCH cx_fdt_input INTO gx_fdt.

        IF <objects>-object IS NOT INITIAL.
          <objects>-object->display_messages( ).
          EXIT.
        ENDIF.
    ENDTRY.


*    IF <objects>-object IS NOT BOUND.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.

    CHECK <objects> IS ASSIGNED.
    CHECK <objects>-object IS BOUND.
    CHECK <objects>-object->is_active( ) = abap_true
      AND <objects>-object->mt_message[] IS INITIAL.

*    CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.
*    IF <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS NOT INITIAL.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.

    TRY.
    gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

    LOOP AT gt_attributes ASSIGNING <attribute>.
      ASSIGN (<attribute>-abap_type) TO <set_data>.
      CHECK sy-subrc = 0.
      <objects>-object->set_selection( iv_name = <attribute>-name
                                       iv_data = <set_data>
                                       iv_type = <attribute>-type ).

    ENDLOOP.

        <objects>-object->main( ).
      CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
        gv_message = gx_sdm_root->mv_text.
        IF sy-batch = abap_true.
          WRITE: / gv_message.
        ELSE.
          MESSAGE gv_message TYPE 'I'.
        ENDIF.
        RETURN.
      CATCH cx_fdt_input INTO gx_fdt.
        CALL METHOD gx_fdt->if_message~get_longtext
          RECEIVING
            result = gv_message.
        IF sy-batch = abap_true.
          WRITE: / gv_message.
        ELSE.
          MESSAGE gv_message TYPE 'I'.
        ENDIF.
        RETURN.

    ENDTRY.

    gr_data = <objects>-object->return_brf_result( ).
    ASSIGN gr_data->* TO <results_val>.

    IF <results_val> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

* collect all the results..
    IF <results_val_all> IS NOT ASSIGNED.
      IF <objects>-object IS BOUND.
        gr_data_empty  = <objects>-object->return_brf_result_structure( ).
        ASSIGN gr_data_empty->* TO <results_val_all>.
      ENDIF.
    ENDIF.

    APPEND LINES OF <results_val> TO <results_val_all>.
  ENDLOOP.

  REFRESH:
   gt_marc_sdm.
  EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
ENDIF.

CHECK <results_val_all> IS ASSIGNED.
SORT <results_val_all> BY id number.
DELETE ADJACENT DUPLICATES FROM <results_val_all>.

****// Process Message
LOOP AT <results_val_all> ASSIGNING <result_val> WHERE type CA 'EAX'.
  IF <result_val>-id IS INITIAL.
    <result_val>-id = '/GDA/SDM1'.
  ENDIF.

  IF <result_val>-number IS INITIAL.
    <result_val>-number = '002'.
  ENDIF.

  IF <result_val>-message IS NOT INITIAL.
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
    APPEND ls_merrdat TO rt_errdat.
  ELSE.

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
    APPEND ls_merrdat TO rt_errdat.
  ENDIF.
ENDLOOP.
*!
