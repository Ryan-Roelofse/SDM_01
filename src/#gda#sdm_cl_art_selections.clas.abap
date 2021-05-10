class /GDA/SDM_CL_ART_SELECTIONS definition
  public
  inheriting from /GDA/SDM_CL_SELECTIONS
  final
  create public .

public section.

  types:
    begin of ty_stpo,
              idnrk type stpo-idnrk,
              stlty type stpo-stlty,
              stlnr type stpo-stlnr,
              end of   ty_stpo .
  types:
    tty_stpo type standard table of ty_stpo .
  types:
    begin of struc_rel.
types: matnr     type mara-matnr.
types: matnr_rel type mara-matnr.
types end of struc_rel .
  types:
    tty_mara_relations type standard table of struc_rel .
  types:
    begin of ty_selscreen,
                   matnr             type range of mara-matnr,
                   ersda             type range of mara-ersda,
                   aenam             type range of mara-aenam,
                   laeda             type range of mara-laeda,
                   ernam             type range of mara-ernam,
                   mtart             type range of mara-mtart,
                   prdha             type range of mara-prdha,
                   mstae             type range of mara-mstae,
                   bwscl             type range of mara-bwscl,
                   attyp             type range of mara-attyp,
                   matkl             type range of mara-matkl,
                   mara_lvorm        type range of mara-lvorm,
                   attyps            type range of mara-attyp,
                   werks             type range of marc-werks,
                   mmsta             type range of marc-mmsta,
                   lgort             type range of mard-lgort,
                   infnr             type range of eina-infnr,
                   lifnr             type range of eina-lifnr,
                   ekorg             type range of eine-ekorg,
                   vdatu             type range of eord-vdatu,
                   bdatu             type range of eord-bdatu,
                   vkorg             type range of mvke-vkorg,
                   vtweg             type range of mvke-vtweg,
                   bwkey             type range of mbew-bwkey,
                   bwtar             type range of mbew-bwtar,
                   lgnum             type range of mlgn-lgnum,
                   lgtyp             type range of mlgt-lgtyp,
                   marc_lvorm        type range of marc-lvorm,
                   mard_lvorm        type range of mard-lvorm,
                   mvke_lvorm        type range of mvke-lvorm,
                   mbew_lvorm        type range of mbew-lvorm,
                   mlgn_lvorm        type range of mlgn-lvorm,
                   mlgt_lvorm        type range of mlgt-lvorm,
                   makt              type c length 1,
                   marc              type c length 1,
                   mard              type c length 1,
                   mvke              type c length 1,
                   mbew              type c length 1,
                   mlgn              type c length 1,
                   mlgt              type c length 1,
                   maw1              type c length 1,
                   mapr              type c length 1,
                   crvm              type c length 1,
                   mlan              type c length 1,
                   marm              type c length 1,
                   mean              type c length 1,
                   eord              type c length 1,
                   eina              type c length 1,
                   eine              type c length 1,
                   wlk1              type c length 1,
                   wlk2              type c length 1,
                   stpo              type c length 1,
                   stko              type c length 1,
                   mast              type c length 1,
                   mg03              type c length 1,
                   myms              type c length 1,
                   mwli              type c length 1,
                   mamt              type c length 1,
                   malg              type c length 1,
                   struc             type c length 1,
                   msgno             type range of bapiret2-number,
                   max_rows          type p_dbacc,
                   record_statistics type c length 1,
                   errors_only       type c length 1,
                 end of ty_selscreen .
  types:
    begin of ty_report_output,
                   matnr          type matnr,
                   maktx          type maktx,
                   mtart          type mtart,
                   matkl          type matkl,
                   mstae          type mstae,
                   message        type bapi_msg,
                   message_id     type symsgid,
                   message_number type symsgno,
                   message_type   type symsgty,
                   count          type i,
                   extra_v1       type symsgv,
                   extra_v2       type symsgv,
                   extra_v3       type symsgv,
                   extra_v4       type symsgv,
                   extra_v5       type symsgv,
                   extra_v6       type symsgv,
                 end of ty_report_output .
  types:
    ty_it_report_output type standard table of ty_report_output .

  data:
    mt_mara           type standard table of /gda/sdm_s_mara_01 .
  data MS_MARA_SPEC type /GDA/SDM_S_MARA_01 .
  data MS_MAKT_SPEC type /GDA/SDM_S_MAKT_01 .
  data MT_MAKT_SPEC type /GDA/SDM_T_MAKT_01 .
  data MT_MARC_SPEC type /GDA/SDM_T_MARC_01 .
  data MT_MARD_SPEC type /GDA/SDM_T_MARD_01 .
  data MT_MVKE_SPEC type /GDA/SDM_T_MVKE_01 .
  data MT_MBEW_SPEC type /GDA/SDM_T_MBEW_01 .
  data MT_MLGN_SPEC type /GDA/SDM_T_MLGN_01 .
  data MT_MLGT_SPEC type /GDA/SDM_T_MLGT_01 .
  data MT_MPOP_SPEC type /GDA/SDM_T_MPOP_01 .
  data MT_MFHM_SPEC type /GDA/SDM_T_MFHM_01 .
  data MT_MEINH_SPEC type /GDA/SDM_T_MEINH_01 .
  data MT_STEUERTAB_SPEC type /GDA/SDM_T_MAT_STEUER_01 .
  data MT_STEUMMTAB_SPEC type /GDA/SDM_T_MG03STEUMM_01 .
  data MT_MARM_SPEC type /GDA/SDM_T_MARM_01 .
  data MT_MEAN_SPEC type /GDA/SDM_T_MEAN_01 .
  data MT_MLAN_SPEC type /GDA/SDM_T_MLAN_01 .
  data MV_OBJECT type MATNR .
  data MT_EORD_SPEC type /GDA/SDM_T_EORD_01 .
  data MT_EINA_SPEC type /GDA/SDM_T_EINA_01 .
  data MT_MAW1_SPEC type /GDA/SDM_T_MAW1_01 .
  data MT_EINE_SPEC type /GDA/SDM_T_EINE_01 .
  data MT_MG03STEUMM_SPEC type /GDA/SDM_T_MG03STEUMM_01 .
  data MT_WLK1_SPEC type /GDA/SDM_T_WLK1_01 .
  data MT_WLK2_SPEC type /GDA/SDM_T_WLK2_01 .
  data MT_MAST_SPEC type /GDA/SDM_T_MAST_01 .
  data MT_MYMS_SPEC type /GDA/SDM_T_MYMS_01 .
  data MT_MWLI_SPEC type /GDA/SDM_T_MWLI_01 .
  data MT_MAMT_SPEC type /GDA/SDM_T_MAMT_01 .
  data MT_TARIFF_SPEC type /GDA/SDM_T_TARIFFS_01 .
  data MT_MALG_SPEC type /GDA/SDM_T_MALG_01 .
  data MT_MARA_RELATIONS type TTY_MARA_RELATIONS .

  methods CONSTRUCTOR .
  methods SET_SELSCREEN
    importing
      !IS_SELSCREEN type TY_SELSCREEN .

  methods BUILD_SPEC
    redefinition .
  methods MAIN
    redefinition .
  methods REFRESH
    redefinition .
protected section.
private section.

  data MT_PRICING_SPEC type /GDA/SDM_T_PRICING_01 .
  data MS_SELSCREEN type TY_SELSCREEN .
  data:
    mt_makt       type hashed table of /gda/sdm_s_makt_01
                    with unique key matnr spras .
  data MT_MARC type /GDA/SDM_T_MARC_01 .
  data MT_MARD type /GDA/SDM_T_MARD_01 .
  data MT_MVKE type /GDA/SDM_T_MVKE_01 .
  data MT_MBEW type /GDA/SDM_T_MBEW_01 .
  data MT_MLGN type /GDA/SDM_T_MLGN_01 .
  data MT_MLGT type /GDA/SDM_T_MLGT_01 .
  data:
    mt_mapr       type sorted table of /gda/sdm_s_mapr_06
                    with unique key matnr werks .
  data:
    mt_marm       type sorted table of /gda/sdm_s_marm_01
                    with unique key  matnr meinh .
  data:
    mt_mean       type sorted table of /gda/sdm_s_mean_01
                    with unique key matnr meinh lfnum .
  data:
    mt_eord       type sorted table of /gda/sdm_s_eord_01
                    with unique key matnr werks zeord .
  data:
    mt_mlan       type sorted table of /gda/sdm_s_mlan_01
                    with unique key matnr aland lfdnr tatyp .
  data MT_EINA type /GDA/SDM_T_EINA_01 .
  data MT_EINE type /GDA/SDM_T_EINE_01 .
  data MT_MG03STEUMM type /GDA/SDM_T_MG03STEUMM_01 .
  data MT_MAW1 type /GDA/SDM_T_MAW1_01 .
  data MT_WLK1 type /GDA/SDM_T_WLK1_01 .
  data MT_WLK2 type /GDA/SDM_T_WLK2_01 .
  data MT_MAST type /GDA/SDM_T_MAST_01 .
  data MT_MYMS type /GDA/SDM_T_MYMS_01 .
  data MT_MWLI type /GDA/SDM_T_MWLI_01 .
  data MT_MAMT type /GDA/SDM_T_MAMT_01 .
  data MT_TARIFF type /GDA/SDM_T_TARIFFS_01 .
  data MT_PRICING type /GDA/SDM_T_PRICING_01 .
  data MT_MALG type /GDA/SDM_T_MALG_01 .
  data MT_STPO type TTY_STPO .
  data MV_EINA_FIRST type C .
  data MV_EORD_FIRST type C .
  data MV_MVKE_FIRST type C .

  methods DETERMINE_SELECTION .
  methods BUILD_MARA
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MAKT
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MARC
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MARD
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MVKE
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MBEW
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MLGN
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MLGT
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MAPR
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MARM
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MLAN
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MEAN
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_EORD
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_EINA
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_EINE
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_STEUMMTAB .
  methods BUILD_MAW1
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_WLK1
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_WLK2
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MYMS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MAMT
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MALG
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_STPO
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MAST
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_MWLI
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_TARIFFS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_PRICING
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
ENDCLASS.



CLASS /GDA/SDM_CL_ART_SELECTIONS IMPLEMENTATION.


  method BUILD_EINA.


    data:
      lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <eina> like line of me->mt_eina.

    if me->ms_selscreen-eina = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_EINA_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from eina
          into table me->mt_eina
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_EINA:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_eina assigning <eina>.
      <eina>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINA'
                                                                           i_contents = <eina> ).
    endloop.
  endmethod.


  method BUILD_EINE.


    data:
      lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <eine> like line of me->mt_eine.

    if me->ms_selscreen-eine = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_EINE_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from eine
          into table me->mt_eine
         for all entries in me->mt_eina
        where infnr = me->mt_eina-infnr.
*          and loekz = abap_false.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_EINA:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_eine assigning <eine>.
      <eine>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EINE'
                                                                           i_contents = <eine> ).
    endloop.
  endmethod.


  method BUILD_EORD.


    data:
      lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_eord> like line of me->mt_eord.

    if me->ms_selscreen-eord = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_EORD_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from eord
          into table me->mt_eord
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
         and werks      in me->ms_selscreen-werks.
*         AND lifnr      IN me->ms_selscreen-lifnr
*         AND vdatu      IN me->ms_selscreen-vdatu
*         AND bdatu      IN me->ms_selscreen-bdatu.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_EORD:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_eord assigning <ls_eord>.
      <ls_eord>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'EORD'
                                                                           i_contents = <ls_eord> ).
    endloop.


  endmethod.


  method BUILD_MAKT.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_makt> like line of me->mt_makt.

    if me->ms_selscreen-makt = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MAKT_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from makt
          into corresponding fields of table me->mt_makt
          for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and spras = sy-langu.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MAKT_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_makt assigning <ls_makt>.
      <ls_makt>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAKT'
                                                                           i_contents = <ls_makt> ).
    endloop.



  endmethod.


  method BUILD_MALG.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_malg> like line of me->mt_malg.

    if me->ms_selscreen-malg = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MALG_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from malg
          into table me->mt_malg
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MALG_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_malg assigning <ls_malg>.
      <ls_malg>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MALG'
                                                                           i_contents = <ls_malg> ).
    endloop.

  endmethod.


  method BUILD_MAMT.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mamt> like line of me->mt_mamt.

    if me->ms_selscreen-mamt = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MAMT_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mamt
          into table me->mt_mamt
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MAMT_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_mamt assigning <ls_mamt>.
      <ls_mamt>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAMT'
                                                                           i_contents = <ls_mamt> ).
    endloop.

  endmethod.


  method BUILD_MAPR.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mapr> like line of me->mt_mapr.

    if me->ms_selscreen-mapr = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MAPR_01' ).

    try.
*/ MAPR (Forecasting) Data
        select (me->mt_field_list)
          from mapr
          into table me->mt_mapr
          for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and werks in me->ms_selscreen-werks.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MAPR_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.



    loop at me->mt_mapr assigning <ls_mapr>.
      <ls_mapr>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAPR'
                                                                           i_contents = <ls_mapr> ).
    endloop.



  endmethod.


  METHOD build_mara.
    TYPES BEGIN OF struc_rel.
    TYPES: matnr     TYPE mara-matnr.
    TYPES: matnr_rel TYPE mara-matnr.
    TYPES END OF struc_rel.

    DATA:
      lt_matnr    TYPE matnr_tty,
      lv_function TYPE string.

    FIELD-SYMBOLS:
      <eina> LIKE LINE OF me->mt_eina,
      <eord> LIKE LINE OF me->mt_eord,
      <mvke> LIKE LINE OF me->mt_mvke.

    DATA:
      lt_mara_variants TYPE STANDARD TABLE OF /gda/sdm_s_mara_01,
      lt_mast          TYPE STANDARD TABLE OF mast,
      lt_stpo          TYPE STANDARD TABLE OF stpo,
      gs_relations     TYPE struc_rel,
      lt_relations     TYPE STANDARD TABLE OF struc_rel,
      gt_relations     TYPE STANDARD TABLE OF struc_rel,
      lt_mara          TYPE STANDARD TABLE OF /gda/sdm_s_mara_01.

    FIELD-SYMBOLS:
      <relations1>    LIKE LINE OF gt_relations,
      <relations2>    LIKE LINE OF lt_relations,
      <mara>          LIKE LINE OF me->mt_mara,
      <mara_variants> LIKE LINE OF lt_mara_variants.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    FIELD-SYMBOLS:
      <ls_mara> LIKE LINE OF me->mt_mara.
    CONSTANTS lc_sap_version TYPE syst_saprl VALUE '750'.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARA_01' ).

    me->determine_selection( ).

    IF me->ms_selscreen-struc EQ abap_false.
      IF sy-saprl >= lc_sap_version.
        lv_function = '/GDA/SDM_MM_MARA_GET_NEW'.
      ELSE.
        lv_function = '/GDA/SDM_MM_MARA_GET_OLD'.
      ENDIF.

      IF mv_eina_first = abap_true.
* Note -- Possibly include a join on material to ensure we have an article..
        SELECT * FROM eina INTO CORRESPONDING FIELDS OF TABLE me->mt_eina
                   WHERE matnr      IN me->ms_selscreen-matnr
                   AND   matkl      IN me->ms_selscreen-matkl
                   AND   lifnr      IN me->ms_selscreen-lifnr
                   AND   infnr      IN me->ms_selscreen-infnr.
        IF sy-subrc = 0.
          LOOP AT me->mt_eina ASSIGNING <eina>.
*        ls_matnr-matnr = <eina>-matnr.
*        collect ls_matnr into lt_matnr.
            COLLECT <eina>-matnr INTO lt_matnr.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF mv_eord_first = abap_true.
        SELECT * FROM eord INTO CORRESPONDING FIELDS OF TABLE me->mt_eord
                 WHERE matnr      IN me->ms_selscreen-matnr
                   AND werks      IN me->ms_selscreen-werks
                   AND lifnr      IN me->ms_selscreen-lifnr
                   AND vdatu      IN me->ms_selscreen-vdatu
                   AND bdatu      IN me->ms_selscreen-bdatu.
        IF sy-subrc = 0.
          LOOP AT me->mt_eord ASSIGNING <eord>.
*        ls_matnr-matnr = <eord>-matnr.
*        collect ls_matnr into lt_matnr.
            COLLECT <eord>-matnr INTO lt_matnr.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF mv_mvke_first = abap_true.
        SELECT * FROM mvke INTO CORRESPONDING FIELDS OF TABLE me->mt_mvke
                 WHERE matnr      IN me->ms_selscreen-matnr
                   AND vkorg      IN me->ms_selscreen-vkorg
                   AND vtweg      IN me->ms_selscreen-vtweg.
        IF sy-subrc = 0.
          LOOP AT me->mt_mvke ASSIGNING <mvke>.
*        ls_matnr-matnr = <mvke>-matnr.
*        collect ls_matnr into lt_matnr.
            COLLECT <mvke>-matnr INTO lt_matnr.
          ENDLOOP.
        ENDIF.
      ENDIF.


      IF mv_eina_first = abap_true OR mv_eord_first = abap_true OR mv_mvke_first = abap_true.
        IF lt_matnr IS NOT INITIAL.
          CALL FUNCTION '/GDA/SDM_MM_MARA_GET_OLD2' "lv_function
            EXPORTING
              x_max_rows   = me->ms_selscreen-max_rows
              xt_materials = lt_matnr
            IMPORTING
              xt_mara      = me->mt_mara[]
            TABLES
              xt_matnr     = me->ms_selscreen-matnr
              xt_ersda     = me->ms_selscreen-ersda
              xt_ernam     = me->ms_selscreen-ernam
              xt_laeda     = me->ms_selscreen-laeda
              xt_aenam     = me->ms_selscreen-aenam
              xt_mtart     = me->ms_selscreen-mtart
              xt_matkl     = me->ms_selscreen-matkl
              xt_mstae     = me->ms_selscreen-mstae
              xt_bwscl     = me->ms_selscreen-bwscl
              xt_attyp     = me->ms_selscreen-attyp
              xt_werks     = me->ms_selscreen-werks
              xt_mmsta     = me->ms_selscreen-mmsta.
        ENDIF.

      ELSE.
        CALL FUNCTION '/GDA/SDM_MM_MARA_GET_OLD2' "lv_function
          EXPORTING
            x_max_rows = me->ms_selscreen-max_rows
          IMPORTING
            xt_mara    = me->mt_mara[]
          TABLES
            xt_matnr   = me->ms_selscreen-matnr
            xt_ersda   = me->ms_selscreen-ersda
            xt_ernam   = me->ms_selscreen-ernam
            xt_laeda   = me->ms_selscreen-laeda
            xt_aenam   = me->ms_selscreen-aenam
            xt_mtart   = me->ms_selscreen-mtart
            xt_matkl   = me->ms_selscreen-matkl
            xt_mstae   = me->ms_selscreen-mstae
            xt_bwscl   = me->ms_selscreen-bwscl
            xt_attyp   = me->ms_selscreen-attyp
            xt_werks   = me->ms_selscreen-werks
            xt_mmsta   = me->ms_selscreen-mmsta.
      ENDIF.

      IF lines( me->mt_mara ) = 0.
        me->mv_message = text-901. "'No data selected'.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
      ENDIF.

    ELSE. "STRUCTURED MATERIAL

      TRY.
*/ Select Data

          SELECT (me->mt_field_list)
                  UP TO me->ms_selscreen-max_rows ROWS
                  FROM mara
                  INTO CORRESPONDING FIELDS OF TABLE me->mt_mara
                   WHERE matnr IN me->ms_selscreen-matnr
                     AND ersda IN me->ms_selscreen-ersda
                     AND ernam IN me->ms_selscreen-ernam
                     AND laeda IN me->ms_selscreen-laeda
                     AND aenam IN me->ms_selscreen-aenam
                     AND mtart IN me->ms_selscreen-mtart
                     AND matkl IN me->ms_selscreen-matkl
                     AND mstae IN me->ms_selscreen-mstae
                     AND attyp IN me->ms_selscreen-attyps.

        CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
          me->mv_message = lx_open_sql_error->get_text( ).
          me->mv_message = |Error /GDA/SDM_S_MARA:| && me->mv_message.
          RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
            EXPORTING
              mv_text = mv_message.
      ENDTRY.

      REFRESH:
       lt_mara_variants.

* Strip out any items which are not top level!
      DELETE me->mt_mara WHERE attyp = '00'.

      IF lines( me->mt_mara ) = 0.
        me->mv_message = text-901. "'No data selected'.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
      ENDIF.

      LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<mara_struc>). " WHERE attyp <> '00'.

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

            APPEND LINES OF lt_mara_variants TO me->mt_mara.

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

                APPEND LINES OF lt_mara_variants TO me->mt_mara.
              ENDIF.

            ENDIF.
        ENDCASE.

      ENDLOOP.

* GS_RELATIONS
*    APPEND LINES OF GT_MARA_VARIANTS TO GT_MARA.
      SORT me->mt_mara.
      DELETE ADJACENT DUPLICATES FROM me->mt_mara.

      lt_relations[] = gt_relations[].
      me->mt_mara_relations = gt_relations[].

      LOOP AT gt_relations ASSIGNING <relations1>.
        LOOP AT lt_relations ASSIGNING <relations2> WHERE matnr <> <relations1>-matnr
                                                      AND matnr_rel = <relations1>-matnr_rel.
          READ TABLE me->mt_mara ASSIGNING <mara> WITH KEY matnr = <relations1>-matnr_rel.
          APPEND <mara> TO lt_mara.
          EXIT.
* this entry should be added back
        ENDLOOP.
      ENDLOOP.

      APPEND LINES OF lt_mara TO me->mt_mara.
    ENDIF.

    SORT me->mt_mara.
    DELETE ADJACENT DUPLICATES FROM me->mt_mara.

    LOOP AT me->mt_mara ASSIGNING <ls_mara>.
      <ls_mara>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                           i_contents = <ls_mara> ).
    ENDLOOP.
  ENDMETHOD.


  method BUILD_MARC.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_marc> like line of me->mt_marc.

    if me->ms_selscreen-marc = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARC_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from marc
          into table me->mt_marc
          for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and werks in me->ms_selscreen-werks
*          and lvorm in me->ms_selscreen-marc_lvorm
          and mmsta in me->ms_selscreen-mmsta.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARC_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_marc assigning <ls_marc>.
      <ls_marc>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                           i_contents = <ls_marc> ).
    endloop.


  endmethod.


  method BUILD_MARD.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mard> like line of me->mt_mard.

    if me->ms_selscreen-mard = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARD_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mard
          into table me->mt_mard
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and werks in me->ms_selscreen-werks
          and lgort in me->ms_selscreen-lgort.
*          and lvorm in me->ms_selscreen-mard_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARD_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_mard assigning <ls_mard>.
      <ls_mard>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                           i_contents = <ls_mard> ).
    endloop.


  endmethod.


  method BUILD_MARM.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
     <ls_marm> like line of me->mt_marm.

    if me->ms_selscreen-marm = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARM_01' ).

    try.
*/ MARM
        select (me->mt_field_list)
          from marm
          into table me->mt_marm
          for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARM_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.



    loop at me->mt_marm assigning <ls_marm>.
      <ls_marm>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARM'
                                                                           i_contents = <ls_marm> ).
    endloop.



  endmethod.


  method build_mast.
    types: begin of lty_stpo,
      idnrk type stpo-idnrk,
      stlty type stpo-stlty,
      stlnr type stpo-stlnr,
    end of lty_stpo.
    data lt_stpo type standard table of lty_stpo.
    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mast> like line of me->mt_mast.

    if me->ms_selscreen-mast = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MAST_01' ).

    try.
*/ Select Data
**Get STPO
*        select  idnrk stlty stlnr from stpo
*           into table lt_stpo
*           for all entries in me->mt_mara
*           where idnrk = me->mt_mara-matnr.
**Get MAST
*        select * from mast
*                 into table me->mt_mast
*                 for all entries in lt_stpo
**                 where ( matnr  =  mt_mara-matnr or
**                         stlnr  =  mt_mara-matnr )
*                 where ( matnr  =  lt_stpo-idnrk or
*                         stlnr  =  lt_stpo-stlnr )
*                 and werks    in me->ms_selscreen-werks.

*Get MAST
        select * from mast
                 into table me->mt_mast
                 for all entries in me->mt_mara
                 where matnr  =  me->mt_mara-matnr
                 and werks    in me->ms_selscreen-werks.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MAST_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.

    endtry.


    loop at me->mt_mast assigning <ls_mast>.
      <ls_mast>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAST'
                                                                           i_contents = <ls_mast> ).
    endloop.

  endmethod.


  method build_maw1.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_maw1> like line of me->mt_maw1.

    if me->ms_selscreen-maw1 = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MAW1_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from maw1
          into table me->mt_maw1
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.
*          and lgnum in me->ms_selscreen-lgnum
*          and lvorm in me->ms_selscreen-mlgn_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MAW1_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_maw1 assigning <ls_maw1>.
      <ls_maw1>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAW1'
                                                                           i_contents = <ls_maw1> ).
    endloop.

  endmethod.


  method BUILD_MBEW.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mbew> like line of me->mt_mbew.

    if me->ms_selscreen-mbew = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MBEW_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mbew
          into table me->mt_mbew
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and bwkey in me->ms_selscreen-bwkey
          and bwtar in me->ms_selscreen-bwtar
          and lvorm in me->ms_selscreen-mbew_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MBEW_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_mbew assigning <ls_mbew>.
      <ls_mbew>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                           i_contents = <ls_mbew> ).
    endloop.



  endmethod.


  method BUILD_MEAN.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
     <ls_mean> like line of me->mt_mean.

    if me->ms_selscreen-mean = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MEAN_01' ).

    try.
*/ MEAN
        select (me->mt_field_list)
           from mean
           into table me->mt_mean
           for all entries in me->mt_mara
         where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MEAN_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_mean assigning <ls_mean>.
      <ls_mean>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEAN'
                                                                           i_contents = <ls_mean> ).
    endloop.


  endmethod.


  METHOD build_mlan.
    TYPES BEGIN OF struc_tax.
    TYPES: matnr     TYPE mara-matnr.
    TYPES: mg03steuer TYPE mg03steuer.
    TYPES END OF struc_tax.

    DATA: gt_mg03         TYPE STANDARD TABLE OF struc_tax,
          gs_mg03         TYPE struc_tax,
          gs_mg03steuer   TYPE mg03steuer,
          gs_mg03_sdm     TYPE /gda/sdm_mlan,
          gt_steuer       LIKE TABLE OF gs_mg03steuer.
    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    FIELD-SYMBOLS:
       <ls_mlan> LIKE LINE OF me->mt_mlan.

    IF me->ms_selscreen-mlan = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MLAN_01' ).

*    try.
**/ MLAN
*        select (me->mt_field_list)
*           from mlan
*           into table me->mt_mlan
*           for all entries in me->mt_mara
*         where matnr = me->mt_mara-matnr.
*
*      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MLAN_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
*    endtry.

* TAX
    LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<mara_struc>).
      CALL FUNCTION 'STEUERTAB_READ'
        EXPORTING
*         KZRFB           = ' '
          matnr           = <mara_struc>-matnr
        TABLES
          steuertab       = gt_steuer
        EXCEPTIONS
          wrong_call      = 1
          steuertab_empty = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        LOOP AT gt_steuer ASSIGNING FIELD-SYMBOL(<steuer>).
          gs_mg03-matnr      = <mara_struc>-matnr.
          gs_mg03-mg03steuer = <steuer>.
          APPEND gs_mg03 TO gt_mg03.
          CLEAR:
            gs_mg03.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_mg03 ASSIGNING FIELD-SYMBOL(<mg03>)." where matnr = x_mara-matnr.
      MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm.
      gs_mg03_sdm-matnr = <mg03>-matnr.
      APPEND gs_mg03_sdm TO me->mt_mlan.

*        MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm_brf.
*        APPEND gs_mg03_sdm_brf TO gt_mg03_sdm_brf.

      CLEAR:
        gs_mg03_sdm.
*          gs_mg03_sdm_brf.
    ENDLOOP.

    LOOP AT me->mt_mlan ASSIGNING <ls_mlan>.
      <ls_mlan>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLAN'
                                                                           i_contents = <ls_mlan> ).
    ENDLOOP.
  ENDMETHOD.


  method BUILD_MLGN.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mlgn> like line of me->mt_mlgn.

    if me->ms_selscreen-mlgn = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MLGN_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mlgn
          into table me->mt_mlgn
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.
*          and lgnum in me->ms_selscreen-lgnum
*          and lvorm in me->ms_selscreen-mlgn_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MLGN_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_mlgn assigning <ls_mlgn>.
      <ls_mlgn>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGN'
                                                                           i_contents = <ls_mlgn> ).
    endloop.


  endmethod.


  method BUILD_MLGT.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mlgt> like line of me->mt_mlgt.

    if me->ms_selscreen-mlgt = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MLGT_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mlgt
          into corresponding fields of table me->mt_mlgt
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.
*          and lgnum in me->ms_selscreen-lgnum
*          and lgtyp in me->ms_selscreen-lgtyp
*          and lvorm in me->ms_selscreen-mlgt_lvorm.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MLGT_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_mlgt assigning <ls_mlgt>.
      <ls_mlgt>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGT'
                                                                           i_contents = <ls_mlgt> ).
    endloop.


  endmethod.


  method BUILD_MVKE.


    data:
      lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mvke> like line of me->mt_mvke.

    if me->ms_selscreen-mvke = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MVKE_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mvke
          into table me->mt_mvke
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr
          and vkorg in me->ms_selscreen-vkorg
          and vtweg in me->ms_selscreen-vtweg
          and lvorm in me->ms_selscreen-mvke_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MVKE_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.

    loop at me->mt_mvke assigning <ls_mvke>.
      <ls_mvke>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                           i_contents = <ls_mvke> ).
    endloop.



  endmethod.


  method BUILD_MWLI.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mwli> like line of me->mt_mwli.

    if me->ms_selscreen-mwli = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MWLI_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from maw1
          into table me->mt_mwli
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MWLI_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_mwli assigning <ls_mwli>.
      <ls_mwli>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MWLI'
                                                                           i_contents = <ls_mwli> ).
    endloop.

  endmethod.


  method BUILD_MYMS.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_myms> like line of me->mt_myms.

    if me->ms_selscreen-myms = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MYMS_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from myms
          into table me->mt_myms
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MYMS_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_myms assigning <ls_myms>.
      <ls_myms>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
                                                                           i_contents = <ls_myms> ).
    endloop.

  endmethod.


  METHOD build_pricing.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error,
      ls_pricing     type /gda/sdm_st_pricing_01.

    TRY.
* Tariff
        LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>).

          CALL FUNCTION '/GDA/SDM_PP_BRF_PRICING1'
            EXPORTING
              x_matnr  = <lfs_mara>-matnr
            IMPORTING
              y_result = ls_pricing.

          ls_pricing-matnr = <lfs_mara>-matnr.
          APPEND ls_pricing TO me->mt_pricing.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.


  method BUILD_SPEC.


    data:
      ls_mpop_sdm        type /gda/sdm_s_mpop_01, "Forecasting
      ls_mpop            type mpop,            "Forecasting
      ls_mfhm            type mfhm,            "PRT
      ls_mfhm_sdm        type /gda/sdm_s_mfhm_06, "PRT
*      ls_smeinh          type smeinh,          "UoM
      ls_smeinh_sdm      type /gda/sdm_s_meinh_01, "UoM
      ls_mean_sdm        type /gda/sdm_s_mean_01,
      ls_marm_sdm        type /gda/sdm_s_marm_01,
      ls_maw1_sdm        type /gda/sdm_s_maw1_01,
      ls_mwli_sdm        type /gda/sdm_s_mwli_01,
      ls_tariffs_sdm     type /gda/sdm_s_tariffs_01,

      ls_wlk1_sdm        type /gda/sdm_s_wlk1_01,
      ls_wlk2_sdm        type /gda/sdm_s_wlk2_01,
      ls_myms_sdm        type /gda/sdm_s_myms_01,
      ls_mamt_sdm        type /gda/sdm_s_mamt_01,
      ls_malg_sdm        type /gda/sdm_s_malg_01,

      ls_eord_sdm        type /gda/sdm_s_eord_01,
      ls_mlan_sdm        type /gda/sdm_mlan,
      lt_steuertab       type standard table of mg03steuer, "Table for Taxes
      ls_steuertab_spec  type /gda/sdm_s_mg03steuer_01,
      lt_steummtab       type standard table of mg03steumm, "Table for Taxes (Purchasing)
      ls_steummtab_spec  type /gda/sdm_s_mg03steumm_06.

    field-symbols:
      <ls_marc> type /gda/sdm_s_marc_01,
      <ls_mast> type /gda/sdm_s_mast_01,
      <ls_mard> type /gda/sdm_s_mard_01,
      <ls_mvke> type /gda/sdm_s_mvke_01,
      <ls_mbew> type /gda/sdm_s_mbew_01,
      <ls_mlgn> type /gda/sdm_s_mlgn_01,
      <ls_mlgt> type /gda/sdm_s_mlgt_01,
      <ls_maw1> type /gda/sdm_s_maw1_01,
      <ls_mwli> type /gda/sdm_s_mwli_01,
      <ls_wlk1> type /gda/sdm_s_wlk1_01,
      <ls_wlk2> type /gda/sdm_s_wlk2_01,
      <ls_myms> type /gda/sdm_s_myms_01,
      <ls_mamt> type /gda/sdm_s_mamt_01,
      <ls_malg> type /gda/sdm_s_malg_01,
      <ls_mapr> type /gda/sdm_s_mapr_06,
      <ls_mean> type /gda/sdm_s_mean_01,
      <ls_marm> type /gda/sdm_s_marm_01,
      <ls_mlan> type /gda/sdm_s_mlan_01,
      <ls_eord> type /gda/sdm_s_eord_01,
      <ls_eina> type /gda/sdm_s_eina_01,
      <ls_eine> type /gda/sdm_s_eine_01,
      <ls_tariffs> type /gda/sdm_s_tariffs_01,
      <steuertab> like line of lt_steuertab,
      <steummtab> like line of lt_steummtab.

*    if me->ms_mara_spec-matnr is initial.
      read table me->mt_mara into me->ms_mara_spec
        with key matnr = me->MV_OBJECT.
*    endif.

*/ MAKT
    if me->ms_selscreen-makt = abap_true.
      read table me->mt_makt into me->ms_makt_spec
        with key matnr = me->MV_OBJECT
                 spras = sy-langu.
    append  me->ms_makt_spec to me->mt_makt_spec.
    endif.

*/ MARC
    if me->ms_selscreen-marc = abap_true.
      read table mt_marc transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at me->mt_marc assigning <ls_marc> from sy-tabix.
          if <ls_marc>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_marc> into table me->mt_marc_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MAST
    if me->ms_selscreen-mast = abap_true.
      read table mt_mast transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at me->mt_mast assigning <ls_mast> from sy-tabix.
          if <ls_mast>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mast> into table me->mt_mast_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MARD
    if me->ms_selscreen-mard = abap_true.
      read table mt_mard transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mard assigning <ls_mard> from sy-tabix.
          if <ls_mard>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mard> into table me->mt_mard_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MVKE
    if me->ms_selscreen-mvke = abap_true.
      read table mt_mvke transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mvke assigning <ls_mvke> from sy-tabix.
          if <ls_mvke>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mvke> into table me->mt_mvke_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MBEW
    if me->ms_selscreen-mbew = abap_true.
      read table mt_mbew transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mbew assigning <ls_mbew> from sy-tabix.
          if <ls_mbew>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mbew> into table me->mt_mbew_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MLGN
    if me->ms_selscreen-mlgn = abap_true.
      read table mt_mlgn transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mlgn assigning <ls_mlgn> from sy-tabix.
          if <ls_mlgn>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mlgn> into table me->mt_mlgn_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MLGT
    if me->ms_selscreen-mlgt = abap_true.
      read table mt_mlgt transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mlgt assigning <ls_mlgt> from sy-tabix.
          if <ls_mlgt>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mlgt> into table me->mt_mlgt_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MAW1
    if me->ms_selscreen-maw1 = abap_true.
      read table mt_maw1 transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_maw1 assigning <ls_maw1> from sy-tabix.
          if <ls_maw1>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_maw1> into table me->mt_maw1_spec.
          endif.
        endloop.
      endif.
    endif.

*/ TARIFFS

      read table mt_tariff transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_tariff assigning <ls_tariffs> from sy-tabix.
          if <ls_tariffs>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_tariffs> into table me->mt_tariff_spec.
          endif.
        endloop.
      endif.


*/ MWLI
    if me->ms_selscreen-mwli = abap_true.
      read table mt_mwli transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mwli assigning <ls_mwli> from sy-tabix.
          if <ls_mwli>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mwli> into table me->mt_mwli_spec.
          endif.
        endloop.
      endif.
    endif.

*/ WLK1
    if me->ms_selscreen-wlk1 = abap_true.
      read table mt_wlk1 transporting no fields
        with key artnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_wlk1 assigning <ls_wlk1> from sy-tabix.
          if <ls_wlk1>-artnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_wlk1> into table me->mt_wlk1_spec.
          endif.
        endloop.
      endif.
    endif.

*/ WLK2
    if me->ms_selscreen-wlk2 = abap_true.
      read table mt_wlk2 transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_wlk2 assigning <ls_wlk2> from sy-tabix.
          if <ls_wlk2>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_wlk2> into table me->mt_wlk2_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MYMS
    if me->ms_selscreen-myms = abap_true.
      read table mt_myms transporting no fields
        with key matnr = MV_OBJECT." binary search.
      if sy-subrc = 0.
        loop at mt_myms assigning <ls_myms> from sy-tabix.
          if <ls_myms>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_myms> into table me->mt_myms_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MAMT
    if me->ms_selscreen-mamt = abap_true.
      read table mt_mamt transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mamt assigning <ls_mamt> from sy-tabix.
          if <ls_mamt>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_mamt> into table me->mt_mamt_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MALG
    if me->ms_selscreen-malg = abap_true.
      read table mt_malg transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_malg assigning <ls_malg> from sy-tabix.
          if <ls_malg>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_malg> into table me->mt_malg_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MAPR (Forecasting)
    if me->ms_selscreen-mapr = abap_true.
      read table mt_mapr transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mapr assigning <ls_mapr> from sy-tabix.
          if <ls_mapr>-matnr <> MV_OBJECT.
            exit.
          else.
*/ Read with FM
            call function 'MPOP_SINGLE_READ'
              exporting
*               kzrfb      = SPACE    " Ind.: Refresh buffer entry for material no.
                matnr      = <ls_mapr>-matnr
*               maxtz      =     " Max. No. of Entries in Buffer
                werks      = <ls_mapr>-werks
              importing
*               wmpop      =     " Work area for MPOP
                o_mpop     = ls_mpop
*        TABLES
*               prowf_tab  =     " Table of forecast values (w/o key)
              exceptions
                not_found  = 1
                wrong_call = 2
                others     = 3.
            if sy-subrc = 0.
              move-corresponding ls_mpop to ls_mpop_sdm.
*              ls_mpop_sdm-sdm_tabkey = <ls_mapr>-sdm_tabkey.
              ls_mpop_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                           i_contents = ls_mpop_sdm ).

              insert ls_mpop_sdm into table me->mt_mpop_spec.
              clear:
               ls_mpop,
               ls_mpop_sdm.
            endif.
          endif.
        endloop.
      endif.
    endif.

*/ CRVM (PRT)
*    if me->ms_selscreen-crvm = abap_true.
*      read table mt_crvm transporting no fields
*        with key matnr = mv_spec_matnr binary search.
*      if sy-subrc = 0.
*        loop at mt_crvm assigning <ls_crvm> from sy-tabix.
*          if <ls_crvm>-matnr <> mv_spec_matnr.
*            exit.
*          else.
**/ Read with FM
*            call function 'MFHM_SINGLE_READ'
*              exporting
**               kzrfb      = ' '    " Ind.: Refresh buffer entry for material no.
*                matnr      = <ls_crvm>-matnr
**               maxtz      =     " Max. No. of Entries in Buffer
*                werks      = <ls_crvm>-werks
*              importing
*                wmfhm      = ls_mfhm
**               o_mfhm     =
*              exceptions
*                not_found  = 1
*                wrong_call = 2
*                others     = 3.
*
*            if sy-subrc = 0.
*              move-corresponding ls_mfhm to ls_mfhm_sdm.
*              ls_mfhm_sdm-sdm_tabkey = <ls_crvm>-sdm_tabkey.
*              insert ls_mfhm_sdm into table me->mt_mfhm_spec.
*              clear:
*               ls_mfhm,
*               ls_mfhm_sdm.
*            endif.
*          endif.
*        endloop.
*      endif.
*    endif.

*/ MLAN (Taxes)
    if me->ms_selscreen-mlan = abap_true.
      call function 'STEUERTAB_READ'
        exporting
*         kzrfb           = ' '
          matnr           = me->MV_OBJECT
        tables
          steuertab       = lt_steuertab
        exceptions
          wrong_call      = 1
          steuertab_empty = 2
          others          = 3. "#EC *

      loop at lt_steuertab assigning <steuertab>.
        move-corresponding <steuertab> to ls_steuertab_spec.
        ls_steummtab_spec-matnr      = ms_mara_spec-matnr.
        ls_steuertab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        append ls_steuertab_spec to me->mt_steuertab_spec.
        clear:
         ls_steuertab_spec.
      endloop.

      call function 'STEUMMTAB_READ'
        exporting
*         kzrfb           = ' '
          matnr           = me->MV_OBJECT
        tables
          steummtab       = lt_steummtab
        exceptions
          wrong_call      = 1
          steummtab_empty = 2
          others          = 3. "#EC *

      loop at lt_steummtab assigning <steummtab>.
        move-corresponding <steummtab> to ls_steummtab_spec.
        ls_steummtab_spec-matnr      = ms_mara_spec-matnr.
        ls_steummtab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        append ls_steummtab_spec to me->mt_steummtab_spec.
        clear:
         ls_steummtab_spec.
      endloop.

      if me->mt_steummtab_spec[] is initial.
        ls_steummtab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        append ls_steummtab_spec to me->mt_steummtab_spec.
      endif.

      read table mt_mlan transporting no fields
        with key matnr = MV_OBJECT.
      if sy-subrc = 0.
        loop at mt_mlan assigning <ls_mlan> from sy-tabix.
          if <ls_mlan>-matnr <> MV_OBJECT.
            exit.
          else.
            move-corresponding <ls_mlan> to ls_mlan_sdm.
            append ls_mlan_sdm to  me->mt_mlan_spec.
          endif.
        endloop.
      endif.
    endif.

*/ MARM (Units of Measure)
    if me->ms_selscreen-marm = abap_true.
      read table mt_marm transporting no fields
       with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_marm assigning <ls_marm> from sy-tabix.
          if <ls_marm>-matnr <> MV_OBJECT.
            exit.
          else.
            move-corresponding <ls_marm> to ls_smeinh_sdm.
            move-corresponding <ls_marm> to ls_marm_sdm.

            append ls_marm_sdm   to me->mt_marm_spec.
            append ls_smeinh_sdm to me->mt_meinh_spec.
            clear ls_smeinh_sdm.
          endif.
        endloop.
      endif.
    endif.

*/ MEAN (Additional EANs)
    if me->ms_selscreen-mean = abap_true.
      read table mt_mean transporting no fields
       with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_mean assigning <ls_mean> from sy-tabix.
          if <ls_mean>-matnr <> MV_OBJECT.
            exit.
          else.
            move-corresponding <ls_mean> to ls_mean_sdm.
            append ls_mean_sdm to me->mt_mean_spec.
            clear:
             ls_mean_sdm.
          endif.
        endloop.
      endif.
    endif.

*/ EORD
    if me->ms_selscreen-eord = abap_true.
      read table mt_eord transporting no fields
       with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_eord assigning <ls_eord> from sy-tabix.
          if <ls_eord>-matnr <> MV_OBJECT.
            exit.
          else.
            move-corresponding <ls_eord> to ls_eord_sdm.
            append ls_eord_sdm to me->mt_eord_spec.
            clear:
             ls_eord_sdm.
          endif.
        endloop.
      endif.
    endif.

*/ EINA
    if me->ms_selscreen-eina = abap_true.
      read table mt_eina transporting no fields
        with key matnr = MV_OBJECT binary search.
      if sy-subrc = 0.
        loop at mt_eina assigning <ls_eina> from sy-tabix.
          if <ls_eina>-matnr <> MV_OBJECT.
            exit.
          else.
            insert <ls_eina> into table me->mt_eina_spec.
          endif.
        endloop.
      endif.
    endif.

*/ EINE
    if me->ms_selscreen-eine = abap_true.
      loop at me->mt_eina_spec assigning field-symbol(<eina_spec>).
        loop at mt_eine assigning  field-symbol(<eine>)
           where infnr = <eina_spec>-infnr.
          insert <eine> into table me->mt_eine_spec.
        endloop.
      endloop.
    endif.
  endmethod.


  method BUILD_STEUMMTAB.

    data:
      steummtab     type standard table of mg03steumm,
      steummtab_sdm type /gda/sdm_s_mg03steumm_06.

    loop at me->mt_marc assigning field-symbol(<marc>).

      call function 'STEUMMTAB_IDENTIFY'
        exporting
          kzrfb           = ''
          werks           = <marc>-werks
        tables
          steummtab       = steummtab
        exceptions
          wrong_call      = 01
          steummtab_empty = 02.

      if sy-subrc <> 0.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.

      endif.

      loop at steummtab assigning field-symbol(<steummtab>).
        steummtab_sdm-matnr = <marc>-matnr.
*        steummtab_sdm-werks = <marc>-werks.
        move-corresponding <steummtab> to steummtab_sdm.
        steummtab_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                           i_contents = <marc> ).

        append steummtab_sdm to mt_mg03steumm.
        clear:steummtab_sdm.

      endloop.

    endloop.
  endmethod.


  method BUILD_STPO.

*    data:
*     lx_open_sql_error type ref to cx_sy_open_sql_error.
*
*    field-symbols:
*      <ls_stpo> like line of me->mt_stpo.
*
*    if me->ms_selscreen-stpo = abap_false.
*      return.
*    endif.
*
*    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_WLK2_01' ).
*
*    try.
**/ Select Data
*        select (me->mt_field_list)
*          from wlk2
*          into table me->mt_wlk2
*         for all entries in me->mt_mara
*        where matnr = me->mt_mara-matnr and
*              vkorg in me->ms_selscreen-vkorg and
*              vtweg in me->ms_selscreen-vtweg.
**          and lgnum in me->ms_selscreen-lgnum
**          and lvorm in me->ms_selscreen-mlgn_lvorm.
*      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_WLK2_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
*    endtry.
*
*
*    loop at me->mt_wlk2 assigning <ls_wlk2>.
*      <ls_wlk2>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WLK2'
*                                                                           i_contents = <ls_wlk2> ).
*    endloop.

  endmethod.


  METHOD build_tariffs.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error,
      ls_mvke         TYPE /gda/sdm_s_mvke_01,
      lt_tariff_sdm   TYPE /gda/sdm_t_tariffs_01,
      lt_mvke_sdm     TYPE /gda/sdm_t_mvke_01.

    TRY.
*/ Select Data
*        select (me->mt_field_list)
*          from maw1
*          into table me->mt_mwli
*         for all entries in me->mt_mara
*        where matnr = me->mt_mara-matnr.

* Tariff
        LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>).

          LOOP AT me->mt_mvke ASSIGNING FIELD-SYMBOL(<lfs_mvke>) WHERE matnr = <lfs_mara>-matnr.
*            APPEND INITIAL LINE TO lt_mvke_sdm ASSIGNING FIELD-SYMBOL(<lfs_mvke_tmp>).
            MOVE-CORRESPONDING <lfs_mvke> TO ls_mvke.

            APPEND ls_mvke TO lt_mvke_sdm.
          ENDLOOP.

          CALL FUNCTION '/GDA/SDM_PP_BRF_TARIFF1'
            EXPORTING
              x_matnr  = <lfs_mara>-matnr
              xt_mvke  = lt_mvke_sdm
            IMPORTING
              y_result = lt_tariff_sdm.

          APPEND LINES OF lt_tariff_sdm TO me->mt_tariff.

          CLEAR lt_mvke_sdm[].

        ENDLOOP.

*        IF NOT me->mt_mvke IS INITIAL.
*          CALL FUNCTION '/GDA/SDM_PP_BRF_TARIFF1'
*            EXPORTING
*              x_matnr  = x_mara-matnr
*              xt_mvke  = lt_mvke_sdm
*            IMPORTING
*              y_result = lt_tariff_sdm.
*        ENDIF.

*      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MWLI_01:| && me->mv_message.
*        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*          EXPORTING
*            mv_text = mv_message.
    ENDTRY.


  ENDMETHOD.


  method BUILD_WLK1.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_wlk1> like line of me->mt_wlk1.

    if me->ms_selscreen-wlk1 = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_WLK1_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from wlk1
          into table me->mt_wlk1
         for all entries in me->mt_mara
        where artnr = me->mt_mara-matnr.
*          and lgnum in me->ms_selscreen-lgnum
*          and lvorm in me->ms_selscreen-mlgn_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_WLK1_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_wlk1 assigning <ls_wlk1>.
      <ls_wlk1>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WLK1'
                                                                           i_contents = <ls_wlk1> ).
    endloop.

  endmethod.


  method BUILD_WLK2.

    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_wlk2> like line of me->mt_wlk2.

    if me->ms_selscreen-wlk2 = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_WLK2_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from wlk2
          into table me->mt_wlk2
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr and
              vkorg in me->ms_selscreen-vkorg and
              vtweg in me->ms_selscreen-vtweg.
*          and lgnum in me->ms_selscreen-lgnum
*          and lvorm in me->ms_selscreen-mlgn_lvorm.
      catch cx_sy_open_sql_error into lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_WLK2_01:| && me->mv_message.
        raise exception type /gda/cx_sdm_exception_handl
          exporting
            mv_text = mv_message.
    endtry.


    loop at me->mt_wlk2 assigning <ls_wlk2>.
      <ls_wlk2>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WLK2'
                                                                           i_contents = <ls_wlk2> ).
    endloop.

  endmethod.


  method CONSTRUCTOR.

    super->constructor( ).

    data(class_name)  = cl_abap_classdescr=>get_class_name( me ).
    me->mv_class_name = class_name+7(30).
    me->mv_main       = 'MV_OBJECT'. "'MV_SPEC_MATNR'.
    me->mv_base_field = 'MATNR'.

*/ Create BRF Exception Object Utility Object
*    IF mo_brf_exc_util IS NOT BOUND.
*      CREATE OBJECT mo_brf_exc_util
*        EXPORTING
*          iv_object_type = gc_object_material
*          iv_period      = sy-datum
*          iv_del_sign    = 'EQ'. "Equal
*    ENDIF.


  endmethod.


  METHOD DETERMINE_SELECTION.
    IF ( me->ms_selscreen-infnr IS NOT INITIAL OR
         me->ms_selscreen-ekorg IS NOT INITIAL OR
         me->ms_selscreen-lifnr IS NOT INITIAL )." AND s_matnr IS INITIAL.
      mv_eina_first = abap_true.
    ENDIF.

    IF ( me->ms_selscreen-vdatu IS NOT INITIAL OR
         me->ms_selscreen-bdatu  IS NOT INITIAL ).
      mv_eord_first = abap_true.
    ENDIF.

    IF ( me->ms_selscreen-vkorg IS NOT INITIAL OR
       me->ms_selscreen-vtweg IS NOT INITIAL ).
      mv_mvke_first = abap_true.
    ENDIF.

  ENDMETHOD.


  method MAIN.

    me->build_mara( ).
    me->build_makt( ).
    me->build_marc( ).
    me->build_mard( ).

    me->build_mvke( ).
*    me->build_mpop( ).
    me->build_mbew( ).
    me->build_mlgn( ).
    me->build_mlgt( ).
    me->build_maw1( ).
    me->build_mast( ).
    me->build_mapr( ).
    me->build_mwli( ).
    me->build_tariffs( ).

    me->build_myms( ).
    me->build_mamt( ).
    me->build_malg( ).
    me->build_wlk1( ).
    me->build_wlk2( ).

*    me->build_mapr( ).
    me->build_marm( ).
    me->build_mean( ).
    me->build_mlan( ).
    me->build_eord( ).
    me->build_eina( ).
    me->build_eine( ).
*    me->build_STEUMMTAB( ).

  endmethod.


  method refresh.
*CALL METHOD SUPER->REFRESH
*    .
    clear: me->mv_object.
*    CLEAR: me->ms_makt_spec.
    free: me->mt_marc_spec, me->mt_mard_spec,
          me->mt_mvke_spec, me->mt_mbew_spec,
          me->mt_mlgn_spec, me->mt_mlgt_spec,
          me->mt_mpop_spec, me->mt_mfhm_spec,
          me->mt_steuertab_spec, me->mt_steummtab_spec,
          me->mt_marm_spec, me->mt_mean_spec,
          me->mt_eine_spec, me->mt_eina_spec,
          me->mt_mlan_spec, me->mt_makt_spec,
          me->mt_maw1_spec, me->mt_wlk1_spec,
          me->mt_wlk2_spec, me->mt_mast_spec,
          me->mt_myms_spec, me->mt_mwli_spec,
          me->mt_mamt_spec, me->mt_malg_spec,
          me->mt_tariff_spec.


  endmethod.


  method SET_SELSCREEN.
    me->ms_selscreen = is_selscreen.

    me->ms_selscreen-makt = abap_true.
    me->ms_selscreen-marc = abap_true.
    me->ms_selscreen-mard = abap_true.
    me->ms_selscreen-mvke = abap_true.
    me->ms_selscreen-mbew = abap_true.
    me->ms_selscreen-mlgn = abap_true.
    me->ms_selscreen-mlgt = abap_true.
    me->ms_selscreen-maw1 = abap_true.
    me->ms_selscreen-mapr = abap_true.
    me->ms_selscreen-crvm = abap_true.
    me->ms_selscreen-mlan = abap_true.
    me->ms_selscreen-marm = abap_true.
    me->ms_selscreen-mean = abap_true.
    me->ms_selscreen-eord = abap_true.
    me->ms_selscreen-eina = abap_true.
    me->ms_selscreen-eine = abap_true.
    me->ms_selscreen-wlk1 = abap_true.
    me->ms_selscreen-wlk2 = abap_true.
    me->ms_selscreen-mast = abap_true.
    me->ms_selscreen-myms = abap_true.
    me->ms_selscreen-mwli = abap_true.
    me->ms_selscreen-mamt = abap_true.
    me->ms_selscreen-malg = abap_true.

  endmethod.
ENDCLASS.
