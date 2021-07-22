class /GDA/SDM_CL_ART_SELECTIONS definition
  public
  inheriting from /GDA/SDM_CL_SELECTIONS
  final
  create public .

public section.

  types:
    BEGIN OF ty_cond_header.
    TYPES: matnr TYPE mara-matnr.
    TYPES: knumh TYPE konh-knumh.
    TYPES END OF ty_cond_header .
  types:
    BEGIN OF struc_tax.
    TYPES: matnr     TYPE mara-matnr.
    TYPES: mg03steuer TYPE mg03steuer.
    TYPES END OF struc_tax .
  types:
    tty_konh TYPE STANDARD TABLE OF konh .
  types:
    tty_mg03 TYPE STANDARD TABLE OF struc_tax .
  types:
    tty_cond_hdr  TYPE SORTED TABLE OF ty_cond_header WITH NON-UNIQUE KEY matnr knumh .
  types:
    BEGIN OF ty_stpo,
                idnrk TYPE stpo-idnrk,
                stlty TYPE stpo-stlty,
                stlnr TYPE stpo-stlnr,
                END OF   ty_stpo .
  types:
    tty_stpo TYPE STANDARD TABLE OF ty_stpo .
  types:
    BEGIN OF struc_rel.
    TYPES: matnr     TYPE mara-matnr.
    TYPES: matnr_rel TYPE mara-matnr.
    TYPES END OF struc_rel .
  types:
    tty_mara_relations TYPE STANDARD TABLE OF struc_rel .
  types:
    BEGIN OF ty_selscreen,
                     matnr             TYPE RANGE OF mara-matnr,
                     ersda             TYPE RANGE OF mara-ersda,
                     aenam             TYPE RANGE OF mara-aenam,
                     laeda             TYPE RANGE OF mara-laeda,
                     ernam             TYPE RANGE OF mara-ernam,
                     mtart             TYPE RANGE OF mara-mtart,
                     prdha             TYPE RANGE OF mara-prdha,
                     mstae             TYPE RANGE OF mara-mstae,
                     bwscl             TYPE RANGE OF mara-bwscl,
                     attyp             TYPE RANGE OF mara-attyp,
                     matkl             TYPE RANGE OF mara-matkl,
                     mara_lvorm        TYPE RANGE OF mara-lvorm,
                     attyps            TYPE RANGE OF mara-attyp,
                     werks             TYPE RANGE OF marc-werks,
                     mmsta             TYPE RANGE OF marc-mmsta,
                     lgort             TYPE RANGE OF mard-lgort,
                     infnr             TYPE RANGE OF eina-infnr,
                     lifnr             TYPE RANGE OF eina-lifnr,
                     ekorg             TYPE RANGE OF eine-ekorg,
                     vdatu             TYPE RANGE OF eord-vdatu,
                     bdatu             TYPE RANGE OF eord-bdatu,
                     vkorg             TYPE RANGE OF mvke-vkorg,
                     vtweg             TYPE RANGE OF mvke-vtweg,
                     bwkey             TYPE RANGE OF mbew-bwkey,
                     bwtar             TYPE RANGE OF mbew-bwtar,
                     lgnum             TYPE RANGE OF mlgn-lgnum,
                     lgtyp             TYPE RANGE OF mlgt-lgtyp,
                     marc_lvorm        TYPE RANGE OF marc-lvorm,
                     mard_lvorm        TYPE RANGE OF mard-lvorm,
                     mvke_lvorm        TYPE RANGE OF mvke-lvorm,
                     mbew_lvorm        TYPE RANGE OF mbew-lvorm,
                     mlgn_lvorm        TYPE RANGE OF mlgn-lvorm,
                     mlgt_lvorm        TYPE RANGE OF mlgt-lvorm,
                     makt              TYPE c LENGTH 1,
                     marc              TYPE c LENGTH 1,
                     mard              TYPE c LENGTH 1,
                     mvke              TYPE c LENGTH 1,
                     mbew              TYPE c LENGTH 1,
                     mlea              TYPE c LENGTH 1,
                     mlgn              TYPE c LENGTH 1,
                     mlgt              TYPE c LENGTH 1,
                     maw1              TYPE c LENGTH 1,
                     mapr              TYPE c LENGTH 1,
                     crvm              TYPE c LENGTH 1,
                     mlan              TYPE c LENGTH 1,
                     marm              TYPE c LENGTH 1,
                     mean              TYPE c LENGTH 1,
                     eord              TYPE c LENGTH 1,
                     eina              TYPE c LENGTH 1,
                     eine              TYPE c LENGTH 1,
                     wlk1              TYPE c LENGTH 1,
                     wlk2              TYPE c LENGTH 1,
                     stpo              TYPE c LENGTH 1,
                     stko              TYPE c LENGTH 1,
                     mast              TYPE c LENGTH 1,
                     mg03              TYPE c LENGTH 1,
                     myms              TYPE c LENGTH 1,
                     mwli              TYPE c LENGTH 1,
                     mamt              TYPE c LENGTH 1,
                     malg              TYPE c LENGTH 1,
                     mpgd              TYPE c LENGTH 1,
                     wrpl              TYPE c LENGTH 1,
                     struc             TYPE c LENGTH 1,
                     msgno             TYPE RANGE OF bapiret2-number,
                     max_rows          TYPE p_dbacc,
                     record_statistics TYPE c LENGTH 1,
                     errors_only       TYPE c LENGTH 1,
                   END OF ty_selscreen .
  types:
    BEGIN OF ty_report_output,
                     matnr          TYPE matnr,
                     maktx          TYPE maktx,
                     mtart          TYPE mtart,
                     matkl          TYPE matkl,
                     mstae          TYPE mstae,
                     message        TYPE bapi_msg,
                     message_id     TYPE symsgid,
                     message_number TYPE symsgno,
                     message_type   TYPE symsgty,
                     count          TYPE i,
                     extra_v1       TYPE symsgv,
                     extra_v2       TYPE symsgv,
                     extra_v3       TYPE symsgv,
                     extra_v4       TYPE symsgv,
                     extra_v5       TYPE symsgv,
                     extra_v6       TYPE symsgv,
                   END OF ty_report_output .
  types:
    ty_it_report_output TYPE STANDARD TABLE OF ty_report_output .

  data:
    mt_mara           TYPE STANDARD TABLE OF /gda/sdm_s_mara_01 .
  data MS_MARA_SPEC type /GDA/SDM_S_MARA_01 .
  data MS_MAKT_SPEC type /GDA/SDM_S_MAKT_01 .
  data MT_MAKT_SPEC type /GDA/SDM_T_MAKT_01 .
  data MT_MARC_SPEC type /GDA/SDM_T_MARC_01 .
  data MT_MARD_SPEC type /GDA/SDM_T_MARD_01 .
  data MT_MVKE_SPEC type /GDA/SDM_T_MVKE_01 .
  data MT_MBEW_SPEC type /GDA/SDM_T_MBEW_01 .
  data MT_MLGN_SPEC type /GDA/SDM_T_MLGN_01 .
  data MT_MLEA_SPEC type /GDA/SDM_T_MLEA_01 .
  data MT_MLGT_SPEC type /GDA/SDM_T_MLGT_01 .
  data MT_MPOP_SPEC type /GDA/SDM_T_MPOP_01 .
  data MT_MFHM_SPEC type /GDA/SDM_T_MFHM_01 .
  data MT_RMMW1_SPEC type /GDA/SDM_T_RMMW1_01 .
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
  data MT_MPGD_SPEC type /GDA/SDM_T_MPGD_01 .
  data MT_WRPL_SPEC type /GDA/SDM_T_WRPL_01 .
  data MT_MG03_SPEC type /GDA/SDM_T_MAT_STEUER_01 .
  data MT_KONH_SPEC type /GDA/SDM_T_KONH_01 .
  data MT_MARA_RELATIONS type TTY_MARA_RELATIONS .
  data MT_PRICING_SPEC type /GDA/SDM_T_PRICING_01 .

  methods CONSTRUCTOR
    importing
      !IV_SPRINT type BOOLEAN optional .
  methods SET_SELSCREEN
    importing
      !IS_SELSCREEN type TY_SELSCREEN .

  methods BUILD_SPEC
    redefinition .
  methods MAIN
    redefinition .
  methods REFRESH
    redefinition .
  methods RSR_DATA_PBO
    redefinition .
protected section.
private section.

  data MS_SELSCREEN type TY_SELSCREEN .
  data:
    mt_makt       type hashed table of /gda/sdm_s_makt_01
                    with unique key matnr spras .
  data MT_MARC type /GDA/SDM_T_MARC_01 .
  data MT_MARD type /GDA/SDM_T_MARD_01 .
  data MT_MVKE type /GDA/SDM_T_MVKE_01 .
  data MT_MBEW type /GDA/SDM_T_MBEW_01 .
  data MT_MLGN type /GDA/SDM_T_MLGN_01 .
  data MT_MLEA type /GDA/SDM_T_MLEA_01 .
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
*  data:
*    mt_mlan       type sorted table of /gda/sdm_s_mlan_01
*                    with unique key matnr aland lfdnr tatyp .
    mt_mlan       type STANDARD TABLE OF /gda/sdm_s_mlan_01 .
*                    with unique key matnr aland lfdnr tatyp .
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
  data MT_MPGD type /GDA/SDM_T_MPGD_01 .
  data MT_WRPL type /GDA/SDM_T_WRPL_01 .
  data MT_STPO type TTY_STPO .
  data MT_KONH type TTY_KONH .
  data MT_COND_HEADER type TTY_COND_HDR .
  data MT_MG03 type TTY_MG03 .
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
  methods BUILD_MLEA
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
  methods BUILD_MPGD
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_WRPL
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
  methods BUILD_MG03
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

    DATA: r_attyp TYPE RANGE OF attyp,
          r_attyp_line LIKE LINE OF r_attyp.

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
*ST-1459
      IF me->ms_selscreen-attyps IS INITIAL and me->mv_sprint is initial.
        r_attyp_line-sign = 'I'.
        r_attyp_line-option = 'EQ'.
        r_attyp_line-low = '00'.
        APPEND  r_attyp_line TO me->ms_selscreen-attyp.
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
        IF me->ms_selscreen-max_rows IS NOT INITIAL.
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
        ELSE.
          CALL FUNCTION '/GDA/SDM_MM_MARA_GET_OLD2' "lv_function
*          EXPORTING
*            x_max_rows = me->ms_selscreen-max_rows
            IMPORTING
              xt_mara  = me->mt_mara[]
            TABLES
              xt_matnr = me->ms_selscreen-matnr
              xt_ersda = me->ms_selscreen-ersda
              xt_ernam = me->ms_selscreen-ernam
              xt_laeda = me->ms_selscreen-laeda
              xt_aenam = me->ms_selscreen-aenam
              xt_mtart = me->ms_selscreen-mtart
              xt_matkl = me->ms_selscreen-matkl
              xt_mstae = me->ms_selscreen-mstae
              xt_bwscl = me->ms_selscreen-bwscl
              xt_attyp = me->ms_selscreen-attyp
              xt_werks = me->ms_selscreen-werks
              xt_mmsta = me->ms_selscreen-mmsta.
        ENDIF.
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
          IF me->ms_selscreen-attyps IS INITIAL.
            r_attyp_line-sign = 'I'.
            r_attyp_line-option = 'NE'.
            r_attyp_line-low = space.
            APPEND  r_attyp_line TO me->ms_selscreen-attyps.
          ENDIF.

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
*      DELETE me->mt_mara WHERE attyp EQ space." AND attyp eq '00'.
      DELETE me->mt_mara WHERE attyp EQ '00'.

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

*            APPEND LINES OF lt_mara_variants TO me->mt_mara.

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

*                APPEND LINES OF lt_mara_variants TO me->mt_mara.
              ENDIF.

            ENDIF.
        ENDCASE.

      ENDLOOP.

* GS_RELATIONS
*    APPEND LINES OF GT_MARA_VARIANTS TO GT_MARA.
      SORT me->mt_mara.
      DELETE ADJACENT DUPLICATES FROM me->mt_mara.
      DELETE me->mt_mara WHERE attyp EQ '00'.

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
*      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MBEW_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
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


  METHOD build_mg03.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error,
     ls_mg03steuer   TYPE mg03steuer,
     lt_steuer       LIKE TABLE OF ls_mg03steuer,
*     lt_mg03         TYPE STANDARD TABLE OF struc_tax,
     ls_mg03         TYPE struc_tax.

    LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>).

      CALL FUNCTION 'STEUERTAB_READ'
        EXPORTING
*         KZRFB           = ' '
          matnr           = <lfs_mara>-matnr
        TABLES
          steuertab       = lt_steuer
        EXCEPTIONS
          wrong_call      = 1
          steuertab_empty = 2
          OTHERS          = 3.

      IF sy-subrc = 0.
        LOOP AT lt_steuer ASSIGNING FIELD-SYMBOL(<lfs_steuer>).
          ls_mg03-matnr      = <lfs_mara>-matnr.
          ls_mg03-mg03steuer = <lfs_steuer>.
          APPEND ls_mg03 TO me->mt_mg03.
          CLEAR:
            ls_mg03.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*    field-symbols:
*      <ls_myms> like line of me->mt_myms.
*
*    if me->ms_selscreen-myms = abap_false.
*      return.
*    endif.
*
*    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MYMS_01' ).
*
*    try.
**/ Select Data
*        select (me->mt_field_list)
*          from myms
*          into table me->mt_myms
*         for all entries in me->mt_mara
*        where matnr = me->mt_mara-matnr.
*
*      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MYMS_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
*    endtry.
*
*
*    loop at me->mt_myms assigning <ls_myms>.
*      <ls_myms>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MYMS'
*                                                                           i_contents = <ls_myms> ).
*    endloop.

  ENDMETHOD.


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


  method BUILD_MLEA.


    data:
     lx_open_sql_error type ref to cx_sy_open_sql_error.

    field-symbols:
      <ls_mlea> like line of me->mt_mlea.

    if me->ms_selscreen-mlea = abap_false.
      return.
    endif.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MLEA_01' ).

    try.
*/ Select Data
        select (me->mt_field_list)
          from mlea
          into CORRESPONDING FIELDS OF table me->mt_mlea
         for all entries in me->mt_mara
        where matnr = me->mt_mara-matnr.

    endtry.


    loop at me->mt_mlea assigning <ls_mlea>.
      <ls_mlea>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLEA'
                                                                           i_contents = <ls_mlea> ).
    endloop.


  endmethod.


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


  METHOD build_mpgd.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    FIELD-SYMBOLS:
      <ls_mpgd> LIKE LINE OF me->mt_mpgd.

    IF me->ms_selscreen-mpgd = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MPGD_01' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM mpgd_v
          INTO CORRESPONDING FIELDS OF TABLE me->mt_mpgd
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MPGD_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
    ENDTRY.


    loop at me->mt_mpgd assigning <ls_mpgd>.
      <ls_mpgd>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                           i_contents = <ls_mpgd> ).
    endloop.

  ENDMETHOD.


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
      ls_pricing     TYPE /gda/sdm_s_pricing_01,
      ls_cond_header TYPE ty_cond_header,
      ls_konh TYPE konh,
      ls_konh_sdm     TYPE /gda/sdm_s_konh_01.

    TRY.
* Pricing
        LOOP AT me->mt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>).
          CALL FUNCTION '/GDA/SDM_PP_BRF_PRICING1'
            EXPORTING
              x_matnr  = <lfs_mara>-matnr
            CHANGING
              c_result = me->mt_pricing.

          LOOP AT me->mt_pricing INTO ls_pricing WHERE matnr = <lfs_mara>-matnr.
            ls_cond_header-matnr = <lfs_mara>-matnr.
            ls_cond_header-knumh = ls_pricing-knumh.
            INSERT ls_cond_header INTO TABLE me->mt_cond_header.
            CLEAR: ls_pricing, ls_cond_header.
          ENDLOOP.
        ENDLOOP.

        IF me->mt_cond_header IS NOT INITIAL.
          SELECT * FROM konh INTO TABLE me->mt_konh
                   FOR ALL ENTRIES IN me->mt_cond_header
                   WHERE knumh    =  me->mt_cond_header-knumh.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD build_spec.


    DATA:
      ls_mpop_sdm        TYPE /gda/sdm_s_mpop_01, "Forecasting
      ls_mpop            TYPE mpop,            "Forecasting
      ls_mfhm            TYPE mfhm,            "PRT
      ls_mfhm_sdm        TYPE /gda/sdm_s_mfhm_06, "PRT
*      ls_smeinh          type smeinh,          "UoM
      ls_smeinh_sdm      TYPE /gda/sdm_s_meinh_01, "UoM
      ls_mean_sdm        TYPE /gda/sdm_s_mean_01,
      ls_marm_sdm        TYPE /gda/sdm_s_marm_01,
      ls_maw1_sdm        TYPE /gda/sdm_s_maw1_01,
      ls_mwli_sdm        TYPE /gda/sdm_s_mwli_01,
      ls_tariffs_sdm     TYPE /gda/sdm_s_tariffs_01,

      ls_wlk1_sdm        TYPE /gda/sdm_s_wlk1_01,
      ls_wlk2_sdm        TYPE /gda/sdm_s_wlk2_01,
      ls_myms_sdm        TYPE /gda/sdm_s_myms_01,
      ls_mamt_sdm        TYPE /gda/sdm_s_mamt_01,
      ls_malg_sdm        TYPE /gda/sdm_s_malg_01,
      ls_mlea_sdm        TYPE /gda/sdm_s_mlea_01,

      ls_konh_sdm        TYPE /gda/sdm_s_konh_01,
      ls_pricing_sdm     TYPE /gda/sdm_s_pricing_01,
      ls_mg03            TYPE struc_tax,

      ls_eord_sdm        TYPE /gda/sdm_s_eord_01,
      ls_mlan_sdm        TYPE /gda/sdm_mlan,
      lt_steuertab       TYPE STANDARD TABLE OF mg03steuer, "Table for Taxes
      ls_steuertab_spec  TYPE /gda/sdm_s_mg03steuer_01,
      lt_steummtab       TYPE STANDARD TABLE OF mg03steumm, "Table for Taxes (Purchasing)
      ls_steummtab_spec  TYPE /gda/sdm_s_mg03steumm_06.

    FIELD-SYMBOLS:
      <ls_marc> TYPE /gda/sdm_s_marc_01,
      <ls_mast> TYPE /gda/sdm_s_mast_01,
      <ls_mard> TYPE /gda/sdm_s_mard_01,
      <ls_mvke> TYPE /gda/sdm_s_mvke_01,
      <ls_mbew> TYPE /gda/sdm_s_mbew_01,
      <ls_mlgn> TYPE /gda/sdm_s_mlgn_01,
      <ls_mlgt> TYPE /gda/sdm_s_mlgt_01,
      <ls_maw1> TYPE /gda/sdm_s_maw1_01,
      <ls_mwli> TYPE /gda/sdm_s_mwli_01,
      <ls_wlk1> TYPE /gda/sdm_s_wlk1_01,
      <ls_wlk2> TYPE /gda/sdm_s_wlk2_01,
      <ls_myms> TYPE /gda/sdm_s_myms_01,
      <ls_mamt> TYPE /gda/sdm_s_mamt_01,
      <ls_malg> TYPE /gda/sdm_s_malg_01,
      <ls_mpgd> TYPE /gda/sdm_s_mpgd_01,
      <ls_wrpl> TYPE /gda/sdm_s_wrpl_01,
      <ls_mlea> TYPE /gda/sdm_s_mlea_01,
      <ls_mapr> TYPE /gda/sdm_s_mapr_06,
      <ls_mean> TYPE /gda/sdm_s_mean_01,
      <ls_marm> TYPE /gda/sdm_s_marm_01,
      <ls_mlan> TYPE /gda/sdm_s_mlan_01,
      <ls_eord> TYPE /gda/sdm_s_eord_01,
      <ls_eina> TYPE /gda/sdm_s_eina_01,
      <ls_eine> TYPE /gda/sdm_s_eine_01,
      <ls_mg03> TYPE struc_tax,
      <ls_konh> TYPE konh,
      <ls_pricing> TYPE /gda/sdm_s_pricing_01,
      <ls_cond_header> TYPE ty_cond_header,
      <ls_tariffs> TYPE /gda/sdm_s_tariffs_01,
      <steuertab> LIKE LINE OF lt_steuertab,
      <steummtab> LIKE LINE OF lt_steummtab.

*    if me->ms_mara_spec-matnr is initial.
    READ TABLE me->mt_mara INTO me->ms_mara_spec
      WITH KEY matnr = me->mv_object.
*    endif.

*/ MAKT
    IF me->ms_selscreen-makt = abap_true.
      READ TABLE me->mt_makt INTO me->ms_makt_spec
        WITH KEY matnr = me->mv_object
                 spras = sy-langu.
      APPEND  me->ms_makt_spec TO me->mt_makt_spec.
    ENDIF.

*/ MARC
    IF me->ms_selscreen-marc = abap_true.
      READ TABLE mt_marc TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT me->mt_marc ASSIGNING <ls_marc> FROM sy-tabix.
          IF <ls_marc>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_marc> INTO TABLE me->mt_marc_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MAST
    IF me->ms_selscreen-mast = abap_true.
      READ TABLE mt_mast TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT me->mt_mast ASSIGNING <ls_mast> FROM sy-tabix.
          IF <ls_mast>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mast> INTO TABLE me->mt_mast_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MARD
    IF me->ms_selscreen-mard = abap_true.
      READ TABLE mt_mard TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mard ASSIGNING <ls_mard> FROM sy-tabix.
          IF <ls_mard>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mard> INTO TABLE me->mt_mard_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MVKE
    IF me->ms_selscreen-mvke = abap_true.
      READ TABLE mt_mvke TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mvke ASSIGNING <ls_mvke> FROM sy-tabix.
          IF <ls_mvke>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mvke> INTO TABLE me->mt_mvke_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MBEW
    IF me->ms_selscreen-mbew = abap_true.
      READ TABLE mt_mbew TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mbew ASSIGNING <ls_mbew> FROM sy-tabix.
          IF <ls_mbew>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mbew> INTO TABLE me->mt_mbew_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLGN
    IF me->ms_selscreen-mlgn = abap_true.
      READ TABLE mt_mlgn TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mlgn ASSIGNING <ls_mlgn> FROM sy-tabix.
          IF <ls_mlgn>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mlgn> INTO TABLE me->mt_mlgn_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLGN
    IF me->ms_selscreen-mlea = abap_true.
      READ TABLE mt_mlea TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mlea ASSIGNING <ls_mlea> FROM sy-tabix.
          IF <ls_mlea>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mlea> INTO TABLE me->mt_mlea_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

* MPGD

    IF me->ms_selscreen-mpgd = abap_true.
      READ TABLE mt_mpgd TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mpgd ASSIGNING <ls_mpgd> FROM sy-tabix.
          IF <ls_mpgd>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mpgd> INTO TABLE me->mt_mpgd_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

* WRPL

    IF me->ms_selscreen-wrpl = abap_true.
      READ TABLE mt_wrpl TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_wrpl ASSIGNING <ls_wrpl> FROM sy-tabix.
*          <ls_wrpl>-sdm_tabkey = <ls_wrpl>-matnr.
          IF <ls_wrpl>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_wrpl> INTO TABLE me->mt_wrpl_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLGT
    IF me->ms_selscreen-mlgt = abap_true.
      READ TABLE mt_mlgt TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mlgt ASSIGNING <ls_mlgt> FROM sy-tabix.
          IF <ls_mlgt>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mlgt> INTO TABLE me->mt_mlgt_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MAW1
    IF me->ms_selscreen-maw1 = abap_true.
      READ TABLE mt_maw1 TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_maw1 ASSIGNING <ls_maw1> FROM sy-tabix.
          IF <ls_maw1>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_maw1> INTO TABLE me->mt_maw1_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ TARIFFS

    READ TABLE mt_tariff TRANSPORTING NO FIELDS
      WITH KEY matnr = mv_object BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT mt_tariff ASSIGNING <ls_tariffs> FROM sy-tabix.
        IF <ls_tariffs>-matnr <> mv_object.
          EXIT.
        ELSE.
          INSERT <ls_tariffs> INTO TABLE me->mt_tariff_spec.
        ENDIF.
      ENDLOOP.
    ENDIF.


*/ MWLI
    IF me->ms_selscreen-mwli = abap_true.
      READ TABLE mt_mwli TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mwli ASSIGNING <ls_mwli> FROM sy-tabix.
          IF <ls_mwli>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mwli> INTO TABLE me->mt_mwli_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ WLK1
    IF me->ms_selscreen-wlk1 = abap_true.
      READ TABLE mt_wlk1 TRANSPORTING NO FIELDS
        WITH KEY artnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_wlk1 ASSIGNING <ls_wlk1> FROM sy-tabix.
          IF <ls_wlk1>-artnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_wlk1> INTO TABLE me->mt_wlk1_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ WLK2
    IF me->ms_selscreen-wlk2 = abap_true.
      READ TABLE mt_wlk2 TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_wlk2 ASSIGNING <ls_wlk2> FROM sy-tabix.
          IF <ls_wlk2>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_wlk2> INTO TABLE me->mt_wlk2_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MYMS
    IF me->ms_selscreen-myms = abap_true.
      READ TABLE mt_myms TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object." binary search.
      IF sy-subrc = 0.
        LOOP AT mt_myms ASSIGNING <ls_myms> FROM sy-tabix.
          IF <ls_myms>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_myms> INTO TABLE me->mt_myms_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MAMT
    IF me->ms_selscreen-mamt = abap_true.
      READ TABLE mt_mamt TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mamt ASSIGNING <ls_mamt> FROM sy-tabix.
          IF <ls_mamt>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_mamt> INTO TABLE me->mt_mamt_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MALG
    IF me->ms_selscreen-malg = abap_true.
      READ TABLE mt_malg TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_malg ASSIGNING <ls_malg> FROM sy-tabix.
          IF <ls_malg>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_malg> INTO TABLE me->mt_malg_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

* MG03 - TAX
    LOOP AT mt_mg03 ASSIGNING <ls_mg03> WHERE matnr = mv_object.
*    move-corresponding <mg03>-mg03steuer to gs_mg03_sdm.
*    gs_mg03_sdm-matnr = <mg03>-matnr.
*    append gs_mg03_sdm to gt_mg03_sdm.
      ls_steuertab_spec-sdm_tabkey = ls_steuertab_spec-matnr = <ls_mg03>-matnr.
      MOVE-CORRESPONDING <ls_mg03>-mg03steuer TO ls_steuertab_spec.
*        ls_steuertab_spec-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'KONH'
*                                                                             i_contents = ls_konh_sdm ).

      INSERT ls_steuertab_spec INTO TABLE me->mt_mg03_spec.

      CLEAR: ls_steuertab_spec.
    ENDLOOP.

* KONH
    READ TABLE me->mt_cond_header ASSIGNING <ls_cond_header>
     WITH KEY matnr = mv_object BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT me->mt_konh ASSIGNING <ls_konh> WHERE knumh = <ls_cond_header>-knumh.
        MOVE-CORRESPONDING <ls_konh> TO ls_konh_sdm.
        ls_konh_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'KONH'
                                                                             i_contents = ls_konh_sdm ).

        APPEND ls_konh_sdm TO me->mt_konh_spec.
        CLEAR ls_konh_sdm.

      ENDLOOP.
    ENDIF.

*PRICING
    LOOP AT me->mt_pricing ASSIGNING <ls_pricing>.
      READ TABLE me->mt_konh ASSIGNING <ls_konh>
                                     WITH KEY knumh = <ls_pricing>-knumh.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <ls_pricing> TO ls_pricing_sdm.
        ls_pricing_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'KONH'
                                                                                        i_contents = ls_pricing_sdm ).
        APPEND ls_pricing_sdm TO me->mt_pricing_spec.
        CLEAR ls_pricing_sdm.
      ENDIF.
    ENDLOOP.



*/ MAPR (Forecasting)
    IF me->ms_selscreen-mapr = abap_true.
      READ TABLE mt_mapr TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mapr ASSIGNING <ls_mapr> FROM sy-tabix.
          IF <ls_mapr>-matnr <> mv_object.
            EXIT.
          ELSE.
*/ Read with FM
            CALL FUNCTION 'MPOP_SINGLE_READ'
              EXPORTING
*               kzrfb      = SPACE    " Ind.: Refresh buffer entry for material no.
                matnr      = <ls_mapr>-matnr
*               maxtz      =     " Max. No. of Entries in Buffer
                werks      = <ls_mapr>-werks
              IMPORTING
*               wmpop      =     " Work area for MPOP
                o_mpop     = ls_mpop
*        TABLES
*               prowf_tab  =     " Table of forecast values (w/o key)
              EXCEPTIONS
                not_found  = 1
                wrong_call = 2
                OTHERS     = 3.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_mpop TO ls_mpop_sdm.
*              ls_mpop_sdm-sdm_tabkey = <ls_mapr>-sdm_tabkey.
              ls_mpop_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAPR' "'MARC'
                                                                           i_contents = ls_mpop_sdm ).

              INSERT ls_mpop_sdm INTO TABLE me->mt_mpop_spec.
              CLEAR:
               ls_mpop,
               ls_mpop_sdm.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

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
    IF me->ms_selscreen-mlan = abap_true.
      CALL FUNCTION 'STEUERTAB_READ'
        EXPORTING
*         kzrfb           = ' '
          matnr           = me->mv_object
        TABLES
          steuertab       = lt_steuertab
        EXCEPTIONS
          wrong_call      = 1
          steuertab_empty = 2
          OTHERS          = 3. "#EC *

      LOOP AT lt_steuertab ASSIGNING <steuertab>.
        MOVE-CORRESPONDING <steuertab> TO ls_steuertab_spec.
        ls_steummtab_spec-matnr      = ms_mara_spec-matnr.
        ls_steuertab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        APPEND ls_steuertab_spec TO me->mt_steuertab_spec.
        CLEAR:
         ls_steuertab_spec.
      ENDLOOP.

      CALL FUNCTION 'STEUMMTAB_READ'
        EXPORTING
*         kzrfb           = ' '
          matnr           = me->mv_object
        TABLES
          steummtab       = lt_steummtab
        EXCEPTIONS
          wrong_call      = 1
          steummtab_empty = 2
          OTHERS          = 3. "#EC *

      LOOP AT lt_steummtab ASSIGNING <steummtab>.
        MOVE-CORRESPONDING <steummtab> TO ls_steummtab_spec.
        ls_steummtab_spec-matnr      = ms_mara_spec-matnr.
        ls_steummtab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        APPEND ls_steummtab_spec TO me->mt_steummtab_spec.
        CLEAR:
         ls_steummtab_spec.
      ENDLOOP.

      IF me->mt_steummtab_spec[] IS INITIAL.
        ls_steummtab_spec-sdm_tabkey = ms_mara_spec-sdm_tabkey.
        APPEND ls_steummtab_spec TO me->mt_steummtab_spec.
      ENDIF.

      READ TABLE mt_mlan TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object.
      IF sy-subrc = 0.
        LOOP AT mt_mlan ASSIGNING <ls_mlan> FROM sy-tabix.
          IF <ls_mlan>-matnr <> mv_object.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_mlan> TO ls_mlan_sdm.
            APPEND ls_mlan_sdm TO  me->mt_mlan_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MARM (Units of Measure)
    IF me->ms_selscreen-marm = abap_true.
      READ TABLE mt_marm TRANSPORTING NO FIELDS
       WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_marm ASSIGNING <ls_marm> FROM sy-tabix.
          IF <ls_marm>-matnr <> mv_object.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_marm> TO ls_smeinh_sdm.
            MOVE-CORRESPONDING <ls_marm> TO ls_marm_sdm.

            APPEND ls_marm_sdm   TO me->mt_marm_spec.
            APPEND ls_smeinh_sdm TO me->mt_meinh_spec.
            CLEAR ls_smeinh_sdm.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MEAN (Additional EANs)
    IF me->ms_selscreen-mean = abap_true.
      READ TABLE mt_mean TRANSPORTING NO FIELDS
       WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mean ASSIGNING <ls_mean> FROM sy-tabix.
          IF <ls_mean>-matnr <> mv_object.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_mean> TO ls_mean_sdm.
            APPEND ls_mean_sdm TO me->mt_mean_spec.
            CLEAR:
             ls_mean_sdm.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ EORD
    IF me->ms_selscreen-eord = abap_true.
      READ TABLE mt_eord TRANSPORTING NO FIELDS
       WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_eord ASSIGNING <ls_eord> FROM sy-tabix.
          IF <ls_eord>-matnr <> mv_object.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_eord> TO ls_eord_sdm.
            APPEND ls_eord_sdm TO me->mt_eord_spec.
            CLEAR:
             ls_eord_sdm.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ EINA
    IF me->ms_selscreen-eina = abap_true.
      READ TABLE mt_eina TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_object BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_eina ASSIGNING <ls_eina> FROM sy-tabix.
          IF <ls_eina>-matnr <> mv_object.
            EXIT.
          ELSE.
            INSERT <ls_eina> INTO TABLE me->mt_eina_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ EINE
    IF me->ms_selscreen-eine = abap_true.
      LOOP AT me->mt_eina_spec ASSIGNING FIELD-SYMBOL(<eina_spec>).
        LOOP AT mt_eine ASSIGNING  FIELD-SYMBOL(<eine>)
           WHERE infnr = <eina_spec>-infnr.
          INSERT <eine> INTO TABLE me->mt_eine_spec.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

*/RMMW1
* populate stores
    DATA   gs_rmmw1      TYPE /gda/sdm_s_rmmw1_01. "RMMW1
    LOOP AT me->mt_marc_spec ASSIGNING FIELD-SYMBOL(<lfs_marc>).
      SELECT SINGLE werks FROM t001w
                          INTO gs_rmmw1-fiwrk
                        WHERE werks = <lfs_marc>-werks
                         AND vlfkz = 'A'.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING <lfs_marc> TO gs_rmmw1.
      APPEND gs_rmmw1 TO me->mt_rmmw1_spec.
    ENDLOOP.

* populate DC
    CLEAR: gs_rmmw1.
    LOOP AT me->mt_marc_spec ASSIGNING <lfs_marc>.
      SELECT SINGLE werks FROM t001w
                          INTO gs_rmmw1-vzwrk
                        WHERE werks = <lfs_marc>-werks
                          AND vlfkz = 'B'.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING <lfs_marc> TO gs_rmmw1.
      APPEND gs_rmmw1 TO me->mt_rmmw1_spec.
    ENDLOOP.

* populate Sales org
    CLEAR: gs_rmmw1.
    LOOP AT me->mt_mvke_spec ASSIGNING FIELD-SYMBOL(<lfs_mvke>).
      gs_rmmw1-vkorg = <lfs_mvke>-vkorg.
      gs_rmmw1-vtweg = <lfs_mvke>-vtweg.
      MOVE-CORRESPONDING <lfs_mvke> TO gs_rmmw1.
      COLLECT gs_rmmw1 INTO me->mt_rmmw1_spec.
    ENDLOOP.

* populate Vendor
    CLEAR: gs_rmmw1.
    LOOP AT me->mt_eina_spec ASSIGNING FIELD-SYMBOL(<lfs_eina>).
      READ TABLE me->mt_eine_spec ASSIGNING FIELD-SYMBOL(<lfs_eine>) WITH KEY infnr = <lfs_eina>-infnr.
      gs_rmmw1-lifnr = <lfs_eina>-lifnr.
      IF <lfs_eine> IS ASSIGNED.
        gs_rmmw1-ekorg = <lfs_eine>-ekorg.
      ENDIF.
      MOVE-CORRESPONDING <lfs_eina> TO gs_rmmw1.
      MOVE-CORRESPONDING <lfs_eine> TO gs_rmmw1.
      APPEND gs_rmmw1 TO  me->mt_rmmw1_spec.
    ENDLOOP.

    me->rsr_data_pbo( ).

  ENDMETHOD.


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


  METHOD BUILD_WRPL.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    FIELD-SYMBOLS:
      <ls_wrpl> LIKE LINE OF me->mt_wrpl.

    IF me->ms_selscreen-wrpl = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_WRPL_01' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM wrpl
          INTO CORRESPONDING FIELDS OF TABLE me->mt_wrpl
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr.

      catch cx_sy_open_sql_error into lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MPGD_01:| && me->mv_message.
*        raise exception type /gda/cx_sdm_exception_handl
*          exporting
*            mv_text = mv_message.
    ENDTRY.


    loop at me->mt_wrpl assigning <ls_wrpl>.
      <ls_wrpl>-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'WRPL'
                                                                           i_contents = <ls_wrpl> ).
    endloop.

  ENDMETHOD.


  method CONSTRUCTOR.

    super->constructor( iv_sprint = iv_sprint ).

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
    me->build_mpgd( ).
    me->build_wrpl( ).
    me->build_mlea( ).
    me->build_mlgt( ).
    me->build_maw1( ).
    me->build_mast( ).
    me->build_mapr( ).
    me->build_mwli( ).
    me->build_mg03( ).
    me->build_tariffs( ).
    me->build_pricing( ).

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


  METHOD refresh.
*CALL METHOD SUPER->REFRESH
*    .
    CLEAR: me->mv_object.
*    CLEAR: me->ms_makt_spec.
    FREE: me->mt_marc_spec, me->mt_mard_spec,
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
          me->mt_eord_spec, me->mt_tariff_spec,
          me->mt_rmmw1_spec, me->mt_mlea_spec,
          me->mt_mpgd_spec, me->mt_wrpl_spec,
          me->mt_meinh_spec.


  ENDMETHOD.


  method rsr_data_pbo.
    data:
     mard type /gda/sdm_s_mard_01.

*CALL METHOD SUPER->RSR_DATA_PBO
*    .
    check sy-tcode = '/GDA/SDM_ART_RSR'.
* This special condition is only relevant when RSR config entries exist
* where primary is MARD and Secondary is MARC

    data:
      setup_primary  type /gda/sdm_setup5,
      setup_secondary type /gda/sdm_setup5.

    if me->mv_do_rsr_logic is initial.
      select single * from /gda/sdm_setup5
                      into setup_primary
                      where object_type = 'ARTICLE'
                        and tabname = '/GDA/SDM_S_MARD_01'
                        and seq     = 01.


      select single * from /gda/sdm_setup5
                      into setup_secondary
                      where object_type = 'ARTICLE'
                        and tabname = '/GDA/SDM_S_MARC_01'
                        and seq     = 02.


      if  setup_primary-tabname   = '/GDA/SDM_S_MARD_01'
      and setup_secondary-tabname = '/GDA/SDM_S_MARC_01'.
        me->mv_do_rsr_logic = 'Y'.
      else.
        me->mv_do_rsr_logic = 'N'.
      endif.
    endif.

    if me->mv_do_rsr_logic = 'Y'.
      loop at me->mt_marc_spec assigning field-symbol(<marc>).
        read table me->mt_mard_spec transporting no fields with key werks = <marc>-werks.
        check sy-subrc <> 0.
        mard-matnr = <marc>-matnr.
        mard-werks = <marc>-werks.
        mard-lgort = '****'.
        mard-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD' i_contents = mard ).
        insert mard into table me->mt_mard_spec.
        clear:
         mard.
      endloop.
    endif.
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
    me->ms_selscreen-mlea = abap_true.
    me->ms_selscreen-mpgd = abap_true.
    me->ms_selscreen-wrpl = abap_true.

  endmethod.
ENDCLASS.
