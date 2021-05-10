class /GDA/SDM_CL_ARTICLE definition
  public
  inheriting from /GDA/SDM_CL_CORE
  final
  create public .

public section.

  interfaces /GDA/SDM_IF_CORE_STATICS .

  types:
    begin of ty_report_output,
            matnr          type matnr,
            maktx          type maktx,
            mtart          type mtart,
            matkl          type matkl,
            mstae          type mstae,
            message        type /gda/sdm_de_message,
            message_id     type symsgid,
            message_number type symsgno,
            message_type   type symsgty,
            count          type /gda/sdm_de_counter,
            extra_v1       type /gda/sdm_extra_information,
            extra_v2       type /gda/sdm_extra_information,
            extra_v3       type /gda/sdm_extra_information,
            extra_v4       type /gda/sdm_extra_information,
            extra_v5       type /gda/sdm_extra_information,
            extra_v6       type /gda/sdm_extra_information,
        end of ty_report_output .
  types:
    ty_it_report_output type table of ty_report_output .
  types:
    begin of ty_report_output_der,
            sdm_type        type  /gda/sdm_de_type,
            sdm_description type val_text,
            matnr           type  matnr,
            table           type  table_name,
            field           type  field_name,
            value           type  text200,
            skip_derivation type  xfeld,
            screen_name     type  text132,
            grey_out        type  /gda/sdm_de_sc_grey,
            hide            type  /gda/sdm_de_sc_hide,
            required        type  /gda/sdm_de_sc_required,
            bold            type  /gda/sdm_de_sc_bold,
            group1          type  /gda/sdm_de_sc_grp1,
            group2          type  /gda/sdm_de_sc_grp2,
            group3          type  /gda/sdm_de_sc_grp3,
            group4          type  /gda/sdm_de_sc_grp4 ,
            sdm_tabkey      type cdtabkey,
            message1        type  char255,
            message2        type  char255,
            message3        type  char255,
        end of ty_report_output_der .
  types:
    ty_it_report_output_der type table of ty_report_output_der .

  constants MC_PRIMARY type TYPENAME value 'MATNR'. "#EC NOTEXT
  class-data MC_OBJECT type /GDA/SDM_DE_OBJECT value 'ARTICLE'. "#EC NOTEXT .  .  .  . " .
  class-data MC_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID value 01. "#EC NOTEXT .  .  .  . " .
  constants MC_BOR type MASSOBJTYP value 'BUS1001'. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_TYPE type /GDA/SDM_DE_TYPE
      !IV_STATS type BOOLEAN
      !IV_STATS_BRF type BOOLEAN optional
      !IV_ERRORS_ONLY type BOOLEAN optional
      !IV_SPRINT type BOOLEAN optional
      !IV_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING optional
      !IV_STEWARDSHIP type ref to /GDA/SDM_CL_STWD_APP_MAIN optional
    raising
      CX_FDT_INPUT .
  class-methods CLASS_CONSTRUCTOR .

  methods /GDA/SDM_IF_ACTION_PROCESSOR~PROCESS_ACTION
    redefinition .
  methods CREATE_BRF_RESULT_STRUCTURE
    redefinition .
  methods GET_OBJECT_KEYS
    redefinition .
  methods GET_PRIMARY_OBJECT
    redefinition .
  methods GET_RSR_STRUCTURE
    redefinition .
  methods GET_RSR_STRUCTURE_KEY
    redefinition .
  methods GET_TABLE_FROM_STRUCTURE
    redefinition .
  methods RETURN_BRF_RESULT_STRUCTURE
    redefinition .
protected section.

  methods CALL_BRF
    redefinition .
  methods CALL_CLASS
    redefinition .
  methods RECORD_STATISTICS
    redefinition .
  methods RECORD_STATISTICS_V2
    redefinition .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_ARTICLE IMPLEMENTATION.


  method /gda/sdm_if_action_processor~process_action.
    call method super->/gda/sdm_if_action_processor~process_action
      exporting
        x_action = x_action
        x_multi  = x_multi
*  IMPORTING
*       y_refresh        =
*       y_action_handled =
*       y_not_authorised =
      .

    data:
      lv_fcode      type          sy-ucomm.

    field-symbols:
      <brf_attributes> like line of me->mt_brf_attributes,
      <ref>            type any,
      <matnr>          type any,
      <mstae>          type any.

* Handle common actions to SDM
    lv_fcode = x_action->get_fcode( ).

* Check for common actions
    case lv_fcode.


      when 'MM43'.
        read table me->mt_brf_attributes_instance assigning <brf_attributes> with key name = '/GDA/SDM_S_MARA_01'
                                                                             type = me->mv_type.
        assign <brf_attributes>-lr_data->* to <ref>.
        assign component 'MATNR' of structure <ref> to <matnr>.

        set parameter id: 'MAT' field <matnr>.
        call transaction 'MM43' and skip first screen.  "#EC CI_CALLTA.

      when 'MM42'.
        read table me->mt_brf_attributes_instance assigning <brf_attributes> with key name = '/GDA/SDM_S_MARA_01'
                                                                             type = me->mv_type.
        assign <brf_attributes>-lr_data->* to <ref>.
        assign component 'MATNR' of structure <ref> to <matnr>.

        set parameter id: 'MAT' field <matnr>.
        call transaction 'MM42' and skip first screen.  "#EC CI_CALLTA.

      when 'VKP5'.
        read table me->mt_brf_attributes_instance assigning <brf_attributes> with key name = '/GDA/SDM_S_MARA_01'
                                                                             type = me->mv_type.
        assign <brf_attributes>-lr_data->* to <ref>.
        assign component 'MATNR' of structure <ref> to <matnr>.

        set parameter id: 'MAT' field <matnr>.
        call transaction 'VKP5'.                        "#EC CI_CALLTA.
      when '/GDA/SDM_STAT_CLEAR'.
        read table me->mt_brf_attributes_instance assigning <brf_attributes> with key name = '/GDA/SDM_S_MARA_01'
                                                                             type = me->mv_type.
        assign <brf_attributes>-lr_data->* to <ref>.
        assign component 'MATNR' of structure <ref> to <matnr>.
        set parameter id: 'MAT' field <matnr>.

        assign component 'MSTAE' of structure <ref> to <mstae>.
        set parameter id: 'MST' field <mstae>.

        call transaction '/GDA/SDM_STAT_CLEAR' and skip first screen. "#EC CI_CALLTA.

      when others.
    endcase.
  endmethod.


  method /GDA/SDM_IF_CORE_STATICS~GET_MASTER_DATA_OBJECT_DESC.
    select single maktx from makt
      into rv_desc
      where matnr = iv_value
        and spras = sy-langu.
  endmethod.


  method call_brf.

    data:
      lt_sdm_gui_out       type standard table of /gda/sdm_s_val_results_key,
      ls_sdm_gui_out       type /gda/sdm_s_val_results_key,
      lt_report_output_der type standard table of ty_report_output_der,
      ls_report_output_der type ty_report_output_der,
      lv_message           type string,
      ls_brf_exc_rep_line  type /gda/sdm_exceptions_alv,
      er_data              type ref to data,
      er_data_empty        type ref to data.

    field-symbols:
      <results>             type standard table,
      <val_results>         like line of me->mt_validation_results,
      <der_results>         like line of me->mt_derivation_results,
      <table>               type any table,
      <any>                 type any,
      <attributes_instance> like line of me->mt_brf_attributes_instance,
      <matnr>               type any,
      <mara>                type /gda/sdm_s_mara_01,
      <makt>                type /gda/sdm_s_makt_01,
      <makt_tab>            type /gda/sdm_t_makt_01,
      <tab>                 type any,
      <value>               type any,
      <name>                type any.

    try.
        call method super->call_brf.
      catch /gda/cx_sdm_exception_handl.
        raise exception type /gda/cx_sdm_exception_handl.
      catch cx_fdt_input.
        raise exception type cx_fdt_input.
    endtry.

* For Reporting - Return BRF results with Master Data
    if me->mv_source = me->mc_rep.

      if me->mv_type = me->mc_validation.
        create data er_data       like lt_sdm_gui_out.
        create data er_data_empty like lt_sdm_gui_out.
      elseif me->mv_type = me->mc_derivation.
        create data er_data       like lt_report_output_der.
        create data er_data_empty like lt_report_output_der.
      else. " Most likely Post Processing
        create data er_data       like lt_sdm_gui_out.
        create data er_data_empty like lt_sdm_gui_out.
      endif.

      assign er_data->*           to <table>.

* Derivation
      if me->mv_type = me->mc_derivation.

        read table me->mt_brf_attributes_instance assigning <attributes_instance> with key name = '/GDA/SDM_S_MARA_01'
                                                                                           type = me->mv_type.
        if sy-subrc = 0.
          assign <attributes_instance>-lr_data->* to <mara>.
        endif.

        read table me->mt_brf_attributes_instance assigning <attributes_instance> with key name = '/GDA/SDM_S_MAKT_01'
                                                                                            type = me->mv_type.
        if sy-subrc = 0.
          assign <attributes_instance>-lr_data->* to <makt>.
        else.
          read table me->mt_brf_attributes_instance assigning <attributes_instance> with key name = '/GDA/SDM_T_MAKT_01'
                                                                                              type = me->mv_type.
        endif.

        if sy-subrc = 0.
          assign <attributes_instance>-lr_data->* to <mara>.
        endif.

        assign me->mo_brf_result->* to <results>.

        loop at <results> assigning <any>.

*          assign component 'VALUE' of structure <any> to <value>.
*          if <value> = 'Config'.
*            assign component 'TABLE' of structure <any> to <tab>.
*
*            replace '_01' with '' into  <tab>.
*            condense <tab> no-gaps.
*
*            if <tab> cs 'T/GDA/SDM_S_'.
*              replace 'T/GDA/SDM_S_' with 'W' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs 'T/GDA/SDM_T_'.
*              replace 'T/GDA/SDM_T_' with 'W' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs '/GDA/SDM_S_'.
*              replace '/GDA/SDM_S_' with '' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs 'T/GDA/SDM_T_'.
*              replace '/GDA/SDM_T_' with '' into  <tab>.
*              condense <tab> no-gaps.
*            endif.
*
*            assign component 'SCREEN_NAME' of structure <any> to <name>.
*
*            if <name> cs 'T/GDA/SDM_S_'.
*              replace 'T/GDA/SDM_S_' with '/GDA/SDM_S_' into  <name>.
*              condense <name> no-gaps.
*            elseif <name> cs '/GDA/SDM_T_'.
*              replace 'T/GDA/SDM_T_' with 'T/GDA/SDM_T_' into  <name>.
*              condense <name> no-gaps.
*            endif.
*
*          endif.

          move-corresponding <any> to ls_report_output_der.
          if <mara> is assigned.
            ls_report_output_der-matnr = <mara>-matnr.
          endif.

* Special Conditions for screen fields names that are not standard
          if ls_report_output_der-table = 'MEINH'.
            ls_report_output_der-table = 'SMEINH'.
          endif.

          ls_report_output_der-sdm_type           = me->mv_type.
          ls_report_output_der-sdm_description    = me->mv_sdm_description.

          collect ls_report_output_der into <table>.
          clear ls_report_output_der.
        endloop.

* Validations and others
      else.

        read table me->mt_brf_attributes_instance assigning <attributes_instance> with key name = '/GDA/SDM_S_MARA_01'
                                                                                            type = me->mv_type.
        if sy-subrc = 0.
          assign <attributes_instance>-lr_data->* to <mara>.
        endif.

        read table me->mt_brf_attributes_instance assigning <attributes_instance> with key name = '/GDA/SDM_S_MAKT_01'
                                                                                            type = me->mv_type.
        if sy-subrc = 0.
          assign <attributes_instance>-lr_data->* to <makt>.
        else.
          read table me->mt_brf_attributes_instance assigning <attributes_instance> with key type = me->mv_type
                                                                                             name = '/GDA/SDM_T_MAKT_01' binary search.
          if sy-subrc = 0.
            assign <attributes_instance>-lr_data->* to  <makt_tab>.
            if <makt_tab> is assigned.
              read table <makt_tab> assigning <makt> with key spras = sy-langu.
            endif.
          endif.
        endif.
        assign me->mo_brf_result->* to <results>.

        loop at <results> assigning <val_results>.

          if <val_results>-id = space.
            continue.
          endif.

          if me->mv_errors_only = abap_true.
            if <val_results>-type na 'EAX'.
              continue.
            endif.
          endif.

          message id <val_results>-id
                type <val_results>-type
              number <val_results>-number
                with <val_results>-message_v1 <val_results>-message_v2
                     <val_results>-message_v3 <val_results>-message_v4
                into lv_message.

* Populate key fields for ARTICLE...
          ls_sdm_gui_out-sdm_type        = me->mv_type.
          ls_sdm_gui_out-sdm_description = me->mv_sdm_description.

          if <mara> is assigned.
            ls_sdm_gui_out-matnr = <mara>-matnr.
            ls_sdm_gui_out-mtart = <mara>-mtart.
            ls_sdm_gui_out-matkl = <mara>-matkl.
            ls_sdm_gui_out-mstae = <mara>-mstae.
          endif.

          if <makt> is assigned.
            ls_sdm_gui_out-maktx = <makt>-maktx.
          endif.

          ls_sdm_gui_out-message        = lv_message.
          ls_sdm_gui_out-id             = <val_results>-id.
          ls_sdm_gui_out-number         = <val_results>-number.
          ls_sdm_gui_out-type           = <val_results>-type.
          ls_sdm_gui_out-extra_v1       = <val_results>-extra_v1.
          ls_sdm_gui_out-extra_v2       = <val_results>-extra_v2.
          ls_sdm_gui_out-extra_v3       = <val_results>-extra_v3.
          ls_sdm_gui_out-extra_v4       = <val_results>-extra_v4.
          ls_sdm_gui_out-extra_v5       = <val_results>-extra_v5.
          ls_sdm_gui_out-extra_v6       = <val_results>-extra_v6.
          ls_sdm_gui_out-ds_link_id     = <val_results>-ds_link_id.
          ls_sdm_gui_out-sdm_tabkey     = <val_results>-sdm_tabkey.

          collect ls_sdm_gui_out into <table>.
          clear ls_sdm_gui_out.

        endloop.
      endif.

      mo_brf_result       = er_data.
      mo_brf_result_empty = er_data_empty.
    else.
      assign me->mo_brf_result->* to <results>.

      if me->mv_type = me->mc_derivation.
        loop at <results>  assigning <any>.
*          assign component 'VALUE' of structure <any> to <value>.
*
          assign component 'VALUE' of structure <any> to <value>.
*          if <value> = 'Config'.
          if <value> = text-100.
            assign component 'TABLE' of structure <any> to <tab>.
* Special Conditions for screen fields names that are not standard
            if <tab> = 'MEINH'.
              <tab> = 'SMEINH'.
            endif.
         endif.
*            assign component 'TABLE' of structure <any> to <tab>.
*
*            replace '_01' with '' into  <tab>.
*            condense <tab> no-gaps.
*
*            if <tab> cs 'T/GDA/SDM_S_'.
*              replace 'T/GDA/SDM_S_' with 'W' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs 'T/GDA/SDM_T_'.
*              replace 'T/GDA/SDM_T_' with 'W' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs '/GDA/SDM_S_'.
*              replace '/GDA/SDM_S_' with '' into  <tab>.
*              condense <tab> no-gaps.
*            elseif <tab> cs 'T/GDA/SDM_T_'.
*              replace '/GDA/SDM_T_' with '' into  <tab>.
*              condense <tab> no-gaps.
*            endif.
*
*            assign component 'SCREEN_NAME' of structure <any> to <name>.
*
*            if <name> cs 'T/GDA/SDM_S_'.
*              replace 'T/GDA/SDM_S_' with '/GDA/SDM_S_' into  <name>.
*              condense <name> no-gaps.
*            elseif <name> cs '/GDA/SDM_T_'.
*              replace 'T/GDA/SDM_T_' with 'T/GDA/SDM_T_' into  <name>.
*              condense <name> no-gaps.
*            endif.
*
*          endif.
*
        endloop.
      endif.
    endif.
  endmethod.


  METHOD call_class.

*    DATA:
*      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
*      ls_sdm_gui_out       TYPE /gda/sdm_s_val_results_key,
*      lt_report_output_der TYPE STANDARD TABLE OF ty_report_output_der,
*      ls_report_output_der TYPE ty_report_output_der,
*      lv_message           TYPE string,
*      ls_brf_exc_rep_line  TYPE /gda/sdm_exceptions_alv,
*      er_data              TYPE REF TO data,
*      er_data_empty        TYPE REF TO data.
*
*    FIELD-SYMBOLS:
*      <results>     TYPE STANDARD TABLE,
*      <val_results> LIKE LINE OF me->mt_validation_results,
*      <table>       TYPE ANY TABLE,
*      <any>         TYPE any,
*      <REF>         type any,
*      <KEY>         type any,
*      <BRF_ATTRIBUTES> like line of ME->MT_BRF_ATTRIBUTES.
*
*
*   READ TABLE ME->MT_BRF_ATTRIBUTES ASSIGNING <BRF_ATTRIBUTES> WITH KEY NAME = '/GDA/SDM_S_MARA_01'.
*
*    CHECK SY-SUBRC = 0.
*    ASSIGN <BRF_ATTRIBUTES>-LR_DATA->* TO <REF>.
*    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <REF> to <KEY>.
*    check sy-subrc = 0.
*
**    check <key> = 'R100000'.
*    TRY.
*        CALL METHOD super->call_class.
*      CATCH /gda/cx_sdm_exception_handl.
*      CATCH cx_fdt_input .
*    ENDTRY.
*
** For Reporting - Return BRF results with Master Data
*    IF me->mv_source = me->mc_rep.
*
*      IF me->mv_type = me->mc_validation.
*        CREATE DATA er_data       LIKE lt_sdm_gui_out.
*        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
*      ELSEIF me->mv_type = me->mc_derivation.
*        CREATE DATA er_data       LIKE lt_report_output_der.
*        CREATE DATA er_data_empty LIKE lt_report_output_der.
*      ELSE.
*        CREATE DATA er_data       LIKE lt_sdm_gui_out.
*        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
*      ENDIF.
*
*      ASSIGN er_data->*           TO <table>.
*
** INDEX TYPE ID NUMBER EXTRA_V1 EXTRA_V2 EXTRA_V4 EXTRA_V5
**3  E ZGD_BRF 532 EINA-RUECK  Return Agreement    5300007316
**4  E ZGD_BRF 533 EINE-APLFZ  Planned Delivery Time in Days   5300007316
**5  E ZGD_BRF 512 MARC-RDPRF  Rnding Profile  RFST
**6  E ZGD_BRF 512 MARC-RDPRF  Rnding Profile  RFST
**7  E ZGD_BRF 514 MARC-RDPRF  Rnding Profile  RFDC
**8  E ZGD_BRF 531 MARC-DISPO  MRP controller  RFDC
*
*      lv_message = 'Demo:BRF: X-site status may not be blank.'.
*
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '510'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'MARA-MSTAE'.
*      ls_sdm_gui_out-extra_v2       = 'X-plant status'.
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*      lv_message = 'Demo:BRF: Valid from date may not be blank if X-site status is maintained.'.
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '511'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'MARA-MSTDE'.
*      ls_sdm_gui_out-extra_v2       = 'Valid from'.
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*      lv_message = 'Demo:BRF: Return Agreement may not be blank EINA'.
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '532'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'EINA-RUECK'.
*      ls_sdm_gui_out-extra_v2       = 'Return Agreement'.
*      ls_sdm_gui_out-extra_v5       = '5300007316'.
*
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*      lv_message = 'Demo:BRF: Planned Delivery Time in Days may not be blank EINE'.
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '533'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'EINE-APLFZ'.
*      ls_sdm_gui_out-extra_v2       = 'Planned Delivery Time in Days'.
*      ls_sdm_gui_out-extra_v5       = '5300007316'.
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*      lv_message = 'Demo:BRF:BRF: Rounding Profile may not be blank if MRP Type is ND. ST'.
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '512'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'MARC-RDPRF'.
*      ls_sdm_gui_out-extra_v2       = 'Rnding Profile'.
*      ls_sdm_gui_out-extra_v2       = 'RFST'.
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*      lv_message = 'Demo:BRF: Rounding Profile may not be blank if MRP Type is ND. DC'.
*      ls_sdm_gui_out-message        = lv_message.
*      ls_sdm_gui_out-id             = '/GDA/SDM1'.
*      ls_sdm_gui_out-number         = '514'.
*      ls_sdm_gui_out-type           = 'E'.
*      ls_sdm_gui_out-extra_v1       = 'MARC-RDPRF'.
*      ls_sdm_gui_out-extra_v2       = 'Rnding Profile'.
*      ls_sdm_gui_out-extra_v4       = 'RFDC'.
*      COLLECT ls_sdm_gui_out INTO <table>.
*      CLEAR ls_sdm_gui_out.
*
*    ENDIF.
*
*    mo_brf_result       = er_data.
*    mo_brf_result_empty = er_data_empty.

  ENDMETHOD.


  METHOD class_constructor.
    DATA:
      ms_function_attributes TYPE /gda/sdm_setup6.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes.

    SELECT * FROM /gda/sdm_setup6
                    INTO TABLE mt_function_attributes
                    WHERE sdm_object = MC_OBJECT
                      AND active     = abap_true.

*    TRY.
    SELECT * FROM /gda/sdm_setup1 INTO CORRESPONDING FIELDS OF TABLE mt_brf_attributes
      WHERE sdm_object = MC_OBJECT
       AND  active     = abap_true.
*           AND  type       = iv_type.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_fdt_input.
*            EXPORTING
*              previous   = lx_fdt
*              mt_message = lx_fdt->mt_message.

    ENDIF.


* Retrieve GUIDs...
    LOOP AT mt_brf_attributes ASSIGNING <brf_attributes>.

      CLEAR:
       <brf_attributes>-data_object.

      READ TABLE mt_function_attributes INTO ms_function_attributes WITH KEY sdm_object = MC_OBJECT
                                                                             type       = <brf_attributes>-type.
      CHECK sy-subrc = 0.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO mv_id
                        WHERE object_type = mc_brf_application
                        AND   name        = ms_function_attributes-application
                        AND   deleted     = space.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO <brf_attributes>-data_object
*                        WHERE object_type    = 'DO'
                        where object_type    = mc_brf_data_object
                        AND   name           = <brf_attributes>-name
                        AND   application_id = mv_id "lv_app_id
                        AND   deleted        = space.


    ENDLOOP.
  ENDMETHOD.


  method constructor.
    data:
      wf_ref     type ref to data,
      lx_root    type ref to cx_root,
      lv_message type string.

    field-symbols:
      <brf_attributes> like line of mt_brf_attributes,
      <ref>            type any.

    try.
        super->constructor( iv_object_type    = iv_object_type
                            iv_object_type_id = '01'
                            iv_source         = iv_source
                            iv_type           = iv_type
                            iv_stats          = iv_stats
                            iv_stats_brf      = iv_stats_brf
                            iv_errors_only    = iv_errors_only
                            iv_mapping        = iv_mapping
                            iv_stewardship    = iv_stewardship ).
      cleanup into lx_root.
        lv_message = lx_root->get_text( ).

*        lv_exit = abap_true.
*        RAISE EXCEPTION TYPE cx_fdt_input
*          EXPORTING
*            previous = lx_fdt.
    endtry.


* SDM_OBJECT
*if me->mt_message[] IS NOT INITIAL.
*  exit.
*endif.
   me->mv_object_value = me->get_primary_object( ).

    loop at mt_brf_attributes assigning <brf_attributes> where type = me->mv_type.
      if <brf_attributes>-ref_type is initial.
        <brf_attributes>-ref_type = '1'.
      endif.
      if <brf_attributes>-ref_type = '2' .
        create data wf_ref type standard table of (<brf_attributes>-name). "Dynamic Structure
        assign wf_ref->* to <ref>.
        get reference of <ref> into <brf_attributes>-lr_data.
      elseif <brf_attributes>-ref_type = '1'.
        create data wf_ref type (<brf_attributes>-name). "Dynamic Structure
        assign wf_ref->* to <ref>.
        get reference of <ref> into <brf_attributes>-lr_data.
      endif.
      append <brf_attributes> to mt_brf_attributes_instance.
    endloop.

  endmethod.


  METHOD create_brf_result_structure.

    DATA:
      er_data_empty        TYPE REF TO data,
      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
      lt_report_output_der TYPE STANDARD TABLE OF ty_report_output_der.


*CALL METHOD SUPER->CREATE_BRF_RESULT_STRUCTURE
*  RECEIVING
*    R_RESULT_STRUCTURE =
*    .

    IF me->mv_type = me->mc_validation.
      CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
    ELSEIF me->mv_type = me->mc_derivation.
      CREATE DATA er_data_empty LIKE lt_report_output_der.
    ELSE.
      CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
    ENDIF.

    r_result_structure = er_data_empty.

  ENDMETHOD.


  method get_object_keys.
*    r_key_fields = value #( ( 'KEY_MATNR' ) ( 'KEY_MAKTX' ) ( 'KEY_ATTYP' ) ( 'KEY_MATKL' ) ( 'KEY_MTART' ) ).
    r_key_fields = value #( ( 'KEY_MATNR' ) ).
  endmethod.


  METHOD get_primary_object.
    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF MT_BRF_ATTRIBUTES_INSTANCE,
      <key_structure>  TYPE any,
      <object_key>     TYPE any.

*    READ TABLE MT_BRF_ATTRIBUTES_INSTANCE ASSIGNING <brf_attributes> WITH KEY type = mv_type
*                                                                              name = '/GDA/SDM_S_MARA_01'.
    READ TABLE mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY type = mv_type
                                                                              name = '/GDA/SDM_S_MARA_01'.

    CHECK sy-subrc = 0.

    ASSIGN <brf_attributes>-lr_data->* TO <key_structure>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <key_structure> TO <object_key>.
    CHECK sy-subrc = 0.
    r_object_main = <object_key>.
  ENDMETHOD.


  METHOD get_rsr_structure.
    r_rsr_str = '/GDA/SDM_S_ARTICLE'.
  ENDMETHOD.


  method GET_RSR_STRUCTURE_KEY.
    R_OBJECT_RSR_KEY    =  'ARTICLE'.
  endmethod.


  method GET_TABLE_FROM_STRUCTURE.
* Generally this would involve structures that dont have SAP standard tables....
    case iv_structure.
      when '/GDA/SDM_S_MG03STEUMM_01'.
        r_tabname = '<SDM_RECORD>-/GDA/MG03STEUMM'.
      when '/GDA/SDM_S_MG03STEUER_01'.
        r_tabname = '<SDM_RECORD>-/GDA/MG03STEUER'.
      when others.
    endcase.

  endmethod.


  method record_statistics.

    me->mv_object_value = me->get_primary_object( ).

    try.
        call method super->record_statistics.
      catch /gda/cx_sdm_exception_handl .
    endtry.
  endmethod.


  method RECORD_STATISTICS_V2.
   me->mv_object_value = me->get_primary_object( ).

    try.
        call method super->record_statistics_v2.
      catch /gda/cx_sdm_exception_handl .
    endtry.
  endmethod.


  method RETURN_BRF_RESULT_STRUCTURE.
    if me->mv_source = me->mc_rep and me->mv_type = me->mc_validation.
      r_result_structure =  me->create_brf_result_structure( ).
      mo_brf_result_empty = r_result_structure.
    else.
      r_result_structure =  super->return_brf_result_structure( ).
    endif.
  endmethod.
ENDCLASS.
