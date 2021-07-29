class /GDA/SDM_CL_ARTICLE definition
  public
  inheriting from /GDA/SDM_CL_CORE
  final
  create public .

public section.

  interfaces /GDA/SDM_IF_CORE_STATICS .

  types:
    BEGIN OF ty_report_output,
              matnr          TYPE matnr,
              maktx          TYPE maktx,
              mtart          TYPE mtart,
              matkl          TYPE matkl,
              mstae          TYPE mstae,
              message        TYPE /gda/sdm_de_message,
              message_id     TYPE symsgid,
              message_number TYPE symsgno,
              message_type   TYPE symsgty,
              count          TYPE /gda/sdm_de_counter,
              extra_v1       TYPE /gda/sdm_extra_information,
              extra_v2       TYPE /gda/sdm_extra_information,
              extra_v3       TYPE /gda/sdm_extra_information,
              extra_v4       TYPE /gda/sdm_extra_information,
              extra_v5       TYPE /gda/sdm_extra_information,
              extra_v6       TYPE /gda/sdm_extra_information,
          END OF ty_report_output .
  types:
    ty_it_report_output TYPE TABLE OF ty_report_output .
  types:
    BEGIN OF ty_report_output_der,
              sdm_type        TYPE  /gda/sdm_de_type,
              sdm_description TYPE val_text,
              matnr           TYPE  matnr,
              linkage         TYPE  matnr,
              attyp           TYPE  attyp,
              table           TYPE  table_name,
              field           TYPE  field_name,
              value           TYPE  text200,
              skip_derivation TYPE  xfeld,
              screen_name     TYPE  text132,
              grey_out        TYPE  /gda/sdm_de_sc_grey,
              hide            TYPE  /gda/sdm_de_sc_hide,
              required        TYPE  /gda/sdm_de_sc_required,
              bold            TYPE  /gda/sdm_de_sc_bold,
              group1          TYPE  /gda/sdm_de_sc_grp1,
              group2          TYPE  /gda/sdm_de_sc_grp2,
              group3          TYPE  /gda/sdm_de_sc_grp3,
              group4          TYPE  /gda/sdm_de_sc_grp4 ,
              sdm_tabkey      TYPE cdtabkey,
              message1        TYPE  char255,
              message2        TYPE  char255,
              message3        TYPE  char255,
          END OF ty_report_output_der .
  types:
    ty_it_report_output_der TYPE TABLE OF ty_report_output_der .
  types:
    BEGIN OF ty_rept_output,
              sdm_type        TYPE /gda/sdm_de_type,
              sdm_description TYPE val_text,
              matnr           TYPE matnr,
              linkage         TYPE matnr,
              attyp           TYPE attyp,
              maktx           TYPE maktx,
              mtart           TYPE mtart,
              matkl           TYPE matkl,
              mstae           TYPE mstae,
              type            TYPE bapi_mtype,
              id              TYPE symsgid,
              number          TYPE symsgno,
              message	        TYPE bapi_msg,
              message_v1      TYPE symsgv,
              message_v2      TYPE symsgv,
              message_v3      TYPE symsgv,
              message_v4      TYPE symsgv,
              extra_v1        TYPE /gda/sdm_extra_information,
              extra_v2        TYPE /gda/sdm_extra_information,
              extra_v3        TYPE /gda/sdm_extra_information,
              extra_v4        TYPE /gda/sdm_extra_information,
              extra_v5        TYPE /gda/sdm_extra_information,
              extra_v6        TYPE /gda/sdm_extra_information,
              ds_link_id      TYPE /gda/sdm_de_ds_link8,
              sdm_tabkey      TYPE cdtabkey,
          END OF ty_rept_output .
  types:
    ty_it_rept_output TYPE TABLE OF ty_rept_output .

  constants MC_PRIMARY type TYPENAME value 'MATNR'. "#EC NOTEXT
  class-data MC_OBJECT type /GDA/SDM_DE_OBJECT value 'ARTICLE'. "#EC NOTEXT .  .  .  .  . " .
  class-data MC_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID value 01. "#EC NOTEXT .  .  .  .  . " .
  constants MC_BOR type MASSOBJTYP value 'BUS1001001'. "#EC NOTEXT

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
    sdm_gui_messages_val type standard table of ty_rept_output,
    sdm_gui_message_val  type ty_rept_output,
    sdm_gui_messages_der type standard table of ty_report_output_der,
    sdm_gui_message_der  type ty_report_output_der,
    message              type string,
    sdm_messages         type ref to data,
    sdm_messages_empty   type ref to data.

   field-symbols:
    <brf_results_val>     like line of me->mt_validation_results,
    <attributes_instance> like line of me->mt_brf_attributes_instance,
    <mara>                type /gda/sdm_s_mara_01,
    <makt>                type /gda/sdm_s_makt_01,
    <makt_tab>            type /gda/sdm_t_makt_01,
    <table>               type any table,
    <brf_results>         type standard table,
    <brf_result>          type any.

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
        create data sdm_messages       like sdm_gui_messages_val.
        create data sdm_messages_empty like sdm_gui_messages_val.
      elseif me->mv_type = me->mc_derivation.
        create data sdm_messages       like sdm_gui_messages_der.
        create data sdm_messages_empty like sdm_gui_messages_der.
      else. " Most likely Post Processing
        create data sdm_messages       like sdm_gui_messages_val.
        create data sdm_messages_empty like sdm_gui_messages_val.
      endif.
      assign sdm_messages->*           to <table>.
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

        assign me->mo_brf_result->* to <brf_results>.


        loop at <brf_results> assigning <brf_result>.
          move-corresponding <brf_result> to sdm_gui_message_der.
          if <mara> is assigned.
            sdm_gui_message_der-matnr = <mara>-matnr.
          endif.

* Special Conditions for screen fields names that are not standard
          if sdm_gui_message_der-table = 'MEINH'.
            sdm_gui_message_der-table = 'SMEINH'.
          endif.

          sdm_gui_message_der-sdm_type           = me->mv_type.
          sdm_gui_message_der-sdm_description    = me->mv_sdm_description.

          collect sdm_gui_message_der into <table>.
          clear sdm_gui_message_der.
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
        assign me->mo_brf_result->* to <brf_results>.

        loop at <brf_results> assigning <brf_results_val>.

          if me->mv_errors_only = abap_true.
            if <brf_results_val>-type na 'EAX'.
              continue.
            endif.
          endif.

          if <brf_results_val>-id = space or <brf_results_val>-sdm_tabkey cs '*'.
            continue.
          endif.

          message id <brf_results_val>-id
                type <brf_results_val>-type
              number <brf_results_val>-number
                with <brf_results_val>-message_v1 <brf_results_val>-message_v2
                     <brf_results_val>-message_v3 <brf_results_val>-message_v4
                into message.

* Populate key fields for ARTICLE...
          sdm_gui_message_val-sdm_type        = me->mv_type.
          sdm_gui_message_val-sdm_description = me->mv_sdm_description.

          if <mara> is assigned.
            sdm_gui_message_val-matnr = <mara>-matnr.
            sdm_gui_message_val-mtart = <mara>-mtart.
            sdm_gui_message_val-matkl = <mara>-matkl.
            sdm_gui_message_val-mstae = <mara>-mstae.
          endif.

          if <makt> is assigned.
            sdm_gui_message_val-maktx = <makt>-maktx.
          endif.
          sdm_gui_message_val-message        = message.
          sdm_gui_message_val-id             = <brf_results_val>-id.
          sdm_gui_message_val-number         = <brf_results_val>-number.
          sdm_gui_message_val-type           = <brf_results_val>-type.
          sdm_gui_message_val-extra_v1       = <brf_results_val>-extra_v1.
          sdm_gui_message_val-extra_v2       = <brf_results_val>-extra_v2.
          sdm_gui_message_val-extra_v3       = <brf_results_val>-extra_v3.
          sdm_gui_message_val-extra_v4       = <brf_results_val>-extra_v4.
          sdm_gui_message_val-extra_v5       = <brf_results_val>-extra_v5.
          sdm_gui_message_val-extra_v6       = <brf_results_val>-extra_v6.
          sdm_gui_message_val-ds_link_id     = <brf_results_val>-ds_link_id.
          sdm_gui_message_val-sdm_tabkey     = <brf_results_val>-sdm_tabkey.

          collect sdm_gui_message_val into <table>.
          clear sdm_gui_message_val.
        endloop.
      endif.
      mo_brf_result       = sdm_messages.
      mo_brf_result_empty = sdm_messages_empty.
    else.
* POE
* Special Conditions for screen fields names that are not standard
      assign me->mo_brf_result->* to <brf_results>.
      if me->mv_type = me->mc_derivation.
        loop at <brf_results>  assigning <brf_result>.
          assign component 'VALUE' of structure <brf_result> to FIELD-SYMBOL(<value>).
          if <value> = text-100.
            assign component 'TABLE' of structure <brf_result> to FIELD-SYMBOL(<table_name>).
            if <table_name> = 'MEINH'.
              <table_name> = 'SMEINH'.
            endif.
          endif.
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
*      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
      lt_sdm_gui_out       TYPE STANDARD TABLE OF ty_rept_output,
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
