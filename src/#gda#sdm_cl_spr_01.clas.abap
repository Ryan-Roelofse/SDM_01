class /GDA/SDM_CL_SPR_01 definition
  public
  inheriting from /GDA/SDM_CL_SPR_UPDATE1
  final
  create public .

public section.

*  aliases PV_EXCEPTION_DETAILS
*    for /GDA/SDM_IF_ACTION_PROC_SPRINT~PV_EXCEPTION_DETAILS .
  methods /GDA/SDM_IF_ACTION_PROC_SPRINT~PROCESS_ACTION
    redefinition .
  methods /GDA/SDM_IF_SPR_UPDATE~UPDATE_DATA_BAPI
    redefinition .
protected section.
private section.

  aliases PT_ACTIONS
    for /GDA/SDM_IF_ACTION_PROC_SPRINT~PT_ACTIONS .
  aliases PT_MASSTABS
    for /GDA/SDM_IF_SPR_UPDATE~PT_MASSTABS .
  aliases PT_MASS_ALLFIELDS
    for /GDA/SDM_IF_SPR_UPDATE~PT_MASS_ALLFIELDS .
  aliases PV_BAPI_NAME
    for /GDA/SDM_IF_SPR_UPDATE~PV_BAPI_NAME .
  aliases PV_BOR_OBJECT
    for /GDA/SDM_IF_SPR_UPDATE~PV_BOR_OBJECT .
  aliases PV_SUPPRESS_MESSAGE
    for /GDA/SDM_IF_ACTION_PROC_SPRINT~PV_SUPPRESS_MESSAGE .
  aliases GET_MASS_FIELDS
    for /GDA/SDM_IF_SPR_UPDATE~GET_MASS_FIELDS .
  aliases GET_MASS_TABLES
    for /GDA/SDM_IF_SPR_UPDATE~GET_MASS_TABLES .
  aliases UPDATE_DATA_BAPI
    for /GDA/SDM_IF_SPR_UPDATE~UPDATE_DATA_BAPI .
ENDCLASS.



CLASS /GDA/SDM_CL_SPR_01 IMPLEMENTATION.


  method /GDA/SDM_IF_ACTION_PROC_SPRINT~PROCESS_ACTION.

    try.
        call method super->process_action
          exporting
            x_action = x_action
            x_multi  = x_multi
*          importing
*           y_refresh = data(need_refresh)
*           y_action_handled =
*           y_not_authorised =
          .
      catch /gda/cx_sdm_exception_handl .
    endtry.

    me->pv_update_value = x_value.

    data(fcode) = x_action->get_fcode( ).
    case fcode.

      when 'MM42'.
        set update task local.
        set parameter id: 'MAT' field me->pv_exception_details-sdm_object_val.
        call transaction 'MM42' and skip first screen.  "#EC CI_CALLTA.
        y_refresh = abap_true.
        me->pv_source_external = abap_true.

      when 'MM43'.
        set parameter id: 'MAT' field me->pv_exception_details-sdm_object_val.
        call transaction 'MM43' and skip first screen.  "#EC CI_CALLTA.

      when 'GENERIC1'.
        me->update_data_bapi( ).
        y_refresh = abap_true.
    endcase.

  endmethod.


  METHOD /gda/sdm_if_spr_update~update_data_bapi.

    DATA:
      is_headdata          TYPE bapie1mathead,
      lt_clientdata        TYPE bapie1marart_tab,
      lt_clientdatax       TYPE bapie1marartx_tab,
      is_plantdata         TYPE bapie1marcrt,
      it_plantdata         TYPE bapie1marcrt_tab,
      it_plantdatax        TYPE bapie1marcrtx_tab,
      it_salesdata         TYPE bapie1mvkert_tab,
      it_salesdatax        TYPE bapie1mvkertx_tab,
      it_valuationdata     TYPE bapie1mbewrt_tab,
      it_valuationdatax    TYPE bapie1mbewrtx_tab,
      it_mat_desc          TYPE bapie1maktrt_tab,
      it_taxclass          TYPE bapie1mlanrt_tab,
      ls_return            TYPE bapireturn1,
      cs_messages          TYPE bapiret2,
      lt_unitofmeasure     TYPE bapie1marmrt_tab,
      lt_unitofmeasurex    TYPE bapie1marmrtx_tab,
      lt_layoutmoduleassgmt TYPE TABLE OF bapie1malgrt,
      lt_layoutmoduleassgmtx TYPE TABLE OF bapie1malgrtx,
      lt_unitofmeasuretexts TYPE bapie1mamtrt_tab,
      lt_addnlclientdata TYPE bapie1maw1rt_tab,
      lt_addnlclientdatax TYPE bapie1maw1rtx_tab,
      lt_vendorean TYPE bapie1mleart_tab,
      lt_posdata TYPE bapie1wlk2rt_tab,
      lt_posdatax TYPE bapie1wlk2rtx_tab,
      lt_internationalartnos TYPE bapie1meanrt_tab,
      lt_salesdata TYPE bapie1mvkert_tab,
      lt_salesdatax TYPE bapie1mvkertx_tab,
      lt_forecastparameters TYPE bapie1mpoprt_tab,
      lt_forecastparametersx TYPE bapie1mpoprtx_tab,
      lt_planningdata TYPE  bapie1mpgdrt_tab,
      lt_planningdatax TYPE bapie1mpgdrtx_tab,
      lt_warehousenumberdata TYPE bapie1mlgnrt_tab,
      lt_warehousenumberdatax TYPE bapie1mlgnrtx_tab,
      lt_storagetypedata TYPE bapie1mlgtrt_tab,
      lt_storagetypedatax TYPE bapie1mlgtrtx_tab,
      lt_valuationdata TYPE bapie1mbewrt_tab,
      lt_valuationdatax TYPE bapie1mbewrtx_tab,
      lt_storagelocationdata TYPE bapie1mardrt_tab,
      lt_storagelocationdatax TYPE bapie1mardrtx_tab,
      lt_plantdata TYPE bapie1marcrt_tab,
      lt_plantdatax TYPE bapie1marcrtx_tab.

    DATA: ls_eord TYPE eord,
          lt_eord TYPE STANDARD TABLE OF eord,
          lt_eordu TYPE STANDARD TABLE OF eordu,
          ls_eina TYPE eina,
          ls_eine TYPE eine.

    FIELD-SYMBOLS:
       <update_field>  TYPE any,
       <update_fieldx> TYPE any,
       <key>           TYPE /gda/sdm_s_usmd_s_value.

    is_headdata-material   = me->pv_exception_details-sdm_object_val.  "convert_matn1_input( iv_matnr ).

    DATA(sap_table) = me->determine_sap_table( iv_sdm_structure = me->pv_exception_details-tabname ).

    DATA(keys) = /gda/cl_sdm_data_model_main=>build_key_from_string( iv_tabkey  = me->pv_exception_details-sdm_tabkey
                                                                     iv_tabname = sap_table ).
    IF me->ps_mapping IS INITIAL.
      me->pv_message-type       = 'E'.
      me->pv_message-id         = '/GDA/SDM_SPRINT'.
      me->pv_message-number     = '012'.
      me->pv_message-message_v1 = me->pv_exception_details-tabname.
      me->pv_message-message_v2 = me->pv_exception_details-field.
      RETURN.
    ENDIF.
    is_headdata-no_appl_log = abap_true.

    CASE me->ps_mapping-bapi_structure.
      WHEN 'BAPIE1MARART'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_clientdata  ASSIGNING FIELD-SYMBOL(<lfs_clientdata>).
        <lfs_clientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_clientdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_clientdatax ASSIGNING FIELD-SYMBOL(<lfs_clientdatax>).
        <lfs_clientdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_clientdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        IF me->ps_mapping-sdm_fieldname = 'MEINH'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = <update_field>
              language       = sy-langu
            IMPORTING
              output         = <update_field>
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
        ENDIF.
      WHEN 'BAPIE1MAKTRT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO it_mat_desc  ASSIGNING FIELD-SYMBOL(<lfs_mat_desc>).
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_mat_desc> TO <update_field>.
        <update_field>        = me->pv_update_value.
        <lfs_mat_desc>-material = is_headdata-material.
        <lfs_mat_desc>-langu     = sy-langu.
        <lfs_mat_desc>-langu_iso = sy-langu.
      WHEN 'BAPIE1MARCRT'.
        DATA:
          view TYPE t130f-pstat,
          name TYPE t130f-fname.

        CONCATENATE  sap_table '-' me->ps_mapping-sdm_fieldname INTO name.
        SELECT SINGLE pstat FROM t130f
                        INTO view
                         WHERE fname = name.
        is_headdata-logdc_view = abap_true.
        is_headdata-logst_view = abap_true.

        APPEND INITIAL LINE TO lt_plantdata  ASSIGNING FIELD-SYMBOL(<lfs_plantdata>).
        <lfs_plantdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_plantdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_plantdatax ASSIGNING FIELD-SYMBOL(<lfs_plantdatax>).
        <lfs_plantdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_plantdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'WERKS'.
        IF sy-subrc = 0.
          <lfs_plantdata>-plant = <key>-value.
          <lfs_plantdatax>-plant = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MARMRT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_unitofmeasure  ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasure>).
        <lfs_unitofmeasure>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasure> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_unitofmeasurex ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasurex>).
        <lfs_unitofmeasurex>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasurex> TO <update_fieldx>.
        <update_fieldx>        = abap_true.
      WHEN 'BAPIE1MALGRT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_layoutmoduleassgmt  ASSIGNING FIELD-SYMBOL(<lfs_layoutmoduleassgmt>).
        <lfs_layoutmoduleassgmt>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_layoutmoduleassgmt> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_layoutmoduleassgmtx ASSIGNING FIELD-SYMBOL(<lfs_layoutmoduleassgmtx>).
        <lfs_layoutmoduleassgmtx>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_layoutmoduleassgmtx> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'MEINH'.
        IF <key>-value IS NOT INITIAL.
          <lfs_layoutmoduleassgmt>-unit = <key>-value.
          <lfs_layoutmoduleassgmtx>-unit = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LAYGR'.
        IF <key>-value IS NOT INITIAL.
          <lfs_layoutmoduleassgmt>-layout_mod = <key>-value.
          <lfs_layoutmoduleassgmtx>-layout_mod = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MAMTRT'.
        is_headdata-pos_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_unitofmeasuretexts  ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasuretexts>).
        <lfs_unitofmeasuretexts>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasuretexts> TO <update_field>.
        <update_field>        = me->pv_update_value.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'SPRAS'.
        IF <key>-value IS NOT INITIAL.
          <lfs_unitofmeasuretexts>-langu = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'MEINH'.
        IF <key>-value IS NOT INITIAL.
          <lfs_unitofmeasuretexts>-alt_unit = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'MTXID'.
        IF <key>-value IS NOT INITIAL.
          <lfs_unitofmeasuretexts>-text_id = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LFDNR'.
        IF <key>-value IS NOT INITIAL.
          <lfs_unitofmeasuretexts>-consec_no = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MAW1RT'.
        is_headdata-list_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_addnlclientdata  ASSIGNING FIELD-SYMBOL(<lfs_addnlclientdata>).
        <lfs_addnlclientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_addnlclientdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_addnlclientdatax ASSIGNING FIELD-SYMBOL(<lfs_addnlclientdatax>).
        <lfs_addnlclientdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_addnlclientdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.
      WHEN 'BAPIE1MWLIRT'.
        is_headdata-list_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_addnlclientdata  ASSIGNING <lfs_addnlclientdata>.
        <lfs_addnlclientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_addnlclientdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_addnlclientdatax ASSIGNING <lfs_addnlclientdatax>.
        <lfs_addnlclientdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_addnlclientdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.
      WHEN 'BAPIE1MLEART'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_vendorean  ASSIGNING FIELD-SYMBOL(<lfs_vendorean>).
        <lfs_vendorean>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_vendorean> TO <update_field>.
        <update_field>        = me->pv_update_value.
      WHEN 'BAPIE1WLK2RT'.
        is_headdata-pos_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_posdata  ASSIGNING FIELD-SYMBOL(<lfs_posdata>).
        <lfs_posdata>-material = is_headdata-material.
        <lfs_posdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_posdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_posdatax ASSIGNING FIELD-SYMBOL(<lfs_posdatax>).
        <lfs_posdatax>-material = is_headdata-material.
        <lfs_posdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_posdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'VKORG'.
        IF <key>-value IS NOT INITIAL.
          <lfs_posdata>-sales_org = <key>-value.
          <lfs_posdatax>-sales_org = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'VTWEG'.
        IF <key>-value IS NOT INITIAL.
          <lfs_posdata>-distr_chan = <key>-value.
          <lfs_posdatax>-distr_chan = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key>-value IS NOT INITIAL.
          <lfs_posdata>-plant = <key>-value.
          <lfs_posdatax>-plant = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MEANRT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_internationalartnos  ASSIGNING FIELD-SYMBOL(<lfs_internationalartnos>).
        <lfs_internationalartnos>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_internationalartnos> TO <update_field>.
        <update_field>        = me->pv_update_value.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'MEINH'.
        IF <key>-value IS NOT INITIAL.
          <lfs_internationalartnos>-unit = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MVKERT'.
        is_headdata-sales_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_salesdata  ASSIGNING FIELD-SYMBOL(<lfs_salesdata>).
        <lfs_salesdata>-material = is_headdata-material.
        <lfs_salesdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_salesdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_salesdatax ASSIGNING FIELD-SYMBOL(<lfs_salesdatax>).
        <lfs_salesdatax>-material = is_headdata-material.
        <lfs_salesdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_salesdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'VKORG'.
        IF <key> IS NOT INITIAL.
          <lfs_salesdata>-sales_org = <key>-value.
          <lfs_salesdatax>-sales_org = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'VTWEG'.
        IF <key> IS NOT INITIAL.
          <lfs_salesdata>-distr_chan = <key>-value.
          <lfs_salesdatax>-distr_chan = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MPOPRT'.
        DATA(lv_sdm_tabkey) = me->pv_exception_details-sdm_tabkey.
        DATA(lv_werks) = me->pv_exception_details-sdm_tabkey+0(4).
        DATA(lv_matnr) = me->pv_exception_details-sdm_tabkey+4(18).
        CONCATENATE lv_matnr lv_werks INTO lv_sdm_tabkey.

        DATA(keys_mpop) = /gda/cl_sdm_data_model_main=>build_key_from_string( iv_tabkey  = lv_sdm_tabkey
                                                                              iv_tabname = sap_table ).

        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_forecastparameters  ASSIGNING FIELD-SYMBOL(<lfs_forecastparameters>).
        <lfs_forecastparameters>-material = is_headdata-material.
        <lfs_forecastparameters>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_forecastparameters> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_forecastparametersx ASSIGNING FIELD-SYMBOL(<lfs_forecastparametersx>).
        <lfs_forecastparametersx>-material = is_headdata-material.
        <lfs_forecastparametersx>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_forecastparametersx> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys_mpop ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key> IS NOT INITIAL.
          <lfs_forecastparameters>-plant = <key>-value.
          <lfs_forecastparametersx>-plant = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MPGDRT'.
        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_planningdata  ASSIGNING FIELD-SYMBOL(<lfs_planningdata>).
        <lfs_planningdata>-material = is_headdata-material.
        <lfs_planningdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_planningdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_planningdatax ASSIGNING FIELD-SYMBOL(<lfs_planningdatax>).
        <lfs_planningdatax>-material = is_headdata-material.
        <lfs_planningdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_planningdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key> IS NOT INITIAL.
          <lfs_planningdatax>-plant = <key>-value.
          <lfs_planningdata>-plant = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MLGNRT'.
        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_warehousenumberdata  ASSIGNING FIELD-SYMBOL(<lfs_warehousenumberdata>).
        <lfs_warehousenumberdata>-material = is_headdata-material.
        <lfs_warehousenumberdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_warehousenumberdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_warehousenumberdatax ASSIGNING FIELD-SYMBOL(<lfs_warehousenumberdatax>).
        <lfs_warehousenumberdatax>-material = is_headdata-material.
        <lfs_warehousenumberdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_warehousenumberdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LGNUM'.
        IF <key> IS NOT INITIAL.
          <lfs_warehousenumberdata>-whse_no = <key>-value.
          <lfs_warehousenumberdatax>-whse_no = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MLGTRT'.
        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_storagetypedata  ASSIGNING FIELD-SYMBOL(<lfs_storagetypedata>).
        <lfs_storagetypedata>-material = is_headdata-material.
        <lfs_storagetypedata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_storagetypedata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_storagetypedatax ASSIGNING FIELD-SYMBOL(<lfs_storagetypedatax>).
        <lfs_storagetypedatax>-material = is_headdata-material.
        <lfs_storagetypedatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_storagetypedatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LGNUM'.
        IF <key> IS NOT INITIAL.
          <lfs_storagetypedata>-whse_no = <key>-value.
          <lfs_storagetypedatax>-whse_no = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LGTYP'.
        IF <key> IS NOT INITIAL.
          <lfs_storagetypedata>-stge_type = <key>-value.
          <lfs_storagetypedatax>-stge_type = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MBEWRT'.
        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_valuationdata  ASSIGNING FIELD-SYMBOL(<lfs_valuationdata>).
        <lfs_valuationdata>-material = is_headdata-material.
        <lfs_valuationdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_valuationdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_valuationdatax ASSIGNING FIELD-SYMBOL(<lfs_valuationdatax>).
        <lfs_valuationdatax>-material = is_headdata-material.
        <lfs_valuationdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_valuationdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'BWKEY'.
        IF <key> IS NOT INITIAL.
          <lfs_valuationdata>-val_area = <key>-value.
          <lfs_valuationdatax>-val_area = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'BWTAR'.
        IF <key> IS NOT INITIAL.
          <lfs_valuationdata>-val_type = <key>-value.
          <lfs_valuationdatax>-val_type = <key>-value.
        ENDIF.
      WHEN 'BAPIE1MARDRT'.
        is_headdata-logdc_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_storagelocationdata  ASSIGNING FIELD-SYMBOL(<lfs_storagelocationdata>).
        <lfs_storagelocationdata>-material = is_headdata-material.
        <lfs_storagelocationdata>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_storagelocationdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_storagelocationdatax ASSIGNING FIELD-SYMBOL(<lfs_storagelocationdatax>).
        <lfs_storagelocationdatax>-material = is_headdata-material.
        <lfs_storagelocationdatax>-function = '004'.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_storagelocationdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key> IS NOT INITIAL.
          <lfs_storagelocationdata>-plant = <key>-value.
          <lfs_storagelocationdatax>-plant = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'LGORT'.
        IF <key> IS NOT INITIAL.
          <lfs_storagelocationdata>-stge_loc = <key>-value.
          <lfs_storagelocationdatax>-stge_loc = <key>-value.
        ENDIF.
      WHEN OTHERS.
        DATA(lv_check) = abap_true.
    ENDCASE.

    IF lv_check EQ abap_false.
      CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
        EXPORTING
          headdata             = is_headdata
        IMPORTING
          return               = ls_return
        TABLES
          clientdata           = lt_clientdata
          clientdatax          = lt_clientdatax
          addnlclientdata      = lt_addnlclientdata
          addnlclientdatax     = lt_addnlclientdatax
          materialdescription  = it_mat_desc
          plantdata            = lt_plantdata
          plantdatax           = lt_plantdatax
          forecastparameters   = lt_forecastparameters
          forecastparametersx  = lt_forecastparametersx
          planningdata         = lt_planningdata
          planningdatax        = lt_planningdatax
          storagelocationdata  = lt_storagelocationdata
          storagelocationdatax = lt_storagelocationdatax
          unitsofmeasure       = lt_unitofmeasure
          unitsofmeasurex      = lt_unitofmeasurex
          unitofmeasuretexts   = lt_unitofmeasuretexts
          internationalartnos  = lt_internationalartnos
          vendorean            = lt_vendorean
          layoutmoduleassgmt   = lt_layoutmoduleassgmt
          layoutmoduleassgmtx  = lt_layoutmoduleassgmtx
          valuationdata        = lt_valuationdata
          valuationdatax       = lt_valuationdatax
          warehousenumberdata  = lt_warehousenumberdata
          warehousenumberdatax = lt_warehousenumberdatax
          storagetypedata      = lt_storagetypedata
          storagetypedatax     = lt_storagetypedatax
          salesdata            = lt_salesdata
          salesdatax           = lt_salesdatax
          posdata              = lt_posdata
          posdatax             = lt_posdatax.

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ELSE.
      IF me->ps_mapping-bapi_structure = 'EORD'.
        lv_sdm_tabkey = me->pv_exception_details-sdm_tabkey.
        IF lv_sdm_tabkey+0(3) = sy-mandt.
          DATA(lv_strlen) = strlen( lv_sdm_tabkey ).
          lv_sdm_tabkey = lv_sdm_tabkey+3(lv_strlen).
        ENDIF.
        keys = /gda/cl_sdm_data_model_main=>build_key_from_string( iv_tabkey  = lv_sdm_tabkey
                                                                   iv_tabname = sap_table ).

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key> IS NOT INITIAL.
          lv_werks = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'ZEORD'.
        IF <key> IS NOT INITIAL.
          DATA(lv_zeord) = <key>-value.
        ENDIF.

        SELECT SINGLE * FROM eord INTO ls_eord WHERE matnr = is_headdata-material
                                                 AND werks = lv_werks AND zeord = lv_zeord.

        APPEND INITIAL LINE TO lt_eord  ASSIGNING FIELD-SYMBOL(<lfs_eord>).
        <lfs_eord> = ls_eord.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_eord> TO <update_field>.
        IF me->ps_mapping-bapi_fieldname = 'VDATU'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = me->pv_update_value
            IMPORTING
              date_internal = <update_field>.
        ELSE.
          <update_field>        = me->pv_update_value.
        ENDIF.

        APPEND INITIAL LINE TO lt_eordu  ASSIGNING FIELD-SYMBOL(<lfs_eordu>).
        <lfs_eordu> = ls_eord.
        <lfs_eordu>-kz = abap_true.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_eordu> TO <update_field>.
        IF me->ps_mapping-bapi_fieldname = 'VDATU'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = me->pv_update_value
            IMPORTING
              date_internal = <update_field>.
        ELSE.
          <update_field>        = me->pv_update_value.
        ENDIF.

        CALL FUNCTION 'ME_UPDATE_SOURCES_OF_SUPPLY'
          TABLES
            xeord = lt_eordu
            yeord = lt_eord.

        IF sy-subrc EQ 0.
          MESSAGE s801(m3) WITH is_headdata-material.
        ENDIF.
      ELSEIF me->ps_mapping-bapi_structure = 'EINA'.
        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'INFNR'.
        IF <key>-value IS NOT INITIAL.
          SELECT SINGLE * FROM eina INTO ls_eina WHERE infnr = <key>-value.
          DATA(ls_eina_old) = ls_eina.
        ENDIF.

        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE ls_eina TO <update_field>.
        <update_field>        = me->pv_update_value.

        CALL FUNCTION 'ME_UPDATE_INFORECORD' IN UPDATE TASK
          EXPORTING
            xeina    = ls_eina
            xeine    = ls_eine
            yeina    = ls_eina_old
            yeine    = ls_eine
            reg_eina = ls_eina.

        IF sy-subrc EQ 0.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          MESSAGE s801(m3) WITH is_headdata-material.
        ENDIF.
      ELSEIF me->ps_mapping-bapi_structure = 'EINE'.
        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'INFNR'.
        IF <key>-value IS NOT INITIAL.
          DATA(lv_infnr) = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'EKORG'.
        IF <key>-value IS NOT INITIAL.
          DATA(lv_ekorg) = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'ESOKZ'.
        IF <key>-value IS NOT INITIAL.
          DATA(lv_esokz) = <key>-value.
        ENDIF.

        READ TABLE keys ASSIGNING <key> WITH KEY fieldname = 'WERKS'.
        IF <key>-value IS NOT INITIAL.
          lv_werks = <key>-value.
        ENDIF.

        SELECT SINGLE * FROM eine INTO ls_eine WHERE infnr = lv_infnr AND ekorg = lv_ekorg
                                                 AND esokz = lv_esokz AND werks = lv_werks.
        DATA(ls_eine_old) = ls_eine.

        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE ls_eine TO <update_field>.
        <update_field>        = me->pv_update_value.

        CALL FUNCTION 'ME_UPDATE_INFORECORD' IN UPDATE TASK
          EXPORTING
            xeina    = ls_eina
            xeine    = ls_eine
            yeina    = ls_eina
            yeine    = ls_eine_old
            reg_eina = ls_eina.

        IF sy-subrc EQ 0.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          MESSAGE s801(m3) WITH is_headdata-material.
        ENDIF.
*      ELSEIF me->ps_mapping-bapi_structure = 'MG03STEUER'.
*      CALL FUNCTION 'STEUERTAB_READ'
*        EXPORTING
*          matnr           = is_headdata-material
*        TABLES
*          steuertab       = lt_steuer
*        EXCEPTIONS
*          wrong_call      = 1
*          steuertab_empty = 2
*          OTHERS          = 3.

      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING ls_return TO me->pv_message.

* ST-386
* This message is too generic
    IF me->pv_message-number = 356.
      CLEAR:
        cs_messages.

      MESSAGE s801(m3) WITH is_headdata-material.

      cs_messages-type   = 'S'.
      cs_messages-id     = 'M3'.
      cs_messages-number = '801'.
      WRITE is_headdata-material TO cs_messages-message_v1.


      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = cs_messages-type
          cl     = cs_messages-id
          number = cs_messages-number
          par1   = cs_messages-message_v1
          par2   = cs_messages-message_v2
          par3   = cs_messages-message_v3
          par4   = cs_messages-message_v4
        IMPORTING
          return = me->pv_message
        EXCEPTIONS
          OTHERS = 1.

*      clear:
*       me->pv_message-message.
*      concatenate 'Material' is_headdata-material 'has been changed' into me->pv_message-message separated by space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
