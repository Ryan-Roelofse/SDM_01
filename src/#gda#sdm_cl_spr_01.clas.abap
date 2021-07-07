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
      lt_storagetypedatax TYPE bapie1mlgtrtx_tab.

    DATA:
          is_headdata1          TYPE bapimathead,
          is_clientdata         TYPE bapi_mara,
          is_clientdatax        TYPE bapi_marax,
          is_plantdata1         TYPE bapi_marc,
          is_plantdatax         TYPE bapi_marcx,
          is_salesdata          TYPE bapi_mvke,
          is_salesdatax         TYPE bapi_mvkex,
          is_valuationdata      TYPE bapi_mbew,
          is_valuationdatax     TYPE bapi_mbewx,
          is_mat_desc1          TYPE bapi_makt,
          it_mat_desc1          TYPE t_bapi_makt,
          is_taxclass1          TYPE bapi_mlan,
          it_taxclass1          TYPE t_bapi_mlan,
          it_extensionin        TYPE t_bapiparex,
          it_extensioninx       TYPE t_bapiparexx,
          is_storagelocationdata TYPE bapi_mard,
          is_storagelocationdatax TYPE bapi_mardx.

    FIELD-SYMBOLS:
       <update_field>  TYPE any,
       <update_fieldx> TYPE any,
       <key>           TYPE /gda/sdm_s_usmd_s_value.

    is_headdata-material   = me->pv_exception_details-sdm_object_val.  "convert_matn1_input( iv_matnr ).
    is_headdata1-material   = me->pv_exception_details-sdm_object_val.  "convert_matn1_input( iv_matnr ).

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

        READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'WERKS'.
        IF sy-subrc = 0.
*          is_plantdata-plant = <key>-value.
*          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdata TO <update_field>.
*          <update_field>        = me->pv_update_value.
*
*          is_plantdatax-plant = <key>-value.
*          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdatax TO <update_fieldx>.
*          <update_fieldx>        = abap_true.
        ELSE.
          me->pv_message-type       = 'E'.
          me->pv_message-id         = '/GDA/SDM_SPRINT'.
          me->pv_message-number     = '012'.
          me->pv_message-message_v1 = me->pv_exception_details-tabname.
          me->pv_message-message_v2 = me->pv_exception_details-field.
          RETURN.
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
      WHEN 'BAPIE1MAMTRT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_unitofmeasuretexts  ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasuretexts>).
        <lfs_unitofmeasuretexts>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasuretexts> TO <update_field>.
        <update_field>        = me->pv_update_value.
      WHEN 'BAPIE1MAW1RT'.
        is_headdata-basic_view = abap_true.
        is_headdata-function   = '004'.

        APPEND INITIAL LINE TO lt_addnlclientdata  ASSIGNING FIELD-SYMBOL(<lfs_addnlclientdata>).
        <lfs_addnlclientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_addnlclientdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_addnlclientdatax ASSIGNING FIELD-SYMBOL(<lfs_addnlclientdatax>).
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
      WHEN OTHERS.
        DATA(lv_check) = 'X'.
    ENDCASE.

    IF lv_check NE 'X'.
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
          forecastparameters   = lt_forecastparameters
          forecastparametersx  = lt_forecastparametersx
          planningdata         = lt_planningdata
          planningdatax        = lt_planningdatax
          unitsofmeasure       = lt_unitofmeasure
          unitsofmeasurex      = lt_unitofmeasurex
          unitofmeasuretexts   = lt_unitofmeasuretexts
          internationalartnos  = lt_internationalartnos
          vendorean            = lt_vendorean
          layoutmoduleassgmt   = lt_layoutmoduleassgmt
          layoutmoduleassgmtx  = lt_layoutmoduleassgmtx
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
      CASE me->ps_mapping-bapi_structure.
        WHEN 'BAPI_MAKT'.
          is_headdata1-basic_view = abap_true.

          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_mat_desc1 TO <update_field>.
          <update_field>        = me->pv_update_value.
          is_mat_desc1-langu     = sy-langu.
          is_mat_desc1-langu_iso = sy-langu.
          APPEND is_mat_desc1 TO it_mat_desc1.
        WHEN 'BAPI_MARA'.
          is_headdata-basic_view = abap_true.

          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_clientdata TO <update_field>.
          <update_field>        = me->pv_update_value.

          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_clientdatax TO <update_fieldx>.
          <update_fieldx>        = abap_true.
        WHEN 'BAPI_MARC'.
          CONCATENATE  sap_table '-' me->ps_mapping-sdm_fieldname INTO name.

* Determine which view to update...
          SELECT SINGLE pstat FROM t130f
                          INTO view
                           WHERE fname = name.
          IF view = 'D'.
            is_headdata1-mrp_view     = abap_true.
          ELSE.
            is_headdata1-storage_view = abap_true.
          ENDIF.

          READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'WERKS'.
          IF sy-subrc = 0.
            is_plantdata-plant = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdata TO <update_field>.
            <update_field>        = me->pv_update_value.
            is_plantdatax-plant = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdatax TO <update_fieldx>.
            <update_fieldx>        = abap_true.
          ELSE.
            me->pv_message-type       = 'E'.
            me->pv_message-id         = '/GDA/SDM_SPRINT'.
            me->pv_message-number     = '012'.
            me->pv_message-message_v1 = me->pv_exception_details-tabname.
            me->pv_message-message_v2 = me->pv_exception_details-field.
            RETURN.
          ENDIF.
        WHEN 'BAPI_MBEW'.
          is_headdata1-account_view = abap_true.

          READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'BWKEY'.
          IF sy-subrc = 0.
            is_valuationdata-val_area  = <key>-value.
            is_valuationdatax-val_area = <key>-value.
            READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'BWTAR'.
            IF sy-subrc = 0.
              is_valuationdata-val_type  = <key>-value.
              is_valuationdatax-val_type = <key>-value.

              ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_valuationdata TO <update_field>.
              <update_field>        = me->pv_update_value.

              ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_valuationdatax TO <update_fieldx>.
              <update_fieldx>        = abap_true.

            ELSE.
              me->pv_message-type       = 'E'.
              me->pv_message-id         = '/GDA/SDM_SPRINT'.
              me->pv_message-number     = '012'.
              me->pv_message-message_v1 = me->pv_exception_details-tabname.
              me->pv_message-message_v2 = me->pv_exception_details-field.
              RETURN.
            ENDIF.
          ELSE.
            me->pv_message-type       = 'E'.
            me->pv_message-id         = '/GDA/SDM_SPRINT'.
            me->pv_message-number     = '012'.
            me->pv_message-message_v1 = me->pv_exception_details-tabname.
            me->pv_message-message_v2 = me->pv_exception_details-field.
            RETURN.
          ENDIF.
        WHEN 'BAPI_MARD'.
          is_headdata-basic_view = abap_true.

          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_storagelocationdata TO <update_field>.
          <update_field>        = me->pv_update_value.

          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_storagelocationdatax TO <update_fieldx>.
          <update_fieldx>        = abap_true.
        WHEN 'BAPI_MLAN'.
          is_headdata1-basic_view = abap_true.
          ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_taxclass1 TO <update_field>.
          <update_field>        = me->pv_update_value.
          APPEND is_taxclass1 TO it_taxclass1.
      ENDCASE.

* Save Material Master Data
      CLEAR cs_messages.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = is_headdata1
          clientdata           = is_clientdata
          clientdatax          = is_clientdatax
          plantdata            = is_plantdata1
          plantdatax           = is_plantdatax
          storagelocationdata  = is_storagelocationdata
          storagelocationdatax = is_storagelocationdatax
          valuationdata        = is_valuationdata
          valuationdatax       = is_valuationdatax
          salesdata            = is_salesdata
          salesdatax           = is_salesdatax
        IMPORTING
          return               = cs_messages
        TABLES
          materialdescription  = it_mat_desc1
*         internationalartnos  = it_internationalartnos
          taxclassifications   = it_taxclass1
          extensionin          = it_extensionin
          extensioninx         = it_extensioninx.


      IF ls_return-type = 'S'.
        DATA:
         commit_message TYPE bapiret2.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = abap_true
          IMPORTING
            return = commit_message.

      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING ls_return TO me->pv_message.
*    me->pv_message = cs_messages.

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
