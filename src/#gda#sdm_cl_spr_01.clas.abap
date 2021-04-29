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

  aliases PV_MESSAGE
    for /GDA/SDM_IF_ACTION_PROC_SPRINT~PV_MESSAGE .
  aliases PV_UPDATE_VALUE
    for /GDA/SDM_IF_ACTION_PROC_SPRINT~PV_UPDATE_VALUE .
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

      when 'MM02'.
        set update task local.
        set parameter id: 'MAT' field me->pv_exception_details-sdm_object_val.
        call transaction 'MM02' and skip first screen.  "#EC CI_CALLTA.
        y_refresh = abap_true.
        me->pv_source_external = abap_true.

      when 'MM03'.
        set parameter id: 'MAT' field me->pv_exception_details-sdm_object_val.
        call transaction 'MM03' and skip first screen.  "#EC CI_CALLTA.

      when 'GENERIC1'.
        me->update_data_bapi( ).
        y_refresh = abap_true.
    endcase.

  endmethod.


  method /GDA/SDM_IF_SPR_UPDATE~UPDATE_DATA_BAPI.
*CALL METHOD SUPER->/GDA/SDM_IF_SPR_UPDATE~UPDATE_DATA_BAPI
*    .

    data:
      is_headdata          type bapimathead,
      is_clientdata        type bapi_mara,
      is_clientdatax       type bapi_marax,
      is_plantdata         type bapi_marc,
      is_plantdatax        type bapi_marcx,
      is_salesdata         type bapi_mvke,
      is_salesdatax        type bapi_mvkex,
      is_valuationdata  type bapi_mbew,
      is_valuationdatax type bapi_mbewx,
      is_mat_desc          type bapi_makt,
      it_mat_desc          type t_bapi_makt,
      it_taxclass          type t_bapi_mlan,
      it_extensionin       type t_bapiparex,
      it_extensioninx      type t_bapiparexx,
      cs_messages          type bapiret2.

    field-symbols:
       <update_field>  type any,
       <update_fieldx> type any,
       <key>           type /gda/sdm_s_usmd_s_value.

    is_headdata-material   = me->pv_exception_details-sdm_object_val.  "convert_matn1_input( iv_matnr ).

    data(sap_table) = me->determine_sap_table( iv_sdm_structure = me->pv_exception_details-tabname ).

    data(keys) = /gda/cl_sdm_data_model_main=>build_key_from_string( iv_tabkey  = me->pv_exception_details-sdm_tabkey
                                                                     iv_tabname = sap_table ).

    if me->ps_mapping is initial.

      me->pv_message-type       = 'E'.
      me->pv_message-id         = '/GDA/SDM_SPRINT'.
      me->pv_message-number     = '012'.
      me->pv_message-message_v1 = me->pv_exception_details-tabname.
      me->pv_message-message_v2 = me->pv_exception_details-field.
      return.
    endif.
    case me->ps_mapping-bapi_structure.
      when 'BAPI_MAKT'.
        is_headdata-basic_view = abap_true.

        assign component me->ps_mapping-bapi_fieldname of structure is_mat_desc to <update_field>.
        <update_field>        = me->pv_update_value.
        is_mat_desc-langu     = sy-langu.
        is_mat_desc-langu_iso = sy-langu.
        append is_mat_desc to it_mat_desc.
      when 'BAPI_MARA'.
        is_headdata-basic_view = abap_true.

        assign component me->ps_mapping-bapi_fieldname of structure is_clientdata to <update_field>.
        <update_field>        = me->pv_update_value.

        assign component me->ps_mapping-bapi_fieldname of structure is_clientdatax to <update_fieldx>.
        <update_fieldx>        = abap_true.

      when 'BAPI_MARC'.

        data:
          view type t130f-pstat,
          name type t130f-fname.

        concatenate  sap_table '-' me->ps_mapping-sdm_fieldname into name.

* Determine which view to update...
        select single pstat from t130f
                        into view
                         where fname = name.
        if view = 'D'.
          is_headdata-mrp_view     = abap_true.
        else.
          is_headdata-storage_view = abap_true.
        endif.

        read table keys assigning <key> with key  fieldname = 'WERKS'.
        if sy-subrc = 0.
          is_plantdata-plant = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_plantdata to <update_field>.
          <update_field>        = me->pv_update_value.

          is_plantdatax-plant = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_plantdatax to <update_fieldx>.
          <update_fieldx>        = abap_true.

        else.
          me->pv_message-type       = 'E'.
          me->pv_message-id         = '/GDA/SDM_SPRINT'.
          me->pv_message-number     = '012'.
          me->pv_message-message_v1 = me->pv_exception_details-tabname.
          me->pv_message-message_v2 = me->pv_exception_details-field.
          return.

        endif.

      when 'BAPI_MVKE'.

        is_headdata-sales_view = abap_true.

        read table keys assigning <key> with key  fieldname = 'VKORG'.
        if sy-subrc = 0.
          is_salesdata-sales_org = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_salesdata to <update_field>.
          <update_field>        = me->pv_update_value.

          is_salesdatax-sales_org = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_salesdatax to <update_fieldx>.
          <update_fieldx>        = abap_true.
        endif.

        read table keys assigning <key> with key  fieldname = 'VTWEG'.
        if sy-subrc = 0.
          is_salesdata-distr_chan = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_plantdata to <update_field>.
          <update_field>        = me->pv_update_value.

          is_salesdatax-distr_chan = <key>-value.
          assign component me->ps_mapping-bapi_fieldname of structure is_plantdatax to <update_fieldx>.
          <update_fieldx>        = abap_true.
        endif.

      when 'BAPI_MBEW'.

        is_headdata-account_view = abap_true.

        read table keys assigning <key> with key  fieldname = 'BWKEY'.
        if sy-subrc = 0.
          is_valuationdata-val_area  = <key>-value.
          is_valuationdatax-val_area = <key>-value.
          read table keys assigning <key> with key  fieldname = 'BWTAR'.
          if sy-subrc = 0.
            is_valuationdata-val_type  = <key>-value.
            is_valuationdatax-val_type = <key>-value.

            assign component me->ps_mapping-bapi_fieldname of structure is_valuationdata to <update_field>.
            <update_field>        = me->pv_update_value.

            assign component me->ps_mapping-bapi_fieldname of structure is_valuationdatax to <update_fieldx>.
            <update_fieldx>        = abap_true.

          else.
            me->pv_message-type       = 'E'.
            me->pv_message-id         = '/GDA/SDM_SPRINT'.
            me->pv_message-number     = '012'.
            me->pv_message-message_v1 = me->pv_exception_details-tabname.
            me->pv_message-message_v2 = me->pv_exception_details-field.
            return.
          endif.
        else.
          me->pv_message-type       = 'E'.
          me->pv_message-id         = '/GDA/SDM_SPRINT'.
          me->pv_message-number     = '012'.
          me->pv_message-message_v1 = me->pv_exception_details-tabname.
          me->pv_message-message_v2 = me->pv_exception_details-field.
          return.
        endif.


*    it_mat_desc
      when others.
        me->pv_message-type       = 'E'.
        me->pv_message-id         = '/GDA/SDM_SPRINT'.
        me->pv_message-number     = '013'.
        me->pv_message-message_v1 = sap_table.
        return.

    endcase.

* Save Material Master Data
    call function 'BAPI_MATERIAL_SAVEDATA'
      exporting
        headdata            = is_headdata
        clientdata          = is_clientdata
        clientdatax         = is_clientdatax
        plantdata           = is_plantdata
        plantdatax          = is_plantdatax
        salesdata           = is_salesdata
        salesdatax          = is_salesdatax
        valuationdata       = is_valuationdata
        valuationdatax      = is_valuationdatax
      importing
        return              = cs_messages
      tables
        materialdescription = it_mat_desc
        taxclassifications  = it_taxclass
        extensionin         = it_extensionin
        extensioninx        = it_extensioninx.

    if cs_messages-type = 'S'.
      data:
       commit_message type bapiret2.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = abap_true
        importing
          return = commit_message.

    endif.


    me->pv_message = cs_messages.

* ST-386
* This message is too generic
    if me->pv_message-number = 356.
      clear:
        cs_messages.

      message s801(m3) with is_headdata-material.

      cs_messages-type   = 'S'.
      cs_messages-id     = 'M3'.
      cs_messages-number = '801'.
      write is_headdata-material to cs_messages-message_v1.


      call function 'BALW_BAPIRETURN_GET2'
        exporting
          type   = cs_messages-type
          cl     = cs_messages-id
          number = cs_messages-number
          par1   = cs_messages-message_v1
          par2   = cs_messages-message_v2
          par3   = cs_messages-message_v3
          par4   = cs_messages-message_v4
*         LOG_NO = ' '
*         LOG_MSG_NO = ' '
*         PARAMETER  = ' '
*         ROW    = 0
*         FIELD  = ' '
        importing
          return = me->pv_message
        exceptions
          others = 1.

*      clear:
*       me->pv_message-message.
*      concatenate 'Material' is_headdata-material 'has been changed' into me->pv_message-message separated by space.
    endif.
  endmethod.
ENDCLASS.
