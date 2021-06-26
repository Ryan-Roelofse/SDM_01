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
*CALL METHOD SUPER->/GDA/SDM_IF_SPR_UPDATE~UPDATE_DATA_BAPI

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
*      it_extensionin       TYPE t_bapiparex,
*      it_extensioninx      TYPE t_bapiparexx,
      ls_return            TYPE bapireturn1,
      cs_messages          TYPE bapiret2,
      lt_unitofmeasure     TYPE bapie1marmrt_tab,
      lt_unitofmeasurex    TYPE bapie1marmrtx_tab.

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
        is_headdata-function   = '0004'.

        APPEND INITIAL LINE TO lt_clientdata  ASSIGNING FIELD-SYMBOL(<lfs_clientdata>).
        <lfs_clientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_clientdata> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_clientdatax ASSIGNING FIELD-SYMBOL(<lfs_clientdatax>).
        <lfs_clientdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_clientdatax> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = <update_field>
            language       = sy-langu
          IMPORTING
            output         = <update_field>
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

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
* Determine which view to update...
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
        is_headdata-function   = '0004'.

        APPEND INITIAL LINE TO lt_unitofmeasure  ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasure>).
        <lfs_clientdata>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasure> TO <update_field>.
        <update_field>        = me->pv_update_value.

        APPEND INITIAL LINE TO lt_unitofmeasurex ASSIGNING FIELD-SYMBOL(<lfs_unitofmeasurex>).
        <lfs_clientdatax>-material = is_headdata-material.
        ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE <lfs_unitofmeasurex> TO <update_fieldx>.
        <update_fieldx>        = abap_true.

      WHEN OTHERS.
        DATA(lv_check) = 'X'.
    ENDCASE.

    IF lv_check NE 'X'.
      CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
        EXPORTING
          headdata            = is_headdata
        IMPORTING
          return              = ls_return
        TABLES
          clientdata          = lt_clientdata
          clientdatax         = lt_clientdatax
          materialdescription = it_mat_desc
          unitsofmeasure      = lt_unitofmeasure
          unitsofmeasurex     = lt_unitofmeasurex.

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ELSE.

      DATA:
            is_headdata1          TYPE bapimathead,
            is_clientdata         TYPE bapi_mara,
            is_clientdatax        TYPE bapi_marax,
            is_plantdata1         TYPE bapi_marc,
            is_plantdatax         TYPE bapi_marcx,
            is_salesdata          TYPE bapi_mvke,
            is_salesdatax         TYPE bapi_mvkex,
            is_warehousedata      TYPE bapi_mlgn,
            is_warehousedatax     TYPE bapi_mlgnx,
            is_valuationdata      TYPE bapi_mbew,
            is_valuationdatax     TYPE bapi_mbewx,
            is_mat_desc1          TYPE bapi_makt,
            it_mat_desc1          TYPE t_bapi_makt,
            it_taxclass1          TYPE t_bapi_mlan,
            it_extensionin        TYPE t_bapiparex,
            it_extensioninx       TYPE t_bapiparexx.

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

        WHEN 'BAPI_MVKE'.

          is_headdata1-sales_view = abap_true.

          READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'VKORG'.
          IF sy-subrc = 0.
            is_salesdata-sales_org = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_salesdata TO <update_field>.
            <update_field>        = me->pv_update_value.

            is_salesdatax-sales_org = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_salesdatax TO <update_fieldx>.
            <update_fieldx>        = abap_true.
          ENDIF.

          READ TABLE keys ASSIGNING <key> WITH KEY  fieldname = 'VTWEG'.
          IF sy-subrc = 0.
            is_salesdata-distr_chan = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdata TO <update_field>.
            <update_field>        = me->pv_update_value.

            is_salesdatax-distr_chan = <key>-value.
            ASSIGN COMPONENT me->ps_mapping-bapi_fieldname OF STRUCTURE is_plantdatax TO <update_fieldx>.
            <update_fieldx>        = abap_true.
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
*
*
**    it_mat_desc
*      when others.
*        me->pv_message-type       = 'E'.
*        me->pv_message-id         = '/GDA/SDM_SPRINT'.
*        me->pv_message-number     = '013'.
*        me->pv_message-message_v1 = sap_table.
*        return.
*
      ENDCASE.
*
* Save Material Master Data
      CLEAR cs_messages.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata            = is_headdata1
          clientdata          = is_clientdata
          clientdatax         = is_clientdatax
          plantdata           = is_plantdata1
          plantdatax          = is_plantdatax
          salesdata           = is_salesdata
          salesdatax          = is_salesdatax
          valuationdata       = is_valuationdata
          valuationdatax      = is_valuationdatax
        IMPORTING
          return              = cs_messages
        TABLES
          materialdescription = it_mat_desc1
          taxclassifications  = it_taxclass1
          extensionin         = it_extensionin
          extensioninx        = it_extensioninx.


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
