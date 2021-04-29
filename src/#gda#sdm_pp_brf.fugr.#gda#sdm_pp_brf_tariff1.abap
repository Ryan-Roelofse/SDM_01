function /gda/sdm_pp_brf_tariff1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MATNR) TYPE  MATNR
*"     REFERENCE(XT_MVKE) TYPE  /GDA/SDM_T_MVKE_01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(Y_RESULT) TYPE  /GDA/SDM_TT_TARIFFS
*"----------------------------------------------------------------------
  types: begin of ty_vkorg.
  types: vkorg type vkorg.
  types: end of ty_vkorg.

  types: begin of ty_bukrs.
  types: bukrs type bukrs.
  types: end of ty_bukrs.

  types: begin of ty_land1.
  types: land1 type land1.
  types: end of ty_land1.

  data:
    lv_text(255),
    lv_found,
    lv_tabname1  type tabname1,
    lt_mvke      type /gda/sdm_t_mvke_01,
    lv_message   type string,
    ls_sales_org type ty_vkorg,
    ls_mara      type mara,
    ls_results   type /gda/sdm_s_tariffs_01,
    lt_comp_code type standard table of ty_bukrs,
    lt_country   type standard table of ty_land1,
    lt_sales_org type standard table of ty_vkorg,
    lo_ref       type ref to data,
    lx_root      type ref to cx_root.

  field-symbols:
    <mvke>         like line of xt_mvke,
    <country>      like line of lt_country,
    <tariff_table> type standard table,
    <tariff>       type any,
    <stcts>        type any,
    <ccngn>        type any,
    <datbi>        type any,
    <datab>        type any,
    <matnr>        type any.

  ls_mara-matnr = x_matnr.

*1)
* Get all sales ordes in MVKE for a material
  if xt_mvke is initial.
    select vkorg from mvke into corresponding fields of table lt_mvke
             where matnr = x_matnr.
  else.
    lt_mvke[] = xt_mvke[].
  endif.

  loop at lt_mvke assigning <mvke>.
    ls_sales_org-vkorg = <mvke>-vkorg.
    collect ls_sales_org into lt_sales_org.
    clear:
      ls_sales_org.
  endloop.

  sort lt_sales_org by vkorg.
  delete adjacent duplicates from lt_sales_org.

*2)
* Get company codes for these sales orders..TVKO
  select bukrs from tvko into corresponding fields of table lt_comp_code
       for all entries in lt_sales_org
      where vkorg  = lt_sales_org-vkorg.

*3)
* Get country for the company code.. T001
  select land1 from t001 into corresponding fields of table lt_country
       for all entries in lt_comp_code
      where  bukrs  = lt_comp_code-bukrs.

*4)
* /SAPSLL/MARITC -MATNR
* /SAPSLL/MARITC-STCTS(ZA01 for country ZA)

  if sy-dbsys = 'HDB'.
    lv_tabname1 = '/SAPSLL/MARITC'.
  else.
    lv_tabname1 = '/GDA/SDM_MARITC'.
  endif.

  create data lo_ref type table of (lv_tabname1).
  assign lo_ref->* to <tariff_table>.

  try.
      select matnr
             stcts
             ccngn
             datab
             datbi from (lv_tabname1)
               into corresponding fields of table <tariff_table>
               where matnr = x_matnr.
    catch cx_sy_dynamic_osql_syntax    into lx_root.
    catch cx_sy_dynamic_osql_semantics into lx_root.
  endtry.

  if lx_root is not initial.
    lv_message = lx_root->get_text( ).
    message lv_message type 'I'.
  endif.

  loop at lt_country assigning <country>.
    ls_results-land1 = <country>-land1.
    loop at <tariff_table> assigning <tariff>.
      assign component 'STCTS' of structure <tariff> to <stcts>.
      if <country> = <stcts>+0(2).
        assign component 'CCNGN' of structure <tariff> to <ccngn>.
        assign component 'DATBI' of structure <tariff> to <datbi>.
        assign component 'DATAB' of structure <tariff> to <datab>.
        assign component 'MATNR' of structure <tariff> to <matnr>.

        if <datbi> = '99991231'.
          ls_results-id    = '0'.
        else.
          ls_results-id    = 'W'.
        endif.
        lv_found         = abap_true.
        ls_results-ccngn = <ccngn>.
        ls_results-datbi = <datbi>.
        ls_results-datab = <datab>.
        ls_results-matnr = <matnr>.

* Get text..
        if sy-dbsys = 'HDB'.
          lv_tabname1 = '/SAPSLL/CLSNRT'.
        else.
          lv_tabname1 = '/GDA/SDM_CLSNRT'.
        endif.

        try.
            select single text from (lv_tabname1)
                     into lv_text
                     where nosct  = <stcts>
                       and ccngn  = <ccngn>
                       and datab <= <datab>
                       and datbi >= <datbi>
                       and langu  = sy-langu.

            if sy-subrc = 0.
              ls_results-text = lv_text.
            endif.
          catch cx_sy_dynamic_osql_syntax    into lx_root.
          catch cx_sy_dynamic_osql_semantics into lx_root.
        endtry.

        if lx_root is not initial.
          lv_message = lx_root->get_text( ).
          message lv_message type 'I'.
        endif.
      else.
        lv_found         = abap_false.
      endif.
    endloop.
    if lv_found = abap_false and ls_results-id = space.
      ls_results-id    = 'E'.
      ls_results-matnr = x_matnr.
    endif.

    ls_results-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                            i_contents = ls_mara ).

    append ls_results to y_result.
    clear:
     ls_results,
     lv_found,
     lv_text.
  endloop.
endfunction.
