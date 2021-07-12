class /GDA/SDM_CL_ADD_SDM_VAL_01 definition
  public
  final
  create public .

public section.

  interfaces /GDA/SDM_IF_BADI_CORE1 .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_ADD_SDM_VAL_01 IMPLEMENTATION.


  METHOD /gda/sdm_if_badi_core1~add_sdm_type.
    DATA:
      ls_sdm_type TYPE /gda/sdm_s_sdm_type.

    CASE x_source.
      WHEN '1'. "Reporting
        ls_sdm_type-sign   = 'I'.
        ls_sdm_type-option = 'EQ'.
        ls_sdm_type-low    = '03'. "gc_pir
        APPEND ls_sdm_type TO xt_sdm_type.

        ls_sdm_type-sign   = 'I'.
        ls_sdm_type-option = 'EQ'.
        ls_sdm_type-low    = '04'. "gc_pri
        APPEND ls_sdm_type TO xt_sdm_type.

        ls_sdm_type-sign   = 'I'.
        ls_sdm_type-option = 'EQ'.
        ls_sdm_type-low    = '05'. "gc_src
        APPEND ls_sdm_type TO xt_sdm_type.

        ls_sdm_type-sign   = 'I'.
        ls_sdm_type-option = 'EQ'.
        ls_sdm_type-low    = '06'. "gc_tar
        APPEND ls_sdm_type TO xt_sdm_type.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
