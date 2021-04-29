class /GDA/SDM_CL_KPI_01 definition
  public
  inheriting from /GDA/SDM_CL_KPI_CORE
  final
  create public .

public section.

  methods RUN
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_KPI_01 IMPLEMENTATION.


  METHOD run.


    DATA:
      ls_sum_object  TYPE /gda/sdm_s_api_sum_object,
      lv_records     TYPE sy-dbcnt,
      ls_sdm_obj_tx  TYPE /gda/sdm_obj_tx,
      ls_sdm_exc_mai TYPE /gda/sdm_exc_mai.

    super->run( ).

* SDM description
    CLEAR ls_sdm_obj_tx.
    READ TABLE me->mt_sdm_obj_tx INTO ls_sdm_obj_tx WITH KEY sdm_object_id = me->/gda/sdm_if_kpi_core~mv_sdm_object_id.
    IF sy-subrc NE 0.

      SELECT SINGLE *
               FROM /gda/sdm_obj_tx
               INTO ls_sdm_obj_tx
              WHERE spras         EQ sy-langu
                AND sdm_object_id EQ me->/gda/sdm_if_kpi_core~mv_sdm_object_id.

      IF sy-subrc EQ 0.
        APPEND ls_sdm_obj_tx TO me->mt_sdm_obj_tx.
      ENDIF.

    ENDIF.

* Objects created
    SELECT COUNT(*)
      INTO lv_records
      FROM mara
     WHERE ersda GE me->mv_from
       AND attyp EQ space.

    IF sy-subrc EQ 0.

      ls_sum_object-sdm_object_id   = me->/gda/sdm_if_kpi_core~mv_sdm_object_id.
      ls_sum_object-sdm_description = ls_sdm_obj_tx-sdm_description.
      ls_sum_object-records         = lv_records.
      APPEND ls_sum_object TO me->/gda/sdm_if_kpi_core~mt_objects_created.

    ENDIF.

* Objects deleted
    SELECT COUNT(*)
      INTO lv_records
      FROM mara
     WHERE laeda GE me->mv_from
       AND lvorm EQ abap_true
       AND attyp EQ space.

    IF sy-subrc EQ 0.

      ls_sum_object-sdm_object_id   = me->/gda/sdm_if_kpi_core~mv_sdm_object_id.
      ls_sum_object-sdm_description = ls_sdm_obj_tx-sdm_description.
      ls_sum_object-records         = lv_records.
      APPEND ls_sum_object TO me->/gda/sdm_if_kpi_core~mt_objects_deleted.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
