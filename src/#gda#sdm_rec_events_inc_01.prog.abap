*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_01
*&---------------------------------------------------------------------*
DATA transfer_to_sec LIKE LINE OF keys_secondary.

* Special Condition 1 - Overide config
IF <main_output>-tabname = '/GDA/SDM_S_MARC_01' AND <main_output_02>-tabname = '/GDA/SDM_S_MARD_01'
AND <secondary>-fieldname = 'LGORT'.
  CONTINUE.
ENDIF.

IF <main_output>-tabname = '/GDA/SDM_S_EINE_01' AND <main_output_02>-tabname = '/GDA/SDM_S_MAKT_01'.
  break rroelofse.

  READ TABLE keys_secondary WITH KEY fieldname = 'INFNR' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE keys_primary ASSIGNING FIELD-SYMBOL(<transfer_to_primary>) WITH KEY fieldname = 'INFNR'.
    CHECK sy-subrc = 0.
    APPEND <transfer_to_primary> TO keys_secondary.
  ENDIF.
ENDIF.

IF <main_output>-tabname = '/GDA/SDM_S_MARC_01' AND <main_output_02>-tabname = '/GDA/SDM_S_WRPL_01'.
  READ TABLE keys_secondary WITH KEY fieldname = 'WERKS' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE keys_secondary ASSIGNING FIELD-SYMBOL(<kunnr_to_werks>) WITH KEY fieldname = 'KUNNR'.
    IF sy-subrc EQ 0.
      transfer_to_sec-fieldname  = 'WERKS'.
      transfer_to_sec-value = <kunnr_to_werks>-value.
      APPEND transfer_to_sec TO keys_secondary.
      CLEAR transfer_to_sec.
    ENDIF.
  ENDIF.
ENDIF.

IF <main_output>-tabname = '/GDA/SDM_S_MARC_01' AND <main_output_02>-tabname = '/GDA/SDM_S_WRPL_01'
AND <secondary>-fieldname = 'KUNNR'.
  CONTINUE.
ENDIF.
