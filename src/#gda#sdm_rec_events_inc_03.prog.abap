*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_3
*&---------------------------------------------------------------------*

IF <primary>-tabname = '/GDA/SDM_S_EINE_01' AND current_table = 'MAKT'.
  READ TABLE current_keys_empty WITH KEY fieldname = 'INFNR' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE context_keys_empty ASSIGNING FIELD-SYMBOL(<transfer_to_primary>) WITH KEY fieldname = 'INFNR'.
    CHECK sy-subrc = 0.
    APPEND <transfer_to_primary> TO current_keys_empty.
  ENDIF.

  READ TABLE current_keys WITH KEY fieldname = 'INFNR' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE context_keys ASSIGNING FIELD-SYMBOL(<transfer_to_primary2>) WITH KEY fieldname = 'INFNR'.
    CHECK sy-subrc = 0.
    APPEND <transfer_to_primary2> TO current_keys.
  ENDIF.
ENDIF.

IF <primary>-tabname = '/GDA/SDM_S_PRICING_01' AND current_table = 'KONH'.
  break rroelofse.
  READ TABLE current_keys_empty WITH KEY fieldname = 'INFNR' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE context_keys_empty ASSIGNING <transfer_to_primary> WITH KEY fieldname = 'INFNR'.
    CHECK sy-subrc = 0.
    APPEND <transfer_to_primary> TO current_keys_empty.
  ENDIF.

  READ TABLE current_keys WITH KEY fieldname = 'INFNR' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE context_keys ASSIGNING <transfer_to_primary2> WITH KEY fieldname = 'INFNR'.
    CHECK sy-subrc = 0.
    APPEND <transfer_to_primary2> TO current_keys.
  ENDIF.

ENDIF.

*IF <primary>-tabname = '/GDA/SDM_S_PRICING_01' AND current_table = 'KONH'.
*  READ TABLE current_keys_empty WITH KEY fieldname = 'KSCHL' TRANSPORTING NO FIELDS.
*  IF sy-subrc <> 0.
*    READ TABLE context_keys_empty ASSIGNING <transfer_to_primary> WITH KEY fieldname = 'KSCHL'.
*    CHECK sy-subrc = 0.
*    APPEND <transfer_to_primary> TO context_keys_empty.
*  ENDIF.
*
*  READ TABLE current_keys WITH KEY fieldname = 'KSCHL' TRANSPORTING NO FIELDS.
*  IF sy-subrc <> 0.
*    READ TABLE context_keys ASSIGNING <transfer_to_primary2> WITH KEY fieldname = 'KSCHL'.
*    CHECK sy-subrc = 0.
*    APPEND <transfer_to_primary2> TO context_keys.
*  ENDIF.
*ENDIF.
