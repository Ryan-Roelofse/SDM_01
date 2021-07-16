*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_3
*&---------------------------------------------------------------------*

if <primary>-tabname = '/GDA/SDM_S_EINE_01' and current_table = 'MAKT'.
  break rroelofse.
  read table current_keys_empty with key fieldname = 'INFNR' transporting no fields.
  if sy-subrc <> 0.
    read table context_keys_empty assigning field-symbol(<transfer_to_primary>) with key fieldname = 'INFNR'.
    check sy-subrc = 0.
    append <transfer_to_primary> to current_keys_empty.
  endif.

  read table current_keys with key fieldname = 'INFNR' transporting no fields.
  if sy-subrc <> 0.
    read table context_keys assigning field-symbol(<transfer_to_primary2>) with key fieldname = 'INFNR'.
    check sy-subrc = 0.
    append <transfer_to_primary2> to current_keys.
  endif.

endif.
