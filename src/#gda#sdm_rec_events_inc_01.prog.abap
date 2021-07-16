*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_01
*&---------------------------------------------------------------------*


* Special Condition 1 - Overide config
if <main_output>-tabname = '/GDA/SDM_S_MARC_01' and <main_output_02>-tabname = '/GDA/SDM_S_MARD_01'
and <secondary>-fieldname = 'LGORT'.
  continue.
endif.

if <main_output>-tabname = '/GDA/SDM_S_EINE_01' and <main_output_02>-tabname = '/GDA/SDM_S_MAKT_01'.
  break rroelofse.

  read table keys_secondary with key fieldname = 'INFNR' transporting no fields.
  if sy-subrc <> 0.
    read table keys_primary assigning field-symbol(<transfer_to_primary>) with key fieldname = 'INFNR'.
    check sy-subrc = 0.
    append <transfer_to_primary> to keys_secondary.
  endif.
endif.
