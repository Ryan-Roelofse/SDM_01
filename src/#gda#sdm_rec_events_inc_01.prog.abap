*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_REC_EVENTS_INC_01
*&---------------------------------------------------------------------*


* Special Condition 1 - Overide config
if <main_output>-tabname = '/GDA/SDM_S_MARC_01' AND <main_output_02>-tabname = '/GDA/SDM_S_MARD_01'
and <SECONDARY>-FIELDNAME = 'LGORT'.
CONTINUE.
endif.


*if <main_output>-tabname = '/GDA/SDM_S_MLGN_01' AND <main_output_02>-tabname = '/GDA/SDM_S_MLGT_01'
*and <SECONDARY>-FIELDNAME = 'LGTYP'.
*  BREAK-POINT.
*CONTINUE.
*endif.
