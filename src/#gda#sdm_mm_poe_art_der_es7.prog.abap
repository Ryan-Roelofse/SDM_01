*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POE_ART_DER_ES7
*&---------------------------------------------------------------------*

 DATA:
   ls_brf_screen_control TYPE /gda/sdm_s_screen_control,
   lt_screen_control     LIKE HASHED TABLE OF ls_brf_screen_control
                         WITH UNIQUE KEY screen_name.

 IMPORT screen_control = lt_screen_control FROM MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.

 LOOP AT fauswtab.
   READ TABLE lt_screen_control INTO ls_brf_screen_control WITH KEY screen_name = fauswtab-fname.
   IF sy-subrc EQ 0 AND ls_brf_screen_control-grey_out = 'X'.
     fauswtab-kzinp = c_0.
   ENDIF.

   READ TABLE lt_screen_control INTO ls_brf_screen_control WITH KEY screen_name = fauswtab-fname.
   IF sy-subrc EQ 0 AND ls_brf_screen_control-grey_out = 'X'.
     fauswtab-kzinp = c_0.
   ENDIF.

   MODIFY fauswtab.
 ENDLOOP.
