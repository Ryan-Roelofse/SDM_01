*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POE_ART_EXPORT_3
*&---------------------------------------------------------------------*

DATA:
  ls_eine TYPE eine.

MOVE-CORRESPONDING wmgeine TO ls_eine.
EXPORT eina FROM weina eine FROM ls_eine TO MEMORY ID 'GD_MM_ARTICLE_VAL_EINA_EINE'.   "Refer to include /GDA/SDM_MM_ART_VALIDATION
