*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POE_ART_VAL_ENH9
*&---------------------------------------------------------------------*

data:
 details_original_material type rmmg1,
 details_dc_material       type rmmw2.

details_original_material = wrmmg1.
details_dc_material       = wrmmw2.

export details_original_material to memory id 'DETAILS_ORIGINAL'.
export details_dc_material       to memory id 'DETAILS_DC'.
