*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POE_MAT_ENH8
*&---------------------------------------------------------------------*
  data:
    ls_screen_control type /gda/sdm_s_screen_control,
    lt_screen_control like hashed table of ls_screen_control
                           with unique key screen_name.

  import screen_control = lt_screen_control from memory id 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR

  loop at lt_screen_control assigning field-symbol(<control>).
    loop at tc_me_8020-cols assigning field-symbol(<lfs_cols>)  where screen-name eq <control>-screen_name.
      <lfs_cols>-screen-intensified = '1'.
      <lfs_cols>-selected           = abap_true.
      <lfs_cols>-selected = abap_true.
    endloop.
  endloop.
