*&---------------------------------------------------------------------*
*& Include zrp_carga_plantillas_rutas_eve
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      " Se realiza la carga de plantillas
      zcl_intf_carga_hojas=>get_instance( )->upload_file( im_v_path = p_file ).

    CATCH cx_root.
      IF sy-batch <> 'X'.
        MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 028.
      ENDIF.
  ENDTRY.
