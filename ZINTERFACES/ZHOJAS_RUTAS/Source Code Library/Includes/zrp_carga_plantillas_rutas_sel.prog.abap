*&---------------------------------------------------------------------*
*& Include zrp_carga_plantillas_rutas_sel
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.

    PARAMETERS p_file TYPE file_name.
    SELECTION-SCREEN PUSHBUTTON /33(45) bt_proc USER-COMMAND but1.
    SELECTION-SCREEN PUSHBUTTON /33(45) bt_ejec USER-COMMAND but2.

SELECTION-SCREEN END OF BLOCK bl01.



AT SELECTION-SCREEN OUTPUT.
  " Asignar el texto al botón
  bt_proc = |{ TEXT-004 }|.
  bt_ejec = |{ TEXT-003 }|.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  DATA lv_filename TYPE string.
  DATA lt_files    TYPE filetable.
  DATA lv_rc       TYPE i.

  cl_gui_frontend_services=>file_open_dialog( EXPORTING window_title      = |{ TEXT-005 }|
                                                        default_extension = '*.txt'
                                              CHANGING  file_table        = lt_files
                                                        rc                = lv_rc ).

  IF lv_rc > 0.
    READ TABLE lt_files INTO lv_filename INDEX 1.
    p_file = lv_filename.
  ELSE.
    p_file = ''.
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'BUT1'.
    zcl_intf_carga_hojas=>get_instance( )->display_directory( ).

  ELSEIF sy-ucomm = 'BUT2'.
    TRY.
        " Ejecutar Proceso de Interface - procesamiento de plantillas
        zcl_intf_proc_hojas=>get_instance( )->main_ctr( ).

      CATCH cx_root.
        IF sy-batch <> 'X'.
          MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 028.
        ENDIF.
    ENDTRY.
  ENDIF.
