FUNCTION zmf_hojas_rutas_wp.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_MODO_VIS) TYPE  CHAR1
*"     VALUE(IV_XSIMU) TYPE  XFELD
*"     VALUE(I_BATCH) TYPE  SYST_BATCH DEFAULT ' '
*"     VALUE(IV_BUFFER) TYPE  XFELD OPTIONAL
*"     VALUE(IV_NOMBRE_ARCHIVO) TYPE  FILE_NAME
*"     VALUE(IV_POSICION) TYPE  I
*"     VALUE(IV_MODULE) TYPE  UFPS_POSID
*"     VALUE(IV_PROGNAME) TYPE  PROGNAME
*"     VALUE(IV_COUNTRY) TYPE  LAND1
*"  TABLES
*"      IT_PLANTILLA1 TYPE  ZTTINTF_PLANTILLA1_STANDARD
*"      IT_PLANTILLA2 TYPE  ZTTINTF_PLANTILLA2_STANDARD
*"      IT_PLANTILLA3 TYPE  ZTTINTF_PLANTILLA3_STANDARD
*"      IT_PLANTILLA4 TYPE  ZTTINTF_PLANTILLA4_STANDARD
*"      IT_PLANTILLA5 TYPE  ZTTINTF_PLANTILLA5_STANDARD
*"      IT_PLANTILLA6 TYPE  ZTTINTF_PLANTILLA6_STANDARD
*"      IT_PLANTILLA7 TYPE  ZTTINTF_PLANTILLA7_STANDARD
*"      IT_MAST TYPE  ZTTINTF_MAST
*"      ET_BAPIRET2 TYPE  BAPIRET2_TAB OPTIONAL
*"      ET_DATA_OK_PLANTILLA1 TYPE  ZTTINTF_PLANTILLA1_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA2 TYPE  ZTTINTF_PLANTILLA2_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA3 TYPE  ZTTINTF_PLANTILLA3_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA4 TYPE  ZTTINTF_PLANTILLA4_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA5 TYPE  ZTTINTF_PLANTILLA5_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA6 TYPE  ZTTINTF_PLANTILLA6_STANDARD
*"       OPTIONAL
*"      ET_DATA_OK_PLANTILLA7 TYPE  ZTTINTF_PLANTILLA7_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA1 TYPE  ZTTINTF_PLANTILLA1_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA2 TYPE  ZTTINTF_PLANTILLA2_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA3 TYPE  ZTTINTF_PLANTILLA3_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA4 TYPE  ZTTINTF_PLANTILLA4_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA5 TYPE  ZTTINTF_PLANTILLA5_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA6 TYPE  ZTTINTF_PLANTILLA6_STANDARD
*"       OPTIONAL
*"      ET_DATA_NOK_PLANTILLA7 TYPE  ZTTINTF_PLANTILLA7_STANDARD
*"       OPTIONAL
*"----------------------------------------------------------------------
  " Variables de Bapi
  DATA lt_task                   TYPE STANDARD TABLE OF bapi1012_tsk_c WITH HEADER LINE.
  DATA ls_task                   TYPE bapi1012_tsk_c.
  DATA lt_materialtaskallocation TYPE STANDARD TABLE OF bapi1012_mtk_c WITH HEADER LINE.
  DATA lS_materialtaskallocation TYPE bapi1012_mtk_c.
  DATA lt_sequence               TYPE STANDARD TABLE OF bapi1012_seq_c WITH HEADER LINE.
  DATA ls_sequence               TYPE bapi1012_seq_c.
  DATA lt_operation              TYPE STANDARD TABLE OF bapi1012_opr_c WITH HEADER LINE.
  DATA ls_operation              TYPE bapi1012_opr_c.
  DATA lt_componentallocation    TYPE STANDARD TABLE OF bapi1012_com_c WITH HEADER LINE.
  DATA ls_componentallocation    TYPE bapi1012_com_c.
  DATA lt_productionresource     TYPE STANDARD TABLE OF bapi1012_prt_c WITH HEADER LINE.
  DATA ls_productionresource     TYPE bapi1012_prt_c.
  DATA lt_inspcharacteristic     TYPE STANDARD TABLE OF bapi1012_cha_c WITH HEADER LINE.
  DATA ls_inspcharacteristic     TYPE bapi1012_cha_c.
  DATA lt_textallocation         TYPE STANDARD TABLE OF bapi1012_txt_hdr_c.
  DATA lt_text                   TYPE STANDARD TABLE OF bapi1012_txt_c.
  DATA ls_text                   TYPE bapi1012_txt_c.
  DATA lv_group                  TYPE plnnr.
  " TODO: variable is assigned but never used (ABAP cleaner)
  DATA lv_groupcounter           TYPE plnal.
  DATA lt_plantilla2             TYPE STANDARD TABLE OF zsintf_plantilla2 WITH HEADER LINE.
  DATA lt_plantilla3             TYPE STANDARD TABLE OF zsintf_plantilla3 WITH HEADER LINE.
  DATA lt_plantilla4             TYPE STANDARD TABLE OF zsintf_plantilla4 WITH HEADER LINE.
  DATA lt_plantilla5             TYPE STANDARD TABLE OF zsintf_plantilla5 WITH HEADER LINE.
  DATA lt_plantilla6             TYPE STANDARD TABLE OF zsintf_plantilla6 WITH HEADER LINE.
  DATA lt_plantilla7             TYPE STANDARD TABLE OF zsintf_plantilla7 WITH HEADER LINE.

  " Variables extras
  DATA lt_return                 TYPE bapiret2_t.
  DATA lwa_return                TYPE bapiret2.
  DATA lv_flag                   TYPE flag.
  DATA lv_message                TYPE string.
  DATA lv_commit_work            TYPE c LENGTH 1 VALUE abap_false.
  DATA lt_parts                  TYPE TABLE OF string.
  DATA lv_check_fechas           TYPE c LENGTH 1 VALUE abap_false.
  DATA lv_plantilla              TYPE string.



  CONSTANTS lc_languaje  TYPE spras   VALUE 'S'.
  CONSTANTS lc_line_from TYPE text_to VALUE '1'.
  CONSTANTS lc_line_to   TYPE text_to VALUE '30'.

  FIELD-SYMBOLS <fs_plantilla2> TYPE zsintf_plantilla2.
  FIELD-SYMBOLS <fs_plantilla3> TYPE zsintf_plantilla3.
  FIELD-SYMBOLS <fs_plantilla4> TYPE zsintf_plantilla4.
  FIELD-SYMBOLS <fs_plantilla5> TYPE zsintf_plantilla5.
  FIELD-SYMBOLS <fs_plantilla6> TYPE zsintf_plantilla6.
  FIELD-SYMBOLS <fs_plantilla7> TYPE zsintf_plantilla7.

  " Instanciar la Clase del Monitor
  go_monitor = zcl_intf_monitor=>get_instance( ).

  LOOP AT it_plantilla1 ASSIGNING FIELD-SYMBOL(<fs_plantilla1>).

    MOVE-CORRESPONDING <fs_plantilla1> TO ls_task.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_task-group_counter
      IMPORTING output = ls_task-group_counter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_task-task_list_group
      IMPORTING output = ls_task-task_list_group.

    APPEND ls_task TO lt_task.

    LOOP AT it_plantilla2 ASSIGNING <fs_plantilla2> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla2> TO ls_materialtaskallocation.
      APPEND <fs_plantilla2> TO lt_plantilla2.

      ls_materialtaskallocation-material = <fs_plantilla2>-material.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_materialtaskallocation-material
        IMPORTING output = ls_materialtaskallocation-material.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_materialtaskallocation-group_counter
        IMPORTING output = ls_materialtaskallocation-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_materialtaskallocation-task_list_group
      IMPORTING output = ls_materialtaskallocation-task_list_group.

      " Se valida que las fechas from y to sean iguales
      IF <fs_plantilla2>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla2>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'Material' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.

      APPEND ls_materialtaskallocation TO lt_materialtaskallocation.

    ENDLOOP.

    LOOP AT it_plantilla3 ASSIGNING <fs_plantilla3> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla3> TO ls_sequence.
      APPEND <fs_plantilla3> TO lt_plantilla3.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_sequence-group_counter
        IMPORTING output = ls_sequence-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_sequence-sequence_no
        IMPORTING output = ls_sequence-sequence_no.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_sequence-return_operation
        IMPORTING output = ls_sequence-return_operation.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_sequence-reference_sequence
        IMPORTING output = ls_sequence-reference_sequence.

     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_sequence-task_list_group
      IMPORTING output = ls_sequence-task_list_group.

      APPEND ls_sequence TO lt_sequence.

      " Se valida qu elas fechas from y to sean iguales
      IF <fs_plantilla3>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla3>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'Secuencia' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.

    ENDLOOP.

    LOOP AT it_plantilla4 ASSIGNING <fs_plantilla4> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla4> TO ls_operation.
      APPEND <fs_plantilla4> TO lt_plantilla4.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_operation-group_counter
        IMPORTING output = ls_operation-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_operation-activity
        IMPORTING output = ls_operation-activity.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_operation-vendor_no
        IMPORTING output = ls_operation-vendor_no.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_operation-sequence_no
        IMPORTING output = ls_operation-sequence_no.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_operation-task_list_group
      IMPORTING output = ls_operation-task_list_group.

      APPEND ls_operation TO lt_operation.

      " Se valida qu elas fechas from y to sean iguales
      IF <fs_plantilla4>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla4>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'Operación' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.
    ENDLOOP.

    LOOP AT it_plantilla5 ASSIGNING <fs_plantilla5> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla5> TO ls_componentallocation.
      APPEND <fs_plantilla5> TO lt_plantilla5.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_componentallocation-group_counter
        IMPORTING output = ls_componentallocation-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_componentallocation-alternative_bom
        IMPORTING output = ls_componentallocation-alternative_bom.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_componentallocation-material
        IMPORTING output = ls_componentallocation-material.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = ls_componentallocation-task_list_group
      IMPORTING output = ls_componentallocation-task_list_group.

      ASSIGN it_mast[ material = ls_materialtaskallocation-material
                      werks    = ls_componentallocation-plant
                      stlal    = ls_componentallocation-alternative_bom ] TO FIELD-SYMBOL(<fs_mast>).

      ls_componentallocation-bom_no = <fs_mast>-stlnr.
      APPEND ls_componentallocation TO lt_componentallocation.

      " Se valida qu elas fechas from y to sean iguales
      IF <fs_plantilla5>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla5>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'Componentes' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.

    ENDLOOP.

    LOOP AT it_plantilla6 ASSIGNING <fs_plantilla6> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla6> TO ls_productionresource.
      APPEND <fs_plantilla6> TO lt_plantilla6.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_productionresource-group_counter
        IMPORTING output = ls_productionresource-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_productionresource-material
        IMPORTING output = ls_productionresource-material.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_productionresource-task_list_group
        IMPORTING output = ls_productionresource-task_list_group.

      APPEND ls_productionresource TO lt_productionresource.

      " Se valida qu elas fechas from y sean iguales
      IF <fs_plantilla6>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla6>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'MAF' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.

    ENDLOOP.

    LOOP AT it_plantilla7 ASSIGNING <fs_plantilla7> WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
      MOVE-CORRESPONDING <fs_plantilla7> TO ls_inspcharacteristic.
      APPEND <fs_plantilla7> TO lt_plantilla7.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_inspcharacteristic-group_counter
        IMPORTING output = ls_inspcharacteristic-group_counter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_inspcharacteristic-activity
        IMPORTING output = ls_inspcharacteristic-activity.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_inspcharacteristic-task_list_group
        IMPORTING output = ls_inspcharacteristic-task_list_group.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = ls_inspcharacteristic-spc_criterion_key
        IMPORTING output = ls_inspcharacteristic-spc_criterion_key.


      APPEND ls_inspcharacteristic TO lt_inspcharacteristic.
      " Se valida que las fechas from y sean iguales
      IF <fs_plantilla7>-valid_from <> <fs_plantilla1>-valid_from OR <fs_plantilla7>-valid_to_date <> <fs_plantilla1>-valid_to_date.
        lv_check_fechas = abap_true.
        CONCATENATE lv_plantilla 'Caracteristicas' INTO lv_plantilla SEPARATED BY ' - '.
      ENDIF.

      ls_inspcharacteristic-target_val = ROUND( val  = ls_inspcharacteristic-target_val  dec = 3 ).
      CONDENSE ls_inspcharacteristic-target_val NO-GAPS.

      ls_inspcharacteristic-lw_tol_lmt = ROUND( val  = ls_inspcharacteristic-lw_tol_lmt  dec = 3 ).
      CONDENSE ls_inspcharacteristic-lw_tol_lmt NO-GAPS.

      ls_inspcharacteristic-up_tol_lmt = ROUND( val  = ls_inspcharacteristic-up_tol_lmt  dec = 3 ).
      CONDENSE ls_inspcharacteristic-up_tol_lmt NO-GAPS.

    ENDLOOP.

    " Se validan que las fechas valid_from y valid_to coincidan
    IF lv_check_fechas = abap_true.
      MESSAGE s039(zintf) WITH lv_plantilla INTO DATA(lv_message_039). " Las fechas valid_from y valid_to no coinciden
      " Retornar los Mensajes de Error
      CLEAR lwa_return.
      lwa_return-type    = 'E'.
      lwa_return-id      = 'ZINTF'.
      lwa_return-number  = '039'.
      lwa_return-message = lv_message_039.
      lwa_return-row     = <fs_plantilla1>-row.
      APPEND lwa_return TO et_bapiret2.
      EXIT.
    ENDIF.

    " Se validan que la hoja de ruta tenga información de la plantilla de materiales y operaciones
    IF lt_materialtaskallocation[] IS INITIAL or lt_operation[] IS INITIAL.
      MESSAGE s041(zintf) WITH lv_plantilla INTO DATA(lv_message_041). " La hoja de ruta debe tener información en materiales y operaciones
      " Retornar los Mensajes de Error
      CLEAR lwa_return.
      lwa_return-type    = 'E'.
      lwa_return-id      = 'ZINTF'.
      lwa_return-number  = '041'.
      lwa_return-message = lv_message_041.
      lwa_return-row     = <fs_plantilla1>-row.
      APPEND lwa_return TO et_bapiret2.
      CONTINUE.
    ENDIF.

    " Se crea texto explicativo de la tabla de cabecera task
    lt_textallocation = VALUE #( ( object_type   = '10'
                                 valid_from    = <fs_plantilla1>-valid_from
                                 valid_to_date = <fs_plantilla1>-valid_to_date
                                 langu         = lc_languaje
                                 line_from     = lc_line_from
                                 LINE_to       = lc_line_to ) ).

    lt_text = VALUE #( ( format_col = 1 text_line = <fs_plantilla1>-description ) ).

    SPLIT <fs_plantilla1>-tdline AT '|' INTO TABLE lt_parts.
    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<fs_part>).
      ls_text = VALUE #( format_col = sy-tabix + 1 text_line = <fs_part> ).
      APPEND ls_text TO lt_text.
    ENDLOOP.

    " Se ejecuta Bapi de creación de Hojas de ruta
    CALL FUNCTION 'BAPI_ROUTING_CREATE'
      EXPORTING profile                = ''
                bomusage               = '4'
                application            = 'PP01'
      IMPORTING group                  = lv_group
                groupcounter           = lv_groupcounter
      TABLES    task                   = lt_task
                materialtaskallocation = lt_materialtaskallocation
                sequence               = lt_sequence
                operation              = lt_operation
                componentallocation    = lt_componentallocation
                productionresource     = lt_productionresource
                inspcharacteristic     = lt_inspcharacteristic
                textallocation         = lt_textallocation
                text                   = lt_text
                return                 = lt_return.

    CLEAR lv_flag.
    LOOP AT lt_return INTO lwa_return.
      IF lwa_return-type = 'A' OR lwa_return-type = 'E'. " Retornar los Mensajes de Error
        lv_flag = abap_true.
        " Retornar los Mensajes de Error
        lwa_return-type    = 'E'.
        lwa_return-row     = <fs_plantilla1>-row.

        lwa_return-message = |{ lwa_return-message } - { lwa_return-parameter }|.
        APPEND lwa_return TO et_bapiret2.

      ENDIF.
      CLEAR lwa_return.
    ENDLOOP.

    IF lv_flag IS INITIAL.
      " Guardar los Registros que han quedado sin error

      APPEND <fs_plantilla1> TO et_data_ok_PLANTILLA1.
      APPEND LINES OF lt_plantilla2 TO et_data_ok_PLANTILLA2.
      APPEND LINES OF lt_plantilla3 TO et_data_ok_PLANTILLA3.
      APPEND LINES OF lt_plantilla4 TO et_data_ok_PLANTILLA4.
      APPEND LINES OF lt_plantilla5 TO et_data_ok_PLANTILLA5.
      APPEND LINES OF lt_plantilla6 TO et_data_ok_PLANTILLA6.
      APPEND LINES OF lt_plantilla7 TO et_data_ok_PLANTILLA7.

      " El registro del material &1 se creo con exito
      MESSAGE s027(zintf) WITH lv_group INTO DATA(lv_message_021).

      " Retornar los Mensajes de Error
      CLEAR lwa_return.
      lwa_return-type    = 'S'.
      lwa_return-id      = 'ZINTF'.
      lwa_return-number  = '021'.
      lwa_return-message = lv_message_021.
      lwa_return-row     = <fs_plantilla1>-row.
      APPEND lwa_return TO et_bapiret2.

      lv_commit_work = abap_true.
    ELSE.
      " Guardar los Registros que han quedado con Error
      APPEND <fs_plantilla1> TO et_data_nok_PLANTILLA1.

      APPEND LINES OF lt_plantilla2 TO et_data_nok_PLANTILLA2.
      APPEND LINES OF lt_plantilla3 TO et_data_nok_PLANTILLA3.
      APPEND LINES OF lt_plantilla4 TO et_data_nok_PLANTILLA4.
      APPEND LINES OF lt_plantilla5 TO et_data_nok_PLANTILLA5.
      APPEND LINES OF lt_plantilla6 TO et_data_nok_PLANTILLA6.
      APPEND LINES OF lt_plantilla7 TO et_data_nok_PLANTILLA7.

    ENDIF.

    FREE:
          lt_return[],
          lt_task,
          ls_task,
          lt_materialtaskallocation,
          lS_materialtaskallocation,
          lt_sequence,
          lt_operation,
          ls_operation,
          lt_componentallocation,
          ls_componentallocation,
          lt_productionresource,
          ls_productionresource,
          lt_inspcharacteristic,
          ls_inspcharacteristic,
          lt_plantilla2,
          lt_plantilla3,
          lt_plantilla4,
          lt_plantilla5,
          lt_plantilla6,
          lt_plantilla7,
          lt_textallocation,
          lv_plantilla,
          lt_text,
          ls_text.

  ENDLOOP.

  sy-subrc = 0.
  IF lv_commit_work = abap_true.
    COMMIT WORK AND WAIT.
  ENDIF.
  IF sy-subrc <> 0.

    " Atrapar el Error del Sistema
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO lv_message
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    " Un Error Ocurrió en HDB
    go_monitor->set_registers_single_error( iv_id_intefaz      = gc_nomb_interfaz " Nombre de la Interface
                                            iv_mensaje_proceso = CONV #( lv_message )       " Texto de mensaje
                                            iv_nombre_archivo  = iv_nombre_archivo          " Nombre del fichero
                                            iv_estado_error    = zintf_gc_verde             " Estado de la Interfaz
                                            iv_num_mensaje     = sy-msgno ).                " Número de mensaje

  ENDIF.
ENDFUNCTION.
