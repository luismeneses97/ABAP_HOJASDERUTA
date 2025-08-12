class ZCL_INTF_PROC_HOJAS_WP definition
  public
  final
  create public .

  PUBLIC SECTION.
    DATA gt_bapiret2           TYPE bapiret2_tab.
    DATA go_cx_root            TYPE REF TO cx_root.
    DATA gv_rows               TYPE i.
    DATA gv_proc_enviados      TYPE i.
    DATA gv_max_jobs           TYPE i.
    DATA gv_tarea_proc         TYPE numc10.
    DATA go_intf_proc_hojas TYPE REF TO zcl_intf_proc_hojas.
    data GV_FINALIZADO type ABAP_BOOL .

    METHODS constructor.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_collector) TYPE REF TO zcl_intf_proc_hojas_wp.

    METHODS set_proc_data_rec
      IMPORTING p_task TYPE CLIKE.

    METHODS set_start_wp
      IMPORTING iv_xsimu          TYPE xfeld
                pmaxjobs          TYPE i
                iv_buffer         TYPE xfeld OPTIONAL
                iv_cant_registros TYPE i OPTIONAL
                i_batch           TYPE syst_batch OPTIONAL
                i_modo            TYPE char1 OPTIONAL
                i_modo_vis        TYPE char1
                i_pnomtask        TYPE char11
                it_data_s         TYPE zintf_tt_cont_proc_hojas
                iv_module         TYPE ufps_posid
                iv_progname       TYPE progname
                iv_country        TYPE land1
      EXPORTING et_bapiret2       TYPE bapiret2_tab
      RAISING   cx_amdp_execution_failed
                cx_sql_exception
                cx_amdp_result_table_error
                cx_amdp_error
                cx_static_check.

protected section.
  PRIVATE SECTION.
    TYPES  BEGIN OF ty_org_ventas.
    TYPES:   mandt TYPE mandt.
             INCLUDE TYPE zstbc_constant.
    TYPES: END OF ty_org_ventas.

    TYPES tt_org_ventas TYPE TABLE OF ty_org_ventas.

    CLASS-DATA mo_collector TYPE REF TO zcl_intf_proc_hojas_wp.

    DATA go_cx_amdp_execution_failed TYPE REF TO cx_amdp_execution_failed.

    METHODS get_buscar_wps
      IMPORTING i_proc_enviados TYPE i
                i_max_jobs      TYPE i.

    METHODS get_recuperar_wps
      CHANGING i_max_jobs TYPE i.

ENDCLASS.



CLASS ZCL_INTF_PROC_HOJAS_WP IMPLEMENTATION.


  METHOD CONSTRUCTOR.

  ENDMETHOD.


  METHOD GET_BUSCAR_WPS.

    DATA: lv_max_wps  TYPE i,                               "#EC NEEDED
          lv_free_wps TYPE i.
    DO.

      " Determinar WPS Disponibles
      CALL FUNCTION 'RFC_SYSTEM_INFO'
        IMPORTING
          maximal_resources = lv_max_wps
          current_resources = lv_free_wps.

      IF sy-subrc <> 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        EXIT.

      ELSEIF lv_free_wps > 2.

        " Controlador para que los WP enviados no superen los Permitidos por Parametrización
        IF gv_proc_enviados < gv_max_jobs.
          EXIT.
        ELSE.

          WAIT UNTIL gv_proc_enviados = 0
            UP TO 1 SECONDS.
          IF sy-subrc <> 0.
            WAIT UNTIL gv_proc_enviados < gv_max_jobs.
          ENDIF.
        ENDIF.

      ELSE.

        WAIT UNTIL i_proc_enviados = 0
        UP TO 1 SECONDS.

        IF sy-subrc <> 0.
          WAIT UNTIL i_proc_enviados < i_max_jobs.
        ENDIF.

      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD get_instance.
    " Instancia Patron Singleton
    IF mo_collector IS NOT BOUND.
      mo_collector = NEW zcl_intf_proc_hojas_wp( ).
    ENDIF.

    " Retornando la Intancia Activa
    ro_collector = mo_collector.
  ENDMETHOD.


  METHOD GET_RECUPERAR_WPS.

    DATA: lv_max_wps  TYPE i,                               "#EC NEEDED
          lv_free_wps TYPE i.

    " Obtener Información de los Work Process Disponibles
    CALL FUNCTION 'RFC_SYSTEM_INFO'
      IMPORTING
        maximal_resources = lv_max_wps
        current_resources = lv_free_wps.

*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    " Si los recursos son mayores al parámetro o existe error al incializar, utilizar parámetro
    IF lv_free_wps > i_max_jobs.

      " Recálcula el Máximo de Jobs siempre dejando 2 WP Disponibles
      IF i_max_jobs > 2.
        i_max_jobs = i_max_jobs - 2.
      ELSE.
        i_max_jobs = i_max_jobs.
      ENDIF.

    ELSE.

      " Recalculando el Máximo de Jobs Permitidos para Operar
      IF lv_free_wps > 2.
        i_max_jobs = lv_free_wps - 2.
      ELSE.
        i_max_jobs = lv_free_wps.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD SET_PROC_DATA_REC.

    DATA: lv_message  TYPE char255,
          lt_bapiret2 TYPE bapiret2_tab,
          lt_data_ok_plantilla1 TYPE zttintf_plantilla1_standard,
          lt_data_ok_plantilla2 TYPE zttintf_plantilla2_standard,
          lt_data_ok_plantilla3 TYPE zttintf_plantilla3_standard,
          lt_data_ok_plantilla4 TYPE zttintf_plantilla4_standard,
          lt_data_ok_plantilla5 TYPE zttintf_plantilla5_standard,
          lt_data_ok_plantilla6 TYPE zttintf_plantilla6_standard,
          lt_data_ok_plantilla7 TYPE zttintf_plantilla7_standard,
          lt_data_nok_plantilla1 TYPE zttintf_plantilla1_standard,
          lt_data_nok_plantilla2 TYPE zttintf_plantilla2_standard,
          lt_data_nok_plantilla3 TYPE zttintf_plantilla3_standard,
          lt_data_nok_plantilla4 TYPE zttintf_plantilla4_standard,
          lt_data_nok_plantilla5 TYPE zttintf_plantilla5_standard,
          lt_data_nok_plantilla6 TYPE zttintf_plantilla6_standard,
          lt_data_nok_plantilla7 TYPE zttintf_plantilla7_standard.

    " Libera Proceso
    IF gv_proc_enviados > 0.

      gv_proc_enviados = gv_proc_enviados - 1.
    ENDIF.

    " Retorna los Mensajes de Error desde el MF Asincrónico ZMF_Hojas_ruta
    RECEIVE RESULTS FROM FUNCTION 'ZMF_HOJAS_RUTAS_WP'
    TABLES et_bapiret2 = lt_bapiret2
           et_data_ok_plantilla1   = lt_data_ok_plantilla1
           et_data_ok_plantilla2   = lt_data_ok_plantilla2
           et_data_ok_plantilla3   = lt_data_ok_plantilla3
           et_data_ok_plantilla4   = lt_data_ok_plantilla4
           et_data_ok_plantilla5   = lt_data_ok_plantilla5
           et_data_ok_plantilla6   = lt_data_ok_plantilla6
           et_data_ok_plantilla7   = lt_data_ok_plantilla7
           et_data_nok_plantilla1  = lt_data_nok_plantilla1
           et_data_nok_plantilla2  = lt_data_nok_plantilla2
           et_data_nok_plantilla3  = lt_data_nok_plantilla3
           et_data_nok_plantilla4  = lt_data_nok_plantilla4
           et_data_nok_plantilla5  = lt_data_nok_plantilla5
           et_data_nok_plantilla6  = lt_data_nok_plantilla6
           et_data_nok_plantilla7  = lt_data_nok_plantilla7
    EXCEPTIONS
      system_failure        = 1  MESSAGE lv_message
      communication_failure = 2  MESSAGE lv_message
      resource_failure      = 3.

    IF sy-subrc NE 0.

      IF sy-batch = abap_false.
        MESSAGE s893(pz) WITH lv_message.
      ELSE.
        WRITE:/ lv_message.
      ENDIF.
    ELSE.

      " Indica que el Proceso ya finalizó
      gv_finalizado = abap_true.

      " Asigna los Mensajes del Log de Proceso
      APPEND LINES OF lt_bapiret2 TO me->gt_bapiret2.

      " Copiar los Archivos a las Carpetas del Servidor PROCESADOS y ERRORES
      APPEND LINES OF lt_data_ok_plantilla1  TO go_intf_proc_hojas->gt_data_ok_plantilla1.
      APPEND LINES OF lt_data_ok_plantilla2  TO go_intf_proc_hojas->gt_data_ok_plantilla2.
      APPEND LINES OF lt_data_ok_plantilla3  TO go_intf_proc_hojas->gt_data_ok_plantilla3.
      APPEND LINES OF lt_data_ok_plantilla4  TO go_intf_proc_hojas->gt_data_ok_plantilla4.
      APPEND LINES OF lt_data_ok_plantilla5  TO go_intf_proc_hojas->gt_data_ok_plantilla5.
      APPEND LINES OF lt_data_ok_plantilla6  TO go_intf_proc_hojas->gt_data_ok_plantilla6.
      APPEND LINES OF lt_data_ok_plantilla7  TO go_intf_proc_hojas->gt_data_ok_plantilla7.
      APPEND LINES OF lt_data_nok_plantilla1 TO go_intf_proc_hojas->gt_data_nok_plantilla1.
      APPEND LINES OF lt_data_nok_plantilla2 TO go_intf_proc_hojas->gt_data_nok_plantilla2.
      APPEND LINES OF lt_data_nok_plantilla3 TO go_intf_proc_hojas->gt_data_nok_plantilla3.
      APPEND LINES OF lt_data_nok_plantilla4 TO go_intf_proc_hojas->gt_data_nok_plantilla4.
      APPEND LINES OF lt_data_nok_plantilla5 TO go_intf_proc_hojas->gt_data_nok_plantilla5.
      APPEND LINES OF lt_data_nok_plantilla6 TO go_intf_proc_hojas->gt_data_nok_plantilla6.
      APPEND LINES OF lt_data_nok_plantilla7 TO go_intf_proc_hojas->gt_data_nok_plantilla7.

      " Asignar los Mensajes a la Clase Principal de la Interface
      go_intf_proc_hojas->gt_bapiret2 = me->gt_bapiret2.
    ENDIF.

  ENDMETHOD.


  METHOD set_start_wp.
    " TODO: parameter IV_CANT_REGISTROS is never used (ABAP cleaner)

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  SET_START_WP                                    "
    "  Título           : Interfaz p.Carga HOJAS DE RUTA                     "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar la carga de hoas de ruta                                     "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"


    DATA lt_file_pack       TYPE zintf_tt_cont_proc_hojas.
    DATA ls_file_pack       TYPE LINE OF zintf_tt_cont_proc_hojas.
    DATA lt_pack_plantilla1 TYPE zttintf_plantilla1_standard.
    DATA lt_pack_plantilla2 TYPE zttintf_plantilla2_standard.
    DATA lt_pack_plantilla3 TYPE zttintf_plantilla3_standard.
    DATA lt_pack_plantilla4 TYPE zttintf_plantilla4_standard.
    DATA lt_pack_plantilla5 TYPE zttintf_plantilla5_standard.
    DATA lt_pack_plantilla6 TYPE zttintf_plantilla6_standard.
    DATA lt_pack_plantilla7 TYPE zttintf_plantilla7_standard.
    DATA lt_bapiret2        TYPE bapiret2_tab.
    DATA lv_pos_ini         TYPE i.
    DATA lv_pos_fin         TYPE i.
    DATA lv_task            TYPE c LENGTH 40.
    DATA lv_prefijo_task    TYPE c LENGTH 11 VALUE 'HOJAS_RUTA'.
    DATA lv_reg TYPE i.
    DATA lt_mast TYPE TABLE OF zsintf_mast.
    DATA lt_plantilla2 type zttintf_plantilla2_standard.
    DATA lv_matnr TYPE matnr18.
    TRY.

        " Cantidad por Default de Registros Enviados por cada Work Process Activado

        lv_reg = iv_cant_registros.
        " Instanciar la Clase de la Interfaz SOP012_client
        go_intf_proc_hojas = zcl_intf_proc_hojas=>get_instance( ).

        " Inicia por Defecto con la cantidad de Jobs estimado
        gv_max_jobs     = pmaxjobs.
        lv_prefijo_task = i_pnomtask.

        " Evalua si la cantidad de Jobs es posible
        get_recuperar_wps( CHANGING i_max_jobs = gv_max_jobs ).

        " Consulta de valores BOM_NO para la tabla COMPONENTALLOCATION

        ASSIGN it_data_s[ 1 ] TO FIELD-SYMBOL(<fs_data>).

        lt_plantilla2 = <fs_data>-plantilla2.

        SORT lt_plantilla2 BY material plant.
        DELETE ADJACENT DUPLICATES FROM lt_plantilla2 COMPARING material plant.

        LOOP AT lt_plantilla2 ASSIGNING FIELD-SYMBOL(<fs_plantilla2_aux>).

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING input  = <fs_plantilla2_aux>-material
            IMPORTING output = lv_matnr.

           <fs_plantilla2_aux>-material = lv_matnr.
        ENDLOOP.

        SELECT matnr, werks, stlal, stlnr
          FROM mast
          INTO TABLE @lt_mast
          FOR ALL ENTRIES IN @lt_plantilla2
          WHERE matnr = @lt_plantilla2-material
            AND werks = @lt_plantilla2-plant.


        CLEAR: lv_pos_ini,
               lv_pos_fin.

        DO.

          " Calculando los Paquetes de Registros que serán enviados a cada Work Process
          lv_pos_ini = lv_pos_fin + 1.
          lv_pos_fin = lv_pos_ini + lv_reg - 1.

          FREE lt_file_pack.
          FREE lt_pack_plantilla1.
          FREE lt_pack_plantilla2.
          FREE lt_pack_plantilla3.
          FREE lt_pack_plantilla4.
          FREE lt_pack_plantilla5.
          FREE lt_pack_plantilla6.
          FREE lt_pack_plantilla7.

          " Crear los Paquetes de Registros para Procesar
          LOOP AT it_data_s ASSIGNING FIELD-SYMBOL(<fs_data_s>).
            APPEND LINES OF <fs_data_s>-plantilla1 FROM lv_pos_ini TO lv_pos_fin TO lt_pack_plantilla1.
            " Buscamos en las demas plantillas los registros que coincidan por el campo task_list_group
            LOOP AT lt_pack_plantilla1 ASSIGNING FIELD-SYMBOL(<fs_plantilla1>).

              LOOP AT <fs_data_s>-plantilla2 ASSIGNING FIELD-SYMBOL(<fs_plantilla2>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter." #EC CI_NO_ORDER
                APPEND <fs_plantilla2> TO lt_pack_plantilla2.
              ENDLOOP.

              LOOP AT <fs_data_s>-plantilla3 ASSIGNING FIELD-SYMBOL(<fs_plantilla3>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
                APPEND <fs_plantilla3> TO lt_pack_plantilla3.
              ENDLOOP.

              LOOP AT <fs_data_s>-plantilla4 ASSIGNING FIELD-SYMBOL(<fs_plantilla4>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
                APPEND <fs_plantilla4> TO lt_pack_plantilla4.
              ENDLOOP.

              LOOP AT <fs_data_s>-plantilla5 ASSIGNING FIELD-SYMBOL(<fs_plantilla5>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
                APPEND <fs_plantilla5> TO lt_pack_plantilla5.
              ENDLOOP.

              LOOP AT <fs_data_s>-plantilla6 ASSIGNING FIELD-SYMBOL(<fs_plantilla6>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
                APPEND <fs_plantilla6> TO lt_pack_plantilla6.
              ENDLOOP.

              LOOP AT <fs_data_s>-plantilla7 ASSIGNING FIELD-SYMBOL(<fs_plantilla7>) WHERE task_list_group = <fs_plantilla1>-task_list_group AND group_counter = <fs_plantilla1>-group_counter.
                APPEND <fs_plantilla7> TO lt_pack_plantilla7.
              ENDLOOP.

            ENDLOOP.

          ENDLOOP.

          " Finaliza el Proceso cuando ya no hayan mas Paquetes
          IF lt_pack_plantilla1 IS INITIAL.
            EXIT.
          ENDIF.

        " Contador de Tareas
        gv_tarea_proc += 1.

        " Asignar Nombre a la Tarea
        lv_task = |{ lv_prefijo_task }{ sy-uzeit }_{ gv_tarea_proc ALPHA = OUT }|.

        " Buscar la cantidad real de Jobs disponibles
        get_buscar_wps( i_proc_enviados = gv_proc_enviados
                        i_max_jobs      = gv_max_jobs ).

        IF i_modo = abap_true.

          " Ejecutar la Tarea mediante Work Process Asincrónico
          "____________________________________________________
          CALL FUNCTION 'ZMF_HOJAS_RUTAS_WP'
            STARTING NEW TASK lv_task
            CALLING me->set_proc_data_rec ON END OF TASK
            EXPORTING i_modo_vis        = i_modo_vis
                      iv_xsimu          = iv_xsimu
                      i_batch           = i_batch
                      iv_buffer         = iv_buffer
                      iv_nombre_archivo = go_intf_proc_hojas->gv_nombre_archivo1
                      iv_posicion       = lv_pos_ini
                      iv_module         = iv_module
                      iv_progname       = iv_progname
                      iv_country        = iv_country
            TABLES    it_plantilla1     = lt_pack_plantilla1         " Paquete de Registros para Crear Ordenes de Compra
                      it_plantilla2     = lt_pack_plantilla2
                      it_plantilla3     = lt_pack_plantilla3
                      it_plantilla4     = lt_pack_plantilla4
                      it_plantilla5     = lt_pack_plantilla5
                      it_plantilla6     = lt_pack_plantilla6
                      it_plantilla7     = lt_pack_plantilla7
                      it_mast           = lt_mast.

          " Espera que todas las Tareas concluyan
          WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_finalizado = abap_true.

          " Retornar todos los Mensajes de Error del Proceso
          APPEND LINES OF gt_bapiret2 TO et_bapiret2.

          " Reinicializar la Variable Controladora de Fin de la Tara
          CLEAR gv_finalizado.
        ELSE.

          " Ejecutar MF en linea Procesamiento Normal
          "____________________________________________________
          CALL FUNCTION 'ZMF_HOJAS_RUTAS_WP'
            EXPORTING i_modo_vis        = i_modo_vis
                      iv_xsimu          = iv_xsimu
                      i_batch           = i_batch
                      iv_buffer         = iv_buffer
                      iv_nombre_archivo = go_intf_proc_hojas->gv_nombre_archivo1
                      iv_posicion       = lv_pos_ini
                      iv_module         = iv_module
                      iv_progname       = iv_progname
                      iv_country        = iv_country
            TABLES    it_plantilla1     = lt_pack_plantilla1         " Paquete de Registros para Crear Ordenes de Compra
                      it_plantilla2     = lt_pack_plantilla2
                      it_plantilla3     = lt_pack_plantilla3
                      it_plantilla4     = lt_pack_plantilla4
                      it_plantilla5     = lt_pack_plantilla5
                      it_plantilla6     = lt_pack_plantilla6
                      it_plantilla7     = lt_pack_plantilla7
                      et_bapiret2       = lt_bapiret2
                      it_mast           = lt_mast.

          APPEND LINES OF lt_bapiret2 TO gt_bapiret2.
          DELETE ADJACENT DUPLICATES FROM gt_bapiret2 COMPARING ALL FIELDS.

        ENDIF.

        ENDDO.

        WAIT UNTIL gv_proc_enviados = 0.

        " Retornar todos los Mensajes de Error del Proceso
        et_bapiret2 = gt_bapiret2.

      CATCH cx_amdp_execution_failed INTO go_cx_root.
      CATCH cx_sql_exception INTO go_cx_root.
      CATCH cx_amdp_result_table_error INTO go_cx_root.
      CATCH cx_amdp_error INTO go_cx_root.
      CATCH cx_static_check INTO go_cx_root.

        RAISE EXCEPTION go_cx_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
