CLASS zcl_intf_proc_hojas DEFINITION
  PUBLIC
  INHERITING FROM zcl_manager_file_root
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      " Estructura p.Marcar los Parámetros
      BEGIN OF ty_parametros,
        iv_xsimu          TYPE xfeld,
        pmaxjobs          TYPE i,
        iv_buffer         TYPE xfeld,
        iv_cant_registros TYPE i,
        i_batch           TYPE syst_batch,
        i_modo            TYPE char1,
        i_modo_vis        TYPE char1,
        i_pnomtask        TYPE char11,
      END OF ty_parametros .
    TYPES:
      TT_headerdata TYPE STANDARD TABLE OF zintf_ty_cont_proc_hojas WITH DEFAULT KEY .

    DATA gv_cant_registros TYPE i .
    DATA gv_pmaxjobs TYPE i .
    DATA gt_data TYPE tt_headerdata .
    DATA gs_headerdata TYPE zintf_ty_cont_proc_hojas .
    DATA go_proc_hojas_wp TYPE REF TO zcl_intf_proc_hojas_wp .
    DATA gs_parametros TYPE ty_parametros .
    DATA go_cx_root TYPE REF TO cx_root .
    DATA go_mgr_file TYPE REF TO zcl_manager_file .
    DATA gv_module TYPE ufps_posid .
    DATA gv_progname TYPE progname .
    DATA gv_country TYPE land1 .
    DATA gt_data_ok_plantilla1 TYPE zttintf_plantilla1_standard .
    DATA gt_data_ok_plantilla2 TYPE zttintf_plantilla2_standard .
    DATA gt_data_ok_plantilla3 TYPE zttintf_plantilla3_standard .
    DATA gt_data_ok_plantilla4 TYPE zttintf_plantilla4_standard .
    DATA gt_data_ok_plantilla5 TYPE zttintf_plantilla5_standard .
    DATA gt_data_ok_plantilla6 TYPE zttintf_plantilla6_standard .
    DATA gt_data_ok_plantilla7 TYPE zttintf_plantilla7_standard .
    DATA gt_data_nok_plantilla1 TYPE zttintf_plantilla1_standard .
    DATA gt_data_nok_plantilla2 TYPE zttintf_plantilla2_standard .
    DATA gt_data_nok_plantilla3 TYPE zttintf_plantilla3_standard .
    DATA gt_data_nok_plantilla4 TYPE zttintf_plantilla4_standard .
    DATA gt_data_nok_plantilla5 TYPE zttintf_plantilla5_standard .
    DATA gt_data_nok_plantilla6 TYPE zttintf_plantilla6_standard .
    DATA gt_data_nok_plantilla7 TYPE zttintf_plantilla7_standard .
    DATA gt_bapiret2 TYPE bapiret2_tab .
    DATA go_intf_proc_hojas TYPE REF TO zcl_intf_proc_hojas .
    DATA:
      gt_nombre_archivo      TYPE TABLE OF file_name .
    DATA gv_nombre_archivo1 TYPE file_name .
    DATA go_monitor TYPE REF TO zcl_intf_monitor .
    DATA gc_nomb_interfaz TYPE zde_id_intefaz VALUE 'HOJAS_RUTA' ##NO_TEXT.
    CLASS-DATA mo_collector TYPE REF TO zcl_intf_proc_hojas .

    METHODS get_split_files
      IMPORTING
        !iv_id_carpeta     TYPE zde_id_carpeta OPTIONAL
        !iv_nombre_archivo TYPE file_name
        !iv_module         TYPE ufps_posid
        !iv_progname       TYPE progname
        !iv_country        TYPE land1 .
    METHODS set_parametros
      RETURNING
        VALUE(rs_parametros) TYPE ty_parametros .
    CLASS-METHODS get_data_maping
      IMPORTING
        !it_data             TYPE tt_headerdata
      RETURNING
        VALUE(rs_headerdata) TYPE cvis_ei_extern_t .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_collector) TYPE REF TO zcl_intf_proc_hojas .
    METHODS set_data_process_bapi
      IMPORTING
        !iv_xsimu          TYPE xfeld
        !pmaxjobs          TYPE i
        !iv_buffer         TYPE xfeld OPTIONAL
        !iv_cant_registros TYPE i OPTIONAL
        !i_batch           TYPE syst_batch OPTIONAL
        !i_modo            TYPE char1 OPTIONAL
        !i_modo_vis        TYPE char1
        !i_pnomtask        TYPE char11
        !it_data_s         TYPE ztt_headerdata_s OPTIONAL
      EXPORTING
        !et_bapiret2       TYPE bapiret2_tab
      RAISING
        cx_amdp_execution_failed
        cx_sql_exception
        cx_amdp_result_table_error
        cx_amdp_error
        cx_static_check .
    METHODS main_ctr
      IMPORTING
        !it_file_data TYPE any OPTIONAL
      RAISING
        cx_amdp_execution_failed
        cx_sql_exception
        cx_amdp_result_table_error
        cx_amdp_error
        cx_static_check .
    METHODS get_data_table
      IMPORTING
        !it_tabla         TYPE any
      RETURNING
        VALUE(rt_tabla_a) TYPE tt_headerdata .
    METHODS set_data_row
      CHANGING
        !ct_data TYPE zsintf_plantilla1 .

    METHODS read_file_interface
        REDEFINITION .
    METHODS set_split_files
        REDEFINITION .
    METHODS set_runtime RETURNING VALUE(rv_time) TYPE char200.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS lc_plantilla1_name TYPE string VALUE 'CABECERA.TXT'.
    CONSTANTS lc_plantilla2_name TYPE string VALUE 'MATERIAL.TXT'.
    CONSTANTS lc_plantilla3_name TYPE string VALUE 'SECUENCIAS.TXT'.
    CONSTANTS lc_plantilla4_name TYPE string VALUE 'OPERACIONES.TXT'.
    CONSTANTS lc_plantilla5_name TYPE string VALUE 'COMPONENTES.TXT'.
    CONSTANTS lc_plantilla6_name TYPE string VALUE 'MAF.TXT'.
    CONSTANTS lc_plantilla7_name TYPE string VALUE 'CARACTERISTICAS.TXT'.
    CONSTANTS lc_plantilla7_name_2 TYPE string VALUE 'CARACTERÍSTICAS.TXT'.
    DATA gv_time_start TYPE syst_uzeit.
    DATA gv_time_end   TYPE syst_uzeit.

ENDCLASS.



CLASS zcl_intf_proc_hojas IMPLEMENTATION.


  METHOD get_data_maping.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa: GET_DATA_MAPING                                  "
    "  Título           : Interfaz p.Carga HOJAS DE RUTA                     "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "                     de obtener los datos hacia la estructura de        "
    "                     entrada                                            "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"
  ENDMETHOD.


  METHOD get_data_table.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  GET_DATA_TABLE                                  "
    "  Título           : Interfaz p.Carga de HOJAS DE RUTA                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal        "
    "                     retornar los datos referenciados como un objeto de "
    "                     datos real para mapeo de la clase.                 "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    FIELD-SYMBOLS <fs_tabla> TYPE STANDARD TABLE.

    CHECK it_tabla IS NOT INITIAL.

    ASSIGN it_tabla->* TO <fs_tabla>.
    rt_tabla_a = CORRESPONDING #( <fs_tabla> ).
  ENDMETHOD.


  METHOD get_instance.
    " Instancia Patron Singleton
    IF mo_collector IS NOT BOUND.
      mo_collector = NEW zcl_intf_proc_hojas( ).
    ENDIF.

    " Retornando la Intancia Activa
    ro_collector = mo_collector.
  ENDMETHOD.


  METHOD main_ctr.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  MAIN_CTR                                        "
    "  Título           : Interfaz p.Carga de HOJAS DE RUTA                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar la carga de HOJAS DE RUTA                                    "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"
    DATA result TYPE c LENGTH 1.
    FREE gv_time_start.
    FREE gv_time_end.

    " Comienza medición de tiempo
    gv_time_start = SY-UZEIT.

    TRY.
        " Instanciar la Clase del Monitor
        go_monitor = zcl_intf_monitor=>get_instance( ).


        FREE gs_headerdata.

        IF sy-batch IS INITIAL.

          " El usuario decide si inicia el Proceso de Interface
          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              defaultoption  = 'N'
              textline1      = TEXT-001 " ¿Desea iniciar el proceso Interface Hojas de ruta ?
              titel          = TEXT-002 " Hojas de ruta
              cancel_display = 'X'
            IMPORTING
              answer         = result.

          IF result <> 'J'.

            " Proceso cancelado por el usuario.
            RAISE EXCEPTION NEW zcx_manager_file( textid = zcx_manager_file=>zcx_cancel_user ).

          ENDIF.

        ENDIF.

        " Instanciar la Clase de la Interfaz SOP012
        go_intf_proc_hojas = zcl_intf_proc_hojas=>get_instance( ).

        " Estableces los Parámetros del Proceso
        go_intf_proc_hojas->gs_parametros = set_parametros( ).

        " Instanciar los Datos como Atributo de Clase
        go_intf_proc_hojas->gt_data       = get_data_table( it_file_data ).

        " Instanciar la Clase Manejadora de Archivos
        go_mgr_file = zcl_manager_file=>instantiate( iv_module   = gv_module
                                                     iv_progname = gv_progname
                                                     iv_country  = gv_country ).

        " Leyendo Archivos, espere por favor...
        MESSAGE s010(zintf) INTO DATA(lv_mess_010).
        cl_progress_indicator=>progress_indicate( i_text               = lv_mess_010
                                                  i_processed          = sy-index
                                                  i_total              = 0
                                                  i_output_immediately = abap_true ).
        " Leer el Archivo del Servidor de Aplicaciones
        go_mgr_file->read( EXPORTING im_v_module          = gv_module
                                     im_v_progname        = gv_progname
                                     im_v_country         = gv_country
                           IMPORTING im_ti_contenido_file = DATA(lti_table_file_contents) ).

        " Lista de Archivos con sus Datos para ser Procesados
        LOOP AT lti_table_file_contents INTO DATA(lt_file_contents).
          " Lectura de los datos del archivo que estan en un String
          read_file_interface( im_v_module      = gv_module
                               im_v_progname    = gv_progname
                               im_v_country     = gv_country
                               it_file_contents = lt_file_contents ).

          IF lt_file_contents IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        " Procesando Archivo
        MESSAGE s035(zintf) INTO DATA(lv_mess_011).
        cl_progress_indicator=>progress_indicate( i_text               = lv_mess_011
                                                  i_processed          = sy-index
                                                  i_total              = 0
                                                  i_output_immediately = abap_true ).
        " Crear hojas de ruta con Paralelismo Asincrónico
        set_data_process_bapi( EXPORTING iv_xsimu          = gs_parametros-iv_xsimu
                                         pmaxjobs          = gs_parametros-pmaxjobs
                                         iv_buffer         = gs_parametros-iv_buffer
                                         iv_cant_registros = gs_parametros-iv_cant_registros
                                         i_batch           = gs_parametros-i_batch           " X = JOB / 'Vacio' = EN LINEA
                                         i_modo            = gs_parametros-i_modo            " MODO DE EJECUCIÓN NORMAL / WORK PROCESS
                                         i_modo_vis        = gs_parametros-i_modo_vis
                                         i_pnomtask        = gs_parametros-i_pnomtask
                               " TODO: variable is assigned but never used (ABAP cleaner)
                               IMPORTING et_bapiret2       = DATA(lt_bapiret2) ).

        " Liberar la Memoria e Inicializar la Tabla de Procesamiento
        FREE go_intf_proc_hojas->gt_data.

        " Eliminamos archivo de carpeta de entrada
        LOOP AT gt_nombre_archivo ASSIGNING FIELD-SYMBOL(<fs_nombre_archivo>).
          zcl_manager_file=>instantiate( )->copy_file( iv_id_carpeta = 'D'
                                                       im_v_filename = CONV #( <fs_nombre_archivo> )
                                                       im_t_content  = lt_file_contents ).
        ENDLOOP.

      CATCH cx_amdp_execution_failed.
      CATCH cx_sql_exception.
      CATCH cx_amdp_result_table_error.
      CATCH cx_amdp_error.
      CATCH cx_static_check.
    ENDTRY.

  ENDMETHOD.


  METHOD read_file_interface.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  READ_FILE_INTERFACE                             "
    "  Título           : Interfaz p.Carga de HOJAS DE RUTA                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar la lectura de los datos del archivo que estan en un string   "
    "  y convertirlos campo a campo de la estructura real de cada interface  "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Definición de Tablas Internas
    DATA lt_headerdata    TYPE zintf_tt_cont_proc_hojas.
    DATA lt_split         TYPE TABLE OF string.
    DATA lv_string        TYPE string.
    DATA lt_file_contents TYPE thcs_string.
    DATA lv_file TYPE  string.
    DATA ls_file_contents TYPE string.
    " Definir Objetos de Referencia
    DATA ref_it           TYPE REF TO data.
    DATA lr_split         TYPE REF TO string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA go_mgr_file      TYPE REF TO zcl_manager_file.
    DATA lo_regex         TYPE REF TO cl_abap_regex.
    DATA lo_matcher       TYPE REF TO cl_abap_matcher.
    DATA lv_pattern_fecha TYPE string VALUE '^\d{2}\.\d{2}\.\d{4}$'. " Expresión regular para validar DD.MM.YYYY
    DATA lv_contador TYPE syst_tabix VALUE 0.
    DATA lt_plantilla1 TYPE zttintf_plantilla1_standard.
    DATA lt_plantilla2 TYPE zttintf_plantilla2_standard.
    DATA lt_plantilla3 TYPE zttintf_plantilla3_standard.
    DATA lt_plantilla4 TYPE zttintf_plantilla4_standard.
    DATA lt_plantilla5 TYPE zttintf_plantilla5_standard.
    DATA lt_plantilla6 TYPE zttintf_plantilla6_standard.
    DATA lt_plantilla7 TYPE zttintf_plantilla7_standard.
    " Variables referenciadas o campos simbólicos para cargar los datos
    FIELD-SYMBOLS <fs_campo> TYPE any.
    FIELD-SYMBOLS <fs_wa>    TYPE any.
    FIELD-SYMBOLS <fs_row>   TYPE any.

    TRY.

        IF it_file_contents IS INITIAL.
          RETURN.
        ENDIF.
        " Instanciar la Clase Manejadora de la Interface
        go_intf_proc_hojas = zcl_intf_proc_hojas=>get_instance( ).

        " Estableces los Parámetros del Proceso
        go_intf_proc_hojas->gs_parametros = set_parametros( ).
        " Instanciar la Clase Manejadora de Archivos
        go_mgr_file = zcl_manager_file=>instantiate( iv_module   = me->gv_module
                                                     iv_progname = me->gv_progname
                                                     iv_country  = me->gv_country ).

        " Leer la Tabla Interna con el Contenido para extraer el Nombre del Archivo
        READ TABLE it_file_contents  INTO ls_file_contents INDEX 1.
        APPEND  ls_file_contents TO gt_nombre_archivo.

        IF to_upper( ls_file_contents ) = lc_plantilla1_name.
          CREATE DATA ref_it TYPE zsintf_plantilla1.
          gv_nombre_archivo1 = ls_file_contents.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla2_name.
          CREATE DATA ref_it TYPE zsintf_plantilla2.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla3_name.
          CREATE DATA ref_it TYPE zsintf_plantilla3.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla4_name.
          CREATE DATA ref_it TYPE zsintf_plantilla4.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla5_name.
          CREATE DATA ref_it TYPE zsintf_plantilla5.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla6_name.
          CREATE DATA ref_it TYPE zsintf_plantilla6.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla7_name OR to_upper( ls_file_contents ) = lc_plantilla7_name_2.
          CREATE DATA ref_it TYPE zsintf_plantilla7.
        ENDIF.

        " Desreferenciar del objeto de datos tabla interna al field symbol
        ASSIGN ref_it->* TO <fs_wa>.

        " Por defecto el sistema asigna como separador un tabulador (Caracter de Escape #)
        DATA(lv_separador) = cl_abap_char_utilities=>horizontal_tab.

        " Descartar la Fila del Nombre del Archivo y nombres de columnda
        lt_file_contents = it_file_contents.

        " Eliminamos lineas de titulo y cabeceras
        DELETE lt_file_contents INDEX 1.
        DELETE lt_file_contents INDEX 1.
        DELETE lt_file_contents INDEX 1.

        SORT lt_file_contents BY table_line.
        " Leer el Contenido Línea a Línea
        LOOP AT lt_file_contents ASSIGNING FIELD-SYMBOL(<fs_data>).

          " Quitar tabulaciones y lineas vacias
          lv_file = <fs_data>.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_file WITH space.
          lv_file = condense( lv_file ).
          IF lv_file IS INITIAL.
            CONTINUE.
          ENDIF.

          lv_contador += 1.

          " Vaciar el Contenido de la Línea con el Caracter de Escape "#" al final
          lv_string = |{ <fs_data> }{ lv_separador }|.

          " Parsear cada Campo a una Tabla Interna
          SPLIT lv_string AT lv_separador INTO TABLE lt_split.

          " Leer los Campos ya Parseados de la Línea del Archivo
          LOOP AT lt_split REFERENCE INTO lr_split.

            DATA(lv_sytabix) = sy-tabix.

            " Leer Campo a Campo y asignar los Valores a cada uno de los Campos de la Estructura
            ASSIGN COMPONENT lv_sytabix OF STRUCTURE <fs_wa> TO <fs_campo>.
            IF sy-subrc = 0.
              " Reemplazamos las comas por puntos del campo COMP_QTY de la componentes y LOT_SZ_MAX de plantilla secuencia
              IF ( to_upper( ls_file_contents ) = lc_plantilla5_name AND lv_sytabix = 20 ) OR
                 ( to_upper( ls_file_contents ) = lc_plantilla3_name AND lv_sytabix = 12 ).
                REPLACE '.' WITH '' INTO lr_split->*.
                REPLACE ',' WITH '.' INTO lr_split->*.
                CONDENSE lr_split->* NO-GAPS.
              ENDIF.

              "Reemplazamos caracter de comillas dobles " por vacio
              IF  to_upper( ls_file_contents ) = lc_plantilla3_name OR
                  to_upper( ls_file_contents ) = lc_plantilla5_name OR
                  to_upper( ls_file_contents ) = lc_plantilla7_name OR
                  to_upper( ls_file_contents ) = lc_plantilla7_name_2.

                REPLACE ALL OCCURRENCES OF '"' IN lr_split->* WITH ''.

              ENDIF.

              " Crear objeto regex para Cambiar formato a campos de fecha
              lo_regex = NEW #( pattern = lv_pattern_fecha ).
              " Crear objeto matcher para comprobar la coincidencia

              IF strlen( lr_split->* ) >= 10.

                DATA(lv_fecha) = lr_split->*+0(10).

                lo_matcher = lo_regex->create_matcher( text = lv_fecha ).
                IF lo_matcher->match( ).
                  lr_split->* = lr_split->*+6(4) && lr_split->*+3(2) && lr_split->*+0(2).
                ENDIF.

              ENDIF.


              <fs_campo> = lr_split->*.


            ENDIF.

            ASSIGN COMPONENT 'ROW' OF STRUCTURE <fs_wa> TO <fs_row>.
            IF sy-subrc = 0.
              <fs_row> = lv_contador.
            ENDIF.

          ENDLOOP.

          IF to_upper( ls_file_contents ) = lc_plantilla1_name.
            APPEND <fs_wa> TO lt_plantilla1.
            DELETE ADJACENT DUPLICATES FROM lt_plantilla1 COMPARING task_list_group group_counter.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla2_name.
            APPEND <fs_wa> TO lt_plantilla2.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla3_name.
            APPEND <fs_wa> TO lt_plantilla3.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla4_name.
            APPEND <fs_wa> TO lt_plantilla4.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla5_name.
            APPEND <fs_wa> TO lt_plantilla5.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla6_name.
            APPEND <fs_wa> TO lt_plantilla6.
          ELSEIF to_upper( ls_file_contents ) = lc_plantilla7_name OR to_upper( ls_file_contents ) = lc_plantilla7_name_2.
            APPEND <fs_wa> TO lt_plantilla7.
          ENDIF.

        ENDLOOP.

        IF to_upper( ls_file_contents ) = lc_plantilla1_name.
          SORT lt_plantilla1 BY task_list_group group_counter.
          gs_headerdata-plantilla1 = lt_plantilla1.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla2_name.
          SORT lt_plantilla2 BY task_list_group group_counter material.
          gs_headerdata-plantilla2 = lt_plantilla2.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla3_name.
          SORT lt_plantilla3 BY task_list_group group_counter sequence_no.
          gs_headerdata-plantilla3 = lt_plantilla3.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla4_name.
          SORT lt_plantilla4 BY task_list_group group_counter sequence_no activity.
          gs_headerdata-plantilla4 = lt_plantilla4.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla5_name.
          SORT lt_plantilla5 BY task_list_group group_counter sequence_no activity.
          gs_headerdata-plantilla5 = lt_plantilla5.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla6_name.
          SORT lt_plantilla6 BY task_list_group group_counter sequence_no activity.
          gs_headerdata-plantilla6 = lt_plantilla6.
        ELSEIF to_upper( ls_file_contents ) = lc_plantilla7_name OR to_upper( ls_file_contents ) = lc_plantilla7_name_2.
          SORT lt_plantilla7 BY task_list_group group_counter sequence_no activity inspchar.
          gs_headerdata-plantilla7 = lt_plantilla7.
        ENDIF.

        APPEND gs_headerdata TO lt_headerdata.

        " Tabla Interna del Proceso Principal de la Interface
        go_intf_proc_hojas->gt_data = CORRESPONDING #( lt_headerdata ).

        " Asigna un Número de Fila a la Tabla p.Controlar la Mensajería de Errores
*        set_data_row( CHANGING ct_data = go_intf_proc_hojas->gt_data ).

        " Evalua la Condición para saber como mover el Archivo Especificamente
        DATA(lv_error) = COND #( WHEN sy-subrc = 0 THEN abap_false ELSE abap_true ).

        " Mover el Archivo Plano de las Carpetas de ERRORES
        IF lv_error IS NOT INITIAL.
          zcl_manager_file=>instantiate( )->move_file( im_v_existe_error = lv_error
                                                       im_v_filename     = CONV #( ls_file_contents )
                                                       im_t_content      = it_file_contents ).
        ENDIF.
        CLEAR lv_error.

      CATCH cx_root INTO DATA(lo_error).

        DATA(lv_mensaje2) = |{ lo_error->get_text( ) }, Plantilla: { ls_file_contents }, Columna: { lv_sytabix }|.
        MESSAGE lv_mensaje2 TYPE 'E'.

    ENDTRY.
  ENDMETHOD.

  METHOD set_data_process_bapi.
    " TODO: parameter IV_BUFFER is never used (ABAP cleaner)

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa: SET_DATA_PROCESS_BAPI                            "
    "  Título           : Interfaz p Carga de hojas de ruta                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: ENE-10-2025                                        "
    "  Descripción      :                                                    "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    TRY.

        " Instanciar la Clase Manejadora del Procesamiento Asincrónico
        go_proc_hojas_wp = zcl_intf_proc_hojas_wp=>get_instance( ).

        " Inicializar Tablas de Proceso
        FREE: go_proc_hojas_wp->gt_bapiret2,
              me->gt_data_ok_plantilla1,
              me->gt_data_nok_plantilla1.

        go_proc_hojas_wp->set_start_wp( EXPORTING iv_xsimu          = iv_xsimu                " Modo Test/Real
                                                  pmaxjobs          = pmaxjobs                " Cantidad Máxima de WorkProcess
                                                  iv_cant_registros = iv_cant_registros       " Cantidad de Registros por WorkProcess
                                                  i_batch           = i_batch                 " Campo de sistema ABAP: Procesamiento de fondo activo
                                                  i_modo            = i_modo                  " Modo Asincrónico o Sincrónico
                                                  i_modo_vis        = i_modo_vis              " Modo Visualización Batch Input
                                                  i_pnomtask        = i_pnomtask              " Nombre del WorkProcess
                                                  it_data_s         = go_intf_proc_hojas->gt_data " Tabla con los Registros a Procesar
                                                  iv_module         = gv_module
                                                  iv_progname       = gv_progname
                                                  iv_country        = gv_country
                                        IMPORTING et_bapiret2       = et_bapiret2 ).          " Mensajes de error

        IF    go_proc_hojas_wp->gt_bapiret2   IS NOT INITIAL
           OR go_intf_proc_hojas->gt_bapiret2 IS NOT INITIAL.

          " Copia Archivos en las Carpetas PROCESADOS y ERRORES
          LOOP AT gt_nombre_archivo ASSIGNING FIELD-SYMBOL(<fs_nombre_archivo>).
            get_split_files( iv_nombre_archivo = <fs_nombre_archivo>
                             iv_module         = gv_module
                             iv_progname       = gv_progname
                             iv_country        = gv_country ).

          ENDLOOP.

        ENDIF.

        " Terminar la medición de tiempo
        gv_time_end = SY-UZEIT.

        " Registrar los Errores o Aciertos en el Monitor de la Interface
        gv_nombre_archivo1 = gv_nombre_archivo1 &&
         | { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
        "         | { sy-uzeit+0(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }|.

        go_monitor->set_registers_multiple_errors( iv_id_intefaz     = gc_nomb_interfaz  " Nombre de la Interface
                                                   iv_nombre_archivo = gv_nombre_archivo1 " Nombre del fichero
                                                   it_bapiret2_tab   = go_proc_hojas_wp->gt_bapiret2
                                                   iv_comentarios    = set_runtime( ) ). " Tabla con Mensajes de error

      CATCH cx_amdp_execution_failed INTO go_cx_root.
      CATCH cx_sql_exception INTO go_cx_root.
      CATCH cx_amdp_result_table_error INTO go_cx_root.
      CATCH cx_amdp_error INTO go_cx_root.
      CATCH cx_static_check INTO go_cx_root.

        RAISE EXCEPTION go_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD set_parametros.
    gv_module = 'INTF'.
    gv_progname = 'ZHOJAS_RUTA'.
    gv_country  = 'CO'.


    gv_pmaxjobs = '12'.
    gv_cant_registros = '50'.

    gs_parametros-iv_xsimu          = abap_true.
    gs_parametros-pmaxjobs          = gv_pmaxjobs.
    gs_parametros-iv_buffer         = abap_false.
    gs_parametros-iv_cant_registros = gv_cant_registros.
    gs_parametros-i_batch           = ' '.          " X = JOB / 'Vacio' = EN LINEA
    gs_parametros-i_modo            = abap_true.    " MODO DE EJECUCIÓN NORMAL / WORK PROCESS
    gs_parametros-i_modo_vis        = abap_false.
    gs_parametros-i_pnomtask        = gv_progname.

    " Retornar los Parámetros
    rs_parametros = gs_parametros.
  ENDMETHOD.


  METHOD set_split_files.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  SET_SPLIT_FILES                                 "
    "  Título           : Interfaz p.Carga de HOJAS DE RUTA                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar una copia de un archivo que según el proceso puede contener  "
    "  errores con lo cual se copia a la carpeta de Errores, o puede no tener"
    "  errores en ese caso se copia a la parpeta de Procesados.              "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    CHECK it_data_hojas_ruta IS NOT INITIAL.

    " Divide Archivos en las Carpetas PROCESADOS y ERRORES
    " Definir Objetos de Referencia
    DATA: ref_it        TYPE REF TO data,
          go_proc_hojas TYPE REF TO zcl_intf_proc_hojas,
          go_mgr_file   TYPE REF TO zcl_manager_file,
          lr_split      TYPE REF TO char40.

    DATA: lt_data    TYPE zttintf_plantilla1_standard,
          lt_content TYPE thcs_string..

    " Variables referenciadas o campos simbólicos para cargar los datos
    FIELD-SYMBOLS: <fs_campo> TYPE any,
                   <fs_wa>    TYPE any,
                   <fs_it>    TYPE ANY TABLE.

    DATA: lv_code_page TYPE cpcodepage,
          lv_string    TYPE string.


    " Transfiriendo los Datos a la Tabla Interna de Procesamiento
*    lt_data = CORRESPONDING #( it_data_hojas_ruta ).

    " Instanciar el objeto de datos tabla interna con referencia a la Estructura de la Interfaz
    CREATE DATA ref_it TYPE zintf_ty_cont_proc_hojas.

    " Desreferenciar del objeto de datos tabla interna al field symbol
    ASSIGN ref_it->* TO <fs_wa>.

    " Por defecto el sistema asigna como separador un tabulador (Caracter de Escape #)
    DATA(lv_separador) = cl_abap_char_utilities=>horizontal_tab.

    " Leer cada Registro Completo del Archivo
    LOOP AT it_data_hojas_ruta ASSIGNING <fs_wa>.

      DO.

        DATA(lv_sytabix) = sy-index.

        " Leer Campo a Campo los Datos y asignarlos en la Cadena String con el Separador
        ASSIGN COMPONENT lv_sytabix OF STRUCTURE <fs_wa> TO <fs_campo>.
        IF sy-subrc = 0.

          " Asignar los Valores a cada uno de los Campos de la Estructura
          lv_string = | { lv_string }{ <fs_campo> }{ lv_separador }|.
        ELSE.

          " Eliminar los Espacios Innecesarios
          CONDENSE lv_string NO-GAPS.

          " Poblar Tabla Interna con los Datos Anónimos
          APPEND lv_string TO lt_content.

          CLEAR: lv_string,
                 lv_sytabix.

          EXIT.
        ENDIF.

      ENDDO.
    ENDLOOP.

    " Instanciar la Clase Manejadora de Archivos con los Parámetros de la Interfaz
    go_mgr_file = zcl_manager_file=>instantiate( EXPORTING iv_module   = iv_module
                                                           iv_progname = iv_progname
                                                           iv_country  = iv_country ).

    " Tomar la Decisión a que Carpeta guarda el Archivo
    IF iv_id_carpeta = gc_id_carpeta_p .

      " Mover el Archivo Plano de las Carpetas PROCESADOS(P)
      go_mgr_file->copy_file( iv_id_carpeta = gc_id_carpeta_p
                              im_v_filename = iv_filename
                              im_t_content  = lt_content ).

    ELSEIF iv_id_carpeta = gc_id_carpeta_e .

      " Mover el Archivo Plano de las Carpetas ERRORES(E)
      go_mgr_file->copy_file( iv_id_carpeta = gc_id_carpeta_e
                              im_v_filename = iv_filename
                              im_t_content  = lt_content ).

    ENDIF.
  ENDMETHOD.


  METHOD set_data_row.
*    DATA lv_contador TYPE int4.
*
*    " Númerar cada Registro
*    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_row>).
*      lv_contador  += 1.
*      <fs_row>-zrow = lv_contador.
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_split_files.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  SET_SPLIT_FILES                                 "
    "  Título           : Interfaz p.Carga de HOJAS DE RUTA                  "
    "  País             : Republica Colombia                                 "
    "  Autor            : Luis Meneses                                       "
    "  Fecha de creación: FEB-28-2025                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar una copia de un archivo que según el proceso puede contener  "
    "  errores con lo cual se copia a la carpeta de Errores, o puede no tener"
    "  errores en ese caso se copia a la parpeta de Procesados.              "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"
    FIELD-SYMBOLS: <fs_data_ok> TYPE ANY TABLE.
    FIELD-SYMBOLS: <fs_data_nok> TYPE ANY TABLE.

    IF to_upper( iv_nombre_archivo ) EQ lc_plantilla1_name.
      ASSIGN me->gt_data_ok_plantilla1 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla1 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla1.
      <fs_data_nok> = gt_data_nok_plantilla1.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla2_name.
      ASSIGN me->gt_data_ok_plantilla2 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla2 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla2.
      <fs_data_nok> = gt_data_nok_plantilla2.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla3_name.
      ASSIGN me->gt_data_ok_plantilla3 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla3 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla3.
      <fs_data_nok> = gt_data_nok_plantilla3.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla4_name.
      ASSIGN me->gt_data_ok_plantilla4 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla4 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla4.
      <fs_data_nok> = gt_data_nok_plantilla4.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla5_name.
      ASSIGN me->gt_data_ok_plantilla5 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla5 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla5.
      <fs_data_nok> = gt_data_nok_plantilla5.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla6_name.
      ASSIGN me->gt_data_ok_plantilla6 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla6 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla6.
      <fs_data_nok> = gt_data_nok_plantilla6.
    ELSEIF to_upper( iv_nombre_archivo ) EQ lc_plantilla7_name OR to_upper( iv_nombre_archivo ) EQ lc_plantilla7_name_2.
      ASSIGN me->gt_data_ok_plantilla7 TO <fs_data_ok>.
      ASSIGN me->gt_data_Nok_plantilla7 TO <fs_data_nok>.
      <fs_data_ok> = gt_data_ok_plantilla7.
      <fs_data_nok> = gt_data_nok_plantilla7.
    ELSE.
      RETURN.
    ENDIF.

    CHECK <fs_data_ok> IS NOT INITIAL OR
          <fs_data_nok> IS NOT INITIAL.
    " Copiar el Archivo con los Datos OK en la Carpeta PROCESADOS
    go_intf_proc_hojas->set_split_files( EXPORTING iv_id_carpeta = me->gc_id_carpeta_p
                                               iv_filename   = CONV #( iv_nombre_archivo )
                                               iv_module     = iv_module
                                               iv_progname   = iv_progname
                                               iv_country    = iv_country
                                               it_data_hojas_ruta    = <fs_data_ok> ).

    " Copiar el Archivo con los Datos con Error en la Carpeta ERRORES
    go_intf_proc_hojas->set_split_files( EXPORTING iv_id_carpeta = me->gc_id_carpeta_e
                                               iv_filename   = CONV #( iv_nombre_archivo )
                                               iv_module     = iv_module
                                               iv_progname   = iv_progname
                                               iv_country    = iv_country
                                               it_data_hojas_ruta   = <fs_data_nok> ).
  ENDMETHOD.

  METHOD set_runtime.
    DATA lv_minutes_start TYPE i.
    DATA lv_minutes_end   TYPE i.
    DATA lv_minutes       TYPE i.
    DATA lv_hours_start   TYPE i.
    DATA lv_hours_end     TYPE i.
    DATA lv_hours         TYPE i.
    DATA lv_h_char        TYPE c LENGTH 2.
    DATA lv_m_char        TYPE c LENGTH 2.

    " Calcular la diferencia en horas
    lv_hours_start = gv_time_start+0(2).
    lv_hours_end = gv_time_end+0(2).
    lv_hours     = lv_hours_end - lv_hours_start.

    " Calcular la diferencia en Minutos
    lv_minutes_start = gv_time_start+2(2).
    lv_minutes_end   = gv_time_end+2(2).
    lv_minutes       = lv_minutes_end - lv_minutes_start.

    IF lv_minutes < 0.
      lv_hours -= 1.
      lv_minutes = lv_minutes + 60.
    ENDIF.

    " Convertir a caracteres con ceros a la izquierda
    lv_h_char = lv_hours.
    lv_m_char = lv_minutes.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = lv_h_char
      IMPORTING output = lv_h_char.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = lv_m_char
      IMPORTING output = lv_m_char.

    " Mostrar resultado
     rv_time = |Tiempo de ejecución: { lv_hours }h { lv_minutes }min|.

  ENDMETHOD.

ENDCLASS.
