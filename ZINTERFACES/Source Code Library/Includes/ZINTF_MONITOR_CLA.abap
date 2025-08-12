    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR_CLA                               "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar el Monitorear los errores de los procesos de carga.          "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"
    CLASS lcl_handle_action DEFINITION.

      PUBLIC SECTION.

        DATA: go_ida      TYPE REF TO if_salv_gui_table_ida,
              go_ida_item TYPE REF TO if_salv_gui_table_ida.

        METHODS constructor IMPORTING io_ida TYPE REF TO if_salv_gui_table_ida.

        METHODS handle_dbclick            FOR EVENT double_click      OF if_salv_gui_table_display_opt .
        METHODS handle_action             FOR EVENT function_selected OF if_salv_gui_toolbar_ida IMPORTING ev_fcode.
*        METHODS handle_refresh            FOR EVENT function_selected OF if_salv_gui_toolbar_ida IMPORTING ev_fcode.


    ENDCLASS.
    CLASS lcl_intf_monitor DEFINITION.

      PUBLIC SECTION.

        " Definición de Variables Globales
        DATA: gv_tabname  TYPE tabname16,
              gv_progname TYPE  progname,
              gv_module   TYPE  ufps_posid VALUE 'INTF'.

        " Metodos Estáticos
        CLASS-METHODS get_instance
          RETURNING
            VALUE(r_instance) TYPE REF TO lcl_intf_monitor .

        METHODS display_alv_1.
        METHODS display_alv_2.
        METHODS get_user.
        METHODS set_module_pai.

        METHODS set_sort        IMPORTING iv_tabname             TYPE tabname16
                                CHANGING  VALUE(ch_mo_alv_ida_1) TYPE REF TO if_salv_gui_table_ida OPTIONAL
                                          VALUE(ch_mo_alv_ida_2) TYPE REF TO if_salv_gui_table_ida OPTIONAL.

        METHODS set_doble_clic  IMPORTING iv_tabname             TYPE tabname16
                                EXPORTING eo_lo_handler          TYPE REF TO lcl_handle_action
                                CHANGING  VALUE(ch_mo_alv_ida_1) TYPE REF TO if_salv_gui_table_ida OPTIONAL
                                          VALUE(ch_mo_alv_ida_2) TYPE REF TO if_salv_gui_table_ida OPTIONAL.

        METHODS set_crea_botones IMPORTING iv_tabname             TYPE tabname16 OPTIONAL
                                 EXPORTING eo_lo_handler          TYPE REF TO lcl_handle_action
                                 CHANGING  VALUE(ch_mo_alv_ida_1) TYPE REF TO if_salv_gui_table_ida OPTIONAL
                                           VALUE(ch_mo_alv_ida_2) TYPE REF TO if_salv_gui_table_ida OPTIONAL.

        METHODS set_layout IMPORTING iv_tabname             TYPE tabname16
                           CHANGING  VALUE(ch_mo_alv_ida_1) TYPE REF TO if_salv_gui_table_ida OPTIONAL
                                     VALUE(ch_mo_alv_ida_2) TYPE REF TO if_salv_gui_table_ida OPTIONAL.

        METHODS set_titulos IMPORTING iv_titulo              TYPE cua_tit_tx
                                      iv_tabname             TYPE tabname16
                            CHANGING  VALUE(ch_mo_alv_ida_1) TYPE REF TO if_salv_gui_table_ida         OPTIONAL
                                      VALUE(ch_mo_alv_ida_2) TYPE REF TO if_salv_gui_table_ida         OPTIONAL
                                      VALUE(ch_go_alv_1_opt) TYPE REF TO if_salv_gui_table_display_opt OPTIONAL
                                      VALUE(ch_go_alv_2_opt) TYPE REF TO if_salv_gui_table_display_opt OPTIONAL.

        " Modifica o Actualiza los Titulos de los Campos del ALV-IDA
        METHODS modify_labels IMPORTING io_alv_ida TYPE REF TO if_salv_gui_table_ida OPTIONAL
                                        iv_tabname TYPE tabname16 OPTIONAL .
        METHODS get_id_proceso RETURNING VALUE(rt_id_proceso) TYPE zintf_tt_id_proceso.

        METHODS get_metodo_controler RETURNING VALUE(rv_metodo) TYPE seocmpname.


      PRIVATE SECTION.

        CLASS-DATA instance TYPE REF TO lcl_intf_monitor.



    ENDCLASS.
    CLASS lcl_intf_monitor IMPLEMENTATION.

      METHOD get_instance.

        " Ejecuta Patrón Singleton de Instanciación
        IF instance IS NOT BOUND.
          instance = NEW #( ).
        ENDIF.

        r_instance = instance.

      ENDMETHOD.

      METHOD get_user.

        " Obtener Nombre del Usuario Final
        SELECT SINGLE name_text INTO @DATA(user)
          FROM user_addrp
         WHERE bname = @sy-uname.

        " Asignar Nombre de Usuario Final al Titulo del Monitor
        IF user IS NOT INITIAL.
          user = |- { user }|.
          SET TITLEBAR 'TIT_MONITOR' WITH user.
        ELSE.
          SET TITLEBAR 'TIT_MONITOR'.
        ENDIF.

      ENDMETHOD.

      METHOD set_module_pai.

        DATA(lv_okcode) = g_okcode.

        CLEAR g_okcode.

        CASE lv_okcode.
          WHEN zintf_con_exit   OR
               zintf_con_exit_% OR
               zintf_con_back   OR
               zintf_con_canc   OR
               zintf_con_canc_%.

*            mo_alv_ida_1->free( ).
*            mo_alv_ida_2->free( ).

            SET SCREEN 0.
            LEAVE SCREEN.
        ENDCASE.

      ENDMETHOD.

      METHOD display_alv_1.

        "Definición de Tablas Internas
        DATA: lr_suname        TYPE uname_range_tab,
              lt_nombres_campo TYPE zintf_tt_nombres_campo,
              lt_sort          TYPE if_salv_gui_types_ida=>yt_sort_rule.

        " Definición de Word Areas
        DATA ls_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key.

        CHECK cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = 'ZTINTF_MONIT_CAB').

        CHECK go_container_100 IS NOT BOUND.

        TRY.

            " Instanciar Contenedor del ALV
            go_container_100 = NEW #( 'D0100_CONTAINER' ).

            " Asigna Nombre de Tabla Transparente
            gv_tabname = 'ZTINTF_MONIT_CAB'.

            me->gv_progname = VALUE #( so_intf[ 1 ]-low DEFAULT abap_false ).

            " Instancia la Clase Manejadora del ALV-IDA con el Nombre de Tabla Transparente
            " y la Instancia de un Contenedor para el ALV-IDA de Cabecera
            mo_alv_ida_1 = cl_salv_gui_table_ida=>create( iv_table_name    = CONV #( gv_tabname )
                                                          io_gui_container = go_container_100 ).



            " Instanciar Clase para Activar Configuración de los Titulos
            me->set_titulos( EXPORTING iv_titulo      = 'Cabecera Monitor'
                                       iv_tabname     = gv_tabname
                             CHANGING ch_mo_alv_ida_1 = mo_alv_ida_1
                                      ch_go_alv_1_opt = go_alv_1_opt ).

            " SORT: Establecer Orden de Clasificación
            me->set_sort( EXPORTING iv_tabname     = gv_tabname
                          CHANGING ch_mo_alv_ida_1 = mo_alv_ida_1 ).

            " DOBLE CLIC: Habilitar el Evento Doble Click
            me->set_doble_clic( EXPORTING iv_tabname      = gv_tabname
                                IMPORTING eo_lo_handler   = DATA(lo_handler)
                                CHANGING  ch_mo_alv_ida_1 = mo_alv_ida_1 ).

            " CREAR BOTONES: Adicionar el Boton EJECUTAR en la Barra de Botonerías
            me->set_crea_botones( EXPORTING iv_tabname      = gv_tabname
                                  IMPORTING eo_lo_handler   = lo_handler
                                  CHANGING  ch_mo_alv_ida_1 = mo_alv_ida_1 ).

            " LAYOUT: Habilitar la Caja ListBox de los Layout
            me->set_layout( EXPORTING iv_tabname     = gv_tabname
                            CHANGING ch_mo_alv_ida_1 = mo_alv_ida_1 ).

            " Modifica o Actualiza los Titulos de los Campos del ALV-IDA
            me->modify_labels( EXPORTING io_alv_ida = mo_alv_ida_1
                                         iv_tabname = gv_tabname ).

            " Mapear los Campos a EXCLUIR Catalogo del ALV
            FREE lt_nombres_campo.
            lt_nombres_campo = VALUE #( ( nombre_campo = 'ID_PROCESO' ) ).

            " Excluir Campos del Catalogo del ALV-IDA
            go_intf_monitor->get_excluir_campos( EXPORTING io_alv_ida       = mo_alv_ida_1
                                                           it_nombres_campo = lt_nombres_campo ).


            "Arma un Range del Usuario que Genera el Reporte para usarlo Automáticamente como Filtro
            lr_suname = VALUE uname_range_tab( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

            "Filtrar el ALV dados los Criterios de Selección
            go_intf_monitor->display_with_condition_range( EXPORTING io_alv_ida   = mo_alv_ida_1 " DownCast de la Instancia
                                                                     ir_uname     = lr_suname    " Usuario
                                                                     ir_intf_name = so_intf[]    " Nombre de la Interfaz
                                                                     ir_nom_arch  = so_noma[] ). " Nombre del Archivo Plano

          CATCH cx_root INTO DATA(lo_root) .

            DATA(lv_message) = lo_root->get_longtext( ).
        ENDTRY.


      ENDMETHOD.

      METHOD display_alv_2.

        "Definición de Tablas Internas
        DATA: lr_suname        TYPE uname_range_tab,
              lt_nombres_campo TYPE zintf_tt_nombres_campo,
              lt_sort          TYPE if_salv_gui_types_ida=>yt_sort_rule.


        " Definición de Word Areas
        DATA ls_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key.

        " Asigna Nombre de Tabla Transparente p.Las Posiciones del Monitor
        gv_tabname = 'ZTINTF_MONIT_DET'.

        CHECK cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = CONV #( gv_tabname ) ).

        CHECK go_container_200 IS NOT BOUND.

        TRY.

            " Instanciar Contenedor del ALV
            go_container_200 = NEW #( 'D0200_CONTAINER' ).

            " Instancia la Clase Manejadora del ALV-IDA con el Nombre de Tabla Transparente
            " y la Instancia de un Contenedor para el ALV-IDA de Posición
            mo_alv_ida_2 = cl_salv_gui_table_ida=>create( iv_table_name    = CONV #( gv_tabname )
                                                          io_gui_container = go_container_200 ).



            " Instanciar Clase para Activar Configuración de los Titulos
            me->set_titulos( EXPORTING iv_titulo      = 'Posiciones del Monitor'
                                       iv_tabname     = gv_tabname
                             CHANGING ch_mo_alv_ida_2 = mo_alv_ida_2
                                      ch_go_alv_2_opt = go_alv_2_opt ).


            " SORT: Establecer orden de clasificación por Cliente
            me->set_sort( EXPORTING iv_tabname      = gv_tabname
                          CHANGING  ch_mo_alv_ida_2 = mo_alv_ida_2 ).



            " DOBLE CLIC: Habilitar el Evento Doble Click
            me->set_doble_clic( EXPORTING iv_tabname      = gv_tabname
                                IMPORTING eo_lo_handler   = DATA(lo_handler)
                                CHANGING  ch_mo_alv_ida_2 = mo_alv_ida_2 ).



            " LAYOUT: Definir la Llave Persistente del Layout
            me->set_layout( EXPORTING iv_tabname     = gv_tabname
                            CHANGING ch_mo_alv_ida_2 = mo_alv_ida_2 ).


            " Modifica o Actualiza los Titulos de los Campos del ALV-IDA
            me->modify_labels( EXPORTING io_alv_ida = mo_alv_ida_2
                                         iv_tabname = gv_tabname ).


            " Mapear los Campos a Excluir Catalogo del ALV
            FREE lt_nombres_campo.
            lt_nombres_campo = VALUE #( ( nombre_campo = 'ID_PROCESO' )
                                        ( nombre_campo = 'GUID_ERROR' ) ).

            " Excluir Campos del Catalogo del ALV-IDA
            go_intf_monitor->get_excluir_campos( EXPORTING io_alv_ida       = mo_alv_ida_2
                                                           it_nombres_campo = lt_nombres_campo ).




            "Arma un Range del Usuario que Genera el Reporte para usarlo Automáticamente como Filtro
            lr_suname = VALUE uname_range_tab( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

            " Filtrar el ALV dados los Criterios de Selección
            go_intf_monitor->display_with_condition_range( EXPORTING io_alv_ida    = mo_alv_ida_2             " DownCast de la Instancia
                                                                     ir_intf_name  = so_intf[]                " Nombre de la Interfaz
                                                                     ir_id_proceso = me->get_id_proceso( ) ). " Id del Proceso



          CATCH cx_root INTO DATA(lo_root) .

            DATA(lv_message) = lo_root->get_longtext( ).
        ENDTRY.

      ENDMETHOD.

      METHOD set_sort.

        DATA lt_sort TYPE if_salv_gui_types_ida=>yt_sort_rule.

        " SORT: Establecer Orden de Clasificación
        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            " SORT: Establecer orden de clasificación por el Nombre de la Interfaz
            INSERT VALUE #( field_name = 'ID_INTERFAZ'
                            descending = abap_false
                            is_grouped = abap_false ) INTO TABLE lt_sort.

            " SORT: Establecer orden de clasificación por Fecha de Creación Descendente
            INSERT VALUE #( field_name = 'FECH_CREACION'
                            descending = abap_true
                            is_grouped = abap_false ) INTO TABLE lt_sort.

            ch_mo_alv_ida_1->default_layout( )->set_sort_order( EXPORTING it_sort_order = lt_sort ).

          WHEN 'ZTINTF_MONIT_DET'.

            " SORT: Establecer orden de clasificación por el Nombre de la Interfaz
            INSERT VALUE #( field_name = 'ID_INTERFAZ'
                            descending = abap_true
                            is_grouped = abap_false ) INTO TABLE lt_sort.

            " SORT: Establecer orden de clasificación por Id del Proceso Descendente
            INSERT VALUE #( field_name = 'ID_PROCESO'
                            descending = abap_true
                            is_grouped = abap_false ) INTO TABLE lt_sort.

            ch_mo_alv_ida_2->default_layout( )->set_sort_order( EXPORTING it_sort_order = lt_sort ).

          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD set_doble_clic.

        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            " DOBLE CLIC: Habilitar el Evento Doble Click
            ch_mo_alv_ida_1->display_options( )->enable_double_click( ).

            " DOBLE CLIC: Habilitar el Modo de Selección
            ch_mo_alv_ida_1->selection( )->set_selection_mode( EXPORTING iv_mode = 'SINGLE' ).

            " EVENTOS: Instanciar los Eventos
            " EVENTOS: Instanciar la Clase del Evento Manejador del Boton 'Consultar Nro.FO'
            eo_lo_handler = NEW lcl_handle_action( io_ida = ch_mo_alv_ida_1 ).

            " EVENTOS: Instanciar la Clase del Evento Manejador del Boton 'Doble Click'
            SET HANDLER eo_lo_handler->handle_dbclick FOR ch_mo_alv_ida_1->display_options( ).

          WHEN 'ZTINTF_MONIT_DET'.

            " DOBLE CLIC: Habilitar el Evento Doble Click
            ch_mo_alv_ida_2->display_options( )->enable_double_click( ).

            " DOBLE CLIC: Habilitar el Modo de Selección
            ch_mo_alv_ida_2->selection( )->set_selection_mode( EXPORTING iv_mode = 'SINGLE' ).

            " EVENTOS: Instanciar los Eventos
            " EVENTOS: Instanciar la Clase del Evento Manejador del Boton 'Consultar Nro.FO'
            eo_lo_handler = NEW lcl_handle_action( io_ida = ch_mo_alv_ida_2 ).

            " EVENTOS: Instanciar la Clase del Evento Manejador del Boton 'Doble Click'
            SET HANDLER eo_lo_handler->handle_dbclick FOR ch_mo_alv_ida_2->display_options( ).

          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD set_crea_botones.

        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            " CREAR BOTONES: Adicionar el Boton EJECUTAR en la Barra de Botonerías
            ch_mo_alv_ida_1->toolbar( )->add_button( EXPORTING iv_fcode = 'EJEC'
                                                               iv_text  = 'Reprocesar' ).

            "ch_mo_alv_ida_1->toolbar( )->add_button( EXPORTING iv_fcode = 'DISP'
            "                                                   iv_text  = 'Consultar Interfaz' ).

            " CREAR BOTONES: Asignar el Evento para el Botón 'Consultar Interfaz' y 'Ejecutar'
            SET HANDLER eo_lo_handler->handle_action FOR ch_mo_alv_ida_1->toolbar( ).

          WHEN 'ZTINTF_MONIT_DET'.


          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD set_layout.

        " Definición de Word Areas
        DATA ls_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key.

        " LAYOUT: Definir la Llave Persistente del Layout
        ls_persistence_key-report_name     = sy-repid.
        DATA(l_global_save_allowed)        = abap_true.
        DATA(l_user_specific_save_allowed) = abap_true.

        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            " LAYOUT: Asignar la Llave Persistente del Layout
            ch_mo_alv_ida_1->layout_persistence( )->set_persistence_options( EXPORTING is_persistence_key      = ls_persistence_key
                                                                                  i_global_save_allowed        = l_global_save_allowed
                                                                                  i_user_specific_save_allowed = l_user_specific_save_allowed ).
            " LAYOUT: Habilitar la Caja ListBox de los Layout
            ch_mo_alv_ida_1->toolbar( )->enable_listbox_for_layouts( ).

          WHEN 'ZTINTF_MONIT_DET'.

            " LAYOUT: Asignar la Llave Persistente del Layout
            ch_mo_alv_ida_2->layout_persistence( )->set_persistence_options( EXPORTING is_persistence_key      = ls_persistence_key
                                                                                  i_global_save_allowed        = l_global_save_allowed
                                                                                  i_user_specific_save_allowed = l_user_specific_save_allowed ).
            " LAYOUT: Habilitar la Caja ListBox de los Layout
            ch_mo_alv_ida_2->toolbar( )->enable_listbox_for_layouts( ).

          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD set_titulos.

        " Instanciar Clase para Activar Configuración de los Titulos
        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            ch_go_alv_1_opt = ch_mo_alv_ida_1->display_options( ).
            ch_go_alv_1_opt->set_title( iv_title = CONV #( iv_titulo ) ).

          WHEN 'ZTINTF_MONIT_DET'.

            ch_go_alv_2_opt = ch_mo_alv_ida_2->display_options( ).
            ch_go_alv_2_opt->set_title( iv_title = CONV #( iv_titulo ) ).

          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD modify_labels.

        " Instanciación DownCast del ALV-IDA
        DATA(lo_alv_ida) = io_alv_ida.

        CASE iv_tabname.
          WHEN 'ZTINTF_MONIT_CAB'.

            " Asignar ls Textos a las Columnas como lo pide el Usuario p.ALV-IDA Cab. Reservas
            lo_alv_ida->field_catalog( )->get_all_fields( IMPORTING ets_field_names = DATA(lts_field_names) ).
            lo_alv_ida->field_catalog( )->set_available_fields( lts_field_names ).

            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'ID_INTERFAZ'         iv_header_text = 'Nomb.Interfaz' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'ESTADO'              iv_header_text = 'Estatus Interfaz' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'MENSAJE_PROCESO'     iv_header_text = 'Mensaje Principal' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'COMENTARIOS'         iv_header_text = 'Comentarios' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'NOMBRE_ARCHIVO'      iv_header_text = 'Nomb.Archivo Plano' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'USUARIO'             iv_header_text = 'Usuario' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'FECH_CREACION'       iv_header_text = 'Fecha Ejecución Interfaz' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'FECH_ACTUALIZA'      iv_header_text = 'Fecha Actualización Interfaz' ).


          WHEN 'ZTINTF_MONIT_DET'.

            FREE: lts_field_names.

            " Asignar ls Textos a las Columnas como lo pide el Usuario p.ALV-IDA Pos. Saldos Reservas
            lo_alv_ida->field_catalog( )->get_all_fields( IMPORTING ets_field_names = lts_field_names ).
            lo_alv_ida->field_catalog( )->set_available_fields( lts_field_names ).

            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'ID_INTERFAZ'         iv_header_text = 'Nomb.Interfaz' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'LINEA_ARCHIVO'       iv_header_text = 'Línea del Error' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'ESTADO'              iv_header_text = 'Estado del Registro' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'NUM_MENSAJE'         iv_header_text = 'Número del Mensaje' ).
            lo_alv_ida->field_catalog( )->set_field_header_texts( EXPORTING iv_field_name = 'MENSAJE'             iv_header_text = 'Descripción del Error' ).

          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD get_id_proceso.

        DATA lr_id_proceso TYPE RANGE OF zde_id_proceso.

        " Obtener el ID_PROCESO de la Interfaz seleccionada
        SELECT SINGLE id_proceso INTO @DATA(lv_id_proceso)
          FROM ztintf_monit_cab
         WHERE id_interfaz    IN @so_intf[]
           AND nombre_archivo IN @so_noma[]
           AND fech_creacion  IN @gr_fech_creacion[]
           AND hora_creacion  IN @gr_hora_creacion[] .

        IF sy-subrc = 0.

          " Retornar el Range de los ID_PROCESO
          APPEND INITIAL LINE TO rt_id_proceso ASSIGNING FIELD-SYMBOL(<fs_id_proceso>).
          <fs_id_proceso>-sign   = 'I'.
          <fs_id_proceso>-option = 'EQ'.
          <fs_id_proceso>-low    = lv_id_proceso.
        ENDIF.

      ENDMETHOD.

      METHOD get_metodo_controler.

        DATA(lv_progname) = |Z{ me->gv_progname }|.

        zcl_amdp_bc_params=>get_constant( EXPORTING im_progname = CONV #( lv_progname )   " Id desarrollo
                                                    im_constant = 'METODO_CTR'            " Nombre constante  METODO_CTR
                                                    im_modulo   = me->gv_module           " Módulo
                                           IMPORTING et_constant = DATA(lt_tconstant_errores) ).

        rv_metodo = VALUE #( lt_tconstant_errores[ 1 ]-value DEFAULT abap_false ).

      ENDMETHOD.


    ENDCLASS.

    CLASS lcl_handle_action IMPLEMENTATION.

      METHOD constructor.

        " Down Cast de la Instancia de la Interfaz ALV-IDA
        go_ida = io_ida.

      ENDMETHOD.

      METHOD handle_dbclick.

        "************************************************************************"
        "                    MQA SOFTWARE FACTORY                                "
        "************************************************************************"
        "  Nombre del Programa:  ZINTF_MONITOR                                   "
        "  Título           : Monitor p.Interfaces SOP                           "
        "  País             : Republica Colombia                                 "
        "  Autor            : Armando Chavez Arango                              "
        "  Fecha de creación: OCT-26-2024                                        "
        "  Descripción      : Este método se dispara cuando se hace el Doble Click
        "  sobre el campo ID_INTEFAZ se Obtiene nombre de la Interfaz y su valor "
        "  en la Grilla del ALV IDA para volver a hacer la consulta sobre la     "
        "  tabla de Posiciones.                                                  "
        "------------------------------------------------------------------------"
        " Modificaciones                                                         "
        "  ID Usuario     Fecha     Transporte     Descripción                   "
        "************************************************************************"


        " Definición de Tablas Internas
        DATA: lr_suname        TYPE uname_range_tab,
***              lr_fech_creacion TYPE zintf_tt_fech_creacion,
***              lr_hora_creacion TYPE zintf_tt_hora_creacion,
              lt_nombres_campo TYPE zintf_tt_nombres_campo.

        " Estructura Cabecera del Monitor
        DATA: ls_ztintf_monit_cab TYPE ztintf_monit_cab,
              lt_named_ranges     TYPE if_salv_service_types=>yt_named_ranges.

        CHECK go_ida IS BOUND.

        " Instanciar la Clase Local del Monitor
        lo_intf_monitor = lcl_intf_monitor=>get_instance( ).

        " Asigna Nombre de Tabla Transparente p.Las Posiciones del Monitor
        lo_intf_monitor->gv_tabname = 'ZTINTF_MONIT_DET'.

        " Verificar si el Usuario ha Seleccionado un Registro del ALV-IDA
        IF go_ida->selection( )->is_row_selected( ).

          " Limpiar el ALV IDA para una Nueva Consulta
          IF mo_alv_ida_2 IS BOUND.
            mo_alv_ida_2->free( ).
          ENDIF.

          IF go_ida_item IS BOUND.
            go_ida_item->free( ).
            FREE go_ida_item.
          ENDIF.

          " Obtener los Registros Seleccionados
          go_ida->selection( )->get_selected_row( IMPORTING es_row = ls_ztintf_monit_cab ).

          " Instancia la Clase Manejadora del ALV-IDA con el Nombre de Tabla Transparente
          " y la Instancia de un Contenedor para el ALV-IDA de Posición
          go_ida_item = cl_salv_gui_table_ida=>create( iv_table_name    = CONV #( lo_intf_monitor->gv_tabname )
                                                       io_gui_container = go_container_200 ).

          IF go_ida_item IS BOUND.



            " Modifica o Actualiza los Titulos de los Campos del ALV-IDA
            lo_intf_monitor->modify_labels( EXPORTING io_alv_ida = go_ida_item
                                                      iv_tabname = CONV #( lo_intf_monitor->gv_tabname ) ).


            " Mapear los Campos a Excluir Catalogo del ALV
            lt_nombres_campo = VALUE #( ( nombre_campo = 'ID_PROCESO' )
                                        ( nombre_campo = 'GUID_ERROR' ) ).

            " Excluir Campos del Catalogo del ALV
            go_intf_monitor->get_excluir_campos( EXPORTING io_alv_ida       = go_ida_item
                                                           it_nombres_campo = lt_nombres_campo ).

            " Condiciones Adicionales p.Sesgar mas la Consulta
            gr_fech_creacion = VALUE #( ( sign = 'I' option = 'EQ' low = ls_ztintf_monit_cab-fech_creacion ) ).
            gr_hora_creacion = VALUE #( ( sign = 'I' option = 'EQ' low = ls_ztintf_monit_cab-hora_creacion ) ).

            " Filtrar el ALV dados los Criterios de Selección
            go_intf_monitor->display_with_condition_range( EXPORTING io_alv_ida       = go_ida_item              " DownCast de la Instancia
                                                                     ir_intf_name     = so_intf[]                " Nombre de la Interfaz
                                                                     ir_id_proceso    = lo_intf_monitor->get_id_proceso( ) ).     " Id del Proceso

          ENDIF.

        ELSE.

          " No Ha Seleccionado Ningún registro
          MESSAGE i003(zintf) DISPLAY LIKE 'E'.

        ENDIF.

      ENDMETHOD.

      METHOD handle_action.

        "************************************************************************"
        "                    MQA SOFTWARE FACTORY                                "
        "************************************************************************"
        "  Nombre del Programa:  ZINTF_MONITOR                                   "
        "  Título           : Monitor p.Interfaces SOP                           "
        "  País             : Republica Colombia                                 "
        "  Autor            : Armando Chavez Arango                              "
        "  Fecha de creación: OCT-26-2024                                        "
        "  Descripción      : Este método se dispara cuando se presiona el botón "
        "  EJECUTAR para volver a procesar la información de los archivos planos "
        "------------------------------------------------------------------------"
        " Modificaciones                                                         "
        "  ID Usuario     Fecha     Transporte     Descripción                   "
        "************************************************************************"

        "Definición de Tablas Internas
        DATA: lr_suname        TYPE uname_range_tab,
              lt_nombres_campo TYPE zintf_tt_nombres_campo.

        " Estructura Cabecera del Monitor
        DATA: ls_ztintf_monit_cab TYPE ztintf_monit_cab,
              lt_named_ranges     TYPE if_salv_service_types=>yt_named_ranges,
              result(1).

        " Instanciar la Clase Global del Monitor
        go_intf_monitor = zcl_intf_monitor=>get_instance( ).

        " Instanciar la Clase Local del Monitor
        lo_intf_monitor = lcl_intf_monitor=>get_instance( ).

        CASE ev_fcode.
          WHEN 'EJEC'. " Se Pulsó el Botón 'Reprocesar'

            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                defaultoption  = 'N'
                textline1      = TEXT-004 " ¿Esta Seguro(a) de Ejecutar el Reproceso de la Interface ?
                titel          = TEXT-003 " Reprocesar Interfaces
                cancel_display = 'X'
              IMPORTING
                answer         = result.

            IF result = 'J'.

              " Reprocesando Inrteface, espere un momento por favor...
              cl_progress_indicator=>progress_indicate( EXPORTING i_text               = TEXT-005
                                                                  i_processed          = sy-index
                                                                  i_total              = 0
                                                                  i_output_immediately = abap_true ).

              " Reprocesa la Interface que se está Cosultando
              go_intf_monitor->run( EXPORTING iv_nomb_interfaz = lo_intf_monitor->gv_progname
                                              iv_metodo        = lo_intf_monitor->get_metodo_controler( ) ).

              " Reproceso Terminó.
              MESSAGE i008(zintf) DISPLAY LIKE 'I'.

            ELSE.

              " Proceso cancelado por el usuario.
              MESSAGE i007(zintf) DISPLAY LIKE 'I'.
            ENDIF.

          WHEN 'DISP'. " Se Pulsó el Botón 'Consultar Interfaz'

            " Verificar que la Instancia de la Clase ALV IDA este arriba
            IF go_ida IS BOUND.

              " Verificar si el Usuario ha Seleccionado un Registro del ALV-IDA
              IF go_ida->selection( )->is_row_selected( ).

                " Asigna Nombre de Tabla Transparente
                lo_intf_monitor->gv_tabname = 'ZTINTF_MONIT_DET'.

                " Limpiar el ALV IDA para una Nueva Consulta
                IF mo_alv_ida_2 IS BOUND.
                  mo_alv_ida_2->free( ).
                ENDIF.

                IF go_ida_item IS BOUND.
                  go_ida_item->free( ).
                  FREE go_ida_item.
                ENDIF.


                " Obtener los Registros Seleccionados de la Cabecera del Monitor
                go_ida->selection( )->get_selected_row( IMPORTING es_row = ls_ztintf_monit_cab ).

                " Instancia la Clase Manejadora del ALV-IDA con el Nombre de Tabla Transparente
                " y la Instancia de un Contenedor para el ALV-IDA de Posición
                go_ida_item = cl_salv_gui_table_ida=>create( iv_table_name    = CONV #( lo_intf_monitor->gv_tabname )
                                                             io_gui_container = go_container_200 ).

                IF go_ida_item IS BOUND.

                  " Modifica o Actualiza los Titulos de los Campos del ALV-IDA
                  lo_intf_monitor->modify_labels( EXPORTING io_alv_ida = go_ida_item
                                                            iv_tabname = lo_intf_monitor->gv_tabname ).


                  " Mapear los Campos a Excluir Catalogo del ALV
                  lt_nombres_campo = VALUE #( ( nombre_campo = 'ID_PROCESO' )
                                              ( nombre_campo = 'GUID_ERROR' ) ).

                  " Excluir Campos del Catalogo del ALV
                  go_intf_monitor->get_excluir_campos( EXPORTING io_alv_ida       = go_ida_item
                                                                 it_nombres_campo = lt_nombres_campo ).


                  " Filtrar el ALV dados los Criterios de Selección
                  go_intf_monitor->display_with_condition_range( EXPORTING io_alv_ida    = go_ida_item              " DownCast de la Instancia
                                                                           ir_intf_name  = so_intf[]                " Nombre de la Interfaz
                                                                           ir_id_proceso = lo_intf_monitor->get_id_proceso( ) ). " Id del Proceso

                ENDIF.

              ELSE.

                " No Ha Seleccionado Ningún registro
                MESSAGE i003(zintf) INTO DATA(lv_message).
                MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
              ENDIF.

            ENDIF.

          WHEN 'REFR'. " Se Pulsó el Botón 'Refrescar'
          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

    ENDCLASS.