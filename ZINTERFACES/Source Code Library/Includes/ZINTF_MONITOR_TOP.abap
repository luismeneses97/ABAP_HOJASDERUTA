"************************************************************************"
"                    MQA SOFTWARE FACTORY                                "
"************************************************************************"
"  Nombre del Programa:  ZINTF_MONITOR_TOP                               "
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

TABLES: ztintf_monit_cab,
        ztintf_monit_det.

" Diferir las Clases
CLASS : lcl_handle_action DEFINITION DEFERRED.
CLASS : lcl_intf_monitor  DEFINITION DEFERRED.

DATA: go_intf_monitor TYPE REF TO zcl_intf_monitor,
      lo_intf_monitor TYPE REF TO lcl_intf_monitor.

" Objetos de Referencia para ALV-IDA
DATA: mo_alv_ida       TYPE REF TO if_salv_gui_table_ida,
      mo_alv_ida_1     TYPE REF TO if_salv_gui_table_ida,
      mo_alv_ida_2     TYPE REF TO if_salv_gui_table_ida,
      go_alv_1_opt     TYPE REF TO if_salv_gui_table_display_opt,
      go_alv_2_opt     TYPE REF TO if_salv_gui_table_display_opt,
      go_container_100 TYPE REF TO cl_gui_custom_container,
      go_container_200 TYPE REF TO cl_gui_custom_container.

" Atrapa Códigos función que ha lanzado PAI
DATA: g_okcode TYPE sy-ucomm,
      gr_fech_creacion TYPE zintf_tt_fech_creacion,
      gr_hora_creacion TYPE zintf_tt_hora_creacion.


"-----------------------------------------"
"        PARAMETROS DE SELECCIÓN          "
"-----------------------------------------"

" Seleccione un criterio de busqueda
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

    SELECT-OPTIONS: so_intf FOR ztintf_monit_cab-id_interfaz    OBLIGATORY NO INTERVALS MATCHCODE OBJECT zsh_nomb_intfaz,  " Nombre de la Interface
                    so_noma FOR ztintf_monit_cab-nombre_archivo OBLIGATORY NO INTERVALS MATCHCODE OBJECT zsh_nomb_archivo. " Nombre del Archivo Plano

  SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.