"************************************************************************"
"                    MQA SOFTWARE FACTORY                                "
"************************************************************************"
"  Nombre del Programa:  ZINTF_MONITOR_FOR                               "
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
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.

  " Instanciar la Clase Local del Monitor
  lo_intf_monitor = lcl_intf_monitor=>get_instance( ).

  " Asignar Nombre de Usuario Final al Titulo del Reporte
  lo_intf_monitor->get_user( ).

  " Desplegar ALV-IDA p.Cabecera del Monitor
  lo_intf_monitor->display_alv_1( ).

  " Desplegar ALV-IDA p.Posiciones del Monitor
  lo_intf_monitor->display_alv_2( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

  " Eventos para Salir y Cancelar Dynpro
  lo_intf_monitor->set_module_pai( ).

ENDMODULE.