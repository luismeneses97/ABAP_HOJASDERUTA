    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR_EVE                               "
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

    START-OF-SELECTION.

      TRY.

          " Instanciar la Clase Global del Monitor
          go_intf_monitor = zcl_intf_monitor=>get_instance( ).

          " Instanciar la Clase Local del Monitor
          lo_intf_monitor = lcl_intf_monitor=>get_instance( ).

          " Dynpro para Desplegar en Dos Contenedores Mestro/Detalle del ALV-IDA
          CALL SCREEN 0100.

        CATCH cx_amdp_execution_failed INTO DATA(lo_execution_failed).

          DATA(lv_message) = lo_execution_failed->get_longtext( ).

      ENDTRY.