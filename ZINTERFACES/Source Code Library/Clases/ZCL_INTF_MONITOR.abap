class ZCL_INTF_MONITOR definition
  public
  inheriting from ZCL_MANAGER_FILE_ROOT
  final
  create public

  global friends ZCL_INTF_SOP007_CREA_PED_VENTA .

public section.

  data GO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  data GR_OBJECT type ref to OBJECT .
  data GV_METODO type SEOCMPNAME .
  data GO_SYSTEM_UUID type ref to IF_SYSTEM_UUID .

  methods DISPLAY_WITH_CONDITION_RANGE
    importing
      !IO_ALV_IDA type ref to IF_SALV_GUI_TABLE_IDA
      !IR_UNAME type ZINTF_TT_UNAME optional
      !IR_INTF_NAME type ZINTF_TT_NOMB_INTERFAZ
      !IR_ID_PROCESO type ZINTF_TT_ID_PROCESO optional
      !IR_NOM_ARCH type ZINTF_TT_NOMB_ARCHIVO optional
      !IR_FECH_CREACION type ZINTF_TT_FECH_CREACION optional
      !IR_HORA_CREACION type ZINTF_TT_HORA_CREACION optional .
  methods GET_EXCLUIR_CAMPOS
    importing
      !IO_ALV_IDA type ref to IF_SALV_GUI_TABLE_IDA optional
      !I_NOMBRE_CAMPO type TABLE_LINE optional
      !IT_NOMBRES_CAMPO type ZINTF_TT_NOMBRES_CAMPO optional .
  methods RUN
    importing
      !IV_NOMB_INTERFAZ type ZDE_ID_INTEFAZ
      !IV_METODO type SEOCMPNAME .
  methods SET_REGISTERS_MULTIPLE_ERRORS
    importing
      !IV_ID_INTEFAZ type ZDE_ID_INTEFAZ
      !IV_NOMBRE_ARCHIVO type FILE_NAME
      !IV_ESTADO_ERROR type ZDE_ESTADO_INTF optional
      !IV_COMENTARIOS type ZDE_COMENTARIOS optional
      !IT_BAPIRET2_TAB type BAPIRET2_TAB .
  methods SET_REGISTERS_SINGLE_ERROR
    importing
      !IV_ID_INTEFAZ type ZDE_ID_INTEFAZ
      !IV_MENSAJE_PROCESO type BAPI_MSG
      !IV_NOMBRE_ARCHIVO type FILE_NAME
      !IV_ESTADO_ERROR type ZDE_ESTADO_INTF
      !IV_NUM_MENSAJE type SYMSGNO .
  class-methods GET_INSTANCE
    returning
      value(RO_COLLECTOR) type ref to ZCL_INTF_MONITOR .
protected section.
private section.

  class-data MO_COLLECTOR type ref to ZCL_INTF_MONITOR .

  methods GET_SEMAFORO_CAB
    importing
      !IT_BAPIRET2_TAB type BAPIRET2_TAB
    returning
      value(RV_ESTADO_ERROR) type ZDE_ESTADO_INTF .
  methods SET_EXCLUIR_CAMPOS
    importing
      !IO_ALV_IDA type ref to IF_SALV_GUI_TABLE_IDA
      !I_NOMBRE_CAMPO type TABLE_LINE
      !IT_NOMBRES_CAMPO type ZINTF_TT_NOMBRES_CAMPO .
ENDCLASS.



CLASS ZCL_INTF_MONITOR IMPLEMENTATION.


  METHOD display_with_condition_range.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este método tiene como objetivo recibir los        "
    "  los Criterios SELECT-OPTIONS del programa para realizar los busquedas "
    "  en la Base de Datos Hana.                                             "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Libera la Instancia dado que se estan trabajando con 2 Instancias de ALV-IDA diferentes
    FREE go_alv_display.

    " Instanciación DownCast del ALV IDA
    go_alv_display ?= io_alv_ida.

    " Instanciar el Collector de Filtros o Criterios de Selección
    DATA(lo_collector) = NEW cl_salv_range_tab_collector( ).

    " Determinar Cuales seran los Criterios de Selección
    lo_collector->add_ranges_for_name( iv_name = 'USUARIO'        it_ranges = ir_uname[] ).         " Usuario Ejecutor del Reporte
    lo_collector->add_ranges_for_name( iv_name = 'ID_PROCESO'     it_ranges = ir_id_proceso[] ).    " Id del Proceso Interface
    lo_collector->add_ranges_for_name( iv_name = 'ID_INTERFAZ'    it_ranges = ir_intf_name[] ).     " Nombre de la Interface
    lo_collector->add_ranges_for_name( iv_name = 'NOMBRE_ARCHIVO' it_ranges = ir_nom_arch[] ).      " Nombre del Archivo Plano
    lo_collector->add_ranges_for_name( iv_name = 'FECH_CREACION'  it_ranges = ir_fech_creacion[] ). " Fecha Crea el Archivo Plano
    lo_collector->add_ranges_for_name( iv_name = 'HORA_CREACION'  it_ranges = ir_hora_creacion[] ). " Hora Crea el Archivo Plano


    lo_collector->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_name_range_pairs) ).
    go_alv_display->set_select_options( it_ranges = lt_name_range_pairs ).

  ENDMETHOD.


  METHOD get_excluir_campos.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este método tiene como objetivo exclir los campos  "
    "  que no deseamos visualizar en el ALV-IDA                              "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    me->set_excluir_campos( EXPORTING io_alv_ida       = io_alv_ida
                                      i_nombre_campo   = i_nombre_campo
                                      it_nombres_campo = it_nombres_campo ).

  ENDMETHOD.


  METHOD get_instance.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este método tiene por objetivo solo verificar la   "
    "  la intancia de la clase para evitar la instanciación multiple.        "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Instancia Patron Singleton
    IF mo_collector IS NOT BOUND.
      mo_collector = NEW zcl_intf_monitor( ).
    ENDIF.

    " Retornando la Intancia Activa
    ro_collector = mo_collector.

  ENDMETHOD.


  METHOD run.
    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este método tiene por objetivo reprocesar una      "
    "  Interface.                                                            "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    TRY.

        " Instanciar las Clases de cada una de las Interfaces
        CASE iv_nomb_interfaz.
          WHEN 'SOP004'.
          WHEN 'SOP008'.

            " Instanciar la Clase Manejadora de la Interface
            gr_object = NEW zcl_intf_sop008( ).

          WHEN 'SOP012_C'.

            " Instanciar la Clase Manejadora de la Interface
            gr_object = NEW zcl_intf_sop012_client( ).
          WHEN 'SOP012_P'.
            " Instanciar la Clase Manejadora de la Interface
            gr_object = NEW zcl_intf_sop012_supplier( ).

          WHEN 'SOP013'.

            " Instanciar la Clase Manejadora de la Interface
            gr_object = NEW zcl_intf_sop013( ).

          WHEN OTHERS.
        ENDCASE.

        " Ejecutar el Proceso Controlador del Proeso
        CALL METHOD gr_object->(iv_metodo).

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).

        DATA(lv_message) = lx_illegal_method->get_longtext( ).
        MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.

    ENDTRY.
  ENDMETHOD.


  METHOD set_excluir_campos.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-26-2024                                        "
    "  Descripción      : Este método tiene como objetivo exclir los campos  "
    "  que no deseamos visualizar en el ALV-IDA                              "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Libera la Instancia dado que se estan trabajando con 2 Instanacias de ALV-IDA diferentes
    FREE go_alv_display.

    " Ejecuta Patrón Singleton Instanciación DownCast de la Clase
    go_alv_display ?= io_alv_ida.

    " Obtener los Campos del Catalogo
    go_alv_display->field_catalog( )->get_all_fields( IMPORTING ets_field_names = DATA(lts_field_names) ).

    LOOP AT it_nombres_campo ASSIGNING FIELD-SYMBOL(<fs_nombres_campo>).

      " Obtener el Indice para eliminar el campo del Catalogo
      READ TABLE lts_field_names TRANSPORTING NO FIELDS WITH KEY table_line = <fs_nombres_campo>-nombre_campo.
      IF sy-subrc = 0.
        DATA(lv_tabix) = sy-tabix.
        DELETE lts_field_names INDEX lv_tabix.
      ENDIF.

    ENDLOOP.

    go_alv_display->field_catalog( )->set_available_fields( lts_field_names ).

  ENDMETHOD.


  METHOD set_registers_multiple_errors.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-29-2024                                        "
    "  Descripción      : Este método tiene por objetivo registrar multiples "
    "  errores que provienen de una estructura BAPIRET2.                     "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    DATA: lv_uuid         TYPE sysuuid_c32,
          lv_mensaje      TYPE string,
          lv_estado_error	TYPE zde_estado_intf.

    CHECK it_bapiret2_tab IS NOT INITIAL.

    " Instanciar la Clase Factory UUID
    go_system_uuid = cl_uuid_factory=>create_system_uuid( ).

    " Generar el UUID
    lv_uuid = go_system_uuid->create_uuid_c32( ).

    " Doble Clic para ver Detalle del Mensaje
    MESSAGE s014(zintf) INTO DATA(lv_mensaje_cab).

    " Crear el Registro de la Cabecera
    INSERT ztintf_monit_cab FROM TABLE @( VALUE #(
                    ( mandt           = sy-mandt
                      id_proceso      = lv_uuid
                      id_interfaz     = iv_id_intefaz
                      estado          = me->get_semaforo_cab( it_bapiret2_tab )
                      mensaje_proceso = lv_mensaje_cab
                      comentarios     = COND #( WHEN iv_comentarios IS INITIAL THEN 'N/A' ELSE iv_comentarios )
                      nombre_archivo  = iv_nombre_archivo
                      usuario         = sy-uname
                      fech_creacion   = sy-datlo
                      hora_creacion   = sy-timlo
                      fech_actualiza  = sy-datlo ) ) ).

    CHECK sy-subrc = 0.

    LOOP AT it_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_bapiret2>).

      " Determinar el Semaforo p.Mensaje del Monitor
      CASE <fs_bapiret2>-type.
        WHEN 'S'.

          " Asignar Semaforo Verde
          lv_estado_error = zintf_gc_verde.
        WHEN 'I'.

          " Asignar Semaforo Verde
          lv_estado_error = zintf_gc_verde.
        WHEN 'E'.

          " Asignar Semaforo Rojo
          lv_estado_error = zintf_gc_rojo.
        WHEN OTHERS.
      ENDCASE.

      " Generar el GUID
      DATA(lv_guid_error) = go_system_uuid->create_uuid_c32( ).

      " Crear el Registro del Detalle de la Cabecera
      INSERT ztintf_monit_det FROM TABLE @( VALUE #(
                          ( mandt         = sy-mandt
                            guid_error    = lv_guid_error
                            id_proceso    = lv_uuid
                            id_interfaz   = iv_id_intefaz
                            linea_archivo = <fs_bapiret2>-row
                            estado        = lv_estado_error
                            num_mensaje   = <fs_bapiret2>-number
                            mensaje       = <fs_bapiret2>-message ) ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD set_registers_single_error.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-29-2024                                        "
    "  Descripción      : Este método tiene por objetivo registrar un error  "
    "  individual a las tablas del monitor.                                  "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    DATA lv_uuid TYPE sysuuid_c32.

    " Instanciar la Clase Factory UUID
    go_system_uuid = cl_uuid_factory=>create_system_uuid( ).

    " Generar el UUID
    lv_uuid = go_system_uuid->create_uuid_c32( ).

    " Determinar el Mensaje de la Cabecera del Monitor
    CASE iv_estado_error.
      WHEN zintf_gc_verde.
        " La Interface &1 terminó satisfactoriamente.
        MESSAGE s005(zintf) WITH iv_id_intefaz INTO DATA(lv_mensaje_cab).
      WHEN zintf_gc_rojo.
        " La Interface &1 contiene errores.
        MESSAGE s006(zintf) WITH iv_id_intefaz INTO lv_mensaje_cab.
      WHEN OTHERS.
    ENDCASE.

    " Crear el Registro de la Cabecera
    INSERT ztintf_monit_cab FROM TABLE @( VALUE #(
                    ( mandt           = sy-mandt
                      id_proceso      = lv_uuid
                      id_interfaz     = iv_id_intefaz
                      estado          = iv_estado_error
                      mensaje_proceso = lv_mensaje_cab
                      comentarios     = 'N/A'
                      nombre_archivo  = iv_nombre_archivo
                      usuario         = sy-uname
                      fech_creacion   = sy-datlo
                      hora_creacion   = sy-timlo
                      fech_actualiza  = sy-datlo ) ) ).
    CHECK sy-subrc = 0.

    " Generar el GUID
    DATA(lv_guid_error) = go_system_uuid->create_uuid_c32( ).

    " Crear el Registro del Detalle de la Cabecera
    INSERT ztintf_monit_det FROM TABLE @( VALUE #(
                        ( mandt         = sy-mandt
                          guid_error    = lv_guid_error
                          id_proceso    = lv_uuid
                          id_interfaz   = iv_id_intefaz
                          linea_archivo = 0
                          estado        = iv_estado_error
                          num_mensaje   = iv_num_mensaje
                          mensaje       = iv_mensaje_proceso ) ) ).

  ENDMETHOD.


  METHOD get_semaforo_cab.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  ZINTF_MONITOR                                   "
    "  Título           : Monitor p.Interfaces SOP                           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: NOV-14-2024                                        "
    "  Descripción      : Este método tiene por objetivo retornar el semaforo"
    "  de la Cabecera del Monitor, si en la tabla LT_BAPIRET2_TAB existen    "
    "  semaforos, Amarillos, Verdes y Rojos, retorna prioritariamente ROJO.  "
    "  Si existen semaforos: Amarillos y Verdes, retorna prioritariamente    "
    "  semaforo en VERDE.                                                    "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    DATA: lt_bapiret2_tab	   TYPE bapiret2_tab,
          lv_estado_verde	   TYPE zde_estado_intf,
          lv_estado_amarillo TYPE zde_estado_intf,
          lv_estado_rojo     TYPE zde_estado_intf.

    CHECK it_bapiret2_tab IS NOT INITIAL.

    " Copia tabla a tabla local
    lt_bapiret2_tab = it_bapiret2_tab.

    SORT lt_bapiret2_tab BY type.
    DELETE ADJACENT DUPLICATES FROM lt_bapiret2_tab COMPARING type.

    LOOP AT lt_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_bapiret2>).

      " Determinar el Semaforo p.Mensaje del Monitor
      CASE <fs_bapiret2>-type.
        WHEN 'S'.

          " Asignar Semaforo Verde
          lv_estado_verde = zintf_gc_verde.
        WHEN 'I'.

          " Asignar Semaforo Amarillo
          lv_estado_amarillo = zintf_gc_amarillo.
        WHEN 'E'.

          " Asignar Semaforo Rojo
          lv_estado_rojo = zintf_gc_rojo.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    " Solo VERDE
    IF lv_estado_verde    IS NOT INITIAL AND
       lv_estado_amarillo IS INITIAL     AND
       lv_estado_rojo     IS INITIAL.

      " Retorna Semaforo en Verde
      rv_estado_error = zintf_gc_verde.

      " Solo AMARILLO
    ELSEIF lv_estado_verde IS INITIAL    AND
       lv_estado_amarillo  IS NOT INITIAL AND
       lv_estado_rojo      IS INITIAL.

      " Retorna Semaforo en Verde
      rv_estado_error = zintf_gc_verde.

      " Solo ROJO
    ELSEIF lv_estado_verde IS INITIAL AND
       lv_estado_amarillo  IS INITIAL AND
       lv_estado_rojo      IS NOT INITIAL.

      " Retorna Semaforo en Rojo
      rv_estado_error = zintf_gc_rojo.

      " Solo VERDE y AMARILLO
    ELSEIF lv_estado_verde    IS NOT INITIAL AND
           lv_estado_amarillo IS NOT INITIAL AND
           lv_estado_rojo     IS INITIAL.

      " Retorna Semaforo en Verde
      rv_estado_error = zintf_gc_verde.

      " Solo AMARILLO y ROJO
    ELSEIF lv_estado_verde    IS INITIAL AND
           lv_estado_amarillo IS NOT INITIAL AND
           lv_estado_rojo     IS NOT INITIAL.

      " Retorna Semaforo en Rojo
      rv_estado_error = zintf_gc_rojo.

    ELSEIF lv_estado_verde    IS NOT INITIAL AND
           lv_estado_amarillo IS NOT INITIAL AND
           lv_estado_rojo     IS NOT INITIAL.

      " Retorna Semaforo en Rojo
      rv_estado_error = zintf_gc_rojo.

    ENDIF.

  ENDMETHOD.
ENDCLASS.