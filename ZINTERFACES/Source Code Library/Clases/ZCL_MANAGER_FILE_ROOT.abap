class ZCL_MANAGER_FILE_ROOT definition
  public
  create public .

public section.

  types:
    tt_table_file_contents TYPE TABLE OF thcs_string .

  constants GC_ID_CARPETA_N type ZDE_ID_CARPETA value 'N' ##NO_TEXT.
  data GC_ID_CARPETA_P type ZDE_ID_CARPETA value 'P' ##NO_TEXT.
  data GC_ID_CARPETA_E type ZDE_ID_CARPETA value 'E' ##NO_TEXT.

    METHODS set_split_files
      IMPORTING iv_id_carpeta  TYPE zde_id_carpeta
                iv_filename    TYPE string
                iv_module      TYPE ufps_posid
                iv_progname    TYPE progname
                iv_country     TYPE land1
                it_data        TYPE zintf_tt_headerdata_sop008 OPTIONAL
                it_data_sop007 TYPE zintf_tt_pedido_venta OPTIONAL
                it_data_c      TYPE ztinft_client OPTIONAL
                it_data_p      TYPE ztinft_supplier OPTIONAL
                it_data_sop011 TYPE ztinft_stock  OPTIONAL
                it_data_hojas_ruta TYPE ANY TABLE  OPTIONAL.

  methods GET_MESSAGE_PUBLIC
    importing
      !IV_CLASS_MESS type ARBGB default 'ZINTF'
      !IV_MSGNR type MSGNR
      !TOKEN1 type NATXT optional
      !TOKEN2 type NATXT optional
      !TOKEN3 type NATXT optional
      !TOKEN4 type NATXT optional
      !TOKEN5 type NATXT optional
    returning
      value(RV_TEXT) type BAPI_MSG .
  methods READ_FILE_INTERFACE
    importing
      !IM_V_FILENAME type STRING optional
      !IM_V_MODULE type UFPS_POSID optional
      !IM_V_PROGNAME type PROGNAME optional
      !IM_V_COUNTRY type LAND1 optional
      !IT_FILE_CONTENTS type THCS_STRING .
  class-methods GET_INSTANCE_ROOT
    returning
      value(RO_COLLECTOR) type ref to ZCL_MANAGER_FILE_ROOT .
protected section.
private section.

  class-data MO_COLLECTOR type ref to ZCL_MANAGER_FILE_ROOT .
ENDCLASS.



CLASS ZCL_MANAGER_FILE_ROOT IMPLEMENTATION.


  METHOD GET_INSTANCE_ROOT.

    " Instancia Patron Singleton
    IF NOT mo_collector IS BOUND.
      mo_collector = NEW zcl_manager_file_root( ).
    ENDIF.

    " Retornando la Intancia Activa
    ro_collector = mo_collector.

  ENDMETHOD.


  METHOD read_file_interface.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  READ_FILE_INTERFACE                             "
    "  Título           : Interfaz p.Carga de Archivos de Procesos           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-18-2024                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar la lectura de los datos del archivo que estasn en un string  "
    "  y convertirlos campo a campo de la estructura real de cada interface  "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Definición de Tablas Internas
    DATA: lt_headerdata           TYPE zintf_tt_headerdata_sop008,
          lti_table_file_contents TYPE TABLE OF thcs_string,
          lt_split                TYPE TABLE OF char40,
          lt_file_contents        TYPE thcs_string.

    DATA: lv_string TYPE string.


    " Definir Objetos de Referencia
    DATA: ref_it      TYPE REF TO data,
          lr_split    TYPE REF TO char40,
          go_mgr_file TYPE REF TO zcl_manager_file.

    " Variables referenciadas o campos simbólicos para cargar los datos
    FIELD-SYMBOLS: <fs_campo> TYPE any,
                   <fs_wa>    TYPE any,
                   <fs_it>    TYPE ANY TABLE.

    TRY.

        " Instanciar la Clase Manejadora de Archivos
        go_mgr_file = zcl_manager_file=>instantiate( ).

        " Instanciar el objeto de datos tabla interna con referencia a la Estructura de la Interfaz
        CREATE DATA ref_it TYPE zintf_ty_content_sop008.

        " Desreferenciar del objeto de datos tabla interna al field symbol
        ASSIGN ref_it->* TO <fs_wa>.

        " Por defecto el sistema asigna como separador un tabulador (Caracter de Escape #)
        DATA(lv_separador) = cl_abap_char_utilities=>horizontal_tab.

        " Leer la Tabla Interna con el Contenido para extraer el Nombre del Archivo
        READ TABLE it_file_contents  INTO DATA(ls_file_contents) INDEX 1.

        lt_file_contents = it_file_contents.
        sort lt_file_contents by table_line.
        " Leer el Contenido Línea a Línea
        LOOP AT lt_file_contents ASSIGNING FIELD-SYMBOL(<fs_data>).

          " Descartar la Fila del Nombre del Archivo
          FIND lv_separador IN <fs_data> MATCH COUNT sy-tabix.
          IF sy-tabix = 0.
            " Si no Encontró ningún MATCH continua al siguiente registro
            CONTINUE.
          ENDIF.

          " Vaciar el Contenido de la Línea con el Caracter de Escape "#" al final
          lv_string = |{ <fs_data> }{ lv_separador }|.

          " Limpiar vacios que no necesitamos
          CONDENSE lv_string NO-GAPS.

          " Parsear cada Campo a una Tabla Interna
          SPLIT lv_string AT lv_separador INTO TABLE lt_split.

          " Leer los Campos ya Parseados de la Línea del Archivo
          LOOP AT lt_split REFERENCE INTO lr_split.

            DATA(lv_sytabix) = sy-tabix.

            " Leer Campo a Campo
            ASSIGN COMPONENT lv_sytabix OF STRUCTURE <fs_wa> TO <fs_campo>.
            IF sy-subrc = 0.

              " Asignar los Valores a cada uno de los Campos de la Estructura
              <fs_campo> = lr_split->*.
            ENDIF.

          ENDLOOP.

          " Poblar la Tabla Interna de la Interface
          APPEND <fs_wa> TO lt_headerdata.
          IF sy-subrc = 0.
          ENDIF.

        ENDLOOP.

        DATA(lv_error) = COND #( WHEN sy-subrc = 0 THEN abap_false ELSE abap_true ).

        " Mover el Archivo Plano de las Carpetas ENTRADA a PROCESADAS o ERRORES
        zcl_manager_file=>instantiate( )->move_file( im_v_existe_error = lv_error
                                                     im_v_filename     = CONV #( im_v_filename )
                                                     im_t_content      = it_file_contents ).

      CATCH cx_sy_create_data_error.

    ENDTRY.

  ENDMETHOD.


  METHOD get_message_public.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  READ_FILE_INTERFACE                             "
    "  Título           : Interfaz p.Carga de Archivos de Procesos           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: OCT-29-2024                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  Obtener el mensaje de una Clase de Mensaje                            "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    DATA: lv_string TYPE string.
    DATA: it_result TYPE match_result_tab.

    " Retornar el Mensaje solicitado
    SELECT SINGLE text INTO @rv_text
      FROM t100
    WHERE sprsl = 'S'
      AND arbgb = @iv_class_mess
      AND msgnr = @iv_msgnr.

    IF token1 IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '&1' IN rv_text WITH token1.
    ENDIF.

    IF token2 IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '&2' IN rv_text WITH token2.
    ENDIF.

    IF token3 IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '&3' IN rv_text WITH token3.
    ENDIF.

    IF token4 IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '&4' IN rv_text WITH token4.
    ENDIF.

    IF token5 IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '&5' IN rv_text WITH token5.
    ENDIF.

  ENDMETHOD.


  METHOD set_split_files.

    "************************************************************************"
    "                    MQA SOFTWARE FACTORY                                "
    "************************************************************************"
    "  Nombre del Programa:  SET_SPLIT_FILES                             "
    "  Título           : Interfaz p.Carga de Archivos de Procesos           "
    "  País             : Republica Colombia                                 "
    "  Autor            : Armando Chavez Arango                              "
    "  Fecha de creación: Nov-08-2024                                        "
    "  Descripción      : Este programa tiene como objetivo principal el     "
    "  realizar una copia de un archivo que según el proceso puede contener  "
    "  errores con lo cual se copia a la carpeta de Errores, o puede no tener"
    "  errores en ese caso se copia a la parpeta de Procesados.              "
    "------------------------------------------------------------------------"
    " Modificaciones                                                         "
    "  ID Usuario     Fecha     Transporte     Descripción                   "
    "************************************************************************"

    " Definir Objetos de Referencia
    DATA: ref_it      TYPE REF TO data,
          go_sop008   TYPE REF TO zcl_intf_sop008,
          go_mgr_file TYPE REF TO zcl_manager_file,
          lr_split    TYPE REF TO char40.

    DATA: lt_data    TYPE STANDARD TABLE OF zintf_ty_content_sop008 WITH DEFAULT KEY,
          lt_content TYPE thcs_string..

    " Variables referenciadas o campos simbólicos para cargar los datos
    FIELD-SYMBOLS: <fs_campo> TYPE any,
                   <fs_wa>    TYPE any,
                   <fs_it>    TYPE ANY TABLE.

    DATA: lv_code_page TYPE cpcodepage,
          lv_string    TYPE string.

    CHECK it_data IS NOT INITIAL.

    " Transfiriendo los Datos a la Tabla Interna de Procesamiento
    lt_data = CORRESPONDING #( it_data ).

    " Instanciar el objeto de datos tabla interna con referencia a la Estructura de la Interfaz
    CREATE DATA ref_it TYPE zintf_ty_content_sop008.

    " Desreferenciar del objeto de datos tabla interna al field symbol
    ASSIGN ref_it->* TO <fs_wa>.

    " Por defecto el sistema asigna como separador un tabulador (Caracter de Escape #)
    DATA(lv_separador) = cl_abap_char_utilities=>horizontal_tab.

    " Leer cada Registro Completo del Archivo
    LOOP AT lt_data ASSIGNING <fs_wa>.

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
                              im_v_filename = CONV #( iv_filename )
                              im_t_content  = lt_content ).

    ELSEIF iv_id_carpeta = gc_id_carpeta_e .

      " Mover el Archivo Plano de las Carpetas ERRORES(E)
      go_mgr_file->copy_file( iv_id_carpeta = gc_id_carpeta_e
                              im_v_filename = CONV #( iv_filename )
                              im_t_content  = lt_content ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.