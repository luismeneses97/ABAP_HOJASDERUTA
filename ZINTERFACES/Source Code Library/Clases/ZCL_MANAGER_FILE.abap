class ZCL_MANAGER_FILE definition
  public
  final
  create public .

public section.

  types:
    ty_table_file_contents TYPE TABLE OF thcs_string .

  data GR_PAISES type ZINTF_TT_PAISES .
  constants GC_INTERFACE_SOP007 type ZDE_ID_INTEFAZ value 'SOP007' ##NO_TEXT.
  constants GC_INTERFACE_SOP008 type ZDE_ID_INTEFAZ value 'SOP008' ##NO_TEXT.
  constants GC_INTERFACE_SOP012 type ZDE_ID_INTEFAZ value 'SOP012' ##NO_TEXT.
  constants GC_INTERFACE_SOP012_C type ZDE_ID_INTEFAZ value 'SOP012_C' ##NO_TEXT.
  constants GC_INTERFACE_SOP012_P type ZDE_ID_INTEFAZ value 'SOP012_P' ##NO_TEXT.
  constants GC_INTERFACE_SOP013 type ZDE_ID_INTEFAZ value 'SOP013' ##NO_TEXT.
  constants GC_INTERFACE_SOP011 type ZDE_ID_INTEFAZ value 'SOP011' ##NO_TEXT.
  data GV_NOMBRE_INTERFACE type ZDE_ID_INTEFAZ .
  class-data M_O_MAIN type ref to ZCL_MANAGER_FILE .
  constants GC_REGEX_SOP013 type STRING value '^MA-\w{4}-\d{8}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.
  constants GC_REGEX_SOP007 type STRING value '^SO-\w{4}-\d{8}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.
  constants GC_REGEX_SOP008 type STRING value '^PO-\w{4}-\d{4}-\d{8}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.
  constants GC_REGEX_SOP012_CLIENT type STRING value '^CL_\d{2}.\d{2}.\d{4}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.
  constants GC_REGEX_SOP012_SUPPLIER type STRING value '^PR_\d{2}.\d{2}.\d{4}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.
  constants GC_REGEX_SOP011 type STRING value '^[CarguecargueCARGUE]{6}_\w{4}_\d{3}_\d{8}_[A-Z]{2}.[txtTXT]{3}$' ##NO_TEXT.  "Cargue_US01_561_25102024.TXT
  constants GC_REGEX_HOJAS_RUTA1 type STRING value '^[CABECERA]{8}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA2 type STRING value '^[MATERIAL]{8}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA3 type STRING value '^[SECUENCIAS]{10}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA4 type STRING value '^[OPERACIONES]{11}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA5 type STRING value '^[COMPONENTES]{11}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA6 type STRING value '^[MAF]{3}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA7 type STRING value '^[CARACTERÍSTICAS]{15}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  constants GC_REGEX_HOJAS_RUTA7_2 type STRING value '^[CARACTERISTICAS]{15}.[txtTXTTxt]{3}$' ##NO_TEXT.  "
  data GO_MONITOR type ref to ZCL_INTF_MONITOR .
  data GO_EXC_DIR type ref to CX_HCS_DIRECTORY_ACCESS_ERROR .
  data GV_NOMBRE_ARCHIVO type FILE_NAME .
  data GV_PATH_ENTRADA type STRING .
  data GV_PATH_PROCESADOS type STRING .
  data GV_PATH_ERRORES type STRING .
  data GT_PAISES type ZINTF_TT_PAISES .

  methods CONSTRUCTOR
    importing
      !IV_MODULE type UFPS_POSID optional
      !IV_PROGNAME type PROGNAME optional
      !IV_COUNTRY type LAND1 optional
    raising
      ZCX_MANAGER_FILE
      CX_STATIC_CHECK .
  methods GET_PATHS
    raising
      ZCX_MANAGER_FILE
      CX_STATIC_CHECK .
  methods READ
    importing
      !IM_V_MODULE type UFPS_POSID
      !IM_V_PROGNAME type PROGNAME
      !IM_V_COUNTRY type LAND1
    exporting
      !IM_TI_CONTENIDO_FILE type TY_TABLE_FILE_CONTENTS
    raising
      ZCX_MANAGER_FILE
      CX_HCS_DIRECTORY_ACCESS_ERROR
      CX_STATIC_CHECK .
  methods DELETE_FILE
    importing
      !IM_PATH_ENTRADA type STRING
      !IM_V_FILENAME type STRING
      !IM_T_CONTENT type THCS_STRING optional .
  methods COPY_FILE
    importing
      !IV_ID_CARPETA type ZDE_ID_CARPETA optional
      !IM_V_EXISTE_ERROR type C optional
      !IM_V_FILENAME type STRING
      !IM_T_CONTENT type THCS_STRING .
  methods MOVE_FILE                                                 " Valores posibles '' o X
    importing
      !IV_ID_CARPETA type ZDE_ID_CARPETA optional
      !IM_V_EXISTE_ERROR type C optional
      !IM_V_FILENAME type STRING
      !IM_T_CONTENT type THCS_STRING .
  class-methods INSTANTIATE
    importing
      !IV_MODULE type UFPS_POSID optional
      !IV_PROGNAME type PROGNAME optional
      !IV_COUNTRY type LAND1 optional
    returning
      value(RO_INST) type ref to ZCL_MANAGER_FILE
    raising
      ZCX_MANAGER_FILE
      CX_STATIC_CHECK .
protected section.
private section.

  data GV_COUNTRY type LAND1 .
  data GT_FILE type THCS_FILE .
  data GV_MODULE type UFPS_POSID .
  data GV_PROGNAME type PROGNAME .
  data GT_BAPIRET type BAPIRET2_T .
  constants LC_ENTRADA type ZID_CONSTANT value 'ENTRADA' ##NO_TEXT.
  constants LC_PROCESADAS type ZID_CONSTANT value 'PROCESADAS' ##NO_TEXT.
  constants LC_ERRORES type ZID_CONSTANT value 'ERRORES' ##NO_TEXT.
  constants LC_PAISES type ZID_CONSTANT value 'PAISES' ##NO_TEXT.
  constants LC_NOMBRE_ARCHIVO type ZID_CONSTANT value 'NOMBRE_ARCHIVO' ##NO_TEXT.
  constants GC_FILE_NAME_SOP008 type ZID_RANGE value 'FILE_NAME_SOP008' ##NO_TEXT.

  methods FILTER_COUNTRIES
    changing
      !CT_FILE type THCS_FILE .
  methods SET_REGEX
    importing
      !IM_V_PROGNAME type PROGNAME
    raising
      ZCX_MANAGER_FILE .
  methods GET_FILES
    raising
      ZCX_MANAGER_FILE
      CX_HCS_DIRECTORY_ACCESS_ERROR .
  methods FILTER_FILES
    raising
      ZCX_MANAGER_FILE .
  methods READ_FILE
    exporting
      !IM_TI_CONTENIDO type TY_TABLE_FILE_CONTENTS
    raising
      ZCX_MANAGER_FILE .
ENDCLASS.



CLASS ZCL_MANAGER_FILE IMPLEMENTATION.


  METHOD instantiate.

    IF m_o_main IS NOT BOUND.

      CREATE OBJECT m_o_main
        EXPORTING
          iv_module   = iv_module
          iv_progname = iv_progname
          iv_country  = iv_country.

*      m_o_main = NEW #( ).

    ENDIF.

    ro_inst = m_o_main.

  ENDMETHOD.


  METHOD filter_files.

    TRY.

        " Validar si el Nombre del Archivo está bien formado
        me->set_regex( EXPORTING im_v_progname = me->gv_progname ).

      CATCH zcx_manager_file INTO DATA(go_cx_root).

        DATA(gv_s_message) = go_cx_root->get_text( ).
        RAISE EXCEPTION go_cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_files.

    DATA l_exc_dir TYPE REF TO cx_hcs_directory_access_error.
    DATA l_text    TYPE string.
    DATA lt_file_ok   TYPE  thcs_file.

    TRY.
        REFRESH gt_file.

        IF gv_path_entrada IS INITIAL.
          RETURN.
        ENDIF.

        TRY.

            " Leer el Archivo Plano desde el Servidor de Aplicación
            cl_hcs_directory_access=>get_directory_content( EXPORTING i_name  = gv_path_entrada
                                                            IMPORTING e_files = gt_file ).

            " Filtra o Descarta los Archivos de los Paises que no se desean Procesar
            IF gv_progname NE 'ZHOJAS_RUTA'.
             me->filter_countries( CHANGING ct_file = gt_file ).
            ENDIF.

          CATCH cx_hcs_directory_access_error INTO go_exc_dir.

            " Obtener el Mensaje de Error
            l_text = go_exc_dir->get_text( ).

            " No se encontraron archivos en la ruta &1
            go_monitor->set_registers_single_error( EXPORTING iv_id_intefaz      = CONV #( gv_progname+1(6) ) " Nombre de la Interface
                                                              iv_mensaje_proceso = CONV #( l_text ) " Texto de mensaje
                                                              iv_nombre_archivo  = 'NA'             " Nombre del fichero
                                                              iv_estado_error    = zintf_gc_rojo    " Estado de la Interfaz
                                                              iv_num_mensaje     = '000' ).         " Número de mensaje

            RAISE EXCEPTION go_exc_dir.
        ENDTRY.

        IF gt_file IS INITIAL.

          " No se encontraron archivos en la ruta &1
          go_monitor->set_registers_single_error( EXPORTING iv_id_intefaz      = CONV #( gv_progname+1(6) ) " Nombre de la Interface
                                                            iv_mensaje_proceso = go_monitor->get_message_public( EXPORTING iv_msgnr = '004'                       " Número de mensaje
                                                                                                                           token1   = CONV #( gv_path_entrada ) ) " Texto de mensaje
                                                            iv_nombre_archivo  = 'NA'             " Nombre del fichero
                                                            iv_estado_error    = zintf_gc_rojo    " Estado de la Interfaz
                                                            iv_num_mensaje     = '004' ).         " Número de mensaje

          RAISE EXCEPTION TYPE zcx_manager_file
            EXPORTING
              textid = zcx_manager_file=>zcx_ruta_not_found
              token1 = CONV #( gv_path_entrada ).

        ENDIF.

      CATCH zcx_manager_file INTO DATA(go_cx_root).
        RAISE EXCEPTION go_cx_root.

      CATCH cx_hcs_directory_access_error INTO go_exc_dir.
        RAISE EXCEPTION go_exc_dir.

    ENDTRY.

  ENDMETHOD.


  METHOD get_paths.

    TYPES ty_paises TYPE RANGE OF zde_paises.


    ".. Se consulta la ruta de los archivos respuestas

    TRY.

        "..Ruta de Entrada
        NEW zcl_amdp_bc_params( )->get_constant( EXPORTING im_progname = gv_progname       "Id desarrollo
                                                            im_constant = lc_entrada        "Nombre constante
                                                            im_modulo   = gv_module         "Módulo
                                                  IMPORTING et_constant = DATA(lt_tconstant_entrada) ).

        READ TABLE lt_tconstant_entrada INTO DATA(wa_tconstant_entrada) INDEX 1.

        gv_path_entrada = wa_tconstant_entrada-value.

        "..Ruta de Procesados
        NEW zcl_amdp_bc_params( )->get_constant( EXPORTING im_progname = gv_progname      " Id desarrollo
                                                           im_constant = lc_procesadas    " Nombre constante
                                                           im_modulo   = gv_module        " Módulo
                                                 IMPORTING et_constant = DATA(lt_tconstant_procesadas) ).

        READ TABLE lt_tconstant_procesadas INTO DATA(wa_tconstant_procesadas) INDEX 1.

        gv_path_procesados = wa_tconstant_procesadas-value.

        "..Ruta de Errores
        NEW zcl_amdp_bc_params( )->get_constant( EXPORTING im_progname = gv_progname      " Id desarrollo
                                                           im_constant = lc_errores       " Nombre constante
                                                           im_modulo   = gv_module        " Módulo
                                                 IMPORTING et_constant = DATA(lt_tconstant_errores) ).

        READ TABLE lt_tconstant_errores INTO DATA(wa_tconstant_errores) INDEX 1.

        gv_path_errores = wa_tconstant_errores-value.


        zcl_amdp_bc_params=>get_range( EXPORTING im_progname = gv_progname      " Id desarrollo
                                                 im_range    = lc_paises        " Rango de Países
                                                 im_modulo   = gv_module        " Módulo
                                       IMPORTING ex_range    = DATA(lt_rango_paises) ).

        " Tabla de Sufijos de Países
        gt_paises = VALUE ty_paises( FOR ls_pais IN lt_rango_paises
                                             ( sign = 'I' option = 'EQ' low = ls_pais-low ) ).

      CATCH cx_static_check INTO DATA(go_CX_STATIC_CHECK).
        RAISE EXCEPTION go_CX_STATIC_CHECK.
      CATCH cx_root INTO DATA(go_cx_root).

        DATA(gv_s_message) = go_cx_root->get_text( ).
        RAISE EXCEPTION go_cx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD move_file.

    DATA lv_code_page TYPE cpcodepage.

    CASE iv_id_carpeta.
      WHEN 'N'. " Entrada (Nuevos)

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path  = gv_path_entrada
                                                  iv_filename      = im_v_filename
                                                  iv_code_page     = lv_code_page
                                                  it_content       = im_t_content
                                       CHANGING   ct_return        = gt_bapiret
                                       EXCEPTIONS file_write_error = 1 ).

      WHEN 'P'. " Procesados

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path  = gv_path_procesados
                                                  iv_filename      = im_v_filename
                                                  iv_code_page     = lv_code_page
                                                  it_content       = im_t_content
                                       CHANGING   ct_return        = gt_bapiret
                                       EXCEPTIONS file_write_error = 1 ).

      WHEN 'E'. " Errores

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path          = gv_path_errores
                                                          iv_filename      = im_v_filename
                                                          iv_code_page     = lv_code_page
                                                          it_content       = im_t_content
                                               CHANGING   ct_return        = gt_bapiret
                                               EXCEPTIONS file_write_error = 1 ).

      WHEN 'D'. " Eliminar Archivos

        me->delete_file( EXPORTING im_path_entrada = gv_path_entrada
                                   im_v_filename   = im_v_filename ).
      WHEN OTHERS.

        IF im_v_existe_error IS INITIAL.

          cl_bnk_file_communication=>write_file( EXPORTING  iv_path          = gv_path_procesados
                                                            iv_filename      = im_v_filename
                                                            iv_code_page     = lv_code_page
                                                            it_content       = im_t_content
                                                 CHANGING   ct_return        = gt_bapiret
                                                 EXCEPTIONS file_write_error = 1 ).
        ELSE.

          cl_bnk_file_communication=>write_file( EXPORTING  iv_path          = gv_path_errores
                                                            iv_filename      = im_v_filename
                                                            iv_code_page     = lv_code_page
                                                            it_content       = im_t_content
                                                 CHANGING   ct_return        = gt_bapiret
                                                 EXCEPTIONS file_write_error = 1 ).

        ENDIF.

    ENDCASE.

    IF sy-subrc = 0.

      " Una vez movido el Archivo de la Carpeta de Entrada a Procesados o Errores lo Elimina
      cl_bnk_file_communication=>delete_file( EXPORTING  iv_path           = gv_path_entrada
                                                         iv_filename       = im_v_filename
                                              EXCEPTIONS file_delete_error = 1
                                                         OTHERS            = 2 ).
    ENDIF.

    IF gt_bapiret IS NOT INITIAL.

      " Construir LOG para el Monitor
      go_monitor->set_registers_multiple_errors( EXPORTING iv_id_intefaz     = CONV #( gv_progname+1(6) ) " Nombre de la Interface
                                                           iv_nombre_archivo = CONV #( im_v_filename )    " Nombre del fichero
                                                           iv_estado_error   = zintf_gc_rojo              " Estado de la Interfaz
                                                           iv_comentarios    = 'N/A'
                                                           it_bapiret2_tab   = gt_bapiret  ).             " Tabla con Mensajes de error

    ENDIF.

  ENDMETHOD.


  METHOD read_file.
    DATA lti_table_file_contents TYPE TABLE OF thcs_string.
    DATA lti_file_contents       TYPE thcs_string.
    DATA lv_code_page            TYPE cpcodepage.

    TRY.

        LOOP AT gt_file INTO DATA(ls_file).

          zcl_bnk_file_communication=>read_file( EXPORTING  iv_path         = gv_path_entrada
                                                           iv_filename     = ls_file-name
                                                           iv_code_page    = lv_code_page
                                                IMPORTING  et_content      = lti_file_contents
                                                CHANGING   ct_return       = gt_bapiret
                                                EXCEPTIONS file_read_error = 1 ).

          IF sy-subrc = 1 OR lti_file_contents IS INITIAL.

            RAISE EXCEPTION NEW zcx_manager_file( textid = zcx_manager_file=>zcx_content_bad
                                                  token1 = CONV #( gv_path_entrada ) ).

          ENDIF.
          IF lti_file_contents IS NOT INITIAL.
            " Eliminamos filas en blanco
*            DELETE lti_file_contents WHERE table_line+0(9)  = '\t\t\t\t\'.

            INSERT ls_file-name INTO lti_file_contents INDEX 1.
            APPEND lti_file_contents TO lti_table_file_contents.
          ENDIF.
          CLEAR lti_file_contents.
        ENDLOOP.

        im_ti_contenido = lti_table_file_contents.

      CATCH zcx_manager_file INTO DATA(go_cx_root).

        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(gv_s_message) = go_cx_root->get_text( ).

        " Error Leyendo Archivo: &1
        go_monitor->set_registers_single_error(
            iv_id_intefaz      = CONV #( gv_progname+1(8) ) " Nombre de la Interface
            iv_mensaje_proceso = go_monitor->get_message_public( iv_msgnr = '009'                    " Número de mensaje
                                                                 token1   = CONV #( ls_file-name ) ) " Texto de mensaje
            iv_nombre_archivo  = CONV #( ls_file-name ) " Nombre del fichero
            iv_estado_error    = zintf_gc_rojo          " Estado de la Interfaz
            iv_num_mensaje     = '009' ).               " Número de mensaje

        " Eliminamos archivo de carpeta de entrada
        move_file( iv_id_carpeta = 'E'
                   im_v_filename = CONV #( ls_file-name )
                   im_t_content  = lti_file_contents ).

        RAISE EXCEPTION go_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD read.

    TRY.

"
        gv_module   = im_v_module.
        gv_progname = im_v_progname.
        gv_country  = im_v_country.

        "..Obtenemos las rutas: Entrada, Procesados y Errores
        get_paths( ).

        "..Obtenemos los archivos de la ruta de entrada
        get_files( ).

        "..Filtramos los archivos por nombre y formato
        filter_files( ).

        "..Leemos en una tabla interna la informacion de los archivos
        read_file( IMPORTING im_ti_contenido = im_ti_contenido_file ).

      CATCH zcx_manager_file INTO DATA(go_cx_root).

        DATA(gv_s_message) = go_cx_root->get_text( ).
        RAISE EXCEPTION go_cx_root.

      CATCH cx_hcs_directory_access_error INTO go_exc_dir.
        RAISE EXCEPTION go_exc_dir.

      CATCH cx_static_check INTO DATA(go_CX_STATIC_CHECK).
        RAISE EXCEPTION go_CX_STATIC_CHECK.

    ENDTRY.

  ENDMETHOD.


  METHOD set_regex.
    DATA lt_filtered_files TYPE thcs_file.
    DATA lv_regex TYPE STRING.
    DATA lv_filename TYPE STRING.
    DATA lv_file_valido TYPE abap_bool.
    DATA lt_regex TYPE TABLE OF string.
    DATA result_tab type match_result_tab.

    TRY.

        " Asignar el Patrón de Busqueda y Validación
        CASE im_v_progname.
          WHEN 'ZSOP007'.

            " Patrón Regular Expresion p.SOP007
            lt_regex = VALUE #( ( gc_regex_sop007 ) ).

          WHEN 'ZSOP013'.

            " Patrón Regular Expresion p.SOP013
            lt_regex = VALUE #( ( gc_regex_sop013 ) ).

          WHEN 'ZSOP008'.

            " Patrón Regular Expresion p.SOP008
            lt_regex = VALUE #( ( gc_regex_sop008 ) ).
          WHEN 'ZSOP012_C'.

            " Patrón Regular Expresion p.SOP012
            lt_regex = VALUE #( ( gc_regex_sop012_client ) ).

          WHEN 'ZSOP012_P'.

            " Patrón Regular Expresion p.SOP012
            lt_regex = VALUE #( ( gc_regex_sop012_supplier ) ).
          WHEN 'ZSOP004'.

          " Patrón Regular Expresion p.SOP004
          WHEN 'ZSOP011'.

            " Patrón Regular Expresion p.SOP011
            lt_regex = VALUE #( ( gc_regex_sop011 ) ).
          WHEN 'ZHOJAS_RUTA'.
            " Patrón Regular Expresion Hojas de ruta
            lt_regex = VALUE #( ( gc_regex_hojas_ruta1 )
                                ( gc_regex_hojas_ruta2 )
                                ( gc_regex_hojas_ruta3 )
                                ( gc_regex_hojas_ruta4 )
                                ( gc_regex_hojas_ruta5 )
                                ( gc_regex_hojas_ruta6 )
                                ( gc_regex_hojas_ruta7 )
                                ( gc_regex_hojas_ruta7_2 ) ).
          WHEN OTHERS.
        ENDCASE.

        LOOP AT gt_file ASSIGNING FIELD-SYMBOL(<fs_file>).

          lv_file_valido = abap_false.
          lv_filename = to_upper( <fs_file>-name ).
          " Validar con el Patrón de Busqueda REGEX

            LOOP AT lt_regex INTO lv_regex.
              FIND ALL OCCURRENCES OF PCRE lv_regex IN lv_filename
                   MATCH COUNT sy-tabix
                   RESULTS result_tab.
              IF sy-tabix <> 0.
                lv_file_valido = abap_true.
                EXIT. " Si ya es válido, salir del LOOP
              ENDIF.
            ENDLOOP.

          IF lv_file_valido = abap_false.

            " Si el archivo es de clientes o proveedores no debe generar error
            IF    lv_regex = gc_regex_sop012_client
               OR lv_regex = gc_regex_sop012_supplier.

              FIND ALL OCCURRENCES OF PCRE gc_regex_sop012_client IN lv_filename
                   MATCH COUNT  sy-tabix
                   RESULTS DATA(result_tab_c).
              IF sy-tabix NE 0.
                continue.
              ENDIF.

              FIND ALL OCCURRENCES OF PCRE gc_regex_sop012_supplier IN lv_filename
                   MATCH COUNT  sy-tabix
                   RESULTS DATA(result_tab_p).
              IF sy-tabix NE 0.
                continue.
              ENDIF.

            ENDIF.

            " Nombre de Archivo mal formado &1
            go_monitor->set_registers_single_error(
                iv_id_intefaz      = CONV #( gv_progname+1(8) ) " Nombre de la Interface
                iv_mensaje_proceso = go_monitor->get_message_public( iv_msgnr = '001'                       " Número de mensaje
                                                                     token1   = CONV #( <fs_file>-name ) ) " Texto de mensaje
                iv_nombre_archivo  = CONV #( <fs_file>-name )  " Nombre del fichero
                iv_estado_error    = zintf_gc_rojo             " Estado de la Interfaz
                iv_num_mensaje     = '001' ).                  " Número de mensaje

            " Eliminar el Archivo de la Carpeta
            me->delete_file( im_path_entrada = gv_path_entrada   " Ruta donde está el Archivo
                             im_v_filename   = <fs_file>-name ). " Nombre del Archivo a Borrar

          ELSE.

            " Guardar los Registros con Nombres bien formados
            APPEND <fs_file> TO lt_filtered_files.
          ENDIF.

        ENDLOOP.

        IF lt_filtered_files IS NOT INITIAL.

          " Si hubo algun archivo con nombre mal formado se elimina
          " del proceso reescribiendo la tabla GT_FILE
          sort lt_filtered_files by name.
          gt_file = lt_filtered_files.

        ENDIF.

      CATCH zcx_manager_file INTO DATA(go_cx_root).

        DATA(gv_s_message) = go_cx_root->get_text( ).
        RAISE EXCEPTION go_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.

    " Asignar los Valores Variables del Proceso
    gv_module   = iv_module.
    gv_progname = iv_progname.
    gv_country  = iv_country.

    " Instanciar la Clase del Monitor
    go_monitor = zcl_intf_monitor=>get_instance( ).

    "..Obtenemos las rutas: Entrada, Procesados y Errores
    get_paths( ).

  ENDMETHOD.


  METHOD delete_file.

    CHECK im_path_entrada IS NOT INITIAL AND
          im_v_filename   IS NOT INITIAL.

    " Eliminar un Archivo del Servidor
    cl_bnk_file_communication=>delete_file( EXPORTING  iv_path           = im_path_entrada
                                                       iv_filename       = im_v_filename
                                            EXCEPTIONS file_delete_error = 1
                                                       OTHERS            = 2 ).
    IF gv_progname = 'ZHOJAS_RUTA'.
      " Plantilla de hojas de ruta procesadas
      MESSAGE s040(zintf) DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.


    IF sy-subrc = 0.
      DATA(lv_message) = |Archivo: { im_v_filename } borrado.|.
      MESSAGE i000(zintf) WITH lv_message DISPLAY LIKE 'S'.
    ELSE.
      lv_message = |Arch.: { im_v_filename } no eliminado|.
      MESSAGE i000(zintf) WITH lv_message DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD copy_file.

    DATA lv_code_page TYPE cpcodepage.

    CASE iv_id_carpeta.
      WHEN 'N'. " Entrada (Nuevos)

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path  = gv_path_entrada
                                                  iv_filename      = im_v_filename
                                                  iv_code_page     = lv_code_page
                                                  it_content       = im_t_content
                                       CHANGING   ct_return        = gt_bapiret
                                       EXCEPTIONS file_write_error = 1 ).

      WHEN 'P'. " Procesados

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path  = gv_path_procesados
                                                  iv_filename      = im_v_filename
                                                  iv_code_page     = lv_code_page
                                                  it_content       = im_t_content
                                       CHANGING   ct_return        = gt_bapiret
                                       EXCEPTIONS file_write_error = 1 ).

      WHEN 'E'. " Errores

        cl_bnk_file_communication=>write_file( EXPORTING  iv_path          = gv_path_errores
                                                          iv_filename      = im_v_filename
                                                          iv_code_page     = lv_code_page
                                                          it_content       = im_t_content
                                               CHANGING   ct_return        = gt_bapiret
                                               EXCEPTIONS file_write_error = 1 ).

      WHEN 'D'. " Eliminar Archivos

        me->delete_file( EXPORTING im_path_entrada = gv_path_entrada
                                   im_v_filename   = im_v_filename ).
      WHEN OTHERS.

    ENDCASE.

    IF gt_bapiret IS NOT INITIAL.

      " Construir LOG para el Monitor
      go_monitor->set_registers_multiple_errors( EXPORTING iv_id_intefaz     = CONV #( gv_progname+1(6) ) " Nombre de la Interface
                                                           iv_nombre_archivo = CONV #( im_v_filename )    " Nombre del fichero
                                                           iv_estado_error   = zintf_gc_rojo              " Estado de la Interfaz
                                                           iv_comentarios    = 'N/A'
                                                           it_bapiret2_tab   = gt_bapiret  ).             " Tabla con Mensajes de error

    ENDIF.

  ENDMETHOD.


  METHOD filter_countries.

    DATA lt_file_ok   TYPE  thcs_file.

    DATA(lt_file) = ct_file.

    me->gv_nombre_interface = CONV #( gv_progname+1(8) ). " Nombre de la Interface

    " Valida si la Interface aplica descartar Archivos por Países
    CHECK me->gv_nombre_interface = me->gc_interface_sop007   OR
          me->gv_nombre_interface = me->gc_interface_sop008   OR
          me->gv_nombre_interface = me->gc_interface_sop012_c OR
          me->gv_nombre_interface = me->gc_interface_sop012_p OR
          me->gv_nombre_interface = me->gc_interface_sop011   OR
          me->gv_nombre_interface = me->gc_interface_sop013   .


    " Recorrer los Códigos de Países Seleccionados
    LOOP AT me->gr_paises ASSIGNING FIELD-SYMBOL(<fs_paises>).

      " Crear el Patrón de Busqueda del Código del País
      DATA(lv_cod_pais) = |_{ <fs_paises>-low }|.

      " Recorrer la Lista de Nombres de Archivos
      LOOP AT lt_file ASSIGNING FIELD-SYMBOL(<fs_file>).
        DATA(lv_tabix) = sy-tabix.

        " Buscar el Patrón del Código de País
        FIND FIRST OCCURRENCE OF lv_cod_pais IN <fs_file>-name MATCH OFFSET DATA(lv_offset).
        IF sy-subrc = 0 AND lv_offset > 0.

          " Si el País lo encuentra en la lista Guardarlo como Necesario
          APPEND INITIAL LINE TO lt_file_ok ASSIGNING FIELD-SYMBOL(<fs_file_ok>).
          <fs_file_ok> = <fs_file>.

          " Salir y buscar un nuevo País y elimina los Archivos de los Países
          " que se necesitan de esta Lista para no volver a evaluarlos
          DELETE lt_file INDEX lv_tabix.

          CLEAR lv_offset.
          EXIT.
        ELSE.

          " Verificar el Siguiente Archivo
          CLEAR lv_offset.
          CONTINUE.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    " Copiar la Lista de Archivos Seleccionados
    ct_file = lt_file_ok.

    FREE: lt_file,
          lt_file_ok.

  ENDMETHOD.
ENDCLASS.