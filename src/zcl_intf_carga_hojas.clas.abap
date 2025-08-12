CLASS zcl_intf_carga_hojas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Métodos públicos
    METHODS: upload_file IMPORTING im_v_path TYPE file_name.

    " Método Singleton para obtener la instancia de la clase
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_intf_carga_hojas.

    " Metodo para mostrar el contenido del directorio en SAP
    METHODS display_directory.


  PRIVATE SECTION.
    " Atributos privados
    DATA gv_path        TYPE string.
    DATA gv_server_path TYPE string.
    DATA gt_file        TYPE thcs_file.

    CONSTANTS gc_progname    TYPE progname VALUE 'ZHOJAS_RUTA'.
    CONSTANTS gc_module      TYPE ufps_posid VALUE 'INTF'.
    CONSTANTS lc_entrada TYPE zid_constant VALUE 'ENTRADA' ##NO_TEXT.
    CONSTANTS gc_regex_hojas_ruta1   TYPE string VALUE '^[CABECERA]{8}.[txtTXTTxt]{3}$' ##NO_TEXT.         "
    CONSTANTS gc_regex_hojas_ruta2   TYPE string VALUE '^[MATERIAL]{8}.[txtTXTTxt]{3}$' ##NO_TEXT.         "
    CONSTANTS gc_regex_hojas_ruta3   TYPE string VALUE '^[SECUENCIAS]{10}.[txtTXTTxt]{3}$' ##NO_TEXT.      "
    CONSTANTS gc_regex_hojas_ruta4   TYPE string VALUE '^[OPERACIONES]{11}.[txtTXTTxt]{3}$' ##NO_TEXT.     "
    CONSTANTS gc_regex_hojas_ruta5   TYPE string VALUE '^[COMPONENTES]{11}.[txtTXTTxt]{3}$' ##NO_TEXT.     "
    CONSTANTS gc_regex_hojas_ruta6   TYPE string VALUE '^[MAF]{3}.[txtTXTTxt]{3}$' ##NO_TEXT.              "
    CONSTANTS gc_regex_hojas_ruta7   TYPE string VALUE '^[CARACTERÍSTICAS]{15}.[txtTXTTxt]{3}$' ##NO_TEXT. "
    CONSTANTS gc_regex_hojas_ruta7_2 TYPE string VALUE '^[CARACTERISTICAS]{15}.[txtTXTTxt]{3}$' ##NO_TEXT. "


    " Atributo de clase para almacenar la única instancia (Singleton)
    CLASS-DATA instance TYPE REF TO zcl_intf_carga_hojas.

    " Métodos privados
    METHODS get_file_path.

    METHODS read_file IMPORTING iv_path           TYPE string
                      RETURNING VALUE(rt_content) TYPE thcs_string.

    METHODS save_to_al11 IMPORTING iv_content     TYPE thcs_string
                                   iv_server_path TYPE string.

    METHODS get_server_path.


    METHODS get_directory_content.

    METHODS set_regex
      IMPORTING
        !im_v_filename TYPE string
      EXPORTING
        !ex_v_error    TYPE c.



ENDCLASS.



CLASS ZCL_INTF_CARGA_HOJAS IMPLEMENTATION.


  METHOD get_instance.
    " Si la instancia no existe, crearla
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.
    " Retornar la instancia única de la clase
    ro_instance = instance.
  ENDMETHOD.


  METHOD upload_file.
    DATA lt_content TYPE TABLE OF string.

        " Obtener la ruta del archivo
        gv_path = im_v_path.
        IF gv_path IS INITIAL.
          MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 034.
          RETURN.
        ENDIF.
        get_server_path( ).
        IF gv_server_path IS INITIAL.
          RETURN.
        ENDIF.
        " Leer el contenido del archivo
        lt_content = read_file( gv_path ).

        " Guardar el contenido en AL11
        save_to_al11( iv_content     = lt_content
                      iv_server_path = gv_server_path ).

        MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 026 WITH gv_server_path.

  ENDMETHOD.


  METHOD get_file_path.
    DATA lv_filename TYPE string.
    DATA lt_files    TYPE filetable.
    DATA lv_rc       TYPE i.
    CONSTANTS lc_formato TYPE string VALUE '*.txt'.
    DATA lv_titulo TYPE string.
    lv_titulo = TEXT-001.
    " Llamar al cuadro de diálogo para seleccionar el archivo
    cl_gui_frontend_services=>file_open_dialog( EXPORTING window_title      = lv_titulo
                                                          default_extension = lc_formato
                                                CHANGING  file_table        = lt_files
                                                          rc                = lv_rc ).

    " Si el usuario selecciona un archivo, asigna la ruta
    IF lv_rc > 0.
      READ TABLE lt_files INTO lv_filename INDEX 1.
      gv_path = lv_filename.
    ELSE.
      gv_path = ''.
    ENDIF.
  ENDMETHOD.


  METHOD read_file.
    DATA lt_raw TYPE TABLE OF string.

    " Leer el archivo usando la clase cl_gui_frontend_services
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename        = iv_path
                                                     filetype        = 'ASC'
                                          CHANGING   data_tab        = lt_raw
                                          EXCEPTIONS file_open_error = 1
                                                     file_read_error = 2
                                                     OTHERS          = 3 ).

    IF sy-subrc <> 0.

      MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 030.
      RETURN.
    ENDIF.

    rt_content = lt_raw.
  ENDMETHOD.


  METHOD save_to_al11.
    DATA lv_line TYPE string.

    " Abrir el archivo en el servidor para escribir en él
    OPEN DATASET iv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 031.
      RETURN.
    ENDIF.

    " Escribir el contenido línea por línea
    LOOP AT iv_content INTO lv_line.
      TRANSFER lv_line TO iv_server_path.
    ENDLOOP.

    " Cerrar el archivo después de escribir
    CLOSE DATASET iv_server_path.
  ENDMETHOD.


  METHOD get_server_path.
    DATA lv_filename    TYPE string.
    DATA lv_server_path TYPE string.
    DATA lv_error TYPE c.

    ".. Se consulta la ruta de los archivos respuestas
    TRY.

        "..Ruta de Entrada
        NEW zcl_amdp_bc_params( )->get_constant( EXPORTING im_progname = gc_progname       " Id desarrollo
                                                           im_constant = lc_entrada        " Nombre constante
                                                           im_modulo   = gc_module         " Módulo
                                                 IMPORTING et_constant = DATA(lt_tconstant_entrada) ).

        READ TABLE lt_tconstant_entrada INTO DATA(wa_tconstant_entrada) INDEX 1.

        lv_server_path = wa_tconstant_entrada-value.
        " Extraer el nombre del archivo desde gv_path (ruta completa)
        FIND REGEX '[^\\]+$' IN gv_path MATCH OFFSET DATA(lv_offset) MATCH LENGTH DATA(lv_length).

       IF sy-subrc = 0.

          lv_filename = gv_path+lv_offset(lv_length).
          ".. Se valida nombre de archivo
          set_regex(
            EXPORTING
              im_v_filename = lv_filename
           IMPORTING
              ex_v_error    = lv_error
          ).

          IF lv_error = abap_true.
            RETURN.
          ENDIF.

        ELSE.
          WRITE: TEXT-001.
        ENDIF.

        gv_server_path = lv_server_path && '/' && lv_filename.

      CATCH zcx_manager_file INTO DATA(go_cx_root).
        "Error en la lectura de la ruta
        MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 029.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_directory_content.
    " Leer los archivos existentes en el Servidor de Aplicación
    TRY.

        IF gv_server_path IS NOT INITIAL.
          CLEAR gt_file.
          cl_hcs_directory_access=>get_directory_content( EXPORTING i_name  = gv_server_path
                                                          IMPORTING e_files = gt_file ).
        ENDIF.

      CATCH cx_hcs_directory_access_error INTO DATA(lo_error).
        MESSAGE lo_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD display_directory.
    TYPES: BEGIN OF ty_data,
             number TYPE i,
             text   TYPE string,

           END OF ty_data.

    TYPES lty_file TYPE TABLE OF ty_data.

    DATA gt_table TYPE REF TO cl_salv_table.
    DATA lt_file  TYPE lty_file.
    DATA ls_file  TYPE ty_data.
    TRY.
        get_server_path( ).
        get_directory_content( ).

        SORT gt_file BY name.
        LOOP AT gt_file ASSIGNING FIELD-SYMBOL(<fs_file>).
          ls_file-text   = <fs_file>-name.
          ls_file-number = sy-tabix.
          APPEND ls_file TO lt_file.
        ENDLOOP.

        cl_salv_table=>factory( IMPORTING r_salv_table = gt_table
                                CHANGING  t_table      = lt_file  ).

        " Todas las funciones activas
        DATA lcl_functions TYPE REF TO cl_salv_functions_list.
        lcl_functions = gt_table->get_functions( ).
        lcl_functions->set_all( abap_true ).

        " Columnas
        DATA lcl_columns TYPE REF TO cl_salv_columns_table.
        lcl_columns = gt_table->get_columns( ).
        lcl_columns->set_optimize( abap_true ).
        " lcl_columns->set_exception_column( value = 'STAT' ).

        " Layout
        DATA lcl_layout TYPE REF TO cl_salv_layout.
        lcl_layout = gt_table->get_layout( ).
        lcl_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
        DATA lv_repid TYPE salv_s_layout_key.
        lv_repid-report = sy-repid.
        lcl_layout->set_key( lv_repid ).
        lcl_layout->set_default( |X| ).

        DATA lr_selections TYPE REF TO cl_salv_selections.
        lr_selections = gt_table->get_selections( ).
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

        gt_table->display( ).

      CATCH cx_salv_msg INTO DATA(lo_error).
        MESSAGE lo_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD set_regex.
    DATA lt_filtered_files TYPE thcs_file.
    DATA lv_regex TYPE STRING.
    DATA lt_regex TYPE TABLE OF string.
    DATA lv_file_valido TYPE abap_bool.
    DATA lv_filename type string.

    TRY.

        " Patrón Regular Expresion
        lt_regex = VALUE #( ( gc_regex_hojas_ruta1 )
                                ( gc_regex_hojas_ruta2 )
                                ( gc_regex_hojas_ruta3 )
                                ( gc_regex_hojas_ruta4 )
                                ( gc_regex_hojas_ruta5 )
                                ( gc_regex_hojas_ruta6 )
                                ( gc_regex_hojas_ruta7 )
                                ( gc_regex_hojas_ruta7_2 ) ).

        " Validar con el Patrón de Busqueda REGEX
        lv_filename = to_upper( im_v_filename ).

        LOOP AT lt_regex INTO lv_regex.
          FIND ALL OCCURRENCES OF PCRE lv_regex IN lv_filename
               MATCH COUNT  sy-tabix
               RESULTS DATA(result_tab).

          IF sy-tabix <> 0.
            lv_file_valido = abap_true.
            EXIT. " Si ya es válido, salir del LOOP
          ENDIF.
        ENDLOOP.

        IF lv_file_valido = abap_false.

          MESSAGE ID 'ZINTF' TYPE 'I' NUMBER 033.
          ex_v_error = abap_true.
          RETURN.

        ENDIF.

      CATCH cx_root.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
