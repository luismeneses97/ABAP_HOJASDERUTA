FUNCTION-POOL ZGF_HOJAS_RUTA.                "MESSAGE-ID ..

* INCLUDE LZGF_SOP008_WPD...                 " Local class definition

" Definición de Constantes
CONSTANTS: gc_nomb_interfaz TYPE zde_id_intefaz VALUE 'HOJAS_RUTA'.
" Definición de Objetos de Referencia
DATA go_monitor TYPE REF TO zcl_intf_monitor.
