class ZCL_BNK_FILE_COMMUNICATION definition
  public
  final
  create public .

public section.

  class-data GV_LOG_HANDLE type BALLOGHNDL .
  class-data GV_BAL_LOG type CHAR1 .
  constants GV_BALNREXT type BALNREXT value 'Application Log PAIN002' ##NO_TEXT.

  class-methods ADD_MESSAGE
    importing
      !I_GUID type GUID_16 optional
    changing
      !CT_RETURN type BAPIRET2_T .
  class-methods APPL_LOG
    importing
      !I_PROBCLASS type BAL_S_MSG-PROBCLASS optional
    changing
      !CT_RETURN type BAPIRET2_T optional .
  class-methods DELETE_FILE
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING
    exceptions
      FILE_DELETE_ERROR .
  class-methods READ_FILE
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING
      !IV_CODE_PAGE type CPCODEPAGE
    exporting
      !ET_CONTENT type THCS_STRING
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      FILE_READ_ERROR .
  class-methods WRITE_FILE
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING
      !IV_CODE_PAGE type CPCODEPAGE
      !IT_CONTENT type THCS_STRING
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      FILE_WRITE_ERROR .
protected section.
private section.

  class-data GV_LINEFEED type XSTRING .

  class-methods DETERMINE_CODE_PAGE
    importing
      !IV_CODE_PAGE_CUST type CPCODEPAGE
    returning
      value(EV_CODE_PAGE) type CPCODEPAGE .
  class-methods DETERMINE_FULL_FILENAME
    importing
      !IV_DIR type STRING
      !IV_FILENAME type STRING
    exporting
      !EV_FULL_FILENAME type STRING
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      FULL_FILENAME_NOT_DETERMINED .
  class-methods SYS_DELETE_FILE
    importing
      !IV_DIR_FILENAME type STRING
    exporting
      !EV_OS_ERROR_MSG type SYMSGV
    exceptions
      AUTHORITY_ERROR
      SYS_FILE_DELETE_ERROR .
  class-methods SYS_READ_FILE
    importing
      !IV_DIR_FILENAME type STRING
      !IV_CODE_PAGE type CPCODEPAGE
    exporting
      !ET_CONTENT type THCS_STRING
      !EV_OS_ERROR_MSG type SYMSGV
    exceptions
      AUTHORITY_ERROR
      CODEPAGE_ERROR
      SYS_FILE_READ_ERROR .
  class-methods SYS_WRITE_FILE
    importing
      !IV_UPD_MODE type CHAR1
      !IV_CODE_PAGE type CPCODEPAGE
      !IV_LINEFEED type XSTRING
      !IV_DIR_FILENAME type STRING
      !IT_CONTENT type THCS_STRING
    exporting
      !EV_OS_ERROR_MSG type SYMSGV
    exceptions
      AUTHORITY_ERROR
      CODEPAGE_ERROR
      SYS_FILE_WRITE_ERROR .
ENDCLASS.



CLASS ZCL_BNK_FILE_COMMUNICATION IMPLEMENTATION.


METHOD ADD_MESSAGE.

  DATA: L_O_LOG      TYPE REF TO CL_BNK_UTIL_LOG,
        L_EXT_NUMBER TYPE BALNREXT,
        L_GUID       TYPE STRING,
        L_WA_RETURN  LIKE LINE OF CT_RETURN,
        L_WA_MESSAGE TYPE BNK_STR_LOG_MESSAGE.

  L_WA_RETURN-TYPE          = SY-MSGTY.
  L_WA_RETURN-ID            = SY-MSGID.
  L_WA_RETURN-NUMBER        = SY-MSGNO.
  L_WA_RETURN-MESSAGE_V1    = SY-MSGV1.
  L_WA_RETURN-MESSAGE_V2    = SY-MSGV2.
  L_WA_RETURN-MESSAGE_V3    = SY-MSGV3.
  L_WA_RETURN-MESSAGE_V4    = SY-MSGV4.
  APPEND L_WA_RETURN TO CT_RETURN.

  L_GUID = I_GUID.
  CONCATENATE SY-UNAME SY-DATUM SY-UZEIT INTO L_EXT_NUMBER.
  CONCATENATE  'MessageProcessingLog' '-' L_EXT_NUMBER INTO L_EXT_NUMBER.
  SET PARAMETER ID 'BALEXT' FIELD L_EXT_NUMBER.

  CALL METHOD CL_BNK_UTIL_LOG=>S_INSTANCE
    IMPORTING
      E_SREF_BNK_UTIL_LOG = L_O_LOG.

  TRY.
    CALL METHOD L_O_LOG->OPEN_LOG
      EXPORTING
        I_LOG_LAYER_TYPE  = CL_BNK_CONSTANTS=>CON_ENT_LAYER
        I_LOG_LAYER_NAME  = CL_BNK_CONSTANTS=>CON_ENT_LAYER_NAME
        I_EXTERNAL_NUMBER = L_EXT_NUMBER.
  ENDTRY.


  L_WA_MESSAGE-MSGTY = L_WA_RETURN-TYPE.
  L_WA_MESSAGE-MSGID = L_WA_RETURN-ID.
  L_WA_MESSAGE-MSGNO = L_WA_RETURN-NUMBER.
  L_WA_MESSAGE-MSGV1 = L_WA_RETURN-MESSAGE_V1.
  L_WA_MESSAGE-MSGV2 = L_WA_RETURN-MESSAGE_V2.
  L_WA_MESSAGE-MSGV3 = L_WA_RETURN-MESSAGE_V3.
  L_WA_MESSAGE-MSGV4 = L_WA_RETURN-MESSAGE_V4.
  TRY.
      CALL METHOD L_O_LOG->ADD_MESSAGE
        EXPORTING
          I_MESSAGE = L_WA_MESSAGE.
    CATCH CX_BNK_LOG .                                  "#EC NO_HANDLER
  ENDTRY.

  TRY.
      CALL METHOD L_O_LOG->CLOSE_LOG.
    CATCH CX_BNK_LOG .                                  "#EC NO_HANDLER
  ENDTRY.


ENDMETHOD.


METHOD APPL_LOG.

  DATA:
    LS_LOG            TYPE BAL_S_LOG,
    LS_MSG            TYPE BAL_S_MSG,
    LS_PAR            TYPE BAL_S_PAR,
    LV_DUMMY          TYPE C,
    LT_BAL_LOGH       TYPE BAL_T_LOGH.

  DATA:
    L_WA_RETURN  LIKE LINE OF CT_RETURN,
    L_WA_MESSAGE TYPE BNK_STR_LOG_MESSAGE.

  L_WA_RETURN-TYPE          = SY-MSGTY.
  L_WA_RETURN-ID            = SY-MSGID.
  L_WA_RETURN-NUMBER        = SY-MSGNO.
  L_WA_RETURN-MESSAGE_V1    = SY-MSGV1.
  L_WA_RETURN-MESSAGE_V2    = SY-MSGV2.
  L_WA_RETURN-MESSAGE_V3    = SY-MSGV3.
  L_WA_RETURN-MESSAGE_V4    = SY-MSGV4.
  APPEND L_WA_RETURN TO CT_RETURN.

  IF GV_BAL_LOG IS INITIAL.

*   Kopfdaten des Log-Eintrags
    LS_LOG-EXTNUMBER = GV_BALNREXT."'Application Log PAIN002'.        "#EC NOTEXT
    LS_LOG-ALUSER    = SY-UNAME.
    LS_LOG-ALPROG    = SY-REPID.
    LS_LOG-OBJECT    = 'FBNK'.                          "#EC NOTEXT
    LS_LOG-SUBOBJECT = 'STATUSREPORT'.                  "#EC NOTEXT

*   Anlegen Log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        I_S_LOG      = LS_LOG
      IMPORTING
        E_LOG_HANDLE = GV_LOG_HANDLE
      EXCEPTIONS
        OTHERS       = 1.

    IF SY-SUBRC = 0.
      GV_BAL_LOG = 'X'.
    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  GV_BAL_LOG = 'X'.

* Meldung hinzufügen
  LS_MSG-MSGTY     = SY-MSGTY.
  LS_MSG-MSGID     = SY-MSGID.
  LS_MSG-MSGNO     = SY-MSGNO.
  LS_MSG-MSGV1     = SY-MSGV1.
  LS_MSG-MSGV2     = SY-MSGV2.
  LS_MSG-MSGV3     = SY-MSGV3.
  LS_MSG-MSGV4     = SY-MSGV4.
  LS_MSG-PROBCLASS = I_PROBCLASS.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      I_S_MSG      = LS_MSG
      I_LOG_HANDLE = GV_LOG_HANDLE
    EXCEPTIONS
      OTHERS       = 1.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Protokoll in Datenbank ablegen
  APPEND GV_LOG_HANDLE TO LT_BAL_LOGH.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      I_T_LOG_HANDLE   = LT_BAL_LOGH
    EXCEPTIONS
      LOG_NOT_FOUND    = 1
      SAVE_NOT_ALLOWED = 2
      NUMBERING_ERROR  = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.  ENDIF.

ENDMETHOD.


METHOD DELETE_FILE.

  DATA: CT_RETURN            TYPE BAPIRET2_T,
        LV_DIR               TYPE STRING,
        LV_DIR_FILENAME      TYPE STRING,
        LV_OS_ERROR_MSG      TYPE SYMSGV,
        LV_FILENAME_MSG      TYPE SYMSGV,
        LV_DUMMY.

  MOVE IV_PATH TO LV_DIR.
  DETERMINE_FULL_FILENAME( EXPORTING
                           IV_DIR           = LV_DIR
                           IV_FILENAME      = IV_FILENAME
                         IMPORTING
                           EV_FULL_FILENAME = LV_DIR_FILENAME
                         CHANGING
                           CT_RETURN = CT_RETURN
                         EXCEPTIONS
                           FULL_FILENAME_NOT_DETERMINED = 1 ).
  IF SY-SUBRC <> 0.
    LV_FILENAME_MSG = IV_FILENAME.
    MESSAGE E040(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
    CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
      CHANGING
        CT_RETURN = CT_RETURN.
    RAISE FILE_DELETE_ERROR.
  ENDIF.

   IF SY-SUBRC = 1.
    RAISE FILE_DELETE_ERROR.
  ENDIF.

*--- delete file
  SYS_DELETE_FILE( EXPORTING
                     IV_DIR_FILENAME        = LV_DIR_FILENAME
                   IMPORTING
                     EV_OS_ERROR_MSG        = LV_OS_ERROR_MSG
                   EXCEPTIONS
                      AUTHORITY_ERROR       = 1
                      SYS_FILE_DELETE_ERROR = 2 ).
  CASE SY-SUBRC.

    WHEN 1.
      LV_FILENAME_MSG = IV_FILENAME.
      MESSAGE E046(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.
      RAISE FILE_DELETE_ERROR.

    WHEN 2.
      IF LV_OS_ERROR_MSG IS NOT INITIAL.
        MESSAGE E036(BNK_COM_INTF) WITH LV_OS_ERROR_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
        CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
          CHANGING
            CT_RETURN = CT_RETURN.
      ENDIF.
      RAISE FILE_DELETE_ERROR.

  ENDCASE.

ENDMETHOD.


METHOD DETERMINE_CODE_PAGE.

  DATA: lv_ucp_param(4) TYPE c.

  CLEAR ev_code_page.

* If the code page is maintained in customizing then it is used.
* If not, the value of  SET/GET parameter UCP assigned to the user is taken.
* This parameter was introduced to handle code pages
* in the report RFEBKA00 (see notes 928965 and 758870).
* It was kept here due to compatibility reasons.

*  IF iv_code_page_cust <> '0000'.
*
*    ev_code_page =  iv_code_page_cust.
*
*  ELSE.

    GET PARAMETER ID 'UCP' FIELD lv_ucp_param.
    IF lv_ucp_param IS NOT INITIAL.
      ev_code_page = lv_ucp_param.
    ENDIF.

*  ENDIF.

ENDMETHOD.


METHOD DETERMINE_FULL_FILENAME.

* Construct fully qualified filename using a directory and a file name.
* Separators (like / or \) between directory and file name are added automatically
* if necessary.

  DATA: LCL_PATH TYPE REF TO CL_FS_PATH,
        LV_DUMMY.

  CLEAR EV_FULL_FILENAME.

  TRY.

      LCL_PATH = CL_FS_PATH=>CREATE( IV_DIR ).
      LCL_PATH->APPEND_PATH_NAME( IV_FILENAME ).
      EV_FULL_FILENAME = LCL_PATH->GET_PATH_NAME( ).

    CATCH CX_SMART_PATH_SYNTAX CX_FS_PATHS_INCOMPATIBLE CX_FS_PATH_ERROR.
      MESSAGE E033(BNK_COM_INTF) INTO LV_DUMMY.         "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.

      RAISE FULL_FILENAME_NOT_DETERMINED.

  ENDTRY.

ENDMETHOD.


METHOD READ_FILE.

  TYPE-POOLS: DSET.

  DATA: LV_DIR              TYPE STRING,
        LV_DIR_FILENAME     TYPE STRING,
        LV_CODE_PAGE        TYPE CPCODEPAGE,
        LV_OS_ERROR_MSG     TYPE SYMSGV,
        LV_FILENAME_MSG     TYPE SYMSGV,
        LV_DUMMY.

* make sure that file content is empty at the beginning

  CLEAR ET_CONTENT.

  MOVE IV_PATH TO LV_DIR.
  DETERMINE_FULL_FILENAME( EXPORTING
                           IV_DIR           = LV_DIR
                           IV_FILENAME      = IV_FILENAME
                         IMPORTING
                           EV_FULL_FILENAME = LV_DIR_FILENAME
                         CHANGING
                           CT_RETURN = CT_RETURN
                         EXCEPTIONS
                           FULL_FILENAME_NOT_DETERMINED = 1 ).
  IF SY-SUBRC <> 0.
    LV_FILENAME_MSG = IV_FILENAME.
    MESSAGE E040(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
    CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
      CHANGING
        CT_RETURN = CT_RETURN.
    RAISE FILE_READ_ERROR.
  ENDIF.

* determine code page

  LV_CODE_PAGE = DETERMINE_CODE_PAGE( IV_CODE_PAGE ).

  SYS_READ_FILE( EXPORTING
                   IV_DIR_FILENAME     = LV_DIR_FILENAME
                   IV_CODE_PAGE        = LV_CODE_PAGE
                 IMPORTING
                   ET_CONTENT          = ET_CONTENT
                   EV_OS_ERROR_MSG     = LV_OS_ERROR_MSG
                 EXCEPTIONS
                   AUTHORITY_ERROR     = 1
                   CODEPAGE_ERROR      = 2
                   SYS_FILE_READ_ERROR = 3 ).

  CASE SY-SUBRC.
    WHEN 1.
      LV_FILENAME_MSG = IV_FILENAME.
      MESSAGE E034(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.
      RAISE FILE_READ_ERROR.
    WHEN 2.
      LV_FILENAME_MSG = IV_FILENAME.
      MESSAGE E035(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.
      RAISE FILE_READ_ERROR.
    WHEN 3.
      IF LV_OS_ERROR_MSG IS NOT INITIAL.
        MESSAGE E036(BNK_COM_INTF) WITH LV_OS_ERROR_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
        CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
          CHANGING
            CT_RETURN = CT_RETURN.
      ENDIF.
      RAISE FILE_READ_ERROR.
  ENDCASE.

ENDMETHOD.


METHOD SYS_DELETE_FILE.

*--- open file

  TRY.

      OPEN DATASET IV_DIR_FILENAME FOR INPUT IN BINARY MODE MESSAGE EV_OS_ERROR_MSG.

      IF SY-SUBRC = 8 OR
         EV_OS_ERROR_MSG IS NOT INITIAL.

        RAISE SYS_FILE_DELETE_ERROR.

      ENDIF.

    CATCH  CX_SY_FILE_OPEN CX_SY_TOO_MANY_FILES.

      RAISE SYS_FILE_DELETE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      RAISE AUTHORITY_ERROR.

  ENDTRY.

*--- delete file

  TRY.

      DELETE DATASET IV_DIR_FILENAME.

      IF SY-SUBRC = 4.
        RAISE SYS_FILE_DELETE_ERROR.
      ENDIF.

    CATCH CX_SY_FILE_OPEN.

      RAISE SYS_FILE_DELETE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      CLOSE DATASET IV_DIR_FILENAME.
      RAISE AUTHORITY_ERROR.

  ENDTRY.

*--- close file

  TRY.
      CLOSE DATASET IV_DIR_FILENAME.
    CATCH  CX_SY_FILE_CLOSE.
      RAISE SYS_FILE_DELETE_ERROR.
  ENDTRY.

ENDMETHOD.


METHOD SYS_READ_FILE.

  DATA: LS_DATASET_ATTR        TYPE DSET_ATTRIBUTES,
        L_FILE_END_REACHED     TYPE BOOLE_D,
        LS_CONTENT             TYPE STRING.

* Read file considering the code page and save the type of linefeed:
* 0A : LF
* 0D0A : CRLF
* 0D25 : CRLF in EBCDIC code page 0100
*
* It is assumed that this method is called only once at the beginnig of file processing,
* so the linefeed will be correct for all subsequent write statements
* which must use the original linefeed.
*
* IF THE USAGE OF THIS METHOD CHANGES, MORE SOPHISTICATED HANDLING FOR
* LINEFEED VARIABLE WILL BE NECESSARY !

* Make sure that the file is closed in case of exception to avoid the exception CX_SY_FILE_OPEN
* in subsequent OPEN DATASET, e.g. in the method SYS_DELETE_FILE!


* open file

  TRY.

      IF IV_CODE_PAGE = '0000'.
        OPEN DATASET IV_DIR_FILENAME FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
                                     "WITH SMART LINEFEED MESSAGE EV_OS_ERROR_MSG.
      ELSE.
        OPEN DATASET IV_DIR_FILENAME FOR INPUT IN LEGACY TEXT MODE CODE PAGE IV_CODE_PAGE
                                     WITH SMART LINEFEED MESSAGE EV_OS_ERROR_MSG.
      ENDIF.

      IF SY-SUBRC = 8 OR
         EV_OS_ERROR_MSG IS NOT INITIAL.
        RAISE SYS_FILE_READ_ERROR.
      ENDIF.

    CATCH  CX_SY_FILE_OPEN  CX_SY_TOO_MANY_FILES.

      RAISE SYS_FILE_READ_ERROR.

    CATCH CX_SY_CODEPAGE_CONVERTER_INIT  CX_SY_CONVERSION_CODEPAGE.

      RAISE CODEPAGE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      RAISE AUTHORITY_ERROR.

  ENDTRY.

* set linefeed

  TRY.

      GET DATASET IV_DIR_FILENAME ATTRIBUTES LS_DATASET_ATTR.
      GV_LINEFEED = LS_DATASET_ATTR-FIXED-LINEFEED.

    CATCH CX_SY_FILE_OPEN_MODE.

      RAISE SYS_FILE_READ_ERROR.

    CATCH CX_SY_FILE_POSITION.

      CLOSE DATASET IV_DIR_FILENAME.
      RAISE SYS_FILE_READ_ERROR.

  ENDTRY.

* read file

  TRY.

      CLEAR L_FILE_END_REACHED.

      WHILE L_FILE_END_REACHED = SPACE.

        READ DATASET IV_DIR_FILENAME INTO LS_CONTENT.
        IF SY-SUBRC = 4.
          L_FILE_END_REACHED = 'X'.
        ELSE.
          APPEND LS_CONTENT TO ET_CONTENT.
        ENDIF.

      ENDWHILE.

    CATCH CX_SY_FILE_OPEN CX_SY_FILE_OPEN_MODE.

      RAISE SYS_FILE_READ_ERROR.

    CATCH  CX_SY_FILE_IO.

      CLOSE DATASET IV_DIR_FILENAME.
      RAISE SYS_FILE_READ_ERROR.

    CATCH CX_SY_CONVERSION_CODEPAGE CX_SY_CODEPAGE_CONVERTER_INIT.

      CLOSE DATASET IV_DIR_FILENAME.
      RAISE CODEPAGE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      CLOSE DATASET IV_DIR_FILENAME.
      RAISE AUTHORITY_ERROR.

  ENDTRY.

* close file
  TRY.
      CLOSE DATASET IV_DIR_FILENAME.
    CATCH  CX_SY_FILE_CLOSE.
      RAISE SYS_FILE_READ_ERROR.
  ENDTRY.

ENDMETHOD.


METHOD SYS_WRITE_FILE.

  DATA: LS_CONTENT      TYPE STRING.

* open file considering the following parameters:
* a) the write mode: overwrite or append
* b) the codepage: yes (-> use correct value) or no
* c) the original linefeed: LF or CRLF (see comments in the method READ_FILE)
*
* Every parameter combination leads to a new OPEN DATASET command

* open file

  TRY.

      IF IV_UPD_MODE = '3' OR IV_UPD_MODE = '4'.

        IF IV_CODE_PAGE = '0000'.

          IF IV_LINEFEED = '0A'.    " overwrite / no codepage / LF
            OPEN DATASET IV_DIR_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                         WITH UNIX LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ELSE.                     " overwrite / no codepage / CRLF
            OPEN DATASET IV_DIR_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                         WITH WINDOWS LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ENDIF.

          IF SY-SUBRC = 8 OR
             EV_OS_ERROR_MSG IS NOT INITIAL.

            RAISE SYS_FILE_WRITE_ERROR.

          ENDIF.

        ELSE.

          IF IV_LINEFEED = '0A'.    " overwrite / codepage / LF
            OPEN DATASET IV_DIR_FILENAME FOR OUTPUT IN LEGACY TEXT MODE CODE PAGE IV_CODE_PAGE
                         WITH UNIX LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ELSE.                     " overwrite / codepage / CRLF
            OPEN DATASET IV_DIR_FILENAME FOR OUTPUT IN LEGACY TEXT MODE CODE PAGE IV_CODE_PAGE
                         WITH WINDOWS LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ENDIF.

          IF SY-SUBRC = 8 OR
             EV_OS_ERROR_MSG IS NOT INITIAL.

            RAISE SYS_FILE_WRITE_ERROR.

          ENDIF.

        ENDIF.

      ELSEIF IV_UPD_MODE = '2'.

        IF IV_CODE_PAGE = '0000'.

          IF IV_LINEFEED = '0A'.    " append / no codepage / LF
            OPEN DATASET IV_DIR_FILENAME FOR APPENDING IN TEXT MODE ENCODING DEFAULT
                         WITH UNIX LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ELSE.                     " append / no codepage / CRLF
            OPEN DATASET IV_DIR_FILENAME FOR APPENDING IN TEXT MODE ENCODING DEFAULT
                         WITH WINDOWS LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ENDIF.

          IF SY-SUBRC = 8 OR
             EV_OS_ERROR_MSG IS NOT INITIAL.

            RAISE SYS_FILE_WRITE_ERROR.

          ENDIF.

        ELSE.

          IF IV_LINEFEED = '0A'.    " append / codepage / LF
            OPEN DATASET IV_DIR_FILENAME FOR APPENDING IN LEGACY TEXT MODE CODE PAGE IV_CODE_PAGE
                         WITH UNIX LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ELSE.                     " append / codepage / CRLF
            OPEN DATASET IV_DIR_FILENAME FOR APPENDING IN LEGACY TEXT MODE CODE PAGE IV_CODE_PAGE
                         WITH WINDOWS LINEFEED MESSAGE EV_OS_ERROR_MSG.
          ENDIF.

          IF SY-SUBRC = 8 OR
             EV_OS_ERROR_MSG IS NOT INITIAL.

            RAISE SYS_FILE_WRITE_ERROR.

          ENDIF.

        ENDIF.

      ENDIF.

    CATCH  CX_SY_FILE_OPEN  CX_SY_TOO_MANY_FILES.

      RAISE SYS_FILE_WRITE_ERROR.

    CATCH CX_SY_CODEPAGE_CONVERTER_INIT  CX_SY_CONVERSION_CODEPAGE.

      RAISE CODEPAGE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      RAISE AUTHORITY_ERROR.

  ENDTRY.

* write file

  TRY.

      LOOP AT IT_CONTENT INTO LS_CONTENT.
        TRANSFER LS_CONTENT TO IV_DIR_FILENAME.
      ENDLOOP.

    CATCH  CX_SY_FILE_IO CX_SY_FILE_OPEN  CX_SY_FILE_OPEN_MODE CX_SY_TOO_MANY_FILES.

      RAISE SYS_FILE_WRITE_ERROR.

    CATCH CX_SY_CODEPAGE_CONVERTER_INIT  CX_SY_CONVERSION_CODEPAGE.

      RAISE CODEPAGE_ERROR.

    CATCH CX_SY_FILE_AUTHORITY.

      RAISE AUTHORITY_ERROR.

  ENDTRY.

* close file

  TRY.
      CLOSE DATASET IV_DIR_FILENAME.
    CATCH  CX_SY_FILE_CLOSE.
      RAISE SYS_FILE_WRITE_ERROR.
  ENDTRY.

ENDMETHOD.


METHOD WRITE_FILE.

  DATA: LV_DIR              TYPE STRING,
        LV_DIR_FILENAME     TYPE STRING,
        LV_CODE_PAGE        TYPE CPCODEPAGE,
        LV_OS_ERROR_MSG     TYPE SYMSGV,
        LV_FILENAME_MSG     TYPE SYMSGV,
        LV_DUMMY.

  MOVE IV_PATH TO LV_DIR.
  DETERMINE_FULL_FILENAME( EXPORTING
                           IV_DIR           = LV_DIR
                           IV_FILENAME      = IV_FILENAME
                         IMPORTING
                           EV_FULL_FILENAME = LV_DIR_FILENAME
                         CHANGING
                           CT_RETURN = CT_RETURN
                         EXCEPTIONS
                           FULL_FILENAME_NOT_DETERMINED = 1 ).
  IF SY-SUBRC <> 0.
    LV_FILENAME_MSG = IV_FILENAME.
    MESSAGE E040(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
    CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
      CHANGING
        CT_RETURN = CT_RETURN.
    RAISE FILE_WRITE_ERROR.
  ENDIF.

* determine code page

  LV_CODE_PAGE = DETERMINE_CODE_PAGE( IV_CODE_PAGE ).

* write file

  SYS_WRITE_FILE( EXPORTING
                    IV_UPD_MODE     = '3'         "overwrite existing file
                    IV_CODE_PAGE    = LV_CODE_PAGE
                    IV_LINEFEED     = GV_LINEFEED
                    IV_DIR_FILENAME = LV_DIR_FILENAME
                    IT_CONTENT      = IT_CONTENT
                  IMPORTING
                    EV_OS_ERROR_MSG = LV_OS_ERROR_MSG
                  EXCEPTIONS
                    AUTHORITY_ERROR      = 1
                    CODEPAGE_ERROR       = 2
                    SYS_FILE_WRITE_ERROR = 3 ).

  CASE SY-SUBRC.
    WHEN 1.
      LV_FILENAME_MSG = IV_FILENAME.
      MESSAGE E043(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.
      RAISE FILE_WRITE_ERROR.
    WHEN 2.
      LV_FILENAME_MSG = IV_FILENAME.
      MESSAGE E044(BNK_COM_INTF) WITH LV_FILENAME_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
      CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
        CHANGING
          CT_RETURN = CT_RETURN.
      RAISE FILE_WRITE_ERROR.
    WHEN 3.
      IF LV_OS_ERROR_MSG IS NOT INITIAL.
        MESSAGE E036(BNK_COM_INTF) WITH LV_OS_ERROR_MSG INTO LV_DUMMY. "#EC MG_PAR_CNT
        CALL METHOD CL_BNK_FILE_COMMUNICATION=>APPL_LOG
          CHANGING
            CT_RETURN = CT_RETURN.
      ENDIF.
      RAISE FILE_WRITE_ERROR.
  ENDCASE.

ENDMETHOD.
ENDCLASS.