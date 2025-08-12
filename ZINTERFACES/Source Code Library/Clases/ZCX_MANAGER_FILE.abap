class ZCX_MANAGER_FILE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  constants:
    begin of ZCX_FILE_NAME_BAD,
      msgid type symsgid value 'ZINTF',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'TOKEN1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FILE_NAME_BAD .
  constants:
    begin of ZCX_RUTA_NOT_FOUND,
      msgid type symsgid value 'ZINTF',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'TOKEN1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RUTA_NOT_FOUND .
  constants:
    begin of ZCX_CANCEL_USER,
      msgid type symsgid value 'ZINTF',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CANCEL_USER .
  constants:
    begin of ZCX_CONTENT_BAD,
      msgid type symsgid value 'ZINTF',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CONTENT_BAD .
  data TOKEN1 type STRING .
  data TOKEN2 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TOKEN1 type STRING optional
      !TOKEN2 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MANAGER_FILE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TOKEN1 = TOKEN1 .
me->TOKEN2 = TOKEN2 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.