REPORT zewf_coa_2019_create_packages.

PARAMETERS p_devc TYPE c LENGTH 20 DEFAULT '$ADVENTOFCODE2019'.
PARAMETERS p_txt1 TYPE c LENGTH 40 LOWER CASE DEFAULT 'Advent Of Code 2019'.
PARAMETERS p_txt2 TYPE c LENGTH 40 LOWER CASE DEFAULT 'Day $'.
PARAMETERS p_days TYPE i DEFAULT 24.
PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.

DATA text     TYPE c LENGTH 80.
DATA devclass TYPE trdevclass.
DATA changed  TYPE c LENGTH 1.
DATA name TYPE devclass.
DATA numc TYPE c LENGTH 2.

PERFORM create_devclass USING p_devc p_txt1.

DO p_days TIMES.

  numc = sy-index.
  name = |{ p_devc }_DAY{ sy-index WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
  text = |{ p_txt1 } - { p_txt2 }|.
  REPLACE '$' WITH numc INTO text.

  PERFORM create_devclass USING name text.
ENDDO.

FORM create_devclass USING name text.

  IF p_test = abap_true.
    WRITE: / name, text.
    RETURN.
  ENDIF.

  devclass-devclass  = name.
  devclass-ctext     = text.
  devclass-as4user   = sy-uname.
  devclass-pdevclass = ''.
  devclass-dlvunit   = 'LOCAL'.
  devclass-component = space.
  devclass-comp_appr = space.
  devclass-comp_text = space.
  devclass-korrflag  = 'X'.
  devclass-namespace = space.
  devclass-parentcl   = p_devc.
  devclass-tpclass    = space.
  devclass-type       = 'N'.
  devclass-target     = space.
  devclass-packtype   = space.
  devclass-restricted = space.
  devclass-mainpack   = space.
  devclass-created_by = sy-uname.
  devclass-created_on = sy-datum.


  CALL FUNCTION 'TRINT_MODIFY_DEVCLASS'
    EXPORTING
      iv_action            = 'CREA'
      iv_dialog            = space
      is_devclass          = devclass
      iv_request           = space
    IMPORTING
      es_devclass          = devclass
      ev_something_changed = changed
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc > 0.
    MESSAGE i000(oo) WITH 'Error creating package' name.
    STOP.
  ELSE.
    WRITE: / 'Pacakge created:', name, text.
  ENDIF.
ENDFORM.                    "create_devclass
