REPORT zewf_coa_2019_06_part2.

CLASS orbit DEFINITION.
  PUBLIC SECTION.
    TYPES:
      _object TYPE c LENGTH 3,
      BEGIN OF _object_count,
        id  TYPE i,
        obj TYPE _object,
        cnt TYPE i,
      END OF _object_count,
      _objects_count TYPE SORTED TABLE OF _object_count
                      WITH UNIQUE KEY id
                      WITH UNIQUE SORTED KEY object COMPONENTS obj ,
      BEGIN OF _map,
        start TYPE _object,
        stopp TYPE _object,
        end   TYPE abap_bool,
      END OF _map,
      _orbit_map TYPE SORTED TABLE OF _map WITH UNIQUE KEY start stopp
                 WITH NON-UNIQUE SORTED KEY target COMPONENTS stopp.

    DATA orbit_map TYPE _orbit_map.
    DATA objects TYPE SORTED TABLE OF _object WITH UNIQUE KEY table_line.

    DATA predecessors TYPE _objects_count.
    DATA you TYPE _objects_count.
    DATA san TYPE _objects_count.

    DATA input TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    METHODS add
      IMPORTING
        map TYPE string.
    METHODS import_objects.
    METHODS go.
    METHODS calc.
    METHODS result.
    METHODS predecessor
      IMPORTING
        object       TYPE _object
      RETURNING
        VALUE(count) TYPE i.
    METHODS add_predecessors
      IMPORTING
        object         TYPE _object
      RETURNING
        VALUE(objects) TYPE _objects_count.

ENDCLASS.

CLASS orbit IMPLEMENTATION.
  METHOD add.
    SPLIT map AT ')' INTO DATA(start) DATA(stopp).
    INSERT VALUE #( start = start stopp = stopp ) INTO TABLE orbit_map.

    TRY.
        INSERT CONV string( start ) INTO TABLE objects.
      CATCH cx_sy_itab_duplicate_key.
    ENDTRY.
    TRY.
        INSERT CONV string( stopp ) INTO TABLE objects.
      CATCH cx_sy_itab_duplicate_key.
    ENDTRY.
  ENDMETHOD.

  METHOD import_objects.
    DATA lines            TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'ST'
        language = 'E'
        name     = 'ADVENT_OF_CODE_DAY_6_INPUT'
        object   = 'TEXT'
      TABLES
        lines    = lines
      EXCEPTIONS
        OTHERS   = 8.

    LOOP AT lines INTO DATA(line).
      INSERT CONV string( line-tdline ) INTO TABLE input.
    ENDLOOP.

  ENDMETHOD.

  METHOD go.

    LOOP AT input INTO DATA(orbit).
      add( orbit ).
    ENDLOOP.

    calc( ).

  ENDMETHOD.

  METHOD calc.

    LOOP AT objects INTO DATA(object).
      INSERT VALUE #( obj = object cnt = predecessor( object ) ) INTO TABLE predecessors.
    ENDLOOP.

    you = add_predecessors( 'YOU' ).
    san = add_predecessors( 'SAN' ).


  ENDMETHOD.

  METHOD predecessor.

    DATA(current) = object.

    DO.
      READ TABLE orbit_map INTO DATA(map) WITH TABLE KEY target COMPONENTS stopp = current.
      IF sy-subrc = 0.
        current = map-start.
        ADD 1 TO count.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD add_predecessors.

    DATA(current) = object.
    DATA(index)   = 1.

    INSERT VALUE #( id = index obj = current cnt = 0 ) INTO TABLE objects.

    DO.
      READ TABLE orbit_map INTO DATA(map) WITH TABLE KEY target COMPONENTS stopp = current.
      IF sy-subrc = 0.
        ADD 1 TO index.
        current = map-start.
        INSERT VALUE #( id = index obj = current cnt = 0 ) INTO TABLE objects.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD result.

    "delete the way that is equal
    "the entries left are these of the different routes of YOU and SAN

    LOOP AT you ASSIGNING FIELD-SYMBOL(<you>).
      READ TABLE san WITH TABLE KEY object COMPONENTS obj = <you>-obj ASSIGNING FIELD-SYMBOL(<san>).
      if sy-subrc = 0.
        <you>-cnt = 1.
        <san>-cnt = 1.
      ENDIF.
    ENDLOOP.

    delete you where cnt = 1.
    delete san where cnt = 1.

    DATA(result) = lines( you ) + lines( san ) - 2.
    MESSAGE |{ result } steps| TYPE 'I'.



  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  DATA(app) = NEW orbit( ).

  app->import_objects( ).
  app->go( ).
  app->result( ).
