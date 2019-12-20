REPORT zewf_coa_2019_20_part1.

INTERFACE unit_test.
ENDINTERFACE.

CLASS solver DEFINITION CREATE PUBLIC FRIENDS unit_test.

  PUBLIC SECTION.
    TYPES: _item TYPE c LENGTH 1,
           _row  TYPE string,
           BEGIN OF _line,
             row TYPE _row,
           END OF _line,
           _maze      TYPE STANDARD TABLE OF _line WITH EMPTY KEY,
           _portal_id TYPE c LENGTH 2,
           BEGIN OF _coordinate,
             x TYPE i,
             y TYPE i,
           END OF _coordinate,
           BEGIN OF _portal_entry,
             id  TYPE _portal_id,
             ty  TYPE c LENGTH 1,
             co1 TYPE _coordinate,
             co2 TYPE _coordinate,
           END OF _portal_entry,
           _portals TYPE SORTED TABLE OF _portal_entry
                          WITH UNIQUE KEY id ty
                          WITH NON-UNIQUE SORTED KEY coordinate1 COMPONENTS co1
                          WITH NON-UNIQUE SORTED KEY coordinate2 COMPONENTS co2.


    METHODS set_maze
      IMPORTING
        maze TYPE _maze.
    METHODS solve
      IMPORTING
        start                 TYPE _portal_id
        end                   TYPE _portal_id
      RETURNING
        VALUE(shortest_route) TYPE i.
  PRIVATE SECTION.
    DATA maze TYPE _maze.
    DATA max_size TYPE i.
    DATA portals TYPE _portals.
    METHODS find_portals
      RETURNING
        VALUE(portals) TYPE _portals.
    METHODS get_item
      IMPORTING
        x           TYPE i
        y           TYPE i
      RETURNING
        VALUE(item) TYPE _item.
ENDCLASS.

CLASS helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_maze
      IMPORTING
        id          TYPE i
      RETURNING
        VALUE(maze) TYPE solver=>_maze.
ENDCLASS.

CLASS helper IMPLEMENTATION.
  METHOD get_maze.
    maze = SWITCH #( id
     WHEN 1 THEN VALUE #(
     ( row = `         A           ` )
     ( row = `         A           ` )
     ( row = `  #######.#########  ` )
     ( row = `  #######.........#  ` )
     ( row = `  #######.#######.#  ` )
     ( row = `  #######.#######.#  ` )
     ( row = `  #######.#######.#  ` )
     ( row = `  #####  B    ###.#  ` )
     ( row = `BC...##  C    ###.#  ` )
     ( row = `  ##.##       ###.#  ` )
     ( row = `  ##...DE  F  ###.#  ` )
     ( row = `  #####    G  ###.#  ` )
     ( row = `  #########.#####.#  ` )
     ( row = `DE..#######...###.#  ` )
     ( row = `  #.#########.###.#  ` )
     ( row = `FG..#########.....#  ` )
     ( row = `  ###########.#####  ` )
     ( row = `             Z       ` )
     ( row = `             Z       ` ) )

     WHEN 2 THEN VALUE #(
     ( row = `                   A                ` )
     ( row = `                   A                ` )
     ( row = `  #################.#############   ` )
     ( row = `  #.#...#...................#.#.#   ` )
     ( row = `  #.#.#.###.###.###.#########.#.#   ` )
     ( row = `  #.#.#.......#...#.....#.#.#...#   ` )
     ( row = `  #.#########.###.#####.#.#.###.#   ` )
     ( row = `  #.............#.#.....#.......#   ` )
     ( row = `  ###.###########.###.#####.#.#.#   ` )
     ( row = `  #.....#        A   C    #.#.#.#   ` )
     ( row = `  #######        S   P    #####.#   ` )
     ( row = `  #.#...#                 #......VT ` )
     ( row = `  #.#.#.#                 #.#####   ` )
     ( row = `  #...#.#               YN....#.#   ` )
     ( row = `  #.###.#                 #####.#   ` )
     ( row = `DI....#.#                 #.....#   ` )
     ( row = `  #####.#                 #.###.#   ` )
     ( row = `ZZ......#               QG....#..AS ` )
     ( row = `  ###.###                 #######   ` )
     ( row = `JO..#.#.#                 #.....#   ` )
     ( row = `  #.#.#.#                 ###.#.#   ` )
     ( row = `  #...#..DI             BU....#..LF ` )
     ( row = `  #####.#                 #.#####   ` )
     ( row = `YN......#               VT..#....QG ` )
     ( row = `  #.###.#                 #.###.#   ` )
     ( row = `  #.#...#                 #.....#   ` )
     ( row = `  ###.###    J L     J    #.#.###   ` )
     ( row = `  #.....#    O F     P    #.#...#   ` )
     ( row = `  #.###.#####.#.#####.#####.###.#   ` )
     ( row = `  #...#.#.#...#.....#.....#.#...#   ` )
     ( row = `  #.#####.###.###.#.#.#########.#   ` )
     ( row = `  #...#.#.....#...#.#.#.#.....#.#   ` )
     ( row = `  #.###.#####.###.###.#.#.#######   ` )
     ( row = `  #.#.........#...#.............#   ` )
     ( row = `  #########.###.###.#############   ` )
     ( row = `           B   J   C                ` )
     ( row = `           U   P   P                ` )  )
    ).
  ENDMETHOD.
ENDCLASS.


CLASS solver IMPLEMENTATION.
  METHOD set_maze.
    me->maze = maze.
    LOOP AT maze INTO DATA(row).
      IF max_size < strlen( row-row ).
        max_size = strlen( row-row ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD solve.
    portals = find_portals( ).
  ENDMETHOD.
  METHOD get_item.
    IF x > max_size.
      item = space.
      RETURN.
    ENDIF.

    IF y > lines( maze ).
      item = space.
      RETURN.
    ENDIF.

    item = substring( val = maze[ y ]-row off = x len = 1 ).

  ENDMETHOD.
  METHOD find_portals.

    DATA item1 TYPE c LENGTH 1.
    DATA item2 TYPE c LENGTH 1.

    DATA coor1 TYPE _coordinate.
    DATA coor2 TYPE _coordinate.

    DATA offset TYPE i.
    DATA line TYPE i.
    DATA portal_id TYPE _portal_id.

    TYPES: _ignore TYPE SORTED TABLE OF _coordinate WITH UNIQUE KEY x y.
    DATA ignore TYPE _ignore.


    LOOP AT maze INTO DATA(row).
      ADD 1 TO line.
      DATA(len) = strlen( row-row ).

      offset = 0.
      DO len TIMES.
        READ TABLE ignore WITH TABLE KEY x = offset y = line TRANSPORTING NO FIELDS.
        CHECK sy-subrc > 0.
        item1 = substring( val = row-row off = offset len = 1 ).
        IF item1 CA sy-abcde.
          coor1-x = offset.
          coor1-y = line.
          item2 = get_item( x = offset + 1 y = line ).
          IF item2 CA sy-abcde.
            DATA(item_chk) = get_item( x = offset + 2 y = line ).
          ELSE.
            item2 = get_item( x = offset y = line + 1 ).
            IF item2 CA sy-abcde.
              item_chk = get_item( x = offset  y = line + 1 ).
            ELSE.
              ADD 1 TO offset.
              CONTINUE.
            ENDIF.
          ENDIF.
          portal_id = item1 && item2 .
          READ TABLE portals
          WITH TABLE KEY
            id = portal_id
            ty  = 'I'
          TRANSPORTING NO FIELDS.
          IF sy-subrc > 0.
            INSERT VALUE #( id = portal_id ty = 'I' co1 = coor1 ) INTO TABLE portals.
          ELSE.
            READ TABLE portals
            WITH TABLE KEY
              id = portal_id
              ty  = 'O'
            TRANSPORTING NO FIELDS.
            IF sy-subrc > 0.
              INSERT VALUE #( id = portal_id ty = 'O' co2 = coor2 ) INTO TABLE portals.
            ENDIF.
          ENDIF.
        ENDIF.

        ADD 1 TO offset.

      ENDDO.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS test_get_item DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION .
    INTERFACES unit_test.

  PRIVATE SECTION.
    DATA cut TYPE REF TO solver.
    METHODS setup.
    METHODS test_empty FOR TESTING.
    METHODS test_char FOR TESTING.
    METHODS test_path FOR TESTING.
    METHODS test_wall FOR TESTING.
ENDCLASS.

CLASS test_get_item IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->set_maze( helper=>get_maze( 1 ) ).
  ENDMETHOD.
  METHOD test_empty.
    cl_abap_unit_assert=>assert_equals(
        act = cut->get_item( x = 2 y = 2 )
        exp = space  ).
  ENDMETHOD.
  METHOD test_char.
    cl_abap_unit_assert=>assert_equals(
        act = cut->get_item( x = 1 y = 9 )
        exp = 'C'  ).
  ENDMETHOD.
  METHOD test_path.
    cl_abap_unit_assert=>assert_equals(
        act = cut->get_item( x = 10 y = 4 )
        exp = '.'  ).
  ENDMETHOD.
  METHOD test_wall.
    cl_abap_unit_assert=>assert_equals(
        act = cut->get_item( x = 5 y = 5 )
        exp = '#'  ).
  ENDMETHOD.

ENDCLASS.

CLASS test_portals DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    INTERFACES unit_test.
  PRIVATE SECTION.
    TYPES: BEGIN OF _portal_test,
             id TYPE solver=>_portal_id,
           END OF _portal_test,
           _portals_test TYPE SORTED TABLE OF _portal_test WITH UNIQUE KEY id.
    DATA cut TYPE REF TO solver.
    METHODS setup.
    METHODS check_maze_1 FOR TESTING.
    METHODS check_maze_2 FOR TESTING.
ENDCLASS.


CLASS test_portals IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD check_maze_1.

    cut->set_maze( helper=>get_maze( 1 ) ).
    DATA(portals_found_org) = cut->find_portals( ).

    DATA(portals_found) = VALUE _portals_test(
                              FOR GROUPS id OF entry IN portals_found_org
                                  GROUP BY entry-id ( VALUE #( id = id ) ) ).

    DATA(portals_exist) = VALUE _portals_test(
          ( id = 'AA' )
          ( id = 'BC' )
          ( id = 'DE' )
          ( id = 'FG' )
          ( id = 'ZZ' )  ).

    cl_abap_unit_assert=>assert_equals(
        act = portals_found
        exp = portals_exist  ).

  ENDMETHOD.

  METHOD check_maze_2.

    cut->set_maze( helper=>get_maze( 2 ) ).

    DATA(portals_found) = VALUE _portals_test(
                              FOR GROUPS id OF entry IN cut->find_portals( )
                                  GROUP BY entry-id ( VALUE #( id = id ) ) ).

    DATA(portals_exist) = VALUE _portals_test(
          ( id = 'AA' )
          ( id = 'AS' )
          ( id = 'BU' )
          ( id = 'CP' )
          ( id = 'DI' )
          ( id = 'JO' )
          ( id = 'JP' )
          ( id = 'LF' )
          ( id = 'VT' )
          ( id = 'QG' )
          ( id = 'YN' )
          ( id = 'ZZ' )
         ).

    cl_abap_unit_assert=>assert_equals(
        act = portals_found
        exp = portals_exist  ).

  ENDMETHOD.

ENDCLASS.


CLASS test_maze_1 DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO solver.
    METHODS setup.
    METHODS solve FOR TESTING.
ENDCLASS.

CLASS test_maze_1 IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    cut->set_maze( helper=>get_maze( 1 ) ).
  ENDMETHOD.

  METHOD solve.
    cl_abap_unit_assert=>assert_equals(
        act                  = cut->solve( start = 'AA' end = 'ZZ' )
        exp                  = 26  ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(main_solver) = NEW solver( ).
