REPORT zewf_coa_2019_03_turtle.

SELECTION-SCREEN COMMENT /1(80) TEXT-001.
SELECTION-SCREEN COMMENT /1(80) TEXT-002.
SELECTION-SCREEN COMMENT /1(80) TEXT-003.
SELECTION-SCREEN COMMENT /1(80) TEXT-004.

PARAMETERS p TYPE i DEFAULT 1.

CASE p.
  WHEN 1.
    DATA(wp1) = 'R1000,U573,L25,U468,L833,D867,R515,D941,L513,D1,L380,U335,L661,D725,L506,U365,L103,D987,L425,U756,R129,D153,R326,U297,L456,'
             && 'D632,L142,U666,R864,D255,R85,D661,L566,D125,R445,U293,R295,D14,R181,D772,R376,U151,L146,D344,L947,D519,L455,D232,L873,U617,'
             && 'R143,D600,R654,D14,R813,U176,L443,U712,R230,U629,L554,U886,L931,D591,R716,U904,R605,D176,R801,U911,L746,D316,R30,U240,R975,'
             && 'D929,L879,U295,L56,U662,R429,U117,R282,D716,R57,D445,L7,D486,R147,D991,R750,D252,R134,U43,L410,D757,R252,U595,R986,U978,L883,'
             && 'D664,R267,D718,R28,U727,R926,U395,L81,D70,L67,D92,R209,D633,L253,D798,R820,U816,R754,U646,R846,D863,L868,U911,L678,D893,R686,'
             && 'D466,L153,D884,L589,U960,L924,U603,R93,D518,L291,D324,L67,D40,R722,U384,R195,D916,R64,D666,R896,D860,R388,D833,L662,D192,'
             && 'R567,U551,L558,U11,L674,U19,L669,U110,R681,D882,L997,U535,R683,U313,L904,U674,L476,D969,L464,D342,R574,D981,R405,D352,R431,'
             && 'U429,L329,D160,L573,U978,R930,U683,R592,D877,L88,D512,R676,U436,R708,U187,L664,U614,L734,D480,L242,U489,R732,U876,L416,D524,'
             && 'R181,U846,L396,D974,L620,D282,L124,D206,R119,U179,L171,D528,R469,U516,L708,D599,R913,U63,R922,D300,L856,U700,L396,D185,R933,'
             && 'D453,L234,D385,R426,D189,L25,U599,L715,U355,L574,D857,R662,D504,R746,U386,R389,U751,R85,U499,R255,D150,R998,U804,L832,D642,'
             && 'R102,U202,R972,U312,L265,D484,R314,D591,L250,U791,L120,D536,L808,D972,L808,D46,L626,D284,R60,D155,L849,D501,L206,U445,L765,'
             && 'U770,L67,U780,R876,D409,R603,U713,L459,D81,L294,D471,R656,U603,R55,D650,L211,D333,L44,D168,L187,D52,R60,D574,R54'.

    DATA(wp2) = 'L1004,U110,R738,D383,R606,U840,L123,D756,L234,D585,R475,U429,L585,D615,L859,D669,L812,U672,L415,D114,L538,D899,R444,D379,L886,'
             && 'D276,R268,D90,R200,D247,L704,D802,L10,U313,R437,D854,R899,U21,L553,D352,L736,U604,R162,D504,R509,D471,R501,D472,L117,U796,L828,'
             && 'U906,R450,U697,R831,D302,R879,U730,R381,U788,L654,U927,R971,D355,L712,D959,L104,D169,L297,U898,R82,D673,R21,D608,L813,U754,'
             && 'L554,U239,L1,U834,R456,D671,L692,D855,L784,U664,R832,U446,L673,D898,R146,U507,L934,D569,R249,D755,L212,D475,R970,U122,R418,'
             && 'U820,L754,U313,L843,D608,R165,D881,L293,U628,R492,D37,L120,U659,L471,D275,R790,U372,L736,U318,L353,U439,L669,U18,R683,U768,'
             && 'R518,U300,L478,U601,R14,U233,L33,U765,L910,U591,R304,D528,R637,D376,L704,U27,L226,U384,R870,U318,L975,U876,R576,U500,R880,D108,'
             && 'L670,U171,R561,U873,L391,U717,L455,D909,L34,U211,R919,U376,L228,D632,L91,U408,R354,U454,L81,D547,L624,U464,R480,D630,L596,D57,'
             && 'L206,U736,R255,U185,L236,U705,L221,D511,L461,U718,R351,D59,L142,U236,R623,D124,R736,D758,L368,D605,L417,U990,R228,D207,L792,'
             && 'U150,L353,U612,R269,D459,L855,U808,L852,U168,R838,D794,R478,U281,L453,D134,L643,D862,L299,D590,L570,D782,L294,U935,R835,U849,'
             && 'R842,U997,R890,U20,L370,D157,R89,U203,L243,U71,R987,D812,R595,U664,L926,D359,L915,D382,R190,D443,R360,U253,R230,D879,L606,D755,'
             && 'R859,U232,R771,U465,R858,D823,R405,D499,L737,U846,R241,D976,R415,U541,L746,D569,L563,D410,L409,D39,R117,U638,R824,D215,R232,'
             && 'U578,R790,U535,R873,D477,R805,U94,L313,U570,L500,U783,L556,U663,L335,U152,L524,D583,L462,U710,R741,U641,L135'.

  WHEN 2.
    wp1 = 'R75,D30,R83,U83,L12,D49,R71,U7,L72'.
    wp2 = 'U62,R66,U55,R34,D71,R55,D58,R83'.
  WHEN 3.
    wp1 = 'R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51'.
    wp2 = 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7'.
  WHEN 4.
    wp1 = 'R8,U5,L5,D3'.
    wp2 = 'U7,R6,D4,L4'.
ENDCASE.

CLASS aoc DEFINITION.
  PUBLIC SECTION.
    METHODS go IMPORTING path TYPE string.
    METHODS show.
    METHODS constructor.
  PRIVATE SECTION.
    METHODS do_moves IMPORTING path TYPE string.
    METHODS calc_moves IMPORTING path TYPE string.
    METHODS move
      IMPORTING
        cmd TYPE clike.
    METHODS calc_move
      IMPORTING
        cmd TYPE clike.

    DATA turtle TYPE REF TO zcl_turtle.
    DATA run TYPE i.
    TYPES: BEGIN OF _gridsize,
             run   TYPE i,
             up    TYPE i,
             down  TYPE i,
             left  TYPE i,
             right TYPE i,
           END OF _gridsize.
    DATA gridsize TYPE _gridsize.
    DATA gridsizes TYPE SORTED TABLE OF _gridsize WITH UNIQUE KEY run.
    TYPES: BEGIN OF _pos,
             x TYPE i,
             y TYPE i,
           END OF _pos.
    DATA turtlepos TYPE _pos.

ENDCLASS.

CLASS aoc IMPLEMENTATION.

  METHOD constructor.

    turtle = zcl_turtle=>new( title = 'Advent Of Code 2019 - Day 3' ).
    turtle->set_pen( pen = VALUE #( stroke_width = 2 ) ).

  ENDMETHOD.

  METHOD go.

    ADD 1 TO run.
    CLEAR gridsize.

    CASE run.
      WHEN 1.
        calc_moves( path ).
        turtle->set_position( position = VALUE #( x = turtlepos-x y = turtlepos-y ) ).
        turtle->set_color_scheme( VALUE #( ( `#0000ff` ) ) ). "blue
      WHEN 2.
        turtle->set_position( position = VALUE #( x = turtlepos-x y = turtlepos-y ) ).
        turtle->set_color_scheme( VALUE #( ( `#ff0000` ) ) ). "red
    ENDCASE.

    do_moves( path ).

  ENDMETHOD.

  METHOD do_moves.

    SPLIT path AT ',' INTO TABLE DATA(moves).

    LOOP AT moves INTO DATA(cmd).
      move( cmd ).
    ENDLOOP.

  ENDMETHOD.

  METHOD calc_moves.

    SPLIT path AT ',' INTO TABLE DATA(moves).

    LOOP AT moves INTO DATA(cmd).
      calc_move( cmd ).
    ENDLOOP.

    gridsize-run = run.
    INSERT gridsize INTO TABLE gridsizes.

    DATA(width) = abs( gridsize-left ) + abs( gridsize-right ).
    turtle->set_width( width ).

    DATA(height) = abs( gridsize-up ) + abs( gridsize-down ).
    turtle->set_height( height ).

    turtlepos-x = width / 2.
    turtlepos-y = height / 2.

  ENDMETHOD.

  METHOD show.
    turtle->show( ).
  ENDMETHOD.

  METHOD calc_move.

    DATA(command) = cmd.
    DATA(direction) = command(1).
    DATA(distance)  = CONV i( shift_left( val = command places = 1 ) ).

    CASE direction.
      WHEN 'R'.
        ADD distance TO gridsize-right.
      WHEN 'L'.
        ADD distance TO gridsize-left.
      WHEN 'U'.
        ADD distance TO gridsize-up.
      WHEN 'D'.
        ADD distance TO gridsize-down.
    ENDCASE.

  ENDMETHOD.

  METHOD move.

    DATA(command) = cmd.
    DATA(direction) = command(1).
    DATA(distance)  = CONV i( shift_left( val = command places = 1 ) ).

    CASE direction.
      WHEN 'R'.
        turtle->set_angle( 0 ).
      WHEN 'L'.
        turtle->set_angle( 180 ).
      WHEN 'U'.
        turtle->set_angle( 270 ).
      WHEN 'D'.
        turtle->set_angle( 90 ).
    ENDCASE.
    turtle->forward( distance ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(aoc3) = NEW aoc( ).
  aoc3->go( wp1 ).
  aoc3->go( wp2 ).
  aoc3->show( ).
