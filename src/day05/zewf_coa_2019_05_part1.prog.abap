REPORT zewf_coa_2019_05_part1.

"unsolved

PARAMETERS p_seq TYPE string.

INITIALIZATION.
 p_seq = `1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,2,23,10,27,1,6,27,31,1,31,6,35,2,35,10,39,1,39,5,43,2,6,43,47,2,47,10,51,1,`
      && `51,6,55,1,55,6,59,1,9,59,63,1,63,9,67,1,67,6,71,2,71,13,75,1,75,5,79,1,79,9,83,2,6,83,87,1,87,5,91,2,6,91,95,1,95,9,99,2,6,99,`
      && `103,1,5,103,107,1,6,107,111,1,111,10,115,2,115,13,119,1,119,6,123,1,123,2,127,1,127,5,0,99,2,14,0,0`.

CLASS aoc2 DEFINITION.
  PUBLIC SECTION.
    TYPES: solution_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    CLASS-METHODS
      solve
        IMPORTING
          sequence        TYPE string
        RETURNING
          VALUE(solution) TYPE solution_table.
ENDCLASS.

START-OF-SELECTION.


  DATA(solution) = aoc2=>solve( p_seq ).
  MESSAGE |solution position "0": { solution[ 1 ] }| TYPE 'I'.


CLASS aoc2 IMPLEMENTATION.
  METHOD solve.
    CHECK sequence IS NOT INITIAL.

    SPLIT sequence AT ',' INTO TABLE DATA(tseq).

    DATA idx TYPE i.
    DATA nxt TYPE i.

    LOOP AT tseq ASSIGNING FIELD-SYMBOL(<seq>).
      idx = sy-tabix.
      IF nxt > 0.
        CHECK idx = nxt.
      ENDIF.
      CASE <seq>.
        WHEN 1.
          DATA(pos1)   = tseq[ idx + 1 ] + 1.
          DATA(pos2)   = tseq[ idx + 2 ] + 1.
          DATA(pos3)   = tseq[ idx + 3 ] + 1.
          DATA(rslt)   = tseq[ pos1 ] + tseq[ pos2 ].
          tseq[ pos3 ] = rslt.
          nxt = idx + 4.
        WHEN 2.
          pos1         = tseq[ idx + 1 ] + 1.
          pos2         = tseq[ idx + 2 ] + 1.
          pos3         = tseq[ idx + 3 ] + 1.
          rslt         = tseq[ pos1 ] * tseq[ pos2 ].
          tseq[ pos3 ] = rslt.
          nxt = idx + 4.
        WHEN 3.
          pos1         = tseq[ idx + 1 ] + 1.
          tseq[ pos2 ] = tseq[ pos1 ].
          nxt = idx + 3.
        when 4.
          pos1         = tseq[ idx + 1 ] + 1.
          tseq[ pos2 ] = tseq[ pos1 ].
          nxt = idx + 3.

        WHEN 99.
          EXIT. "from loop.
      ENDCASE.
    ENDLOOP.

    solution = tseq.

  ENDMETHOD.

ENDCLASS.

CLASS test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
  PRIVATE SECTION.
    METHODS sequence_1 FOR TESTING.
    METHODS sequence_2 FOR TESTING.
    METHODS sequence_3 FOR TESTING.
    METHODS sequence_4 FOR TESTING.

ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD sequence_1.
    DATA(seq) = `1,0,0,0,99`. "1 + 1 = 2
    DATA(sol) = aoc2=>solve( seq ).
    cl_abap_unit_assert=>assert_equals(
        act                  = sol[ 1 ]
        exp                  = 2
        msg                  = 'I expected 2 at line 1!'  ).

  ENDMETHOD.

  METHOD sequence_2.
    DATA(seq) = `2,3,0,3,99`. "3 * 2 = 6
    DATA(sol) = aoc2=>solve( seq ).
    cl_abap_unit_assert=>assert_equals(
        act                  = sol[ 4 ]
        exp                  = 6
        msg                  = 'I expected 6! at line 4'  ).
  ENDMETHOD.

  METHOD sequence_3.
    DATA(seq) = `2,4,4,5,99,0`. "9801
    DATA(sol) = aoc2=>solve( seq ).
    cl_abap_unit_assert=>assert_equals(
        act                  = sol[ 6 ]
        exp                  = 9801
        msg                  = 'I expected 9801 at line 6!'  ).
  ENDMETHOD.

  METHOD sequence_4.
    DATA(seq) = `1,1,1,4,99,5,6,0,99`. "30
    DATA(sol) = aoc2=>solve( seq ).
    cl_abap_unit_assert=>assert_equals(
        act                  = sol[ 1 ]
        exp                  = 30
        msg                  = 'I expected 30 at line 1!'  ).
  ENDMETHOD.

ENDCLASS.
