REPORT zewf_coa_2019_02_part1.

PARAMETERS p_iseq TYPE string.

INITIALIZATION.

  p_iseq = '1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,2,23,10,27,1,6,27,31,1,31,6,35,2,35,10,39,1,39,5,43,2,6,43,47,2,47,10,51,1,'
         && '51,6,55,1,55,6,59,1,9,59,63,1,63,9,67,1,67,6,71,2,71,13,75,1,75,5,79,1,79,9,83,2,6,83,87,1,87,5,91,2,6,91,95,1,95,9,99,2,6,99,'
         && '103,1,5,103,107,1,6,107,111,1,111,10,115,2,115,13,119,1,119,6,123,1,123,2,127,1,127,5,0,99,2,14,0,0'.


START-OF-SELECTION.

  SPLIT p_iseq AT ',' INTO TABLE DATA(tseq).

  DATA idx TYPE i.
  data nxt type i.

  LOOP AT tseq ASSIGNING FIELD-SYMBOL(<seq>).
    idx = sy-tabix.
    if nxt > 0.
    check idx = nxt.
    endif.
    CASE <seq>.
      WHEN 1.
        DATA(pos1) = tseq[ idx + 1 ] + 1.
        DATA(pos2) = tseq[ idx + 2 ] + 1.
        DATA(pos3) = tseq[ idx + 3 ] + 1.
        DATA(rslt) = tseq[ pos1 ] + tseq[ pos2 ].
        tseq[ pos3 ] = rslt.
        nxt = idx + 4.
      WHEN 2.
        pos1 = tseq[ idx + 1 ] + 1.
        pos2 = tseq[ idx + 2 ] + 1.
        pos3 = tseq[ idx + 3 ] + 1.
        rslt = tseq[ pos1 ] * tseq[ pos2 ].
        tseq[ pos3 ] = rslt.
        nxt = idx + 4.
      WHEN 99.
        EXIT. "from loop.
    ENDCASE.
  ENDLOOP.

  LOOP AT tseq INTO DATA(seq).
    WRITE: seq NO-GAP, ',' NO-GAP.
  ENDLOOP.
