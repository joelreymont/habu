\ main.f - the fixture's last file. Its top level stores a quotation into its
\ own cell and 5 into count.f's counter; RUN bumps twice, to 7, prints Hi when
\ the count is 7 and ok when DOUBLE agrees.

package FILE-OBJECTS-MAIN
private
variable Q
CAST: XT ( n -- [ -- ] )
: SET-Q ( -- ) [: FILE-OBJECTS-COUNT:BUMP ;] Q xt! ;
SET-Q
5 FILE-OBJECTS-COUNT:TALLY !
public
: RUN ( -- )
   FILE-OBJECTS-SHAPE:BUMP  Q @ XT execute
   FILE-OBJECTS-COUNT:TALLY @ 7 = if FILE-OBJECTS-COUNT:HI then
   21 FILE-OBJECTS-SHAPE:DOUBLE 42 = if ." ok" cr then ;
;package
