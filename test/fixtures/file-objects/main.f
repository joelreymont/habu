\ main.f - the program's entry, RUN. Its top level stores a quotation into its
\ own cell and 5 into count.f's counter; RUN bumps twice, to 7, prints Hi when
\ the count is 7 and ok when DOUBLE agrees. AT-EDGE answers shape.f's EDGE,
\ which build.f's cell parts from main.f's data. The top level also lays a data
\ cell naming its own Q, one naming count.f's TALLY, a scratch cell and a data
\ cell holding 0; COLORS holds three strings.

package FILE-OBJECTS-MAIN
private
variable Q
CAST: XT ( n -- [ -- ] )
: SET-Q ( -- ) [: FILE-OBJECTS-COUNT:BUMP ;] Q xt! ;
SET-Q
5 FILE-OBJECTS-COUNT:TALLY !
variable TO-Q  Q NULL-PTR - TO-Q !  TO-Q ptr-cell-mark
variable TO-TALLY  FILE-OBJECTS-COUNT:TALLY NULL-PTR - TO-TALLY !  TO-TALLY ptr-cell-mark
variable SCRATCH  7 SCRATCH !  SCRATCH scratch-cell-mark
variable NOWHERE  NOWHERE ptr-cell-mark
: COLORS ( -- ) s" red" type  s" green" type  s" blue" type ;
: AT-EDGE ( -- ptr u8 ) FILE-OBJECTS-SHAPE:EDGE ;
public
: RUN ( -- )
   FILE-OBJECTS-SHAPE:BUMP  Q @ XT execute
   FILE-OBJECTS-COUNT:TALLY @ 7 = if FILE-OBJECTS-COUNT:HI then
   21 FILE-OBJECTS-SHAPE:DOUBLE 42 = if ." ok" cr then ;
;package
