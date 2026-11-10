\ kept-aio.f - its top level starts aio, keeps a ticket in OLD, awaits it and
\ stops; CHECK, which the program runs, starts aio again, mints a ticket over
\ the same record and prints `aio refused` when AWAIT of OLD is E-AIO-STATE,
\ else `aio` and the code AWAIT answered.

require src/lib/error.f
require src/lib/aio.f

package FILE-OBJECTS-KEPT-AIO
private
TYPED-VARIABLE OLD AIO:ticket
TYPED-VARIABLE MINE AIO:ticket

: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

: KEPT-AWAIT ( -- ) OLD @ AIO:AWAIT OUTCOME-DROP ;
: MINE-AWAIT ( -- ) MINE @ AIO:AWAIT OUTCOME-DROP ;

AIO:START
0 >MS AIO:TIMEOUT OLD !
KEPT-AWAIT
AIO:STOP

public
\ When the kept await is accepted it took MINE's record, so MINE's own await
\ is caught and its code printed too.
: CHECK ( -- )
   AIO:START
   0 >MS AIO:TIMEOUT MINE !
   [: KEPT-AWAIT ;] catch {: code:n :}
   code E-AIO-STATE = if
      s" aio refused" type cr
      MINE-AWAIT
   else
      s" aio " type code . cr
      [: MINE-AWAIT ;] catch {: own:n :}
      own 0<> if s" own " type own . cr then
   then
   AIO:STOP ;
;package
