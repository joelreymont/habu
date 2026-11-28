\ server.f - the PostgreSQL server of the SPARQL tests.
\ A test file run by test/db/pg-cluster.f gives MAIN its name and its body.
\ MAIN connects by the file's one script argument, the cluster's conninfo,
\ runs the body on that connection and closes it; without the argument it
\ dies naming the harness. RUN-SQL, RUN, RUN-TEXT and COMPILE are the
\ body's.
\ STORAGE CLASS. PROCESS-WIDE: one connection.

require src/core/exit-status.f
require src/lib/test/assert.f
require src/lib/arena.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/fs.f
require src/lib/json.f
require src/lib/pg.f
require src/lib/sparql.f
require src/lib/json/value.f

package SPARQL-TEST
private

TYPED-VARIABLE CONN PG:connection

\ A failed script shows PostgreSQL's message and SQLSTATE.
: SCRIPT-OK ( PG:result -- PG:result )
   PG:OUTCOME MATCH PG:outcome
      ok OF true ENDOF
      rows OF true ENDOF
      failed OF type cr type cr false ENDOF
   ;MATCH TTRUE ;

: FAILURE+ ( str str ^VEC:vector<u8> -- )
   {: state:str msg:str out:^ :}
   state out VEC:APPEND s" : " out VEC:APPEND msg out VEC:APPEND ;

: KEEP ( ^VEC:vector<u8> PG:result -- bool PG:result )
   >r {: out:^ :} r>
   PG:OUTCOME MATCH PG:outcome
      ok OF false ENDOF
      rows OF 0 PG:>ROW 0 PG:>COL PG:TEXT$ out VEC:APPEND true ENDOF
      failed OF out FAILURE+ false ENDOF
   ;MATCH swap ;

: CONNECT ( -- )
   1 4 PG:CONFIGURE
   0 SCRIPT-ARGV$ s" " PG:CONNECT MATCH PG:connect-result
      connected OF ENDOF
      rejected OF type cr PG:E-CONNECT throw ENDOF
   ;MATCH CONN ! ;

: SERVE ( [ -- ] -- )
   CONNECT CONN @ s" SET enable_hashagg = off" [: SCRIPT-OK ;] PG:WITH-SCRIPT
   execute CONN @ PG:CLOSE ;

public

: TEXT-VEC ( ^ARENA:arena -- ^VEC:vector<u8> )
   ARENA:ALLOCATOR 0 [type] u8 VEC:NEW ;

: $ ( ^VEC:vector<u8> -- str ) VEC:SPAN SPAN:$ ;

private

: SCRIPT-RUN ( str ^ARENA:arena -- )
   {: path:str a:^ :}
   a TEXT-VEC {: text:^ :}
   path text FS:READ-FILE+ TTRUE
   CONN @ text $ [: SCRIPT-OK ;] PG:WITH-SCRIPT ;

public

: RUN-SQL ( str -- ) {: path:str :}
   path T-LABEL
   [: path swap SCRIPT-RUN ;] ARENA:WITH ;

\ Runs a statement with the parameters staged; true when it answered a
\ row. The first row's one text, or the failure as `SQLSTATE: message`,
\ goes to the vector.
: RUN ( str ^VEC:vector<u8> -- bool )
   {: sql:str out:^ :}
   out CONN @ sql [: KEEP ;] PG:WITH-RESULT ;

\ RUN with one text parameter.
: RUN-TEXT ( str str ^VEC:vector<u8> -- bool )
   {: sql:str par:str out:^ :}
   CONN @ PG:PARAMS CONN @ par PG:TEXT+
   sql out RUN ;

\ The query's statement on the description, or its rejection text, goes to
\ the vector; true with its parameters staged.
: COMPILE ( str str ^VEC:vector<u8> -- bool )
   CONN @ PG:PARAMS
   [: CONN @ swap PG:TEXT+ ;] SPARQL:COMPILE ;

: HAS? ( JSON:value str -- bool )
   JSON:FIND MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

private

: USAGE ( str ^ARENA:arena -- ) {: name:str a:^ :}
   a TEXT-VEC {: out:^ :}
   name out VEC:APPEND
   s" : no conninfo argument; test/db/pg-cluster.f runs this file"
   out VEC:APPEND
   out $ EXIT-STATUS:USAGE die ;

public

: MAIN ( str [ -- ] -- ) {: name:str body :}
   SCRIPT-ARGC 1 <> if name [: USAGE ;] ARENA:WITH then
   body SERVE ;

;package
