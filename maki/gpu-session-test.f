\ gpu-session-test.f - host-only fake GPU session lifecycle matrix.

require lib/test.f
require lib/test/outcome.f
require lib/process-fork.f
require maki/gpu-session.f

package GPU-SESSION-TEST
public

: CLOSE-STDERR ( -- )
   2 close ;

;package

package GPU
private

1 constant FT-OPEN
2 constant FT-INIT
3 constant FT-GET
4 constant FT-RETAIN
5 constant FT-SET
6 constant FT-CREATE
7 constant FT-SYNC
8 constant FT-DESTROY
9 constant FT-RELEASE

64 constant FT-CAP
create FT-OPS FT-CAP cells allot
create FT-ARGS FT-CAP cells allot
variable FT-N

variable FT-OPEN-RC
variable FT-INIT-RC
variable FT-INIT-X
variable FT-GET-RC
variable FT-GET-X
variable FT-RETAIN-RC
variable FT-RETAIN-X
variable FT-CTX
variable FT-SET-I
variable FT-SET-RC
variable FT-SET-X
variable FT-SET-N
variable FT-STREAM
variable FT-CREATE-RC
variable FT-CREATE-X
variable FT-SYNC-RC
variable FT-SYNC-X
variable FT-DESTROY-RC
variable FT-DESTROY-X
variable FT-RELEASE-RC
variable FT-RELEASE-X

17 constant FT-DEV
101 constant FT-CTX0
201 constant FT-STREAM0

: FT-LOG ( n n -- ) {: op:n arg:n :}
   op FT-N @ cells FT-OPS + !
   arg FT-N @ cells FT-ARGS + !
   1 FT-N +! ;

: FT-OP@ ( n -- n )
   cells FT-OPS + @ ;

: FT-ARG@ ( n -- n )
   cells FT-ARGS + @ ;

: FT-LOG-RESET ( -- )
   0 FT-N ! ;

: FT-RESET ( -- )
   FT-LOG-RESET
   0 FT-OPEN-RC !
   0 FT-INIT-RC !
   0 FT-INIT-X !
   0 FT-GET-RC !
   0 FT-GET-X !
   0 FT-RETAIN-RC !
   0 FT-RETAIN-X !
   FT-CTX0 FT-CTX !
   -1 FT-SET-I !
   0 FT-SET-RC !
   0 FT-SET-X !
   0 FT-SET-N !
   FT-STREAM0 FT-STREAM !
   0 FT-CREATE-RC !
   0 FT-CREATE-X !
   0 FT-SYNC-RC !
   0 FT-SYNC-X !
   0 FT-DESTROY-RC !
   0 FT-DESTROY-X !
   0 FT-RELEASE-RC !
   0 FT-RELEASE-X ! ;

: FT-X ( n -- )
   dup 0 <> if throw then drop ;

: FT-FOPEN ( -- )
   FT-OPEN 0 FT-LOG
   FT-OPEN-RC @ dup 0 <> if throw then drop ;

: FT-FINIT ( n -- rc ) {: flags:n :}
   FT-INIT flags FT-LOG
   FT-INIT-X @ FT-X
   FT-INIT-RC @ >RC ;

: FT-FGET ( ptr a idx -- rc ) {: out:ptr idx:idx :}
   FT-GET idx IDX>N FT-LOG
   FT-GET-X @ FT-X
   FT-DEV out !
   FT-GET-RC @ >RC ;

: FT-FRETAIN ( ptr a cuda-dev -- rc ) {: out:ptr dev:cuda-dev :}
   FT-RETAIN dev CUDA-DEV>N FT-LOG
   FT-RETAIN-X @ FT-X
   FT-CTX @ out !
   FT-RETAIN-RC @ >RC ;

: FT-FSET ( cuda-ctx -- rc ) {: ctx:cuda-ctx :}
   FT-SET ctx CUDA-CTX>N FT-LOG
   FT-SET-N @ {: i:n :}
   1 FT-SET-N +!
   i FT-SET-I @ = if
      FT-SET-X @ FT-X
      FT-SET-RC @
   else 0 then >RC ;

: FT-FCREATE ( ptr a n -- rc ) {: out:ptr flags:n :}
   FT-CREATE flags FT-LOG
   FT-CREATE-X @ FT-X
   FT-STREAM @ out !
   1 FT-STREAM +!
   FT-CREATE-RC @ >RC ;

: FT-FSYNC ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   FT-SYNC stream CUDA:STREAM>N FT-LOG
   FT-SYNC-X @ FT-X
   FT-SYNC-RC @ >RC ;

: FT-FDESTROY ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   FT-DESTROY stream CUDA:STREAM>N FT-LOG
   FT-DESTROY-X @ FT-X
   FT-DESTROY-RC @ >RC ;

: FT-FRELEASE ( cuda-dev -- rc ) {: dev:cuda-dev :}
   FT-RELEASE dev CUDA-DEV>N FT-LOG
   FT-RELEASE-X @ FT-X
   FT-RELEASE-RC @ >RC ;

: FT-ON ( -- )
   [: FT-FOPEN ;] MKD:OPEN!
   [: FT-FINIT ;] MKD:CUINIT!
   [: FT-FGET ;] MKD:CUDEVICEGET!
   [: FT-FRETAIN ;] MKD:CTXRETAIN!
   [: FT-FRELEASE ;] MKD:CTXRELEASE!
   [: FT-FSET ;] MKD:CTXSET!
   [: FT-FCREATE ;] MKD:STREAMCREATE!
   [: FT-FSYNC ;] MKD:STREAMSYNC!
   [: FT-FDESTROY ;] MKD:STREAMDESTROY! ;

: FT-OFF ( -- )
   MKD:USE-REAL ;

: FT-CLOSE-ANY ( GPU:session -- )
   CLOSE MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: FT-OPEN-ERR ( n -- ) {: want:n :}
   OPEN MATCH result
      ok OF FT-CLOSE-ANY 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: FT-MUST-OPEN ( -- GPU:session )
   OPEN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: FT-MUST-CLOSE ( GPU:session -- )
   CLOSE MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: FT-CLOSE-ERR ( GPU:session n -- ) {: want:n :}
   CLOSE MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: FT-CLOSE-LOG ( -- )
   FT-N @ 5 T=
   0 FT-OP@ FT-SET T=
   1 FT-OP@ FT-SYNC T=
   2 FT-OP@ FT-DESTROY T=
   3 FT-OP@ FT-SET T=
   3 FT-ARG@ 0 T=
   4 FT-OP@ FT-RELEASE T= ;

: FT-ACQUIRE-ERRORS ( -- )
   FT-RESET 31 FT-OPEN-RC ! 31 FT-OPEN-ERR  FT-N @ 1 T=
   FT-RESET 32 FT-INIT-RC ! 32 FT-OPEN-ERR  FT-N @ 2 T=
   FT-RESET 33 FT-GET-RC ! 33 FT-OPEN-ERR  FT-N @ 3 T=

   FT-RESET 999 FT-CTX ! 34 FT-RETAIN-RC !
   34 FT-OPEN-ERR
   FT-N @ 4 T=
   3 FT-OP@ FT-RETAIN T=

   FT-RESET 0 FT-CTX ! 35 FT-RELEASE-RC !
   E-CUDA FT-OPEN-ERR
   FT-N @ 5 T=
   4 FT-OP@ FT-RELEASE T=

   FT-RESET 0 FT-SET-I ! 36 FT-SET-RC !
   36 FT-OPEN-ERR
   FT-N @ 6 T=
   5 FT-OP@ FT-RELEASE T=

   FT-RESET 999 FT-STREAM ! 37 FT-CREATE-RC !
   1 FT-SET-I ! 38 FT-SET-RC ! 39 FT-RELEASE-RC !
   37 FT-OPEN-ERR
   FT-N @ 8 T=
   6 FT-OP@ FT-SET T=
   6 FT-ARG@ 0 T=
   7 FT-OP@ FT-RELEASE T=

   FT-RESET 0 FT-STREAM !
   E-CUDA FT-OPEN-ERR
   FT-N @ 8 T=
   6 FT-OP@ FT-SET T=
   7 FT-OP@ FT-RELEASE T= ;

: FT-ACQUIRE-THROWS ( -- )
   FT-RESET 61 FT-INIT-X ! 61 FT-OPEN-ERR  FT-N @ 2 T=
   FT-RESET 62 FT-GET-X ! 62 FT-OPEN-ERR  FT-N @ 3 T=
   FT-RESET 63 FT-RETAIN-X ! 63 FT-OPEN-ERR  FT-N @ 4 T=

   FT-RESET 0 FT-SET-I ! 64 FT-SET-X ! 69 FT-RELEASE-RC !
   64 FT-OPEN-ERR
   FT-N @ 6 T=
   5 FT-OP@ FT-RELEASE T=

   FT-RESET 65 FT-CREATE-X !
   1 FT-SET-I ! 67 FT-SET-X ! 68 FT-RELEASE-X !
   65 FT-OPEN-ERR
   FT-N @ 8 T=
   6 FT-OP@ FT-SET T=
   7 FT-OP@ FT-RELEASE T= ;

: FT-CLOSE-ERRORS ( -- )
   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   1 FT-SET-I ! 41 FT-SET-RC !
   41 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   42 FT-SYNC-RC !
   42 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   43 FT-DESTROY-RC !
   43 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   2 FT-SET-I ! 44 FT-SET-RC !
   44 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   45 FT-RELEASE-RC !
   45 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   1 FT-SET-I ! 46 FT-SET-RC !
   47 FT-SYNC-RC ! 48 FT-DESTROY-RC ! 49 FT-RELEASE-RC !
   46 FT-CLOSE-ERR FT-CLOSE-LOG ;

: FT-CLOSE-THROWS ( -- )
   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   1 FT-SET-I ! 71 FT-SET-X !
   71 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   72 FT-SYNC-X !
   72 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   73 FT-DESTROY-X !
   73 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   2 FT-SET-I ! 74 FT-SET-X !
   74 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   75 FT-RELEASE-X !
   75 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   1 FT-SET-I ! 81 FT-SET-RC !
   82 FT-SYNC-X ! 83 FT-DESTROY-RC ! 84 FT-RELEASE-X !
   81 FT-CLOSE-ERR FT-CLOSE-LOG

   FT-RESET FT-MUST-OPEN FT-LOG-RESET
   1 FT-SET-I ! 85 FT-SET-X !
   86 FT-SYNC-RC ! 87 FT-DESTROY-X ! 88 FT-RELEASE-RC !
   85 FT-CLOSE-ERR FT-CLOSE-LOG ;

: FT-TWO-NEWEST ( -- )
   FT-RESET FT-MUST-OPEN FT-MUST-OPEN
   2 FT-ARG@ 0 T=
   5 FT-ARG@ 1 T=
   8 FT-ARG@ 0 T=
   11 FT-ARG@ 1 T=
   FT-LOG-RESET
   FT-MUST-CLOSE FT-MUST-CLOSE
   FT-N @ 10 T=
   2 FT-ARG@ FT-STREAM0 1 + T=
   7 FT-ARG@ FT-STREAM0 T= ;

: FT-TWO-OLDEST ( -- )
   FT-RESET FT-MUST-OPEN FT-MUST-OPEN swap
   FT-LOG-RESET
   FT-MUST-CLOSE FT-MUST-CLOSE
   FT-N @ 10 T=
   2 FT-ARG@ FT-STREAM0 T=
   7 FT-ARG@ FT-STREAM0 1 + T= ;

PTR-VARIABLE FT-SAVED
$86 constant FT-FAULT

: FT-SAVE ( GPU:session -- GPU:session )
   GS-TAKE dup HOST-BYTES> FT-SAVED ! GS-MINT ;

: FT-CHILD ( -- )
   GPU-SESSION-TEST:CLOSE-STDERR
   FT-SAVED @ c@ drop
   s" " 0 die ;

: FT-WAIT-FAULT ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if FT-CHILD then
   pid PROC-WAIT-OUTCOME MATCH outcome
      exited OF FT-FAULT T= ENDOF
      signaled OF drop 0 1 T= ENDOF
      timeout OF 0 1 T= ENDOF
   ;MATCH ;

: FT-MEM-RELEASE ( -- )
   FT-RESET FT-MUST-OPEN FT-SAVE FT-MUST-CLOSE
   FT-WAIT-FAULT ;

: FT-ABORT-RELEASE ( -- )
   FT-RESET
   51 FT-INIT-X !
   NULL$ drop [: GS-ALLOC ;] catch {: code:n :}
   code 0 <> if drop code throw then
   dup FT-SAVED !
   GS-START MATCH result
      ok OF FT-CLOSE-ANY 0 1 T= ENDOF
      err OF 51 T= ENDOF
   ;MATCH
   FT-WAIT-FAULT ;

: FT-RUN ( -- )
   T-RESET
   FT-ON
   FT-ACQUIRE-ERRORS
   FT-ACQUIRE-THROWS
   FT-CLOSE-ERRORS
   FT-CLOSE-THROWS
   FT-TWO-NEWEST
   FT-TWO-OLDEST
   FT-MEM-RELEASE
   FT-ABORT-RELEASE
   T-REPORT
   FT-OFF ;

FT-RUN

;package
s" gpu-session-test: ok" type cr
