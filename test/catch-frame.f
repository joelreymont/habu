\ catch-frame.f - a caught throw restores the COMPLETE caller execution frame
\ (dot habu-restore-complete-exec-abb8baca): data SP, user return-stack depth
\ (RSP-CELL), loop-stack depth (LOOPSP-CELL), and the handler chain. The checker
\ models `catch` as restoring both typed stacks (src/core/checker.f RSCATCH), so a
\ throwing quotation that leaves >r litter or an open ?do frame would otherwise
\ clobber caller return/loop state with no type mismatch.
\
\ Native gates run it against HABU_UNDER_TEST; tools/bootstrap.sh runs the same
\ file against the Gforth-recovered candidate, so native and recovery must agree.
\ The in-process cases assert the restored VALUES a checked r>/loop-index sees; the
\ forged-frame child fixtures assert the sentinel/underflow guards fail closed with
\ ENGINE-ERROR:CATCH-STACK (87) BEFORE any restore store touches caller memory.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package CATCH-FRAME-TEST
private

$800 constant CAP
10000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot

variable ACC

\ ---- in-process: a caught throw restores caller return/loop state ----

: CALLER-RSTK-PRESERVED ( -- n )       \ r> after a throwing >r quotation yields the caller's value
   42 >r [: 99 >r 7 throw ;] catch drop r> ;

: RSP-DRIFT ( -- n )                   \ user return-stack depth drift across a throwing >r quotation
   data-base RSP-CELL + @ {: r0:n :}
   [: 88 >r 7 throw ;] catch drop
   data-base RSP-CELL + @ r0 - ;

: LOOPSP-DRIFT ( -- n )                \ loop-stack depth drift across a throw inside ?do
   data-base LOOPSP-CELL + @ {: l0:n :}
   [: 4 0 ?do 9 throw loop ;] catch drop
   data-base LOOPSP-CELL + @ l0 - ;

: LOOP-INDEX-INTACT ( -- n )           \ caller's ?do index survives a throwing inner ?do
   0 ACC !
   3 0 ?do
      [: 5 0 ?do 9 throw loop ;] catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;                             \ 0 + 1 + 2 = 3

: INNER-CATCH ( -- )                   \ a nested catch that returns normally (return-balanced)
   [: 55 >r 3 throw ;] catch drop ;

: NESTED-RSP-DRIFT ( -- n )            \ repeated throws + a nested catch: return depth does not drift
   data-base RSP-CELL + @ {: r0:n :}
   100 0 ?do [: 42 >r 7 throw ;] catch drop loop
   [: INNER-CATCH  99 >r 7 throw ;] catch drop
   data-base RSP-CELL + @ r0 - ;

: NESTED-LOOPSP-DRIFT ( -- n )         \ repeated throws + a nested catch: loop depth does not drift
   data-base LOOPSP-CELL + @ {: l0:n :}
   100 0 ?do [: 4 0 ?do 8 throw loop ;] catch drop loop
   [: INNER-CATCH  99 >r 7 throw ;] catch drop
   data-base LOOPSP-CELL + @ l0 - ;

: IN-PROCESS ( -- )
   s" caught throw preserves caller >r value" T-LABEL
   CALLER-RSTK-PRESERVED 42 T=
   s" return-stack depth does not drift" T-LABEL
   RSP-DRIFT 0 T=
   s" loop-stack depth does not drift" T-LABEL
   LOOPSP-DRIFT 0 T=
   s" caller ?do index survives inner throw" T-LABEL
   LOOP-INDEX-INTACT 3 T=
   s" nested catch + repeated throws: no return drift" T-LABEL
   NESTED-RSP-DRIFT 0 T=
   s" nested catch + repeated throws: no loop drift" T-LABEL
   NESTED-LOOPSP-DRIFT 0 T= ;

\ ---- forged/underflow handler frames fail closed before any restore ----

variable ERRLEN

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: DIAG$ ( -- ptr u8 n )  s" hb: catch frame corrupt" ;

: ERR$ ( -- ptr u8 n )  ERR ERRLEN @ ;

: FORGE-RC ( ptr u8 n -- n )           \ run forge source on stdin under HB$; leaves rc, ERR/ERRLEN hold stderr
   {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   HB$ >LEN src srcu >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE RC>N {: rc:n :}   \ ( outlen errlen ) rc bound
   nip ERRLEN !                             \ record captured stderr length
   rc ;

: FORGE-FAILS-CLOSED ( ptr u8 n ptr u8 n -- )   \ ( label forge-src -- ): rc 87 + exact diagnostic
   {: lbl:ptr lblu:n src:ptr srcu:n :}
   lbl lblu T-LABEL
   src srcu FORGE-RC ENGINE-ERROR:CATCH-STACK T=
   ERR$ DIAG$ T$= ;

: FORGED-FRAMES ( -- )
   s" forged sentinel fails closed rc 87"
   s" create FF 64 allot  $DEADBEEF FF 56 + !  FF data-base HND-CELL + !  5 throw"
   FORGE-FAILS-CLOSED
   s" adjacent sentinel mutation fails closed rc 87"
   s" create FF 64 allot  CATCH-FRAME-MAGIC FF 56 + !  1 FF 56 + +!  FF data-base HND-CELL + !  5 throw"
   FORGE-FAILS-CLOSED
   s" saved-depth underflow fails closed rc 87"
   s" create FF 64 allot  CATCH-FRAME-MAGIC FF 56 + !  -1 FF 40 + !  FF data-base HND-CELL + !  5 throw"
   FORGE-FAILS-CLOSED
   s" saved-depth over region fails closed rc 87"
   s" create FF 64 allot  CATCH-FRAME-MAGIC FF 56 + !  RSTK-CELLS 1 + FF 40 + !  FF data-base HND-CELL + !  5 throw"
   FORGE-FAILS-CLOSED ;

public

: RUN ( -- )
   T-RESET
   IN-PROCESS
   FORGED-FRAMES
   T-REPORT
   s" catch-frame: ok" type cr ;

;package

CATCH-FRAME-TEST:RUN
