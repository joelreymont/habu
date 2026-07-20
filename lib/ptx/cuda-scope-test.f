\ cuda-scope-test.f - host-only injection-matrix proof for the CUDA resource scope.
\
\ Portable: every case runs with a RECORDING FAKE driver injected into
\ CUDA-SCOPE's release defers (REL-*!), so no GPU or libcuda is touched. The
\ fake logs each release handle in order and can be armed to fail a chosen
\ handle with a chosen raw CUresult, which lets the suite pin the exact
\ exception-safety contract: reverse-order unwind, exactly-once release,
\ ownership transfer, repeated-cleanup no-op, failure after every acquisition
\ point, partial allocation, each and simultaneous cleanup failures, primary
\ error wins while cleanup errors are retained, and cleanup-only propagation.
\ Real-driver coverage rides on the caller-migration dots
\ (habu-migrate-maki-cuda-22bd5f0a, habu-migrate-ptx-tool-f57679ef).

require lib/test.f
require lib/ptx/cuda-scope.f

package CUDA-SCOPE-TEST

-7777 constant E-PRIMARY                          \ a body error distinct from E-CUDA (-5002)

\ ---- recording fake driver ------------------------------------------------
64 constant RLOG-CAP
create RLOG RLOG-CAP cells allot                  \ released handles, in release order
variable RLOG-N
: RLOG-RESET ( -- )  0 RLOG-N ! ;
: RLOG-PUT ( n -- )
   RLOG-N @ RLOG-CAP >= if E-CUDA throw then
   RLOG-N @ cells RLOG + !
   RLOG-N @ 1+ RLOG-N ! ;
: RLOG@ ( n -- n )  cells RLOG + @ ;              \ (idx -- handle)
: RLOG-N@ ( -- n )  RLOG-N @ ;

16 constant FSET-CAP
create FSET-H  FSET-CAP cells allot               \ armed-to-fail handles
create FSET-RC FSET-CAP cells allot               \ their raw CUresult
variable FSET-N
: FSET-RESET ( -- )  0 FSET-N ! ;
: ARM ( n n -- )  {: h:n rc:n :}                  \ (handle rc -- ) release of handle returns rc
   FSET-N @ FSET-CAP >= if E-CUDA throw then
   h FSET-H  FSET-N @ cells + !
   rc FSET-RC FSET-N @ cells + !
   FSET-N @ 1+ FSET-N ! ;
: FAIL-RC ( n -- n )  {: h:n :}                   \ (handle -- rc) armed rc for handle, else 0
   0
   FSET-N @ 0 do
      FSET-H i cells + @ h = if drop FSET-RC i cells + @ then
   loop ;
: FREL ( n -- n )  {: h:n :}                      \ the injected release op for every kind
   h RLOG-PUT
   h FAIL-RC ;
: INSTALL-FAKES ( -- )
   [: FREL ;] CUDA-SCOPE:REL-CTX!
   [: FREL ;] CUDA-SCOPE:REL-MOD!
   [: FREL ;] CUDA-SCOPE:REL-EVT!
   [: FREL ;] CUDA-SCOPE:REL-DEVPTR! ;
: HARNESS-RESET ( -- )
   RLOG-RESET  FSET-RESET  CUDA-SCOPE:RESET ;

\ handle per kind: ctx=10, mod=20, evt=30, devptr=40; owned-index i -> (i+1)*10
: EXP-H ( n -- n )  1+ 10 * ;

\ ---- scope bodies + wrappers (SCOPE takes a body quotation) ----------------
variable KVAR
: OWN-FIRST ( n -- )                              \ own the first k of ctx,mod,evt,devptr in order
   dup 0 > if 10 >CUDA-DEV    CUDA-SCOPE:OWN-PRIMARY-CTX then
   dup 1 > if 20 >CUDA-MOD    CUDA-SCOPE:OWN-MODULE then
   dup 2 > if 30 >CUDA-EVENT  CUDA-SCOPE:OWN-EVENT then
   dup 3 > if 40 >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR then
   drop ;
: BODY-2 ( -- )  2 OWN-FIRST ;                    \ own ctx+mod, succeed
: BODY-4 ( -- )  4 OWN-FIRST ;                    \ own all four, succeed
: BODY-AK ( -- )  KVAR @ OWN-FIRST  E-PRIMARY throw ;   \ own first k, then throw primary
: BODY-2-THROW ( -- )  BODY-2  E-PRIMARY throw ;
: BODY-PARTIAL ( -- )  40 >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR  E-PRIMARY throw ;  \ alloc A owned; alloc B fails before OWN

: SC-2 ( -- )        [: BODY-2 ;]       CUDA-SCOPE:SCOPE ;
: SC-4 ( -- )        [: BODY-4 ;]       CUDA-SCOPE:SCOPE ;
: SC-AK ( -- )       [: BODY-AK ;]      CUDA-SCOPE:SCOPE ;
: SC-2-THROW ( -- )  [: BODY-2-THROW ;] CUDA-SCOPE:SCOPE ;
: SC-PARTIAL ( -- )  [: BODY-PARTIAL ;] CUDA-SCOPE:SCOPE ;

\ ---- cases -----------------------------------------------------------------

: T-TRANSFER ( -- )                               \ ownership transfer + exactly-once release
   HARNESS-RESET
   CUDA-SCOPE:DEPTH 0 T=
   10 >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   CUDA-SCOPE:DEPTH 1 T=
   20 >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   CUDA-SCOPE:DEPTH 2 T=
   [: CUDA-SCOPE:UNWIND ;] 0 TTHROWSQ
   CUDA-SCOPE:DEPTH 0 T=
   RLOG-N@ 2 T= ;

: T-REVERSE ( -- )                                \ reverse-order unwind, each released exactly once
   HARNESS-RESET
   10 >CUDA-DEV    CUDA-SCOPE:OWN-PRIMARY-CTX
   20 >CUDA-MOD    CUDA-SCOPE:OWN-MODULE
   30 >CUDA-EVENT  CUDA-SCOPE:OWN-EVENT
   40 >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   [: CUDA-SCOPE:UNWIND ;] 0 TTHROWSQ
   RLOG-N@ 4 T=
   0 RLOG@ 40 T=  1 RLOG@ 30 T=  2 RLOG@ 20 T=  3 RLOG@ 10 T=
   CUDA-SCOPE:DEPTH 0 T= ;

: T-REPEAT ( -- )                                 \ repeated cleanup is a proved no-op (no second driver call)
   HARNESS-RESET
   10 >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   20 >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   [: CUDA-SCOPE:UNWIND CUDA-SCOPE:UNWIND ;] 0 TTHROWSQ
   RLOG-N@ 2 T=
   CUDA-SCOPE:DEPTH 0 T= ;

: T-FAIL-AFTER ( -- )                             \ failure after every acquisition point unwinds the owned prefix, reversed
   5 1 do
      HARNESS-RESET
      i KVAR !
      [: SC-AK ;] E-PRIMARY TTHROWSQ
      RLOG-N@ i T=                                \ exactly the i owned released - none skipped, none extra
      0 RLOG@ i 1- EXP-H T=                       \ first released = last owned (reverse)
      i 1- RLOG@ 10 T=                            \ last released = first owned (ctx)
      CUDA-SCOPE:DEPTH 0 T=
   loop ;

: T-PARTIAL ( -- )                                \ partial allocation: only the acquired half is released
   HARNESS-RESET
   [: SC-PARTIAL ;] E-PRIMARY TTHROWSQ
   RLOG-N@ 1 T=  0 RLOG@ 40 T=
   CUDA-SCOPE:DEPTH 0 T= ;

: T-CLEANUP-MOD ( -- )                            \ a failing early release does NOT skip later ones; cleanup-only propagates E-CUDA
   HARNESS-RESET
   20 5 ARM                                       \ mod (released first) fails rc=5
   [: SC-2 ;] E-CUDA TTHROWSQ
   RLOG-N@ 2 T=  0 RLOG@ 20 T=  1 RLOG@ 10 T=     \ ctx still released after mod failed
   CUDA-SCOPE:CLEANUP-ERRORS 1 T=
   CUDA-SCOPE:CLEANUP-FIRST 5 T=
   CUDA-SCOPE:DEPTH 0 T= ;

: T-CLEANUP-EACH ( -- )                           \ every kind's release can fail and still propagate E-CUDA after full unwind
   4 0 do
      HARNESS-RESET
      i EXP-H 7 ARM                               \ arm the i-th kind's handle to fail
      [: SC-4 ;] E-CUDA TTHROWSQ
      RLOG-N@ 4 T=                                \ all four still attempted
      CUDA-SCOPE:CLEANUP-ERRORS 1 T=
      CUDA-SCOPE:CLEANUP-FIRST 7 T=
      CUDA-SCOPE:DEPTH 0 T=
   loop ;

: T-CLEANUP-MULTI ( -- )                          \ simultaneous cleanup failures: first-released rc retained, all counted
   HARNESS-RESET
   40 9 ARM   10 5 ARM                            \ devptr (released first) rc=9, ctx (released last) rc=5
   [: SC-4 ;] E-CUDA TTHROWSQ
   RLOG-N@ 4 T=
   CUDA-SCOPE:CLEANUP-ERRORS 2 T=
   CUDA-SCOPE:CLEANUP-FIRST 9 T=                  \ first failing release encountered wins
   CUDA-SCOPE:DEPTH 0 T= ;

: T-PRIMARY-WINS ( -- )                           \ primary + cleanup both fail: primary propagates, cleanup retained
   HARNESS-RESET
   20 5 ARM                                       \ a cleanup also fails during unwind
   [: SC-2-THROW ;] E-PRIMARY TTHROWSQ            \ body error wins over E-CUDA
   RLOG-N@ 2 T=  0 RLOG@ 20 T=  1 RLOG@ 10 T=     \ full unwind still happened
   CUDA-SCOPE:CLEANUP-ERRORS 1 T=                 \ cleanup error retained though primary won
   CUDA-SCOPE:CLEANUP-FIRST 5 T=
   CUDA-SCOPE:DEPTH 0 T= ;

: RUN ( -- )
   T-RESET
   INSTALL-FAKES
   T-TRANSFER
   T-REVERSE
   T-REPEAT
   T-FAIL-AFTER
   T-PARTIAL
   T-CLEANUP-MOD
   T-CLEANUP-EACH
   T-CLEANUP-MULTI
   T-PRIMARY-WINS
   T-REPORT ;

RUN

;package
