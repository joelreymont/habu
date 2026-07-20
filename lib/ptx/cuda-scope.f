\ cuda-scope.f - exception-safe CUDA resource ownership scope (package CUDA-SCOPE).
\
\ lib/ptx/cuda-driver.f exposes nominal handles and RC0 but no ownership: callers
\ retain a primary context, load modules, create events, and allocate device
\ memory into ambient variables and release them only on the happy path. A throw
\ between two acquisitions skips the release of everything already acquired; a
\ throw inside a release skips every later release; and a stale nonzero variable
\ left behind gets double-freed by the next attempt. This package removes all
\ three failure modes with one reusable mechanism.
\
\ MECHANISM. An ownership LEDGER: a LIFO of (kind, handle) rows, one row per
\ resource whose ownership the caller has transferred in (OWN-PRIMARY-CTX /
\ OWN-MODULE / OWN-EVENT / OWN-DEVPTR). UNWIND releases every owned row in
\ REVERSE acquisition order. Each row is CONSUMED (its slot zeroed and popped)
\ before its release is attempted, so a release is attempted exactly once and a
\ repeated UNWIND is a proved no-op - never a second driver call on a stale
\ handle. SCOPE is the named owner boundary: it runs a body quotation under a
\ fresh ledger frame and unwinds that frame on BOTH normal return and throw
\ (RAII). SCOPE frames nest - each frame unwinds only the rows it owns.
\
\ ERROR SEMANTICS. The PRIMARY operation error wins: if the body throws, that
\ code propagates after the ledger is fully unwound. Cleanup errors are never
\ lost: every failing release is RETAINED (CLEANUP-ERRORS count, CLEANUP-FIRST
\ the first raw CUresult) and REPORTED, and when ONLY cleanup fails (body ok)
\ the cleanup failure propagates as E-CUDA. A cleanup throw never aborts the
\ unwind - the remaining rows are still released.
\
\ INJECTION. The four release ops are `defer` words (REL-CTX/REL-MOD/REL-EVT/
\ REL-DEVPTR), armed to the real driver by USE-REAL-DRIVER at load. Host-only
\ tests re-point them (REL-CTX! ...) at recording fakes to prove every path -
\ failure after each acquisition, partial allocation, each and simultaneous
\ cleanup failures, reverse order, exactly-once, transfer, repeated cleanup -
\ with no GPU. The package is intentionally left unsealed: the release defers
\ are the injection seam.
\
\ TYPING. Resource kinds are typed states in a fixed table (K-CTX/K-MOD/K-EVT/
\ K-DEVPTR); the OWN-* entry points take the nominal handle roles and store the
\ raw projection. Migrating the ledger row to a declared-private linear owner
\ type (so a moved-out handle is unusable by the checker, not merely zeroed) is
\ the standing capability dot habu-epic-type-habu-a34713f0, not this file.

require lib/errors.f                 \ E-TBL-BOUNDS (ledger/frame overflow, suite.f idiom)
require lib/ptx/cuda-driver.f        \ package CUDA release words + nominal handle converters + E-CUDA

package CUDA-SCOPE

\ ---- resource kinds (typed states) and ledger capacity ---------------------
0 constant K-CTX                     \ primary context, released by device handle
1 constant K-MOD                     \ loaded module
2 constant K-EVT                     \ event
3 constant K-DEVPTR                  \ device allocation

64 constant LG-CAP                   \ max simultaneously-owned resources
16 constant BS-CAP                   \ max nested SCOPE frames

create LG-KIND   LG-CAP cells allot  \ per-row resource kind
create LG-HANDLE LG-CAP cells allot  \ per-row raw handle
variable LG-N                        \ owned-row count (ledger depth)

create BS BS-CAP cells allot         \ nested-frame base markers (ledger depth at SCOPE entry)
variable BS-N                        \ nesting depth

variable CU-KIND                     \ row being released (kept off the data stack across catch)
variable CU-HANDLE
variable UW-FAILS                    \ release failures within the current UNWIND (throw decision)
variable CLN-N                       \ retained cleanup-error count for the active boundary (report)
variable CLN-FIRST                   \ first retained raw CUresult for the active boundary

\ ---- injected release table: one defer per kind, real driver by default -----
defer REL-CTX    ( n -- n )          \ raw dev handle    -> raw CUresult
defer REL-MOD    ( n -- n )          \ raw module handle -> raw CUresult
defer REL-EVT    ( n -- n )          \ raw event handle  -> raw CUresult
defer REL-DEVPTR ( n -- n )          \ raw device ptr    -> raw CUresult

: R-CTX    ( n -- n )  >CUDA-DEV    CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE RC>N ;
: R-MOD    ( n -- n )  >CUDA-MOD    CUDA:CU-MODULE-UNLOAD              RC>N ;
: R-EVT    ( n -- n )  >CUDA-EVENT  CUDA:CU-EVENT-DESTROY              RC>N ;
: R-DEVPTR ( n -- n )  >CUDA-DEVPTR CUDA:CU-MEM-FREE                   RC>N ;

\ ---- ledger internals ------------------------------------------------------
: PUSH ( n n -- )  {: k:n h:n :}                \ record one owned (kind,handle) row (exactly once per call)
   LG-N @ LG-CAP >= if E-TBL-BOUNDS throw then
   k LG-KIND   LG-N @ cells + !
   h LG-HANDLE LG-N @ cells + !
   LG-N @ 1+ LG-N ! ;

: POP ( -- )                                    \ consume the top row into CU-KIND/CU-HANDLE, zero its slot
   LG-N @ 1- {: i:n :}
   i LG-N !
   LG-KIND   i cells + @ CU-KIND !
   LG-HANDLE i cells + @ CU-HANDLE !
   0 LG-KIND   i cells + !
   0 LG-HANDLE i cells + ! ;

: DO-REL ( n n -- n )                           \ (kind handle -- raw rc) dispatch to the injected release op
   swap case
      K-CTX    of REL-CTX    endof
      K-MOD    of REL-MOD    endof
      K-EVT    of REL-EVT    endof
      K-DEVPTR of REL-DEVPTR endof
      E-CUDA throw
   endcase ;

: REPORT ( n n n -- )                           \ (kind handle rc -- ) surface a cleanup failure
   s" cuda-scope: cleanup rc=" type .
   s" kind=" type swap .
   s" handle=" type . cr ;

: RECORD ( n -- )                               \ (rc -- ) retain first + count, then report
   CLN-N @ 0= if dup CLN-FIRST ! then
   1 CLN-N +!
   CU-KIND @ CU-HANDLE @ rot REPORT ;

: REL-CUR ( -- )                                \ release the consumed row; throw E-CUDA on nonzero rc
   CU-KIND @ CU-HANDLE @ DO-REL
   dup 0<> if RECORD E-CUDA throw then
   drop ;

: UNWIND-TO ( n -- )                            \ (base -- ) release rows above base, reverse order; throw E-CUDA if any failed
   0 UW-FAILS !
   begin dup LG-N @ < while
      POP
      [: REL-CUR ;] catch 0<> if 1 UW-FAILS +! then
   repeat
   drop
   UW-FAILS @ 0<> if E-CUDA throw then ;

\ ---- SCOPE frame internals -------------------------------------------------
: FRAME-ENTER ( -- )                            \ push a base marker; reset the boundary report at the outermost frame
   BS-N @ BS-CAP >= if E-TBL-BOUNDS throw then
   LG-N @ BS-N @ cells BS + !
   BS-N @ 0= if 0 CLN-N ! 0 CLN-FIRST ! then
   BS-N @ 1+ BS-N ! ;

: FRAME-EXIT ( -- )                             \ pop this frame's base and unwind exactly its rows
   BS-N @ 1- dup BS-N ! cells BS + @ UNWIND-TO ;

: COMBINE ( n n -- )                             \ (primary cleanup -- ) primary error wins; else cleanup propagates
   over 0<> if
      drop throw
   else
      nip dup 0<> if throw then drop
   then ;

public

: USE-REAL-DRIVER ( -- )                         \ (re)arm the release table to the real CUDA driver
   [: R-CTX ;]    is REL-CTX
   [: R-MOD ;]    is REL-MOD
   [: R-EVT ;]    is REL-EVT
   [: R-DEVPTR ;] is REL-DEVPTR ;

: REL-CTX!    ( [ n -- n ] -- )  is REL-CTX ;    \ inject a fake ctx release (host tests)
: REL-MOD!    ( [ n -- n ] -- )  is REL-MOD ;
: REL-EVT!    ( [ n -- n ] -- )  is REL-EVT ;
: REL-DEVPTR! ( [ n -- n ] -- )  is REL-DEVPTR ;

: RESET ( -- )                                   \ empty the ledger, frames, and boundary report
   0 LG-N !  0 BS-N !  0 CLN-N !  0 CLN-FIRST ! ;

: DEPTH ( -- n )  LG-N @ ;                        \ owned-row count
: CLEANUP-ERRORS ( -- n )  CLN-N @ ;              \ retained cleanup-failure count for the active boundary
: CLEANUP-FIRST  ( -- n )  CLN-FIRST @ ;          \ first retained raw CUresult (0 if none)

: OWN-PRIMARY-CTX ( cuda-dev -- )    CUDA-DEV>N    K-CTX    swap PUSH ;   \ transfer: release via cuDevicePrimaryCtxRelease(dev)
: OWN-MODULE      ( cuda-mod -- )    CUDA-MOD>N    K-MOD    swap PUSH ;
: OWN-EVENT       ( cuda-event -- )  CUDA-EVENT>N  K-EVT    swap PUSH ;
: OWN-DEVPTR      ( cuda-devptr -- ) CUDA-DEVPTR>N K-DEVPTR swap PUSH ;

: UNWIND ( -- )  0 UNWIND-TO ;                    \ release the whole ledger now; throw E-CUDA on any cleanup failure

: SCOPE ( [ -- ] -- )                             \ named owner boundary: run body, unwind its frame on return or throw
   FRAME-ENTER
   catch
   [: FRAME-EXIT ;] catch
   COMBINE ;

USE-REAL-DRIVER

;package
