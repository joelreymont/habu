\ top-row-hook-test.f - top-row hook engine regressions (dot
\ habu-typed-top-engine-2b2e88aa, docs/typed-top-level.md §2.1 / §5.2).
\
\ The engine carries a sealed top-row hook cell (layout.f TOP-HOOK-CELL)
\ installed only through the fail-closed `set-top-check` prim (habu1.f
\ BSETTOPCHECK, mirroring BSETCHECK's DBASE<=xt<CP window). With a hook
\ installed, every interpret-dispatch token class emits one event
\ ( token-a token-u class flags ) through habu2.f LTOPHOOK: number literals
\ (EM-INTERPRET-NUMBER), pushing string keywords (s"/S\" and c"/C\"), char
\ literals (C-CHAR), interpret tick (C-TICK, LFIND flags), and found words
\ pre-BLR (EM-INTERPRET-FIND, LFIND flags). Positives: an in-process logging
\ hook observes the exact (class, token, flags) sequence for one deliberate
\ token window — including certified min-in arity in the tick/word flags —
\ and `top-check@` round-trips the installed xt. Negatives (child processes,
\ both cold-prefix source paths): an invalid install dies rc 70 with the
\ named diagnostic before any dispatch BLR, and a raw store into the sealed
\ cell traps E-SEAL-VIOLATION while both one-cell neighbors stay writable.
\ Hook uninstalled = today's dispatch, proven by the rest of the native gate.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\   lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\   lib/process-env.f test/top-row-hook-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package TOP-ROW-HOOK-TEST

\ ---- event log ---------------------------------------------------------------
32 constant TRH-CAP
512 constant TRH-ARENA-CAP
23 constant TRH-WINDOW-N            \ events expected from the probe window below
1 constant TRH-WORD-FLAGS           \ LFIND: found, no certified min-in (prims, data records)
3 constant TRH-IMM-FLAGS            \ LFIND: found + DNAME-IMM
$101 constant TRH-PROBE-FLAGS       \ LFIND: found + certified min-in 1 (bits 8-15)
$401 constant TRH-LOG-FLAGS         \ LFIND: found + certified min-in 4

create TRH-CLS TRH-CAP cells allot
create TRH-FLG TRH-CAP cells allot
create TRH-TOFF TRH-CAP cells allot
create TRH-TLEN TRH-CAP cells allot
create TRH-ARENA TRH-ARENA-CAP allot
variable TRH-N
variable TRH-AU
variable TRH-OK-A

: TRH-RESET ( -- )
   0 TRH-N !  0 TRH-AU ! ;

: TRH-SLOT ( ptr a n -- ptr a )
   cells + ;

: TRH-ROOM! ( n -- )                \ reject a window larger than the fixture log
   TRH-AU @ + TRH-ARENA-CAP > if E-TEST-CAPACITY throw then
   TRH-N @ TRH-CAP < 0= if E-TEST-CAPACITY throw then ;

: TRH-ARENA-FULL ( -- )
   TRH-RESET
   TRH-ARENA-CAP 1+ TRH-ROOM! ;

: TRH-ROW-FULL ( -- )
   TRH-RESET
   TRH-CAP TRH-N !
   0 TRH-ROOM! ;

: TRH-CAPACITY-TESTS ( -- )
   s" arena saturation throws the reserved test capacity code" T-LABEL
   [: TRH-ARENA-FULL ;] E-TEST-CAPACITY TTHROWSQ
   s" event-row saturation throws the reserved test capacity code" T-LABEL
   [: TRH-ROW-FULL ;] E-TEST-CAPACITY TTHROWSQ ;

\ The installed top-row hook: record ( class, token bytes, flags ) per event.
\ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role for BYTE-COPY
: TRH-LOG ( ptr u8 n n n -- ) {: a u:n cls:n flg:n :}
   u TRH-ROOM!
   a TRH-ARENA TRH-AU @ + u BYTE-COPY
   cls TRH-CLS TRH-N @ TRH-SLOT !
   flg TRH-FLG TRH-N @ TRH-SLOT !
   TRH-AU @ TRH-TOFF TRH-N @ TRH-SLOT !
   u TRH-TLEN TRH-N @ TRH-SLOT !
   TRH-AU @ u + TRH-AU !
   TRH-N @ 1 + TRH-N ! ;

: TRH-EV-CLS ( n -- n )
   TRH-CLS swap TRH-SLOT @ ;

: TRH-EV-FLG ( n -- n )
   TRH-FLG swap TRH-SLOT @ ;

: TRH-EV$ ( n -- ptr u8 n ) {: i:n :}
   TRH-ARENA TRH-TOFF i TRH-SLOT @ +
   TRH-TLEN i TRH-SLOT @ ;

\ typed-local-lint: allow-bare-local - a keeps the ptr u8 expected-token role for T$=
: TRH-EV-ASSERT ( n n n ptr u8 n -- ) {: i:n cls:n flg:n a u:n :}
   i TRH-EV-CLS cls T=
   i TRH-EV-FLG flg T=
   i TRH-EV$ a u T$= ;

\ ---- probe words -------------------------------------------------------------
: TRH-PROBE ( n -- n )
   1 + ;

: TRH-IMM ( -- ) ; immediate

\ ---- expected window log -----------------------------------------------------
: TRH-ASSERT-LOG ( -- )
   s" window event count" T-LABEL
   TRH-N @ TRH-WINDOW-N T=
   s" window (class, token, flags) sequence" T-LABEL
   0  TOP-EV-WORD TRH-WORD-FLAGS  s" top-check@" TRH-EV-ASSERT
   1  TOP-EV-TICK TRH-LOG-FLAGS   s" TRH-LOG" TRH-EV-ASSERT
   2  TOP-EV-WORD TRH-WORD-FLAGS  s" =" TRH-EV-ASSERT
   3  TOP-EV-WORD TRH-WORD-FLAGS  s" TRH-OK-A" TRH-EV-ASSERT
   4  TOP-EV-WORD TRH-WORD-FLAGS  s" !" TRH-EV-ASSERT
   5  TOP-EV-NUM  0               s" 42" TRH-EV-ASSERT
   6  TOP-EV-WORD TRH-WORD-FLAGS  s" drop" TRH-EV-ASSERT
   7  TOP-EV-STR  0               S\" s\q" TRH-EV-ASSERT
   8  TOP-EV-WORD TRH-WORD-FLAGS  s" 2drop" TRH-EV-ASSERT
   9  TOP-EV-CSTR 0               S\" c\q" TRH-EV-ASSERT
   10 TOP-EV-WORD TRH-WORD-FLAGS  s" drop" TRH-EV-ASSERT
   11 TOP-EV-CHAR 0               s" A" TRH-EV-ASSERT
   12 TOP-EV-WORD TRH-WORD-FLAGS  s" drop" TRH-EV-ASSERT
   13 TOP-EV-TICK TRH-PROBE-FLAGS s" TRH-PROBE" TRH-EV-ASSERT
   14 TOP-EV-WORD TRH-WORD-FLAGS  s" drop" TRH-EV-ASSERT
   15 TOP-EV-NUM  0               s" 7" TRH-EV-ASSERT
   16 TOP-EV-WORD TRH-PROBE-FLAGS s" TRH-PROBE" TRH-EV-ASSERT
   17 TOP-EV-WORD TRH-WORD-FLAGS  s" drop" TRH-EV-ASSERT
   18 TOP-EV-WORD TRH-IMM-FLAGS   s" TRH-IMM" TRH-EV-ASSERT
   19 TOP-EV-STR  0               S\" S\\\q" TRH-EV-ASSERT
   20 TOP-EV-WORD TRH-WORD-FLAGS  s" 2drop" TRH-EV-ASSERT
   21 TOP-EV-NUM  0               s" 0" TRH-EV-ASSERT
   22 TOP-EV-WORD TRH-WORD-FLAGS  s" set-top-check" TRH-EV-ASSERT ;

\ ---- in-process logging-hook window -------------------------------------------
\ Every token between the install and the uninstall is deliberate: the expected
\ log above enumerates the full event sequence, including the trailing
\ `0 set-top-check` events (the word event fires pre-BLR, so the uninstall call
\ itself is the last logged event).
T-RESET
TRH-CAPACITY-TESTS
TRH-RESET
' TRH-LOG set-top-check
top-check@ ' TRH-LOG = TRH-OK-A !
42 drop
\ comment lines are skipped before dispatch: no event
s" hi" 2drop
c" hey" drop
char A drop
' TRH-PROBE drop
7 TRH-PROBE drop
TRH-IMM
S\" esc" 2drop
0 set-top-check

s" top-check@ returned the installed xt inside the window" T-LABEL
TRH-OK-A @ TTRUE
s" uninstall clears the cell" T-LABEL
top-check@ 0 T=
0 set-top-check
s" uninstall is idempotent with no hook installed" T-LABEL
top-check@ 0 T=
TRH-ASSERT-LOG

\ ---- child forges: fail-closed install + sealed cell ---------------------------
2048 constant TRH-IO-CAP
10000 constant TRH-TIMEOUT-MS
70 constant TRH-REJECT-RC

variable TRH-ROOT-U
variable TRH-CHILD-U
variable TRH-IN-U
variable TRH-OUT-U
variable TRH-ERR-U
variable TRH-EXITED
variable TRH-RC

create TRH-ROOT-BUF FS-PATH-CAP allot
create TRH-CHILD-BUF FS-PATH-CAP allot
create TRH-IN TRH-IO-CAP allot
create TRH-OUT TRH-IO-CAP allot
create TRH-ERR TRH-IO-CAP allot
create TRH-EMPTY 1 allot

: TRH-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: TRH-ROOT ( -- ptr u8 n )
   TRH-ROOT-BUF TRH-ROOT-U @ ;

: TRH-CHILD ( -- ptr u8 n )
   TRH-CHILD-BUF TRH-CHILD-U @ ;

: TRH-IN$ ( -- ptr u8 n )
   TRH-IN TRH-IN-U @ ;

: TRH-OUT$ ( -- ptr u8 n )
   TRH-OUT TRH-OUT-U @ ;

: TRH-ERR$ ( -- ptr u8 n )
   TRH-ERR TRH-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the candidate;
\ standalone runs fall back to bin/hb.
: TRH-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: TRH-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF TRH-RC ! 0 0= TRH-EXITED ! ENDOF
     signaled OF TRH-RC ! 0 0= 0= TRH-EXITED ! ENDOF
     timeout OF 0 TRH-RC ! 0 0= 0= TRH-EXITED ! ENDOF
   ;MATCH
   LEN>N TRH-ERR-U !  LEN>N TRH-OUT-U ! ;

: TRH-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u TRH-IO-CAP > if E-FS-CAPACITY throw then
   a TRH-IN u BYTE-COPY
   u TRH-IN-U ! ;

\ Run the program as a --load file with empty stdin.
: TRH-RUN-LOAD ( ptr u8 n -- )
   TRH-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TRH-CHILD >LEN PROC-ARGV+
   TRH-HB$ >LEN  TRH-EMPTY 0 >LEN  TRH-OUT TRH-IO-CAP >LEN
   TRH-ERR TRH-IO-CAP >LEN  TRH-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   TRH-STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: TRH-RUN-STDIN ( ptr u8 n -- )
   TRH-IN!
   PROC-ARGV-RESET
   TRH-HB$ >LEN  TRH-IN$ >LEN  TRH-OUT TRH-IO-CAP >LEN
   TRH-ERR TRH-IO-CAP >LEN  TRH-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   TRH-STORE! ;

: TRH-LF ( -- )
   10 SB-APPEND-C ;

: TRH-LINE ( ptr u8 n -- )
   SB-APPEND TRH-LF ;

: TRH-ASSERT-INVALID ( -- )         \ fail-closed install: named rc-70 diagnostic
   TRH-EXITED @ TTRUE
   TRH-RC @ TRH-REJECT-RC T=
   TRH-ERR$ s" set-top-check: invalid top-row hook xt" CONTAINS? TTRUE ;

: TRH-ASSERT-OK ( -- )
   TRH-EXITED @ TTRUE
   TRH-RC @ 0 T= ;

: TRH-ASSERT-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   TRH-OUT$ a u CONTAINS? TTRUE ;

: TRH-BARE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   TRH-LINE
   SB$ ;

: TRH-NEG-INSTALL ( -- )
   s" 1 set-top-check fails closed via --load (mirrors BSETCHECK window)" T-LABEL
   s" 1 set-top-check" TRH-BARE$ TRH-RUN-LOAD
   TRH-ASSERT-INVALID
   s" 1 set-top-check fails closed via stdin pipe" T-LABEL
   s" 1 set-top-check" TRH-BARE$ TRH-RUN-STDIN
   TRH-ASSERT-INVALID
   s" DATA-region address fails closed (outside [DBASE, CP))" T-LABEL
   s" data-base 8 + set-top-check" TRH-BARE$ TRH-RUN-LOAD
   TRH-ASSERT-INVALID
   s" bare set-top-check fails closed pre-execution (LARITY min 1)" T-LABEL
   s" set-top-check" TRH-BARE$ TRH-RUN-LOAD
   TRH-EXITED @ TTRUE
   TRH-RC @ TRH-REJECT-RC T=
   TRH-ERR$ s" E-UNDERFLOW: " CONTAINS? TTRUE ;

: TRH-SEAL-FORGE$ ( -- ptr u8 n )   \ raw ! into the sealed cell must trap
   SB-RESET
   s" data-base TOP-HOOK-CELL + 99 swap !" TRH-LINE
   SB$ ;

: TRH-BELOW-FORGE$ ( -- ptr u8 n )  \ one cell below the band stays writable
   SB-RESET
   s" data-base TOP-HOOK-CELL 8 - + 99 swap !" TRH-LINE
   SB$ ;

: TRH-ABOVE-FORGE$ ( -- ptr u8 n )  \ one cell past the band stays writable
   SB-RESET
   s" data-base TOP-HOOK-CELL 8 + + 99 swap !" TRH-LINE
   SB$ ;

: TRH-NEG-SEAL ( -- )
   s" raw ! into TOP-HOOK-CELL traps E-SEAL-VIOLATION" T-LABEL
   TRH-SEAL-FORGE$ TRH-RUN-LOAD
   TRH-EXITED @ TTRUE
   TRH-RC @ E-SEAL-VIOLATION T=
   s" one cell below the band stays writable" T-LABEL
   TRH-BELOW-FORGE$ TRH-RUN-LOAD
   TRH-ASSERT-OK
   s" one cell past the band stays writable" T-LABEL
   TRH-ABOVE-FORGE$ TRH-RUN-LOAD
   TRH-ASSERT-OK ;

: TRH-CHILD-HOOK$ ( -- ptr u8 n )   \ valid install runs; hook observes each event
   SB-RESET
   s" variable CH-N" TRH-LINE
   s" : CH-HOOK ( ptr u8 n n n -- ) 2drop drop drop 1 CH-N +! ;" TRH-LINE
   s" ' CH-HOOK set-top-check" TRH-LINE
   s" 1 2 + . cr" TRH-LINE
   s" 0 set-top-check" TRH-LINE
   s" CH-N @ . cr" TRH-LINE
   SB$ ;

: TRH-POSITIVES ( -- )
   s" valid install dispatches normally and counts 7 window events" T-LABEL
   TRH-CHILD-HOOK$ TRH-RUN-LOAD
   TRH-ASSERT-OK
   s" 3" TRH-ASSERT-OUT
   s" 7" TRH-ASSERT-OUT
   s" valid install via stdin pipe" T-LABEL
   TRH-CHILD-HOOK$ TRH-RUN-STDIN
   TRH-ASSERT-OK
   s" 7" TRH-ASSERT-OUT ;

: TRH-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-trh" TMPDIR-MKDIR {: a:ptr u:n :}
   a u TRH-ROOT-BUF TRH-ROOT-U TRH-COPY!
   TRH-ROOT CLEANUP-TREE+
   TRH-ROOT s" forge.f" TRH-CHILD-BUF JOIN-PATH TRH-CHILD-U ! ;

: TRH-CLEANUP ( -- )
   CLEANUP-RUN
   TRH-ROOT EXISTS? TFALSE ;

: TRH-MAIN ( -- )
   TRH-PREPARE
   TRH-NEG-INSTALL
   TRH-NEG-SEAL
   TRH-POSITIVES
   TRH-CLEANUP
   T-REPORT
   s" top-row-hook: ok" type cr ;

TRH-MAIN

;package
