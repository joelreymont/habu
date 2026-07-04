\ seal.f - friend-arena seal regressions (TFAM 2b-i).
\
\ Proves the post-seal raw-write guard (PROT-GUARD, exit E-SEAL-VIOLATION) traps
\ every store sink whose target lands in the sealed crown-jewel band
\ [data-base+FRIEND-ARENA, +FRIEND-ARENA-LEN), that the latch itself is inside
\ the band (one-way seal), that a read syscall whose buffer starts in the band is
\ trapped, that a free hole below the band stays writable, and that normal
\ language features which update the protected cells through engine primitives
\ still work post-seal.
\
\ Each fixture is a standalone forge program run in a fresh child engine, so it
\ cannot include layout.f; the offsets are named locally (SLF-*), never bare
\ magic numbers. The child engine is HABU_UNDER_TEST when the gate sets it, else
\ bin/hb, so the assertions run against the freshly built sealed candidate.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f test/seal.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

2048 constant SLV-CAP
10000 constant SLV-TIMEOUT-MS
83 constant SLV-SEAL-RC             \ E-SEAL-VIOLATION child exit status

variable SLV-ROOT-U
variable SLV-CHILD-U
variable SLV-IN-U
variable SLV-OUT-U
variable SLV-ERR-U
variable SLV-KIND
variable SLV-RC

create SLV-ROOT-BUF FS-PATH-CAP allot
create SLV-CHILD-BUF FS-PATH-CAP allot
create SLV-IN SLV-CAP allot          \ stdin-piped forge source
create SLV-OUT SLV-CAP allot
create SLV-ERR SLV-CAP allot
create SLV-EMPTY 1 allot            \ zero-length stdin

: SLV-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SLV-ROOT ( -- ptr u8 n )
   SLV-ROOT-BUF SLV-ROOT-U @ ;

: SLV-CHILD ( -- ptr u8 n )
   SLV-CHILD-BUF SLV-CHILD-U @ ;

: SLV-IN$ ( -- ptr u8 n )
   SLV-IN SLV-IN-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the sealed
\ candidate; standalone runs fall back to bin/hb.
: SLV-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: SLV-LF ( -- )
   10 SB-APPEND-C ;

\ --- forge sources: each names its offset, then hits one write/read sink. ---

: SLV-CUR-FORGE$ ( -- ptr u8 n )            \ ! into CUR-CELL ($28)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" data-base SLF-CUR + 99 swap !" SB-APPEND SLV-LF
   SB$ ;

: SLV-LATCH-FORGE$ ( -- ptr u8 n )          \ ! over the latch itself ($20)
   SB-RESET
   s" $20 constant SLF-LATCH" SB-APPEND SLV-LF
   s" data-base SLF-LATCH + 0 swap !" SB-APPEND SLV-LF
   SB$ ;

: SLV-WIDN-C-FORGE$ ( -- ptr u8 n )         \ c! into WIDN-CELL ($30)
   SB-RESET
   s" $30 constant SLF-WIDN" SB-APPEND SLV-LF
   s" data-base SLF-WIDN + 99 swap c!" SB-APPEND SLV-LF
   SB$ ;

: SLV-WIDN-ADD-FORGE$ ( -- ptr u8 n )       \ +! into WIDN-CELL ($30)
   SB-RESET
   s" $30 constant SLF-WIDN" SB-APPEND SLV-LF
   s" data-base SLF-WIDN + 1 swap +!" SB-APPEND SLV-LF
   SB$ ;

: SLV-ATOMIC-FORGE$ ( -- ptr u8 n )         \ atomic! into CUR-CELL ($28)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" data-base SLF-CUR + 99 swap atomic!" SB-APPEND SLV-LF
   SB$ ;

: SLV-READ-FORGE$ ( -- ptr u8 n )           \ read syscall buffer starts in the band ($28)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 data-base SLF-CUR + 8 read drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-HOLE-FORGE$ ( -- ptr u8 n )           \ ! into a free hole below the band ($1A0)
   SB-RESET
   s" $1A0 constant SLF-HOLE" SB-APPEND SLV-LF
   s" data-base SLF-HOLE + 99 swap !" SB-APPEND SLV-LF
   SB$ ;

\ Post-seal language exercise: define words, a package + qualified word, a
\ TRUSTED: word, and a DEFER + IS target, then use them. Each updates a protected
\ cell (CUR/WIDN/DEF-WL/TSIG/PKG-*/DEFER-*) through engine primitives, not raw
\ stores, so all must still work with the band sealed.
: SLV-LANG-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-SQUARE ( n -- n ) dup * ;" SB-APPEND SLV-LF
   s" package SLFPKG" SB-APPEND SLV-LF
   s" public" SB-APPEND SLV-LF
   s" : WIN ( -- n ) 7 ;" SB-APPEND SLV-LF
   s" end-package" SB-APPEND SLV-LF
   s" TRUSTED: SLF-TRUSTX ( -- n ) 3 ;" SB-APPEND SLV-LF
   s" defer SLF-ACT ( -- n )" SB-APPEND SLV-LF
   s" : SLF-SETUP ( -- ) [: SLF-TRUSTX ;] is SLF-ACT ;" SB-APPEND SLV-LF
   s" : SLF-MAIN ( -- ) SLF-SETUP 5 SLF-SQUARE SLFPKG:WIN + SLF-ACT + . cr ;" SB-APPEND SLV-LF
   s" SLF-MAIN" SB-APPEND SLV-LF
   SB$ ;

\ --- child spawn + outcome capture ---

: SLV-STORE! ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
   kind SLV-KIND !  code SLV-RC !
   erru LEN>N SLV-ERR-U !  outu LEN>N SLV-OUT-U ! ;

: SLV-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SLV-CAP > if E-FS-CAPACITY throw then
   a SLV-IN u BYTE-COPY
   u SLV-IN-U ! ;

\ Run the forge as a --load file with empty stdin.
: SLV-RUN-LOAD ( ptr u8 n -- )
   SLV-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SLV-CHILD >LEN PROC-ARGV+
   SLV-HB$ >LEN  SLV-EMPTY 0 >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  SLV-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SLV-STORE! ;

\ Run the forge as a piped stdin program (no --load), the other cold-prefix path.
: SLV-RUN-STDIN ( ptr u8 n -- )
   SLV-IN!
   PROC-ARGV-RESET
   SLV-HB$ >LEN  SLV-IN$ >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  SLV-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SLV-STORE! ;

: SLV-ASSERT-SEAL ( -- )                    \ child died with the seal-violation exit
   SLV-KIND @ PROC-OUTCOME-EXIT T=
   SLV-RC @ SLV-SEAL-RC T= ;

: SLV-ASSERT-OK ( -- )                      \ child exited cleanly
   SLV-KIND @ PROC-OUTCOME-EXIT T=
   SLV-RC @ 0 T= ;

: SLV-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-seal" TMPDIR-MKDIR {: a:ptr u:n :}
   a u SLV-ROOT-BUF SLV-ROOT-U SLV-COPY!
   SLV-ROOT CLEANUP-TREE+
   SLV-ROOT s" forge.f" SLV-CHILD-BUF JOIN-PATH SLV-CHILD-U ! ;

: SLV-CLEANUP ( -- )
   CLEANUP-RUN
   SLV-ROOT EXISTS? TFALSE ;

: SLV-NEGATIVES ( -- )
   s" ! into CUR-CELL traps via --load" T-LABEL
   SLV-CUR-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ! into CUR-CELL traps via stdin pipe" T-LABEL
   SLV-CUR-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL
   s" overwrite of the latch itself traps (one-way seal)" T-LABEL
   SLV-LATCH-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" c! into WIDN-CELL traps" T-LABEL
   SLV-WIDN-C-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" +! into WIDN-CELL traps" T-LABEL
   SLV-WIDN-ADD-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" atomic! into the band traps" T-LABEL
   SLV-ATOMIC-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" read buffer starting in the band traps" T-LABEL
   SLV-READ-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL ;

: SLV-POSITIVES ( -- )
   s" free hole below the band stays writable" T-LABEL
   SLV-HOLE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" post-seal define/package/trusted/defer still work" T-LABEL
   SLV-LANG-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK ;

: SLV-MAIN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-NEGATIVES
   SLV-POSITIVES
   SLV-CLEANUP
   T-REPORT
   s" seal-test: ok" type cr ;

SLV-MAIN
