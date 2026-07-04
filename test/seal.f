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

\ Each remaining sink hand-wires a DIFFERENT register into PROT-GUARD, so one
\ forge per sink proves that sink's own wiring. Register noted per forge.

: SLV-ATADD-FORGE$ ( -- ptr u8 n )          \ atomic-add addr (x10=B) into the band ($28)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 99 data-base SLF-CUR + atomic-add drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-ATCAS-FORGE$ ( -- ptr u8 n )          \ atomic-cas addr (x11=C, the only C guard) into band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 0 data-base SLF-CUR + atomic-cas drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-IOCTL-FORGE$ ( -- ptr u8 n )          \ ioctl arg (x2) into the band; bogus fd, guard fires first
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 0 data-base SLF-CUR + ioctl drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-POLL-FORGE$ ( -- ptr u8 n )           \ poll pollfd array (x0) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" data-base SLF-CUR + 1 0 poll drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-READLINK-FORGE$ ( -- ptr u8 n )       \ readlink link buffer (x2) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 data-base SLF-CUR + $40 readlink drop" SB-APPEND SLV-LF   \ NULL path: guard fires before the syscall
   SB$ ;

: SLV-STAT-FORGE$ ( -- ptr u8 n )           \ stat64 statbuf (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 data-base SLF-CUR + stat64 drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-LSTAT-FORGE$ ( -- ptr u8 n )          \ lstat64 statbuf (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 data-base SLF-CUR + lstat64 drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-DENTS-BUF-FORGE$ ( -- ptr u8 n )      \ getdirentries64 dirent buffer (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" 0 data-base SLF-CUR + $40 0 getdirentries64 drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-DENTS-BASEP-FORGE$ ( -- ptr u8 n )    \ getdirentries64 basep (x3) into the band; buffer in a free hole
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" $1A0 constant SLF-HOLE" SB-APPEND SLV-LF
   s" 0 data-base SLF-HOLE + $40 data-base SLF-CUR + getdirentries64 drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-MMAP-FORGE$ ( -- ptr u8 n )           \ mmap addr (x0) into the band (MAP_FIXED-class remap guard)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" data-base SLF-CUR + $1000 3 $1002 -1 0 mmap drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-HOLE-FORGE$ ( -- ptr u8 n )           \ ! into a free hole below the band ($1A0)
   SB-RESET
   s" $1A0 constant SLF-HOLE" SB-APPEND SLV-LF
   s" data-base SLF-HOLE + 99 swap !" SB-APPEND SLV-LF
   SB$ ;

: SLV-PAST-BAND2-FORGE$ ( -- ptr u8 n )     \ ! at $3D00 (one past band 2) stays writable
   SB-RESET
   s" $3D00 constant SLF-PAST" SB-APPEND SLV-LF
   s" data-base SLF-PAST + 99 swap !" SB-APPEND SLV-LF
   SB$ ;

\ Legit FORGET marks live in the code region (>= DATA-START, in the DBASE region),
\ so the cp!/ndict! guards must leave a real save-mark/roll-back/redefine intact.
: SLV-FORGET-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-A ( -- n ) 1 ;" SB-APPEND SLV-LF
   s" cp@ ndict@" SB-APPEND SLV-LF                 \ save code/dict marks
   s" : SLF-B ( -- n ) 2 ;" SB-APPEND SLV-LF       \ define past the marks
   s" ndict! cp!" SB-APPEND SLV-LF                 \ FORGET: roll both back
   s" : SLF-C ( -- n ) SLF-A 3 + ;" SB-APPEND SLV-LF
   s" SLF-C . cr" SB-APPEND SLV-LF
   SB$ ;

\ Second guarded band (TFAM 2b-v): the protected-WID registry [$3CB8, $3D00). A raw
\ store to the count cell ($3CB8) or the u32 table ($3CC0) must trap, so user source
\ can neither zero the count (un-protecting every sealed WID) nor overflow the table.
: SLV-PWIDN-FORGE$ ( -- ptr u8 n )          \ ! into the registry count cell ($3CB8)
   SB-RESET
   s" $3CB8 constant SLF-PWIDN" SB-APPEND SLV-LF
   s" data-base SLF-PWIDN + 0 swap !" SB-APPEND SLV-LF
   SB$ ;

: SLV-PWIDT-FORGE$ ( -- ptr u8 n )          \ c! into the registry table ($3CC0)
   SB-RESET
   s" $3CC0 constant SLF-PWIDT" SB-APPEND SLV-LF
   s" data-base SLF-PWIDT + 9 swap c!" SB-APPEND SLV-LF
   SB$ ;

: SLV-PWIDT-END-FORGE$ ( -- ptr u8 n )      \ ! at the band-2 last byte (PROT-REG-OFF+PROT-REG-LEN-1 = $3CFF)
   SB-RESET
   s" $3CFF constant SLF-PWEND" SB-APPEND SLV-LF
   s" data-base SLF-PWEND + 0 swap !" SB-APPEND SLV-LF
   SB$ ;

\ Code-emit sinks (habu-range-reject-cp-e2eed7e4): cp! sets the JIT code pointer;
\ a post-seal cp! that redirects emission into either protected band must trap
\ E-SEAL-VIOLATION AT THE SINK, not silently corrupt the band nor die via the
\ incidental word-creation bounds check. One forge per band proves cp!'s own guard
\ wiring. ndict! (the paired FORGET count sink) writes records into the disjoint
\ dict-record region (DBASE, not data-base), bounded < DICT-CAP by the room check,
\ so it cannot land in a data-base band; the legit round-trip forge below proves
\ its guard leaves normal FORGET intact.
: SLV-CPSET-B2-FORGE$ ( -- ptr u8 n )       \ cp! CP into the registry band ($3CB8), then emit
   SB-RESET
   s" $3CB8 constant SLF-PWIDN" SB-APPEND SLV-LF
   s" data-base SLF-PWIDN + cp!" SB-APPEND SLV-LF
   s" : SLF-FOO ( -- n ) 1 ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-CPSET-B1-FORGE$ ( -- ptr u8 n )       \ cp! CP into the crown-jewel band ($20), then emit
   SB-RESET
   s" $20 constant SLF-LATCH" SB-APPEND SLV-LF
   s" data-base SLF-LATCH + cp!" SB-APPEND SLV-LF
   s" : SLF-FOO ( -- n ) 1 ;" SB-APPEND SLV-LF
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
   SLV-READ-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ! into the protected-WID registry count traps (band 2)" T-LABEL
   SLV-PWIDN-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" c! into the protected-WID registry table traps (band 2)" T-LABEL
   SLV-PWIDT-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ! at the band-2 upper boundary (PROT-REG-OFF+PROT-REG-LEN-1) traps" T-LABEL
   SLV-PWIDT-END-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" cp! redirecting emission into band 2 traps at the sink" T-LABEL
   SLV-CPSET-B2-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" cp! redirecting emission into band 1 traps at the sink" T-LABEL
   SLV-CPSET-B1-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL ;

\ One forge per remaining guarded sink — each sink hand-wires its own PROT-GUARD
\ register, so testing one proves nothing about another's wiring.
: SLV-NEGATIVES-SINKS ( -- )
   s" atomic-add addr (x10) into the band traps" T-LABEL
   SLV-ATADD-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" atomic-cas addr (x11, only C guard) into the band traps" T-LABEL
   SLV-ATCAS-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ioctl arg (x2) into the band traps" T-LABEL
   SLV-IOCTL-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" poll pollfd array (x0) into the band traps" T-LABEL
   SLV-POLL-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" readlink link buffer (x2) into the band traps" T-LABEL
   SLV-READLINK-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" stat64 statbuf (x1) into the band traps" T-LABEL
   SLV-STAT-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" lstat64 statbuf (x1) into the band traps" T-LABEL
   SLV-LSTAT-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" getdirentries64 dirent buffer (x1) into the band traps" T-LABEL
   SLV-DENTS-BUF-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" getdirentries64 basep (x3) into the band traps" T-LABEL
   SLV-DENTS-BASEP-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" mmap addr (x0) into the band traps" T-LABEL
   SLV-MMAP-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL ;

: SLV-POSITIVES ( -- )
   s" free hole below the band stays writable" T-LABEL
   SLV-HOLE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" store one past band 2 ($3D00) stays writable" T-LABEL
   SLV-PAST-BAND2-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" legit cp!/ndict! FORGET round-trip still works" T-LABEL
   SLV-FORGET-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" post-seal define/package/trusted/defer still work" T-LABEL
   SLV-LANG-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK ;

: SLV-MAIN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-NEGATIVES
   SLV-NEGATIVES-SINKS
   SLV-POSITIVES
   SLV-CLEANUP
   T-REPORT
   s" seal-test: ok" type cr ;

SLV-MAIN

\ --- Prove-absence: guarded sinks NOT covered by an automated forge above ---
\ The 16 PROT-GUARD sinks: BSTORE/BPLUSSTORE/BCSTORE/BATSTORE/BATADD (x10=B),
\ BATCAS (x11=C), BREAD/BSTAT64/BLSTAT64 (x1), BIOCTL/BREADLINK (x2), BPOLL/BMMAP
\ (x0), BGETDIRENTRIES64 (x1 dirent buf + x3 basep) — all covered above. The two
\ remaining sinks are compiler-internal and are covered by hand-review only:
\   1. patch32 (BPATCH32, x10=B): the JIT engine-text patcher. Forging it would
\      run a real mprotect RW->store->RX + i-cache invalidation on a live page,
\      which is unsafe to exercise, so no automated forge. Its guard uses the B
\      register, which IS proven by the BSTORE/BPLUSSTORE/BCSTORE/BATSTORE/
\      atomic-add forges above; only patch32's own guard line is hand-reviewed.
\   2. snap-rebase (BSNAPREBASE, src/habu/habu2.f: 8 PROT-GUARD 16 PROT-GUARD):
\      the snapshot relocation primitive, driven only by the snapshot builder.
\      Forging it would run dictionary-relative relocation rewrites, unsafe to
\      exercise. Its x8/x16 guard registers are unique to this sink and are not
\      exercised by any other forge, so they remain hand-review only.
