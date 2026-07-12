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
variable SLV-EXITED                 \ bool: child completed by exit
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
   s" data-base SLF-CUR + $1000 3 $1012 -1 0 mmap drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-HOLE-FORGE$ ( -- ptr u8 n )           \ ! into a free hole below the band ($1A0)
   SB-RESET
   s" $1A0 constant SLF-HOLE" SB-APPEND SLV-LF
   s" data-base SLF-HOLE + 99 swap !" SB-APPEND SLV-LF
   SB$ ;

: SLV-PAST-BAND2-FORGE$ ( -- ptr u8 n )     \ ! at $40C8 (one past band 2, = TASK-USER-BASE) stays writable
   SB-RESET
   s" $40C8 constant SLF-PAST" SB-APPEND SLV-LF
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

\ Sealed-dictionary truncation guard (TFAM 2b-iii). FORGET-DEFS-FROM /
\ HIDE-DEFS-FROM resolve a name to a dict index and truncate ndict (FORGET also
\ rewinds CP) to it; a name resolving to an ENGINE record (index below the
\ seal-time watermark SEAL-NDICT-CELL, captured by SEAL-CAPTURE at the end of the
\ engine source) would retire engine definitions and rewind CP into engine code.
\ Post-seal user source must be rejected fail-closed (E-SEAL-VIOLATION). `WORDS`
\ is an engine word defined in xref.f, well below the watermark.
: SLV-FORGET-ENGINE-FORGE$ ( -- ptr u8 n )       \ FORGET an engine definition
   SB-RESET
   S\" s\" WORDS\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-ENGINE-FORGE$ ( -- ptr u8 n )         \ HIDE an engine definition
   SB-RESET
   S\" s\" WORDS\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ The engine-prefix TAIL gap (dot habu-tfam-2b-iii-5d25b52f): src/os/script-argv.f
\ loads AFTER xref.f, so a watermark captured at end-of-xref left its words
\ FORGET/HIDEable (proven rc 0). The SEAL-CAPTURE token appended at the true
\ engine-prefix end closes it: the tail must trap like any engine definition.
: SLV-FORGET-TAIL-FORGE$ ( -- ptr u8 n )         \ FORGET the script-argv tail
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-TAIL-FORGE$ ( -- ptr u8 n )           \ HIDE the script-argv tail
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Direct checker signature-registry truncation (bypasses the FORGET/HIDE index
\ guard, but forgets an engine word's checker signature so it can be redefined =
\ spoof). The public CHECKER-USIGS-TRUNCATE-FROM rejects any post-seal call.
: SLV-USIG-TRUNC-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" CHECKER-USIGS-TRUNCATE-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Positive: a user mark defined AFTER the seal is above the watermark, so
\ FORGET/HIDE of it stays a working round-trip (retire user tail, redefine).
: SLV-FORGET-USER-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   s" : SLF-UB ( -- n ) 2 ;" SB-APPEND SLV-LF
   s" SLF-UB . cr" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-USER-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Second guarded band (TFAM 2b-v): protected metadata [$3CB8, $40C8) contains the
\ protected-WID registry and UNCGH-CELL. A raw store to the registry or reporter hook
\ must trap, so user source cannot unprotect sealed WIDs or redirect uncaught throws.
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

: SLV-BAND2-END-FORGE$ ( -- ptr u8 n )       \ c! at the protected metadata last byte ($40C7)
   SB-RESET
   s" $40C7 constant SLF-B2END" SB-APPEND SLV-LF
   s" data-base SLF-B2END + 0 swap c!" SB-APPEND SLV-LF
   SB$ ;

: SLV-UNCGH-FORGE$ ( -- ptr u8 n )          \ ! into the uncaught-throw reporter hook ($40C0)
   SB-RESET
   s" $40C0 constant SLF-UNCGH" SB-APPEND SLV-LF
   s" data-base SLF-UNCGH + 0 swap !" SB-APPEND SLV-LF
   SB$ ;

\ Code-emit capability: cp! may select only an aligned instruction in the full
\ DBASE code interval. A DATA address must trap at cp! itself, before any delayed
\ emission can cross a protected band. One forge per band proves the independent
\ code-region guard. ndict! (the paired FORGET count sink) writes records into the disjoint
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

\ FFI seal guard (TFAM 2b-iii, cat 5 FFI-writer): a sealed-band pointer packed as a
\ LIVE integer/pointer FFI arg must trap E-SEAL-VIOLATION at the trampoline BEFORE
\ the foreign call, else the callee writes through it and tampers a sealed cell.
\ Routed via the checked FFI package with an explicit writable extent; fn=0 so
\ the whole-span guard must fire first (an unguarded BLR to 0
\ signals/crashes, a distinct outcome from EXIT 83). One forge per protected band.
: SLV-FFI-B1-FORGE$ ( -- ptr u8 n )         \ 16-byte write crosses band-1 lower edge
   SB-RESET
   s" require lib/ffi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! FFI:ARGS FFI:REG-LENS 1 SLF-RET ffi-call-bounded ;" SB-APPEND SLV-LF
   s" data-base $18 + SLF-WRITE drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-FFI-B2-FORGE$ ( -- ptr u8 n )         \ 16-byte write crosses band-2 lower edge
   SB-RESET
   s" require lib/ffi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! FFI:ARGS FFI:REG-LENS 1 SLF-RET ffi-call-bounded ;" SB-APPEND SLV-LF
   s" data-base $3CB8 + SLF-WRITE drop" SB-APPEND SLV-LF
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
   s" ;package" SB-APPEND SLV-LF
   s" TRUSTED: SLF-TRUSTX ( -- n ) 3 ;" SB-APPEND SLV-LF
   s" defer SLF-ACT ( -- n )" SB-APPEND SLV-LF
   s" : SLF-SETUP ( -- ) [: SLF-TRUSTX ;] is SLF-ACT ;" SB-APPEND SLV-LF
   s" : SLF-MAIN ( -- ) SLF-SETUP 5 SLF-SQUARE SLFPKG:WIN + SLF-ACT + . cr ;" SB-APPEND SLV-LF
   s" SLF-MAIN" SB-APPEND SLV-LF
   SB$ ;

\ --- child spawn + outcome capture ---

: SLV-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF SLV-RC ! 0 0= SLV-EXITED ! ENDOF
     signaled OF SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
     timeout OF 0 SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
   ;MATCH
   LEN>N SLV-ERR-U !  LEN>N SLV-OUT-U ! ;

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
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-SEAL-RC T= ;

: SLV-ASSERT-OK ( -- )                      \ child exited cleanly
   SLV-EXITED @ TTRUE
   SLV-RC @ 0 T= ;

\ --- protected-WID table capacity (dot habu-seal-protwid-cap-6f1c9d2b) --------------
\ Each PUBLIC ADT family registers one protected WID (xref.f PROT-WID-CTOR-ADD ->
\ prot-wid-add); the table holds PROT-WID-MAX (256, raised from 16) entries. A batch
\ child declaring K public families is generated with unique letter-pair names (each
\ family scopes its own 'foo' variant, so the variant name may repeat). Red-first: at
\ the old cap of 16 the 17th family exits E-SEAL-PACKAGE (84) with NO diagnostic, so
\ both the "succeed past 16" (rc 0) and the "labeled overflow" (stderr names the
\ table) assertions fail. Driven via SLV-RUN-LOAD (a file), not stdin: 257 families
\ exceed the 2 KB SLV-IN buffer.
16384 constant PWG-CAP                       \ room for 256+ families at ~46 bytes each
create PWG-BUF PWG-CAP allot
variable PWG-U

: PWG$ ( -- ptr u8 n )  PWG-BUF PWG-U @ ;
: PWG-RESET ( -- )  0 PWG-U ! ;
: PWG-C ( n -- ) {: c:n :}                   \ append one raw byte
   PWG-U @ 1+ PWG-CAP > if E-FS-CAPACITY throw then
   c  PWG-BUF PWG-U @ +  c!
   PWG-U @ 1+ PWG-U ! ;
: PWG-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   PWG-U @ u + PWG-CAP > if E-FS-CAPACITY throw then
   a  PWG-BUF PWG-U @ +  u BYTE-COPY
   PWG-U @ u + PWG-U ! ;
: PWG-FAMILY ( n -- ) {: i:n :}              \ one public 1-variant SUMTYPE, unique name f<a-z><a-z>
   s" SUMTYPE f" PWG-APPEND
   $61 i 26 / +   PWG-C
   $61 i 26 mod + PWG-C
   s"  1 VARIANT foo a ;VARIANT ;SUMTYPE" PWG-APPEND
   10 PWG-C ;
: PWG-GEN ( n -- ptr u8 n ) {: k:n :}        \ K public families as one batch program
   PWG-RESET
   0 begin dup k < while dup PWG-FAMILY 1+ repeat drop
   PWG$ ;

84 constant SLV-PWID-RC                      \ E-SEAL-PACKAGE: protected-WID table full
: SLV-ERR$ ( -- ptr u8 n )  SLV-ERR SLV-ERR-U @ ;
: SLV-ASSERT-PWID-FULL ( -- )                \ child died with the LABELED protected-WID-full exit
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-PWID-RC T=
   SLV-ERR$ s" hb: protected-WID table full" CONTAINS? TTRUE ;

: SLV-PWID-CAP ( -- )
   s" 17 public ADT families succeed past the old 16 cap" T-LABEL
   17 PWG-GEN SLV-RUN-LOAD SLV-ASSERT-OK
   s" 256 public families succeed at the raised cap" T-LABEL
   256 PWG-GEN SLV-RUN-LOAD SLV-ASSERT-OK
   s" the 257th public family overflows -> labeled 'protected-WID table full' exit 84" T-LABEL
   257 PWG-GEN SLV-RUN-LOAD SLV-ASSERT-PWID-FULL ;

\ --- publish-into-protected-word guard (dot habu-label-two-silent-bd8e5d09) -----------
\ Once the friend latch is sealed, publishing a definition into a protected WID (a public
\ family's generated constructor package) via `<pkg>:WORD` must fail closed. The guard
\ (habu2.f C-STORE-DEF-NAME) now names itself on fd 2 (LPROTPUB) with the offending word
\ before exit 84. Red-first: the bare exit left stderr empty, so the CONTAINS assertion
\ fails. (The sibling AOT boot-pass gate EM-AOTWIDGATE is also now labeled (LPROTAOT); it
\ fires only on a crafted AOT/snapshot image, so its forced-reject fixture is dotted.)
: SLV-PUBLISH-FORGE$ ( -- ptr u8 n )         \ define a word into a public family's protected WID
   SB-RESET
   s" SUMTYPE foo 1 VARIANT bar a ;VARIANT ;SUMTYPE" SB-APPEND SLV-LF
   s" : foo:BOGUS ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-ASSERT-PROT-PUBLISH ( -- )             \ child died E-SEAL-PACKAGE naming the publish guard
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-PWID-RC T=
   SLV-ERR$ s" hb: cannot publish into protected word" CONTAINS? TTRUE ;

: SLV-PROT-PUBLISH ( -- )
   s" : foo:BOGUS ; into a public family's protected WID -> labeled exit 84 (stdin)" T-LABEL
   SLV-PUBLISH-FORGE$ SLV-RUN-STDIN SLV-ASSERT-PROT-PUBLISH
   s" : foo:BOGUS ; into a protected WID -> labeled exit 84 (--load)" T-LABEL
   SLV-PUBLISH-FORGE$ SLV-RUN-LOAD SLV-ASSERT-PROT-PUBLISH ;

package OWNER-WID-TEST

$4000 constant CAP
create BUF CAP allot
variable USED

: RESET ( -- )
   0 USED ! ;

: C+ ( n -- ) {: c:n :}
   USED @ 1+ CAP > if E-FS-CAPACITY throw then
   c BUF USED @ + c!
   USED @ 1+ USED ! ;

: $+ ( ptr u8 n -- ) {: a:ptr u:n :}
   USED @ u + CAP > if E-FS-CAPACITY throw then
   a BUF USED @ + u BYTE-COPY
   USED @ u + USED ! ;

: LF ( -- )
   10 C+ ;

: HEADER ( -- )
   s" require lib/test.f" $+ LF
   s" T-RESET" $+ LF
   s" package OWNER-WID-CHECK" $+ LF
   s" variable PROT0" $+ LF
   s" data-base PROT-WID-N-CELL + @ PROT0 !" $+ LF
   s" public" $+ LF
   s" : PREFLIGHT ( n n n -- bool ) owner-wid-preflight? ;" $+ LF
   s" : PUBLIC? ( n -- bool ) owner-wid-public? ;" $+ LF
   s" : PRIVATE? ( n -- bool ) owner-wid-private? ;" $+ LF
   s" : MEMBER? ( n -- bool ) owner-wid? ;" $+ LF
   s" : CONSTRUCTOR-ADDED? ( -- bool ) data-base PROT-WID-N-CELL + @ PROT0 @ 1+ = ;" $+ LF
   s" ;package" $+ LF
   s" data-base OWNER-WID-N-CELL + @ 0 T=" $+ LF
   s" $1000 $2000 1 OWNER-WID-CHECK:PREFLIGHT TTRUE" $+ LF
   s" $1000 $2000 0 OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" 0 $2000 OWNER-WID-MAX OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" $1000 0 OWNER-WID-MAX OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" $1000 $1000 OWNER-WID-MAX OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" $100000000 $2000 OWNER-WID-MAX OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" $1000 $100000000 OWNER-WID-MAX OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" $1000 $2000 OWNER-WID-MAX 1+ OWNER-WID-CHECK:PREFLIGHT TFALSE" $+ LF
   s" data-base OWNER-WID-N-CELL + @ 0 T=" $+ LF
   s" data-base OWNER-WID-OFF + @ 0 T=" $+ LF ;

: ROLES ( -- )
   s" $1000 OWNER-WID-CHECK:PUBLIC? TFALSE" $+ LF
   s" $2000 OWNER-WID-CHECK:PRIVATE? TFALSE" $+ LF
   s" $1000 OWNER-WID-CHECK:MEMBER? TFALSE" $+ LF
   s" $10FF OWNER-WID-CHECK:PUBLIC? TFALSE" $+ LF
   s" $20FF OWNER-WID-CHECK:PRIVATE? TFALSE" $+ LF
   s" $10FF OWNER-WID-CHECK:MEMBER? TFALSE" $+ LF
   s" $20FF OWNER-WID-CHECK:MEMBER? TFALSE" $+ LF ;

: CONSTRUCTOR ( -- )
   \ Type-family and variant tails are lowercase system vocabulary.
   s" SUMTYPE owf 1 VARIANT same a ;VARIANT ;SUMTYPE" $+ LF
   s" OWNER-WID-CHECK:CONSTRUCTOR-ADDED? TTRUE" $+ LF
   s" data-base OWNER-WID-N-CELL + @ 0 T=" $+ LF ;

: SOURCE$ ( -- ptr u8 n )
   RESET HEADER ROLES CONSTRUCTOR
   s" T-REPORT" $+ LF
   BUF USED @ ;

public

: ADD-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : OWT-FORGE ( n n -- bool ) owner-wid-add ;" SB-APPEND SLV-LF
   SB$ ;

: ADD-LOOKUP-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/test.f" SB-APPEND SLV-LF
   s" T-RESET" SB-APPEND SLV-LF
   S\" s\" owner-wid-add\" 0 search-wl 0 T=" SB-APPEND SLV-LF
   s" T-REPORT" SB-APPEND SLV-LF
   SB$ ;

: BAD$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: ins:ptr inu:n prim:ptr primu:n :}
   SB-RESET
   s" : OWT-BAD ( " SB-APPEND
   ins inu SB-APPEND
   s"  -- n ) " SB-APPEND
   prim primu SB-APPEND
   s"  ;" SB-APPEND SLV-LF
   SB$ ;

: ASSERT-ADD-HIDDEN ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ 70 T=
   SLV-ERR$ s" owner-wid-add" CONTAINS? TTRUE ;

: ASSERT-CHECKER-BOOL ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ 70 T=
   SLV-ERR$ s" expected: n" CONTAINS? TTRUE
   SLV-ERR$ s" actual: bool" CONTAINS? TTRUE ;

: CHECKER-REJECTS ( -- )
   s" n n n" s" owner-wid-preflight?" BAD$ SLV-RUN-LOAD ASSERT-CHECKER-BOOL
   s" n" s" owner-wid-public?" BAD$ SLV-RUN-LOAD ASSERT-CHECKER-BOOL
   s" n" s" owner-wid-private?" BAD$ SLV-RUN-LOAD ASSERT-CHECKER-BOOL
   s" n" s" owner-wid?" BAD$ SLV-RUN-LOAD ASSERT-CHECKER-BOOL ;

: COUNT-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" $47C0 constant SLF-OWNER-N" SB-APPEND SLV-LF
   s" data-base SLF-OWNER-N + 0 swap !" SB-APPEND SLV-LF
   SB$ ;

: TABLE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" $47C8 constant SLF-OWNER-ROWS" SB-APPEND SLV-LF
   s" data-base SLF-OWNER-ROWS + 9 swap c!" SB-APPEND SLV-LF
   SB$ ;

: END-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" $4FC8 constant SLF-OWNER-END" SB-APPEND SLV-LF
   s" data-base SLF-OWNER-END 1- + 0 swap c!" SB-APPEND SLV-LF
   SB$ ;

: PAST-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" $4FC8 constant SLF-OWNER-END" SB-APPEND SLV-LF
   s" data-base SLF-OWNER-END + dup @ swap !" SB-APPEND SLV-LF
   SB$ ;

: FFI-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/ffi.f" SB-APPEND SLV-LF
   s" $47C0 constant SLF-OWNER-N" SB-APPEND SLV-LF
   s" TRUSTED: OWF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: OWF-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! FFI:ARGS FFI:REG-LENS 1 OWF-RET ffi-call-bounded ;" SB-APPEND SLV-LF
   s" data-base SLF-OWNER-N 8 - + OWF-WRITE drop" SB-APPEND SLV-LF
   SB$ ;

: LAYOUT ( -- )
   OWNER-WID-N-CELL $47C0 T=
   OWNER-WID-OFF OWNER-WID-N-CELL cell + T=
   OWNER-WID-ROW 8 T=
   OWNER-WID-PUB 0 T=
   OWNER-WID-PRI 4 T=
   EVAL-FRAME EVAL-MAX-DEPTH EVAL-FRAME-SIZE * + OWNER-WID-N-CELL <= TTRUE
   OWNER-WID-OFF OWNER-WID-MAX OWNER-WID-ROW * + OWNER-WID-END T=
   OWNER-WID-END TXN-STATE-OFF <= TTRUE ;

: RUN ( -- )
   s" checked source cannot call the internal owner-WID mutator" T-LABEL
   ADD-FORGE$ SLV-RUN-LOAD ASSERT-ADD-HIDDEN
   s" ordinary source cannot look up the internal owner-WID mutator" T-LABEL
   ADD-LOOKUP-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" owner-WID query primitives retain exact checked bool effects" T-LABEL
   CHECKER-REJECTS
   s" owner WID registry layout fits before lowering state" T-LABEL
   LAYOUT
   s" owner-WID preflight and role queries are checked and read-only" T-LABEL
   SOURCE$ SLV-RUN-LOAD SLV-ASSERT-OK ;

;package

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
   s" c! at the band-2 upper boundary (PROT-REG-OFF+PROT-REG-LEN-1) traps" T-LABEL
   SLV-BAND2-END-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ! into the uncaught-throw reporter hook traps" T-LABEL
   SLV-UNCGH-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" ! into the owner-WID count traps (band 3)" T-LABEL
   OWNER-WID-TEST:COUNT-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" c! into the owner-WID table traps (band 3)" T-LABEL
   OWNER-WID-TEST:TABLE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" c! at the owner-WID band's last byte traps" T-LABEL
   OWNER-WID-TEST:END-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
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
   SLV-MMAP-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" FFI live pointer arg into band 1 traps before the call" T-LABEL
   SLV-FFI-B1-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" FFI live pointer arg into band 2 traps before the call" T-LABEL
   SLV-FFI-B2-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" FFI live pointer arg crossing owner-WID band traps before the call" T-LABEL
   OWNER-WID-TEST:FFI-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL ;

\ Sealed-dictionary truncation guard (TFAM 2b-iii): FORGET-DEFS-FROM /
\ HIDE-DEFS-FROM of an engine definition (below the seal-time ndict watermark)
\ must trap E-SEAL-VIOLATION on both cold-prefix entry paths.
: SLV-NEGATIVES-TRUNCATE ( -- )
   s" FORGET-DEFS-FROM of an engine def traps via --load" T-LABEL
   SLV-FORGET-ENGINE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" FORGET-DEFS-FROM of an engine def traps via stdin pipe" T-LABEL
   SLV-FORGET-ENGINE-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of an engine def traps via --load" T-LABEL
   SLV-HIDE-ENGINE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of an engine def traps via stdin pipe" T-LABEL
   SLV-HIDE-ENGINE-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL
   s" direct CHECKER-USIGS-TRUNCATE-FROM traps via --load" T-LABEL
   SLV-USIG-TRUNC-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" direct CHECKER-USIGS-TRUNCATE-FROM traps via stdin pipe" T-LABEL
   SLV-USIG-TRUNC-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL
   s" FORGET-DEFS-FROM of the script-argv tail traps via --load" T-LABEL
   SLV-FORGET-TAIL-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" FORGET-DEFS-FROM of the script-argv tail traps via stdin pipe" T-LABEL
   SLV-FORGET-TAIL-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of the script-argv tail traps via --load" T-LABEL
   SLV-HIDE-TAIL-FORGE$ SLV-RUN-LOAD SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of the script-argv tail traps via stdin pipe" T-LABEL
   SLV-HIDE-TAIL-FORGE$ SLV-RUN-STDIN SLV-ASSERT-SEAL ;

: SLV-POSITIVES ( -- )
   s" free hole below the band stays writable" T-LABEL
   SLV-HOLE-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" store one past band 2 ($40C8) stays writable" T-LABEL
   SLV-PAST-BAND2-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" store one past owner-WID band ($4FC8) stays writable" T-LABEL
   OWNER-WID-TEST:PAST-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" legit cp!/ndict! FORGET round-trip still works" T-LABEL
   SLV-FORGET-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" FORGET-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-FORGET-USER-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" HIDE-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-HIDE-USER-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK
   s" post-seal define/package/trusted/defer still work" T-LABEL
   SLV-LANG-FORGE$ SLV-RUN-LOAD SLV-ASSERT-OK ;

: SLV-MAIN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-NEGATIVES
   SLV-NEGATIVES-SINKS
   SLV-NEGATIVES-TRUNCATE
   SLV-POSITIVES
   SLV-PWID-CAP
   SLV-PROT-PUBLISH
   OWNER-WID-TEST:RUN
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
