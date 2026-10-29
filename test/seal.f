\ seal.f - friend-arena seal regressions: the post-seal raw-write guard
\ (PROT-GUARD, exit ENGINE-ERROR:SEAL-VIOLATION) traps every store sink into a
\ sealed band, and engine primitives that update protected cells still work.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f test/seal.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/subject.f
require src/lib/engine-candidate.f
require test/tail-parity.f

package SEAL-SUITE

2048 constant SLV-CAP
30000 constant SLV-TIMEOUT-MS
ENGINE-ERROR:SEAL-VIOLATION constant SLV-SEAL-RC

variable SLV-ROOT-U
variable SLV-CHILD-U
variable SLV-IN-U
variable SLV-OUT-U
variable SLV-ERR-U
variable SLV-EXITED                 \ bool: child completed by exit
variable SLV-RC

create SLV-ROOT-BUF PATH-CAP allot
create SLV-CHILD-BUF PATH-CAP allot
create SLV-IN SLV-CAP allot          \ stdin-piped forge source
create SLV-OUT SLV-CAP allot
create SLV-ERR SLV-CAP allot
create SLV-EMPTY 1 allot            \ zero-length stdin

: SLV-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SLV-ROOT ( -- ptr u8 n )
   SLV-ROOT-BUF SLV-ROOT-U @ ;

: SLV-CHILD ( -- ptr u8 n )
   SLV-CHILD-BUF SLV-CHILD-U @ ;

: SLV-IN$ ( -- ptr u8 n )
   SLV-IN SLV-IN-U @ ;

: SLV-LF ( -- )
   10 SB-APPEND-C ;

\ Forge sources for the crown-jewel band [data-base+FRIEND-ARENA,
\ +FRIEND-ARENA-LEN): each names its offset and hits one sink. The latch is
\ inside the band (one-way seal); a free hole below the band stays writable.

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

\ Each remaining sink hand-wires a different register into PROT-GUARD, so one
\ forge per sink proves that sink's own wiring.
\ A forge that passes a band address where its sink takes an n runs with the
\ checker suspended (`check@ 0 set-check ... set-check`), so the seal refuses it.

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
   s" check@ 0 set-check 0 data-base SLF-CUR + $40 readlink drop set-check" SB-APPEND SLV-LF   \ NULL path: guard fires before the syscall
   SB$ ;

: SLV-STAT-FORGE$ ( -- ptr u8 n )           \ stat64 statbuf (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" check@ 0 set-check 0 data-base SLF-CUR + stat64 drop set-check" SB-APPEND SLV-LF
   SB$ ;

: SLV-LSTAT-FORGE$ ( -- ptr u8 n )          \ lstat64 statbuf (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" check@ 0 set-check 0 data-base SLF-CUR + lstat64 drop set-check" SB-APPEND SLV-LF
   SB$ ;

: SLV-DENTS-BUF-FORGE$ ( -- ptr u8 n )      \ getdirentries64 dirent buffer (x1) into the band
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" check@ 0 set-check 0 data-base SLF-CUR + $40 0 getdirentries64 drop set-check" SB-APPEND SLV-LF
   SB$ ;

: SLV-DENTS-BASEP-FORGE$ ( -- ptr u8 n )    \ getdirentries64 basep (x3) into the band; buffer in a free hole
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" $1A0 constant SLF-HOLE" SB-APPEND SLV-LF
   s" check@ 0 set-check 0 data-base SLF-HOLE + $40 data-base SLF-CUR + getdirentries64 drop set-check" SB-APPEND SLV-LF
   SB$ ;

: SLV-MMAP-FORGE$ ( -- ptr u8 n )           \ mmap addr (x0) into the band (MAP_FIXED-class remap guard)
   SB-RESET
   s" $28 constant SLF-CUR" SB-APPEND SLV-LF
   s" check@ 0 set-check data-base SLF-CUR + $1000 3 $1012 -1 0 mmap drop set-check" SB-APPEND SLV-LF
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

\ Sealed-dictionary truncation guard: FORGET-DEFS-FROM and HIDE-DEFS-FROM
\ truncate ndict (FORGET also rewinds CP) to a name's dict index. An index below
\ the seal-time watermark SEAL-NDICT-CELL (SEAL-CAPTURE, at the end of the
\ engine source) would retire engine definitions and rewind CP into engine code,
\ so post-seal source is refused (ENGINE-ERROR:SEAL-VIOLATION). `WORDS` is an
\ engine word (xref.f), well below the watermark.
: SLV-FORGET-ENGINE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-ENGINE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ The engine-prefix tail: src/os/script-argv.f loads after xref.f, and
\ SEAL-CAPTURE sits at the true engine-prefix end, so the tail traps like any
\ engine definition.
: SLV-FORGET-TAIL-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-TAIL-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Public ndict! may forget user definitions, but it cannot retire the sealed
\ engine. The attack continues from already compiled code and would evaluate a
\ reserved-package publication attempt if the dictionary sink failed open.
: SLV-NDICT-CONTINUATION-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: SLF-NDICT-ATTACK ( -- )" SB-APPEND SLV-LF
   s"    0 set-check 0 ndict!" SB-APPEND SLV-LF
   S\"    s\" package TFAM public : SLF-PWN ( -- ) ; ;package\" evaluate ;" SB-APPEND SLV-LF
   s" SLF-NDICT-ATTACK" SB-APPEND SLV-LF
   SB$ ;

\ Direct checker signature-registry truncation bypasses the FORGET/HIDE index
\ guard but would forget an engine word's checker signature so it could be
\ redefined (a spoof). CHECKER-USIGS-TRUNCATE-FROM refuses any post-seal call.
: SLV-USIG-TRUNC-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" CHECKER-USIGS-TRUNCATE-FROM" SB-APPEND SLV-LF
   SB$ ;

\ A user mark defined after the seal is above the watermark, so FORGET/HIDE of
\ it stays a working round-trip (retire the user tail, redefine).
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

\ Second guarded band: protected metadata [$3CB8, $40C8) contains the
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

\ cp! may select only an aligned instruction in the full DBASE code interval: a
\ data address traps at cp! itself, before any delayed emission can cross a
\ protected band. One forge per band proves the code-region guard. ndict! writes
\ records into the disjoint dict-record region (DBASE, not data-base), bounded
\ by DICT-CAP through the room check, so it cannot land in a data-base band; the
\ legit round-trip forge above proves its guard leaves FORGET intact. Each cp!
\ forge runs with the checker suspended for the same reason.
: SLV-CPSET-B2-FORGE$ ( -- ptr u8 n )       \ cp! CP into the registry band ($3CB8), then emit
   SB-RESET
   s" $3CB8 constant SLF-PWIDN" SB-APPEND SLV-LF
   s" check@ 0 set-check data-base SLF-PWIDN + cp! set-check" SB-APPEND SLV-LF
   s" : SLF-FOO ( -- n ) 1 ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-CPSET-B1-FORGE$ ( -- ptr u8 n )       \ cp! CP into the crown-jewel band ($20), then emit
   SB-RESET
   s" $20 constant SLF-LATCH" SB-APPEND SLV-LF
   s" check@ 0 set-check data-base SLF-LATCH + cp! set-check" SB-APPEND SLV-LF
   s" : SLF-FOO ( -- n ) 1 ;" SB-APPEND SLV-LF
   SB$ ;

\ A sealed-band pointer packed as a live integer or pointer FFI arg must trap
\ ENGINE-ERROR:SEAL-VIOLATION at the trampoline before the foreign call, else
\ the callee writes through it into a sealed cell. Routed through the checked
\ FFI package with an explicit writable extent; fn=0, so the whole-span guard
\ must fire first (an unguarded BLR to 0 crashes, an outcome distinct from exit
\ 83). One forge per protected band.
: SLV-FFI-B1-FORGE$ ( -- ptr u8 n )         \ 16-byte write crosses band-1 lower edge
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! FFI:ARGS FFI:REG-LENS 1 SLF-RET ffi-call-bounded ;" SB-APPEND SLV-LF
   s" data-base $18 + SLF-WRITE drop" SB-APPEND SLV-LF
   SB$ ;

: SLV-FFI-B2-FORGE$ ( -- ptr u8 n )         \ 16-byte write crosses band-2 lower edge
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! FFI:ARGS FFI:REG-LENS 1 SLF-RET ffi-call-bounded ;" SB-APPEND SLV-LF
   s" data-base $3CB8 + SLF-WRITE drop" SB-APPEND SLV-LF
   SB$ ;

\ Raw abi trampoline (no schema extents): the nargs-threaded guard must trap a
\ sealed-band pointer packed as the x8 indirect-result register before the
\ foreign BLR, else the callee writes the result through it into a sealed cell.
\ has-sret=1, int-nargs=0 -> only x8 is checked.
: SLV-FFI-SRET-FORGE$ ( -- ptr u8 n )       \ sealed-band pointer as the x8 sret out-param
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-SRET ( ptr a -- n ) {: p:ptr :} FFI:RESET p FFI:X8-READABLE! FFI:ARGS FFI:FLOATS FFI:STACK 0 0 1 SLF-RET ffi-call-abi ;" SB-APPEND SLV-LF
   s" data-base $28 + SLF-SRET drop" SB-APPEND SLV-LF
   SB$ ;

\ Raw abi trampoline: a sealed-band pointer packed as a high integer register
\ arg (x6, live only at int-nargs=7) must trap: the guard sweeps every live slot
\ [0..int-nargs), not just the low registers. Slots 0..5 hold benign scalars so
\ the trap is attributable to arg[6].
: SLV-FFI-A6-FORGE$ ( -- ptr u8 n )         \ sealed-band pointer as the arg[6] register arg
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND SLV-LF
   s" TRUSTED: SLF-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ;" SB-APPEND SLV-LF
   s" TRUSTED: SLF-A6 ( ptr a -- n ) {: p:ptr :} FFI:RESET 6 0 ?do 1 i FFI:VALUE! loop p FFI:>CELL 6 FFI:VALUE! FFI:ARGS FFI:FLOATS FFI:STACK 0 7 0 SLF-RET ffi-call-abi ;" SB-APPEND SLV-LF
   s" data-base $28 + SLF-A6 drop" SB-APPEND SLV-LF
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

: SLV-STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N SLV-ERR-U !  outu LEN>N SLV-OUT-U !
   oc MATCH outcome
     exited OF SLV-RC ! 0 0= SLV-EXITED ! ENDOF
     signaled OF SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
     timeout OF src u SLV-OUT SLV-OUT-U @ SLV-ERR SLV-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: SLV-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SLV-CAP > if E-FS-CAPACITY throw then
   a SLV-IN u BYTE-COPY
   u SLV-IN-U ! ;

: SLV-RUN-LOAD ( ptr u8 n -- ) {: src:ptr u:n :}
   SLV-CHILD src u FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SLV-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  SLV-EMPTY 0 >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  SLV-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u SLV-STORE! ;

\ The piped stdin program (no --load) is the other cold-prefix path.
: SLV-RUN-STDIN ( ptr u8 n -- )
   SLV-IN!
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  SLV-IN$ >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  SLV-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SLV-IN$ SLV-STORE! ;

\ Fixtures run in SUBJECT forks of the sealed candidate; clean-start
\ registry-capacity fixtures run directly.
: SLV-SUBJECT ( ptr u8 n -- ) {: src:ptr u:n :}
   src u SLV-OUT SLV-CAP >LEN SLV-ERR SLV-CAP >LEN
   SLV-TIMEOUT-MS >MS SUBJECT:RUN src u SLV-STORE! ;

: SLV-ASSERT-SEAL ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-SEAL-RC T= ;

: SLV-ASSERT-OK ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ 0 T= ;

\ Protected-WID capacity: each public ADT family protects one wordlist (xref.f
\ PROT-WID-CTOR-ADD -> prot-wid-add). The registry is a WID-indexed bitmap
\ bounded by PROT-WID-MAX, and prot-wid-room reports the ids left. At zero room
\ a declaration is refused at the preflight with E-PROTECTION-CAP (7169), not at
\ the irreversible prot-wid-add (exit 84), which would break atomic publication.
\ The child burns room to a known value with `wordlist`, the allocator a
\ declaration consumes, and reads prot-wid-room rather than assuming a baseline.
16384 constant PWG-CAP                       \ room for 256+ families at ~46 bytes each
create PWG-BUF PWG-CAP allot
variable PWG-U

: PWG$ ( -- ptr u8 n )  PWG-BUF PWG-U @ ;
: PWG-RESET ( -- )  0 PWG-U ! ;
: PWG-C ( n -- ) {: c:n :}
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
   s" <a> VARIANT foo a ;VARIANT ;SUMTYPE" PWG-APPEND
   10 PWG-C ;
: PWG-GEN ( n -- ptr u8 n ) {: k:n :}        \ K public families as one batch program
   PWG-RESET
   0 begin dup k < while dup PWG-FAMILY 1+ repeat drop
   PWG$ ;

\ Burn `prot-wid-room - spare` wordlist ids in the child, leaving exactly `spare`,
\ then declare one public family.
: PWG-BURN ( n -- ptr u8 n ) {: spare:n :}
   PWG-RESET
   s" : PWG-BURN-N ( n -- ) 0 ?do wordlist drop loop ;" PWG-APPEND  10 PWG-C
   s" prot-wid-room " PWG-APPEND
   $30 spare 10 / + PWG-C  $30 spare 10 mod + PWG-C
   s"  - PWG-BURN-N" PWG-APPEND  10 PWG-C
   \ An ENUM, not a SUMTYPE: the unified ENUM front end renders the declaration
   \ diagnostic packet, which is where the reason text under test appears.
   s" ENUM zz DERIVE eq" PWG-APPEND  10 PWG-C
   s"    z-a" PWG-APPEND  10 PWG-C
   s"    z-b" PWG-APPEND  10 PWG-C
   s" ;ENUM" PWG-APPEND  10 PWG-C
   PWG$ ;

\ The ENUM front end renders its refusal before it throws 7169, so the child
\ exits 70, the refusal status (src/habu/habu2.f RC-REJECT).
70 constant SLV-PWID-PREFLIGHT-RC
: SLV-ERR$ ( -- ptr u8 n )  SLV-ERR SLV-ERR-U @ ;
: SLV-ASSERT-PWID-PREFLIGHT ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-PWID-PREFLIGHT-RC T=
   SLV-ERR$ s" hb: uncaught throw code 7169" CONTAINS? TTRUE
   \ The diagnostic names the registry, not the family that declared next.
   SLV-ERR$ s" the protected-wordlist registry is full" CONTAINS? TTRUE ;

\ The irreversible sink refuses an id it has no bit for, names the bound, and exits
\ 84 rather than writing outside the band. That range check is the whole
\ memory-safety argument for a primitive that takes a caller-supplied index.
: PWG-OOR-FORGE$ ( -- ptr u8 n )
   s" PROT-WID-MAX prot-wid-add" ;
: SLV-ASSERT-PWID-OOR ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   SLV-ERR$ S\" hb: protected-WID id above the bound\n" CONTAINS? TTRUE ;

: SLV-PWID-CAP ( -- )
   s" 17 public ADT families succeed past the old 16 cap" T-LABEL
   17 PWG-GEN SLV-RUN-LOAD SLV-ASSERT-OK
   s" a public family succeeds while protected-WID room remains" T-LABEL
   8 PWG-BURN SLV-RUN-LOAD SLV-ASSERT-OK
   s" a public family at zero room is rejected before protected-WID publication" T-LABEL
   0 PWG-BURN SLV-RUN-LOAD SLV-ASSERT-PWID-PREFLIGHT
   s" prot-wid-add refuses an id at the bitmap bound and names it" T-LABEL
   PWG-OOR-FORGE$ SLV-RUN-LOAD SLV-ASSERT-PWID-OOR ;

\ After the seal, publishing into a protected WID (a public family's generated
\ constructor package) via `<pkg>:WORD` fails closed: habu2.f C-STORE-DEF-NAME
\ names itself on fd 2 (LPROTPUB) with the offending word before exit 84. The
\ sibling AOT boot-pass gate EM-AOTWIDGATE (LPROTAOT) fires only on a crafted
\ AOT or snapshot image and has no fixture here.
: SLV-PUBLISH-FORGE$ ( -- ptr u8 n )         \ define a word into a public family's protected WID
   SB-RESET
   s" SUMTYPE foo<a> VARIANT bar a ;VARIANT ;SUMTYPE" SB-APPEND SLV-LF
   s" : foo:BOGUS ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-ASSERT-PROT-PUBLISH ( -- )             \ child died ENGINE-ERROR:SEAL-PACKAGE naming the publish guard
   SLV-EXITED @ TTRUE
   SLV-RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   SLV-ERR$ s" hb: cannot publish into protected word" CONTAINS? TTRUE ;

: SLV-PROT-PUBLISH ( -- )
   s" : foo:BOGUS ; into a protected WID -> labeled exit 84" T-LABEL
   SLV-PUBLISH-FORGE$ SLV-SUBJECT SLV-ASSERT-PROT-PUBLISH ;

\ The two engine-reserved OWNER-API wordlists are permanently protected, so a
\ checked program cannot forge a record into them with set-current. Without this,
\ `2 set-current : X ;` would stamp X with OWNER-API-PRI-WID -- the marker that
\ seals a record as a system-private engine helper and hides it from raw word
\ searches -- and a crafted image could reach that forged code by a direct branch,
\ which the ahead-of-time closure walker resolves to any record by its exact code
\ entry (src/habu/aot-closure.f FINDADDR-PTR).
: SLV-OWNER-PRI-FORGE$ ( -- ptr u8 n )       \ publish into the private engine-API wordlist (helper marker)
   SB-RESET
   s" 2 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-PUB-FORGE$ ( -- ptr u8 n )       \ publish into the public engine-API wordlist
   SB-RESET
   s" 1 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-FORGE ( -- )
   s" : X ; via 2 set-current into OWNER-API-PRI-WID -> labeled exit 84" T-LABEL
   SLV-OWNER-PRI-FORGE$ SLV-SUBJECT SLV-ASSERT-PROT-PUBLISH
   s" : X ; via 1 set-current into OWNER-API-PUB-WID -> labeled exit 84" T-LABEL
   SLV-OWNER-PUB-FORGE$ SLV-SUBJECT SLV-ASSERT-PROT-PUBLISH ;

: RESULT ( -- ptr u8 n ptr u8 n n )
   SLV-OUT SLV-OUT-U @ SLV-ERR SLV-ERR-U @ SLV-RC @ ;

: NEG-LOAD ( ptr u8 n -- )
   2dup SLV-RUN-LOAD SLV-ASSERT-SEAL
   RESULT TAIL-PARITY:SNAPSHOT
   SLV-SUBJECT SLV-ASSERT-SEAL
   RESULT TAIL-PARITY:SAME ;

: NEG-STDIN ( ptr u8 n -- )
   2dup SLV-RUN-STDIN SLV-ASSERT-SEAL
   RESULT TAIL-PARITY:SNAPSHOT
   SLV-SUBJECT SLV-ASSERT-SEAL
   RESULT TAIL-PARITY:SAME ;

: POS-LOAD ( ptr u8 n -- )
   2dup SLV-RUN-LOAD SLV-ASSERT-OK
   RESULT TAIL-PARITY:SNAPSHOT
   SLV-SUBJECT SLV-ASSERT-OK
   RESULT TAIL-PARITY:SAME ;

: SLV-PARITY-TEST ( -- )
   s" direct --load and subject preserve raw seal results" T-LABEL
   SLV-CUR-FORGE$ NEG-LOAD
   s" direct stdin and subject preserve raw seal results" T-LABEL
   SLV-CUR-FORGE$ NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   SLV-LANG-FORGE$ POS-LOAD ;

: SLV-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-seal" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u SLV-ROOT-BUF SLV-ROOT-U SLV-COPY!
   SLV-ROOT FS:CLEANUP-TREE+
   SLV-ROOT s" forge.f" SLV-CHILD-BUF FS:JOIN-PATH SLV-CHILD-U ! ;

: SLV-CLEANUP ( -- )
   FS:CLEANUP-RUN
   SLV-ROOT FS:EXISTS? TFALSE ;

: SLV-NEGATIVES ( -- )
   s" ! into CUR-CELL traps" T-LABEL
   SLV-CUR-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" overwrite of the latch itself traps (one-way seal)" T-LABEL
   SLV-LATCH-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" c! into WIDN-CELL traps" T-LABEL
   SLV-WIDN-C-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" +! into WIDN-CELL traps" T-LABEL
   SLV-WIDN-ADD-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" atomic! into the band traps" T-LABEL
   SLV-ATOMIC-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" read buffer starting in the band traps" T-LABEL
   SLV-READ-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" ! into the protected-WID registry count traps (band 2)" T-LABEL
   SLV-PWIDN-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" c! into the protected-WID registry table traps (band 2)" T-LABEL
   SLV-PWIDT-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" c! at the band-2 upper boundary (PROT-REG-OFF+PROT-REG-LEN-1) traps" T-LABEL
   SLV-BAND2-END-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" ! into the uncaught-throw reporter hook traps" T-LABEL
   SLV-UNCGH-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" cp! redirecting emission into band 2 traps at the sink" T-LABEL
   SLV-CPSET-B2-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" cp! redirecting emission into band 1 traps at the sink" T-LABEL
   SLV-CPSET-B1-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL ;

: SLV-NEGATIVES-SINKS ( -- )
   s" atomic-add addr (x10) into the band traps" T-LABEL
   SLV-ATADD-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" atomic-cas addr (x11, only C guard) into the band traps" T-LABEL
   SLV-ATCAS-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" ioctl arg (x2) into the band traps" T-LABEL
   SLV-IOCTL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" poll pollfd array (x0) into the band traps" T-LABEL
   SLV-POLL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" readlink link buffer (x2) into the band traps" T-LABEL
   SLV-READLINK-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" stat64 statbuf (x1) into the band traps" T-LABEL
   SLV-STAT-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" lstat64 statbuf (x1) into the band traps" T-LABEL
   SLV-LSTAT-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" getdirentries64 dirent buffer (x1) into the band traps" T-LABEL
   SLV-DENTS-BUF-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" getdirentries64 basep (x3) into the band traps" T-LABEL
   SLV-DENTS-BASEP-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" mmap addr (x0) into the band traps" T-LABEL
   SLV-MMAP-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" FFI live pointer arg into band 1 traps before the call" T-LABEL
   SLV-FFI-B1-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" FFI live pointer arg into band 2 traps before the call" T-LABEL
   SLV-FFI-B2-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" raw abi FFI x8 sret out-param into band traps before the call" T-LABEL
   SLV-FFI-SRET-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" raw abi FFI high arg[6] register into band traps before the call" T-LABEL
   SLV-FFI-A6-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL ;

: SLV-NEGATIVES-TRUNCATE ( -- )
   s" FORGET-DEFS-FROM of an engine def traps" T-LABEL
   SLV-FORGET-ENGINE-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of an engine def traps" T-LABEL
   SLV-HIDE-ENGINE-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" direct CHECKER-USIGS-TRUNCATE-FROM traps" T-LABEL
   SLV-USIG-TRUNC-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" FORGET-DEFS-FROM of the script-argv tail traps" T-LABEL
   SLV-FORGET-TAIL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of the script-argv tail traps" T-LABEL
   SLV-HIDE-TAIL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" public ndict! continuation cannot cross the engine watermark" T-LABEL
   SLV-NDICT-CONTINUATION-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL ;

: SLV-POSITIVES ( -- )
   s" free hole below the band stays writable" T-LABEL
   SLV-HOLE-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" store one past band 2 ($40C8) stays writable" T-LABEL
   SLV-PAST-BAND2-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" legit cp!/ndict! FORGET round-trip still works" T-LABEL
   SLV-FORGET-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" FORGET-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-FORGET-USER-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" HIDE-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-HIDE-USER-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" post-seal define/package/trusted/defer still work" T-LABEL
   SLV-LANG-FORGE$ SLV-SUBJECT SLV-ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-PARITY-TEST
   SLV-NEGATIVES
   SLV-NEGATIVES-SINKS
   SLV-NEGATIVES-TRUNCATE
   SLV-POSITIVES
   SLV-PWID-CAP
   SLV-PROT-PUBLISH
   SLV-OWNER-FORGE
   SLV-CLEANUP
   T-REPORT
   s" seal-test: ok" type cr ;

;package

SEAL-SUITE:RUN

\ Guarded sinks no forge covers. The rest (BSTORE BPLUSSTORE BCSTORE BATSTORE
\ BATADD on x10=B, BATCAS on x11=C, BREAD BSTAT64 BLSTAT64 on x1, BIOCTL
\ BREADLINK on x2, BPOLL BMMAP on x0, BGETDIRENTRIES64 on x1 and x3) are forged
\ above. Two compiler-internal sinks are covered by review only:
\ - patch32 (BPATCH32, x10=B), the engine-text patcher: a forge would store into
\   a live code page through the alias and flush its i-cache line. Its B
\   register is proven by the store forges; only its own guard line is reviewed.
\ - snap-rebase (BSNAPREBASE, src/habu/habu2.f), driven only by the snapshot
\   builder: a forge would run its relocation rewrites. Its span guard over
\   [x8, x16) is used by no other sink.
