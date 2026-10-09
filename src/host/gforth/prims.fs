\ src/host/gforth/prims.fs - one Gforth body per src/habu/prims.f row, each
\ registered as a seeded record of the global wordlist before the kernel
\ loads (docs/bootstrap.md stage 1; habu2.f EMIT-SCOPE-REC "SEEDED").
\
\ A row the host performs gets its body here, in reader.fs or as Gforth's own
\ word; a row it cannot perform gets REFUSE's body, which prints
\ `hb: gforth has no <row>` and exits 76 as kernel-x64.f REFUSE-BODY does.
\ PRIMS-CHECK runs once prims.f has loaded: a row with no body, or a body with
\ no row, dies naming it, and each body's record takes its row's min-in byte.

\ ---- the span guard (habu1.f:295 GUARD-SPAN) ----------------------------------
\ The protected bands are src/habu/layout.f's DATA-BANDS table, which the
\ prefix loads before the seal: row ix is DATA-BANDS:LEN bytes from the
\ data-space offset DATA-BANDS:OFF, and the row whose length is zero ends it.
\ SEAL-FRIEND copies the rows into BANDS as [start, end) addresses; until
\ BANDS is set the guard passes every write, as native's does while the
\ friend latch is open. From then on a write of len bytes at addr whose span
\ wraps or meets a band exits ENGINE-ERROR:SEAL-VIOLATION with no message and
\ no exit hook; a zero-length write passes. Every sink native guards that the
\ host performs takes it before it writes (src/habu/layout.f:413-424): ! xt!
\ +! c! patch32, and the spans read, realpath and munmap name.
variable BANDS  variable BANDS-N
\ A word the seal reads, by its name or its qualified name; a missing one ends
\ the boot naming it, as GLOBAL-XT does.
: SEAL-XT ( c-addr u -- xt ) 2dup LFIND ?dup if nip nip @ exit then RC-REJECT RC-DIE ;
: READ-BANDS ( -- )
   s" DATA-BANDS:OFF" SEAL-XT  s" DATA-BANDS:LEN" SEAL-XT {: row-off row-len :}
   0 begin dup row-len execute while 1+ repeat {: n :}
   n 2* cells allocate throw {: t :}
   n 0 ?do
      i row-off execute D  dup i row-len execute +  t i 2* cells + 2!
   loop
   t BANDS !  n BANDS-N ! ;
: MEETS? ( addr end band -- flag ) 2@ {: a e s t :} a t u<  e s u>  and ;
: SPAN-GUARD ( addr len -- ) {: a u :}
   BANDS @ 0= u 0= or ?exit
   a u + {: e :}
   e a u< if RC-SEAL-VIOLATION (bye) then
   BANDS @ BANDS-N @ 2* cells bounds ?do
      a e i MEETS? if RC-SEAL-VIOLATION (bye) then
   2 cells +loop ;
: HB-! ( x addr -- ) dup cell SPAN-GUARD ! ;            \ habu1.f:1789 BSTORE, habu2.f:8002 BXTSTORE
: HB-+! ( n addr -- ) dup cell SPAN-GUARD +! ;          \ habu1.f:1795 BPLUSSTORE
: HB-C! ( c addr -- ) dup 1 SPAN-GUARD c! ;             \ habu1.f:1801 BCSTORE

\ ---- bodies the host serves itself ------------------------------------------
\ Division truncates toward zero (src/habu/prim-ref.f:223 DIVREM); a zero
\ divisor throws ARITH-ABI:E-DIV-ZERO (src/habu/arith-abi.f:29).
-6400 constant E-DIV-ZERO
: DIVREM ( n n -- rem quot ) dup 0= if E-DIV-ZERO throw then >r s>d r> sm/rem ;
: HB-/ ( n n -- n ) DIVREM nip ;
: HB-MOD ( n n -- n ) DIVREM drop ;
\ Views and fields (habu1.f BPTRFIELD, BADDRESSVIEW): address arithmetic only.
: HB-PTR-FIELD ( ptr n -- ptr ) cells + ;
\ die ( c-addr u rc -- ) (habu1.f:1935 BDIE): the message and a newline on
\ fd 2, then the exit path.
: HB-DIE ( c-addr u rc -- ) >r ERR ERR-NL r> HB-EXIT ;
: HB-CLOSE ( fd -- ) close drop ;
\ 2>r 2r> 2r@ (habu1.f:1858-1865 B2TOR B2RFROM B2RFETCH): the pair as one
\ two-cell block of the return stack (reader.fs RT-N>R).
: HB-2>R ( x x -- ) 2 RT-N>R ;
: HB-2R> ( -- x x ) 2 RT-NR> ;
: HB-2R@ ( -- x x ) 2 RT-NR@ ;
\ Anonymous mappings (habu1.f BMAPANON, BMUNMAP): zeroed pages, rc 0 on success.
: HB-MAP-ANON ( u -- addr rc )
   0 swap MMAP-PROT MMAP-FLAGS -1 0 HOST-MMAP dup -1 = if -1 else 0 then ;
\ The OS rows the boot stream reaches, through libc (Gforth libcc, as
\ layout.fs binds mmap); each answers -1 where the native syscall's carry
\ answers it (habu1.f SYS-PUSH).
c-library habu_gforth_prims
\c #include <stdlib.h>
\c #include <fcntl.h>
\c #include <unistd.h>
c-function HOST-REALPATH realpath a a -- a
c-function HOST-ACCESS access a n -- n
c-function HOST-OPEN open a n n -- n
c-function HOST-READ read n a n -- n
c-function HOST-FREE free a -- void
end-c-library
: HB-OPEN-RD ( pathz -- fd ) 0 0 HOST-OPEN ;   \ src/os/macos/sys.f OS-OPEN-RD
: HB-READ ( fd buf len -- n ) 2dup SPAN-GUARD HOST-READ ;          \ habu1.f:1982 BREAD
: HB-MUNMAP ( addr len -- n ) 2dup SPAN-GUARD HOST-MUNMAP ;        \ habu1.f:2013 BMUNMAP
\ realpath (habu1.f:2590 REALPATH): the resolved path and its NUL reach dst
\ only when both fit. Its length; -2 for a capacity not above 0 or a path that
\ does not fit; -1 when the path does not resolve. A capacity above 0 is
\ guarded whole, before the path resolves.
: HB-REALPATH ( pathz dst cap -- n )
   {: z dst cap :}
   cap 0 <= if -2 exit then
   dst cap SPAN-GUARD
   z 0 HOST-REALPATH ?dup 0= if -1 exit then {: r :}
   r 0 begin 2dup + c@ while 1+ repeat nip {: u :}
   u cap < if r dst u 1+ move u else -2 then
   r HOST-FREE ;
\ evaluate-closed (habu1.f:3411 B-EVAL-CLOSED; include.f:1218 INCLUDE-EVALUATE):
\ the text, kept for the run as LOAD-FILE keeps a file, is read by the host's
\ reader (EVAL-TEXT) above its own floor, which refuses the token that reaches
\ under it (reader.fs UNDERDEPTH), and a throw out of it unwinds to the
\ caller's handler. Its net effect is ( -- ): what it leaves is dropped and
\ refused E-EVAL-RESIDUE.
-3804 constant E-EVAL-RESIDUE          \ src/habu/stack-abi.f:62 STACK-ABI:E-EVAL-RESIDUE
: HB-EVAL-CLOSED ( c-addr u -- )
   save-mem  depth 2 - {: floor :}  EVAL-TEXT
   depth floor - ?dup if 0 ?do drop loop E-EVAL-RESIDUE throw then ;
\ run-rc ( pathz -- rc ) has no specification row (prims.f:1011 UNROWED:), so
\ its record states no inputs and passes the interpreter's min-in gate. Native's
\ body (habu1.f:784 BRUNRC) reads pathz first, and with no cell above the floor
\ that read is refused (habu2.f LFLOORREC). The host reads its operand the same
\ way before it refuses the row.
s" run-rc" REFUSAL-XT constant RUN-RC-REFUSAL
: HB-RUN-RC ( pathz -- rc )
   depth FLOOR-DEPTH @ - 1 < if UNDERDEPTH then  RUN-RC-REFUSAL execute ;
\ patch32 ( w addr -- ) (habu1.f:2744 BPATCH32): the low 32 bits of w at addr.
\ Native flips the target's pages writable and syncs the instruction cache
\ around the store; the host's records are plain memory, so the guarded store
\ is all.
: HB-PATCH32 ( w addr -- ) dup 4 SPAN-GUARD l! ;
\ Record flag stamps (habu1.f BWIDEMARK, BINTMARK, BMININMARK). Marking ORs:
\ min-in-mark ORs the low byte of n into bits 52-59 of record ix's flags, as
\ PRIMS-CHECK stamps each row's min-in byte and the seal pass
\ (src/core/internal-mark.f) stamps every certified engine record's.
: HB-WIDE-MARK ( -- ) DNAME-WIDE NDICT @ 1- FLAG! ;
: HB-INT-MARK ( ix -- ) DNAME-INT swap FLAG! ;
: MIN-IN-MARK ( ix n -- ) $FF and 52 lshift swap FLAG! ;
\ The protected-wid bitmap (habu1.f:3920 BPROTWIDADD): past the bound, the
\ message on fd 2 and exit ENGINE-ERROR:SEAL-PACKAGE with no exit hook.
84 constant RC-SEAL-PACKAGE          \ src/core/engine-error.f:7 ENGINE-ERROR:SEAL-PACKAGE
: HB-PROT-WID-ADD ( wid -- )
   dup PROT-WID-MAX u>= if
      drop s" hb: protected-WID id above the bound" RC-SEAL-PACKAGE RC-DIE then
   1 over 63 and lshift  swap 6 rshift cells PROT-BITS-OFF + D
   dup @ rot or swap ! ;
\ prot-wid-room (habu1.f:4038 BPROTWIDROOM): the wids left below the bound, 0
\ once WIDN reaches it.
: HB-PROT-WID-ROOM ( -- n ) PROT-WID-MAX WIDN-CELL D@ - 0 max ;
\ The seal (habu1.f:3817 BSEALCAPQ, :3821 BSEALCAP, :3839 BSEALFRIEND).
\ SEAL-CAPTURE names each pre-trust defer still pending on fd 2 and exits 73
\ with no exit hook; with the table drained it records NDICT as the seal-time
\ watermark, which seal-captured? reports and ndict! keeps. SEAL-FRIEND stores
\ FRIEND-ARENA-LEN in FRIEND-LATCH-CELL, both src/habu/layout.f facts, and arms
\ SPAN-GUARD with the band table. The seal is one-way: a later SEAL-FRIEND
\ changes nothing, as native's stores the latch's own value again, and it
\ resolves no name in its caller's scope.
73 constant UNDRAINED-RC               \ habu1.f:3837 BSEALCAP
: HB-SEAL-CAPTURE ( -- )
   PD-N @ ?dup if
      0 do s" hb: undrained pre-trust defer: " ERR i PD-ENTRY 2@ ERR ERR-NL loop
      UNDRAINED-RC (bye) then
   NDICT @ SEAL-NDICT-CELL D! ;
: HB-SEAL-CAPTURED? ( -- flag ) SEAL-NDICT-CELL D@ 0<> ;
: HB-SEAL-FRIEND ( -- )
   BANDS @ ?exit
   s" FRIEND-ARENA-LEN" SEAL-XT execute  s" FRIEND-LATCH-CELL" SEAL-XT execute D!
   READ-BANDS ;
\ Hook cells (habu1.f:3611 BSETCHECK, :3782 BSETTOPCHECK, :3747 BSETPREFLIGHT).
\ set-check and set-top-check take 0 to uninstall; any other xt must be a live
\ code entry, else the installer's diagnostic goes to fd 2 and the process
\ exits 70 with no exit hook. Native's live code is its JIT region below CP
\ (DBASE <= xt < CP). The host's code is Gforth's dictionary, the sections
\ Gforth's in-dictionary? walks (Gforth sections.fs:62-66 which-section?),
\ each live from its start to its dp. 0 set-check also clears the preflight
\ cell. set-preflight installs once: into the empty cell only a live code
\ entry, so never 0; over an installed hook only the same xt, which changes
\ nothing. Each refusal names itself on fd 2 and exits 70 with no exit hook.
: LIVE-XT? ( xt -- flag )
   false [: over section-start @ section-dp @ within or ;] sections-execute nip ;
: HOOK-XT ( xt c-addr u -- xt ) {: xt a u :}
   xt 0<> xt LIVE-XT? 0= and if a u RC-REJECT RC-DIE then xt ;
: HB-SET-CHECK ( xt -- )
   s" set-check: invalid checker xt" HOOK-XT
   dup HOOK-CELL D!  0= if 0 COMPILE-PREFLIGHT-CELL D! then ;
: HB-SET-TOP-CHECK ( xt -- )
   s" set-top-check: invalid top-row hook xt" HOOK-XT TOP-HOOK-CELL D! ;
: HB-SET-PREFLIGHT ( xt -- )
   COMPILE-PREFLIGHT-CELL D@ ?dup if
      = ?exit  s" set-preflight: invalid or replaced hook" RC-REJECT RC-DIE then
   dup LIVE-XT? 0= if s" set-preflight: invalid hook" RC-REJECT RC-DIE then
   COMPILE-PREFLIGHT-CELL D! ;
\ The code pointer (prims.f 578-590, 657-658). The host's code is Gforth words
\ in Gforth's own dictionary, placed only for a definition that succeeds, so
\ no reader rewinds code and CP never moves from the code area's first slot,
\ DBASE+DICT-SIZE, where the native boot starts it. cp! of that slot does
\ nothing; any other is a code rewind the host does not have.
HB-DBASE DICT-SIZE + constant CP-SLOT
: HB-CP@ ( -- n ) CP-SLOT ;
: HB-CP! ( n -- )
   CP-SLOT <> if s" hb: gforth has no code rewind" ERR ERR-NL REFUSE-RC HB-EXIT then ;
: HB-NDICT@ ( -- n ) NDICT @ ;

\ ---- registration ------------------------------------------------------------
\ PRIM ( xt "row" -- ) seeds the row's global record with body xt; REFUSE
\ ( "row" -- ) seeds it with the refusal body; INT marks the newest seed
\ DNAME-INT, as habu1.f registers it under ENGINE-PRIMS:GLOBAL-INT-WID.
: SEED-ROW ( xt c-addr u -- ) 0 REC-PEND drop REC-PUBLISH ;
: PRIM ( xt "row" -- ) parse-name SEED-ROW ;
: REFUSE ( "row" -- ) parse-name 2dup REFUSAL-XT -rot SEED-ROW ;
: INT ( -- ) DNAME-INT NDICT @ 1- FLAG! ;

\ ---- the rows, in src/habu/prims.f order -------------------------------------
' HB-FINALLY PRIM finally
REFUSE c2-invoke
REFUSE c2-init-stow
REFUSE c2-records-stow
REFUSE unit-compile-run INT
REFUSE source-unit-run INT
' dup PRIM dup         ' drop PRIM drop       ' swap PRIM swap
' over PRIM over       ' nip PRIM nip         ' tuck PRIM tuck
' rot PRIM rot         ' -rot PRIM -rot       ' 2dup PRIM 2dup
' 2drop PRIM 2drop     ' 2swap PRIM 2swap     ' 2over PRIM 2over
' + PRIM +             ' - PRIM -             ' * PRIM *
' and PRIM and         ' or PRIM or           ' xor PRIM xor
' 1+ PRIM 1+           ' 1- PRIM 1-           ' negate PRIM negate
' invert PRIM invert   ' 0= PRIM 0=           ' 0< PRIM 0<
' = PRIM =             ' < PRIM <             ' > PRIM >
' <> PRIM <>           ' <= PRIM <=           ' >= PRIM >=
' HB-/ PRIM /          ' HB-MOD PRIM mod      REFUSE /mod
' abs PRIM abs         ' min PRIM min         ' max PRIM max
' lshift PRIM lshift   ' rshift PRIM rshift   ' cells PRIM cells
' cell+ PRIM cell+     REFUSE chars          REFUSE char+
' @ PRIM @             ' HB-! PRIM !
' HB-! PRIM xt!                  \ a store plus a declaration only the snapshot loader reads
' drop PRIM ptr-cell-mark        \ that declaration alone
REFUSE addr-cells-abi
REFUSE snapshot-format
' HB-PTR-FIELD PRIM ptr-field
' noop PRIM byte-view            ' noop PRIM cell-view
' HB-+! PRIM +!        ' c@ PRIM c@           ' HB-C! PRIM c!
\ codegen.fs meets no mode-6 operand (habu2.f:11598-11602 CMM=6): WITH-FIELD's lib/c2-memory.f stops here.
REFUSE atomic@         REFUSE atomic!
REFUSE atomic-add      REFUSE atomic-cas      REFUSE fence
REFUSE run-in-stack
' HB-EVAL-CLOSED PRIM evaluate-closed
' count PRIM count
' HB-DOT PRIM .
REFUSE .s
' depth PRIM depth
' HB-HERE PRIM here
' HB-TOK-IMM? PRIM tok-imm?
' HB-SCOPE-KIND? PRIM scope-kind?
' HB-ALLOT PRIM allot  ' HB-ALIGN PRIM align
' HB-, PRIM ,          ' HB-C, PRIM c,
' HB-TYPE PRIM type
' throw PRIM throw
' HB-DIE PRIM die
REFUSE open
' HB-READ PRIM read
REFUSE ioctl
' HB-MAP-ANON PRIM map-anon
REFUSE mmap
' HB-OPEN-RD PRIM open-rd         ' HOST-ACCESS PRIM access
REFUSE unlink          REFUSE rename          REFUSE chmod
REFUSE symlink         REFUSE readlink
' HB-REALPATH PRIM realpath
REFUSE mkdir           REFUSE rmdir
REFUSE stat64          REFUSE lstat64         REFUSE getdirentries64
REFUSE pipe            REFUSE dup2            REFUSE fcntl
REFUSE poll            REFUSE kill            REFUSE setpgid
REFUSE spawn-io        REFUSE spawn-argv-io   REFUSE spawn-argv-env-io
REFUSE spawn-argv-env-cwd-io
REFUSE fork            REFUSE wait-status
' HB-PATCH32 PRIM patch32
REFUSE code-publish
REFUSE native-unit-publish INT
REFUSE callmap-set     REFUSE addrmap-set     REFUSE xref-retarget
' HB-INT-MARK PRIM int-mark INT
' MIN-IN-MARK PRIM min-in-mark INT
REFUSE reloc-maps-clear
REFUSE does-patch      REFUSE does-record     REFUSE snap-rebase
' write PRIM write
' HB-CLOSE PRIM close
REFUSE close-rc
REFUSE epoch-seconds   REFUSE mono-ns
REFUSE prof-on         REFUSE prof-report     REFUSE prof-off
REFUSE prof-reset      REFUSE prof-rate       REFUSE prof-json
REFUSE prof-row        REFUSE prof-pc>rec
REFUSE rbase
' HB-CP@ PRIM cp@      ' HB-CP! PRIM cp!
' HB-DBASE PRIM dbase@
REFUSE check@          REFUSE tier@           REFUSE tick-order@
REFUSE code-origin
REFUSE executable-build-enter INT
REFUSE executable-build-leave INT
' HB-SET-CHECK PRIM set-check
' HB-SET-PREFLIGHT PRIM set-preflight
' HB-SET-TOP-CHECK PRIM set-top-check
REFUSE set-tier
' HB-NDICT@ PRIM ndict@
' HB-NDICT! PRIM ndict!
REFUSE seed-ndict!
REFUSE ndict-append INT
REFUSE def-occ-select INT
REFUSE def-occ-resolve INT
REFUSE namespace-record INT
REFUSE namespace-private INT
REFUSE alias-record INT
REFUSE package-scope! INT
REFUSE def-open INT
REFUSE body-append INT
REFUSE trust-sig! INT
REFUSE created-sig! INT
REFUSE def-close INT
REFUSE def-create INT
REFUSE replay-open INT
REFUSE replay-close INT
REFUSE replay-widn! INT
REFUSE replay-record INT
REFUSE record-wid! INT
REFUSE replay-private INT
' HB-SEAL-CAPTURE PRIM SEAL-CAPTURE
' HB-SEAL-CAPTURED? PRIM seal-captured?
' HB-SEAL-FRIEND PRIM SEAL-FRIEND
' HB-DRAIN-PRETRUST PRIM DRAIN-PRETRUST
' HB-DATA PRIM data-base
' HB-PROT-WID-ADD PRIM prot-wid-add
' HB-PROT-WID-ROOM PRIM prot-wid-room
REFUSE policy-admit    REFUSE policy-seal
' NEW-WID PRIM wordlist
' HB-GET-CURRENT PRIM get-current
' HB-SET-CURRENT PRIM set-current
' HB-SEARCH-WL PRIM search-wl
' REC-FIND PRIM xref-search-wl INT
' HB-SCOPE-FIND PRIM scope-find
' HB-PARSE-NAME PRIM parse-name
' NUM-PARSE PRIM num-parse
' HB-WIDE-MARK PRIM wide-mark
REFUSE ffi-call        REFUSE ffi-call-n      REFUSE ffi-call-bounded
REFUSE task-entry      REFUSE callback-entry
REFUSE ffi-call-abi-bounded   REFUSE ffi-call-abi-r-bounded
REFUSE ffi-call-abi    REFUSE ffi-call-abi-r
REFUSE f+              REFUSE f-              REFUSE f*
REFUSE f/              REFUSE fnegate         REFUSE fabs
REFUSE fsqrt           REFUSE f<              REFUSE f>
REFUSE f=              REFUSE f0<             REFUSE f0=
REFUSE s>f             REFUSE f>s
REFUSE f.
REFUSE emit            ' HB-CR PRIM cr        REFUSE space
REFUSE u.
' CREATED PRIM create
REFUSE getpid          REFUSE proc-watch-open REFUSE kill-errno
REFUSE execve
' HB-MUNMAP PRIM munmap
REFUSE execute-floor
' execute PRIM execute
' HB-CATCH PRIM catch
REFUSE evaluate
' ?dup PRIM ?dup
' HB-2>R PRIM 2>r        ' HB-2R> PRIM 2r>        ' HB-2R@ PRIM 2r@
' HB-RUN-RC PRIM run-rc
REFUSE top-check@
NDICT @ SEED-N !

\ ---- the check against prims.f (boot.fs runs it after kernel file 8) ---------
\ PRIM-SPEC's public words read the table: COUNT, NAME$, MIN-IN, FIND.
: SPEC-DIE ( c-addr u -- )
   s" hb: gforth host: prims.f has no " ERR ERR ERR-NL RC-REJECT HB-EXIT ;
: SPEC-XT ( c-addr u -- xt ) {: a u :}
   s" PRIM-SPEC" WL-NAMESPACE REC-FIND dup 0= if drop s" PRIM-SPEC" SPEC-DIE then
   a u rot @ REC-FIND dup 0= if drop a u SPEC-DIE then @ ;
: SEED-OF ( c-addr u -- rec|0 ) {: a u :}
   SEED-N @ 0 ?do i REC dup REC-NAME$ a u str= if unloop exit then drop loop 0 ;
: PRIMS-CHECK ( -- )
   s" COUNT" SPEC-XT s" NAME$" SPEC-XT s" MIN-IN" SPEC-XT s" FIND" SPEC-XT
   {: count name$ min-in find :}
   count execute 0 ?do
      i name$ execute 2dup SEED-OF 0= if
         s" prims: specification row without a backend body: " ERR ERR
         REFUSE-RC COMPILE-DIE then 2drop
   loop
   SEED-N @ 0 ?do
      i REC REC-NAME$ find execute dup 0< if
         drop s" prims: engine primitive absent from the specification table: " ERR
         i REC REC-NAME$ ERR REFUSE-RC COMPILE-DIE then
      i swap min-in execute MIN-IN-MARK
   loop ;
