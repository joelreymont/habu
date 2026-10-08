\ src/host/gforth/prims.fs - one Gforth body per src/habu/prims.f row, each
\ registered as a seeded record of the global wordlist before the kernel
\ loads (docs/bootstrap.md stage 1; habu2.f EMIT-SCOPE-REC "SEEDED").
\
\ A row the host performs gets its body here, in reader.fs or as Gforth's own
\ word; a row it cannot perform gets REFUSE's body, which prints
\ `hb: gforth has no <row>` and exits 76 as kernel-x64.f REFUSE-BODY does.
\ PRIMS-CHECK runs once prims.f has loaded: a row with no body, or a body with
\ no row, dies naming it, and each body's record takes its row's min-in byte.

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
\ Anonymous mappings (habu1.f BMAPANON, BMUNMAP): zeroed pages, rc 0 on success.
: HB-MAP-ANON ( u -- addr rc )
   0 swap MMAP-PROT MMAP-FLAGS -1 0 HOST-MMAP dup -1 = if -1 else 0 then ;
\ Record flag stamps (habu1.f BWIDEMARK, BINTMARK, BMININMARK); PRIMS-CHECK
\ stamps the min-in bytes itself, so min-in-mark's row is refused.
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
\ Hook cells (habu1.f BSETCHECK, BSETPREFLIGHT).
: HB-SET-CHECK ( xt -- ) HOOK-CELL D! ;
: HB-SET-PREFLIGHT ( xt -- ) COMPILE-PREFLIGHT-CELL D! ;
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
' @ PRIM @             ' ! PRIM !
' ! PRIM xt!                     \ a store plus a declaration only the snapshot loader reads
' drop PRIM ptr-cell-mark        \ that declaration alone
REFUSE addr-cells-abi
REFUSE snapshot-format
' HB-PTR-FIELD PRIM ptr-field
' noop PRIM byte-view            ' noop PRIM cell-view
' +! PRIM +!           ' c@ PRIM c@           ' c! PRIM c!
REFUSE atomic@         REFUSE atomic!
REFUSE atomic-add      REFUSE atomic-cas      REFUSE fence
REFUSE run-in-stack
REFUSE evaluate-closed
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
REFUSE read
REFUSE ioctl
' HB-MAP-ANON PRIM map-anon
REFUSE mmap
REFUSE open-rd         REFUSE access
REFUSE unlink          REFUSE rename          REFUSE chmod
REFUSE symlink         REFUSE readlink
REFUSE realpath
REFUSE mkdir           REFUSE rmdir
REFUSE stat64          REFUSE lstat64         REFUSE getdirentries64
REFUSE pipe            REFUSE dup2            REFUSE fcntl
REFUSE poll            REFUSE kill            REFUSE setpgid
REFUSE spawn-io        REFUSE spawn-argv-io   REFUSE spawn-argv-env-io
REFUSE spawn-argv-env-cwd-io
REFUSE fork            REFUSE wait-status
REFUSE patch32
REFUSE code-publish
REFUSE native-unit-publish INT
REFUSE callmap-set     REFUSE addrmap-set     REFUSE xref-retarget
' HB-INT-MARK PRIM int-mark INT
REFUSE min-in-mark INT
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
REFUSE set-top-check
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
REFUSE SEAL-CAPTURE
' false PRIM seal-captured?   \ habu1.f:3817 BSEALCAPQ; SEAL-CAPTURE is refused, so nothing seals
REFUSE SEAL-FRIEND
' HB-DRAIN-PRETRUST PRIM DRAIN-PRETRUST
' HB-DATA PRIM data-base
' HB-PROT-WID-ADD PRIM prot-wid-add
REFUSE prot-wid-room
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
' HOST-MUNMAP PRIM munmap
REFUSE execute-floor
' execute PRIM execute
' HB-CATCH PRIM catch
REFUSE evaluate
' ?dup PRIM ?dup
REFUSE 2>r             REFUSE 2r>             REFUSE 2r@
REFUSE run-rc
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
