\ Native target compilation ends before the transient source writer loads.

require tools/native-build-deps.f
require src/lib/executable-build.f
require src/lib/tier.f
require src/lib/sort.f

package NATIVE-BUILD

\ The retained host's baked SRCLOC package predates these target cells.
$2D68 constant OWNED-SOURCE-CELL
$2D70 constant OWNED-SOURCE-SIZE-CELL

$1000 constant SMOKE-CAP
10000 constant SMOKE-TIMEOUT-MS

create SMOKE-OUT SMOKE-CAP allot
create SMOKE-ERR SMOKE-CAP allot

create PRIOR-ENGINE-ROOT FS-PATH-CAP allot
variable PRIOR-ENGINE-ROOT-U

\ The driver comes from the donor tree; every target source belongs to CWD.
\ Copy ENGINE$ because ENGINE! overwrites its backing buffer. The fresh target
\ loader receives this same root through TARGET-ENGINE-ROOT.
: TARGET-ROOT-ENTER ( -- )
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   root PRIOR-ENGINE-ROOT rootu BYTE-COPY
   rootu PRIOR-ENGINE-ROOT-U !
   SOURCE-ROOT:CWD$ SOURCE-ROOT:ENGINE! ;

: TARGET-ROOT-LEAVE ( -- )
   PRIOR-ENGINE-ROOT PRIOR-ENGINE-ROOT-U @ SOURCE-ROOT:ENGINE! ;

: WITH-TARGET-ROOT ( R [ R -- S ] -- S )
   TARGET-ROOT-ENTER [: TARGET-ROOT-LEAVE ;] finally ;

: CLASS-WANTED$ ( -- ptr u8 n )
   CLASS-WANTED @ CLASS-WHITEBOX = if s" whitebox image" exit then
   s" product image" ;

\ The running host may predate a new engine callback, and its reserved bands may be
\ smaller than this tree's. Keep the host's actual engine declarations -- the cells
\ below ITS heap floor -- and discard every declaration belonging to the retired
\ heap above it.
\
\ THE BOUNDARY IS THE HOST'S, NOT THE TREE'S. A tree whose reserved bands grew
\ moves DATA-START and the host that builds it did not move with it, so classifying
\ the host's rows by the source constant keeps the host's own heap rows, which the
\ capture then refuses by value (src/habu/aot-capture.f ACAP-TARGET-REFUSE, exit
\ 74). The engine publishes its floor at boot in BOOT-LAYOUT:HEAP-START-CELL.
\
\ FILTERED, NOT TRUNCATED. A fixed-band cell is registered where its kind is
\ decided, and for two of them that happens after the heap has started growing, so
\ the rows are NOT partitioned in registration order -- measured 2026-09-12, a
\ product engine carries 9 engine rows of which 2 sit past its first heap row, and
\ stopping at the first heap row dropped them. Compacting in place keeps every
\ engine row and preserves the order the registrar wrote.
: ADDR-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: ADDR-ROW@ ( n -- n ) ADDRESS-CELLS:ROW@ ;

: ADDR-ROW-OFF ( n -- n ) ADDR-ROW@ SNAP-RELOC:XTCELL-OFF-MASK and ;

: HOST-HEAP-START ( -- n )
   data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

: CHECK-HOST-LAYOUT ( -- )
   \ Older hosts register the retired observer CODE cell even when it holds zero.
   \ Remove its declaration before translating the remaining fixed host rows.
   $2CF0 CELL ADDRESS-CELLS:REMOVE-XT-SPAN
   NATIVE-LAYOUT:CURRENT HOST-HEAP-START NATIVE-LAYOUT:CHECK
   ADDR-ROWS 0 ?do
      i ADDR-ROW@ {: raw:n :}
      raw SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
      off HOST-HEAP-START < if
         NATIVE-LAYOUT:CURRENT off raw SNAP-RELOC:XTCELL-DATA-TAG and 0<>
         NATIVE-LAYOUT:TRANSLATE drop
      then
   loop ;

: KEEP-ROWS-BELOW ( n -- ) ADDRESS-CELLS:KEEP-BELOW ;

: RESET-ADDRESS-ROWS ( -- )
   HOST-HEAP-START {: floor:n :}
   floor 0= floor DATA-START <> or if
      s" native-build: host layout does not match its published heap floor" 76 die
   then
   floor KEEP-ROWS-BELOW ;

\ The checker owner's record holds its reset entry as a code address integer.
CAST: RESET-XT ( n -- [ -- ] )

\ The retained host's literal rows come from the package that owns them, which
\ publishes them while it is open, one segment of the active pool at a time
\ (src/compiler/native/string.f SEGMENTS, SOURCE-SPAN, SOURCE-ROWS). This driver
\ used to find ARENA, ARENA-CAP, R-OFF and R-LEN by name in NSTR's PRIVATE
\ wordlist, through the namespace record, after the package had closed - four
\ private names a shipped engine had to keep alive for one caller. A call needs
\ none of them.
: LITERAL-SOURCE-ROWS ( n -- ptr u8 n ptr n ptr n )
   NSTR:SOURCE-ROWS ;

: LITERAL-SPAN-REFUSE ( -- )
   s" native-build: source literal arena outside capture" BUILD-RC die ;

: CHECK-LITERAL-SPAN ( n -- )
   NSTR:SOURCE-SPAN {: arena:ptr size:n :}
   arena NULL-PTR BYTE-VIEW - {: start:n :}
   start AOT-ARM:D0 @ < start AOT-ARM:D1 @ > or if LITERAL-SPAN-REFUSE then
   AOT-ARM:D1 @ start - {: remaining:n :}
   remaining CELL < if LITERAL-SPAN-REFUSE then
   size 0 < size remaining CELL - > or if LITERAL-SPAN-REFUSE then ;

\ The checker publishes its own declaration owner into the engine's target
\ declaration cell as it loads (src/core/checker.f), so the owner is a cell
\ read: the retained host's before LOAD-TARGET and the freshly loaded target's
\ after it, which is exactly the late binding this word used to get by
\ evaluating `package CHECKER-REG DECLARATIONS ;package` - a private name
\ resolved at runtime, and an unchecked span to carry its result.
: CHECKER-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ ;

\ The execution token belongs to the retained private checker owner.
: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ RESET-XT execute ;

\ The window's target seam: the build's target (tools/build-target.f, the host
\ unless `--target` named another) chooses which OS sources the window is given,
\ and an unknown target is refused rather than defaulted. From here on the
\ window's own target.f answers HB-TARGET-* for everything the window compiles,
\ while this engine's predicates stay its own.
: NB-TARGET-CORE-FILES ( -- )
   BUILD-TARGET:LINUX? if
      s" src/os/linux/target.f" included
      s" src/os/linux/layout-constants.f" included
      s" src/os/linux/layout.f" included
      exit
   then
   BUILD-TARGET:MACOS? if
      s" src/os/macos/target.f" included
      s" src/os/macos/layout.f" included
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      s" src/os/linux-x86-64/target.f" included
      s" src/os/linux/layout-constants.f" included
      s" src/os/linux/layout.f" included
      exit
   then
   s" native-build: unknown target" 76 die ;

\ The discarded build host remains callable through this compiled continuation,
\ but none of its dictionary records or address declarations enters the window.
\ The hook clears and the dictionary rewind are src/habu/window-rewind.f's
\ top-level text: seed-ndict! is a top-level boundary no checked body names.
: REWIND-CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n found:bool :}
   path pathu answer answeru found NFILE-INPUTS:ADD-CANON
   answer answeru found ;

: REWIND-LOAD ( -- )
   ['] REWIND-CANON ['] SOURCE-ROOT:READ-OS SOURCE-INPUT:USE
   [: s" src/habu/window-rewind.f" included ;] catch {: rc:n :}
   SOURCE-INPUT:RESET
   rc 0<> if rc throw then ;

: LOGICAL-RESET ( ptr u8 -- )
   RESET-CHECKER
   [: REWIND-LOAD ;] SOURCE-ROOT:WITH-CWD
   RESET-ADDRESS-ROWS ;

\ OPEN-TARGET-XT answers a target operation's entry as a code address integer.
CAST: SOURCE-RESET-XT ( n -- [ -- ] )
CAST: SOURCE-ENGINE-XT ( n -- [ ptr u8 n -- ] )
CAST: SOURCE-ROLES-XT ( n -- [ ptr u8 ptr u8 [ ptr u8 n -- ] -- ] )
CAST: PREFIX-COMPILE-XT ( n -- [ ptr u8 n -- ] )
CAST: ACTIVE-COMPILER-XT ( n -- [ -- ptr u8 n bool ] )
CAST: SOURCE-INCLUDED-XT ( n -- [ ptr u8 n -- ] )
CAST: SOURCE-INPUT-USE-XT
   ( n -- [ [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- ] )
CAST: SOURCE-INPUT-OBSERVER-XT
   ( n -- [ [ ptr u8 n ptr u8 n ptr u8 n -- ] [ -- ] -- ] )
CAST: SOURCE-READ-XT ( n -- [ ptr u8 n ptr u8 n -- ptr u8 n ] )

\ Before the fresh include.f creates another SOURCE-INPUT, observe the real
\ buffers through the retained loader. The FILE reader then carries the same
\ observation across the handoff and through cached child validation.
: PREFIX-NOOP ( -- ) ;

: PREFIX-INPUT ( ptr u8 n ptr u8 n ptr u8 n -- )
   NFILE-INPUTS:ADD-ROOT ;

: PREFIX-START ( -- )
   BUILD-PRODUCER:START
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ BUILD-PRODUCER:OPTION
   ['] PREFIX-INPUT ['] PREFIX-NOOP SOURCE-INPUT:USE-OBSERVER ;

: ROLE-PTR ( ptr u8 n bool -- ptr u8 )
   {: digest:ptr size:n known:bool :}
   known 0= size 32 <> or if
      s" native-build: missing loaded producer role" BUILD-RC die
   then
   digest ;

\ A fresh operation's entry lies inside the open target code window, from B0 up
\ to the code pointer.
: OPEN-CODE ( n -- n ) {: xt:n :}
   xt AOT-ARM:B0 @ < xt cp@ >= or if
      s" native-build: operation outside target window" BUILD-RC die
   then
   xt ;

\ Resolve a fresh operation inside the open target code window. Its retained
\ namesake would act for the host: SOURCE-INPUT:RESET would activate the wrong
\ loader after LOGICAL-RESET, and CHECKER-REG:HANDOVER would leave the host's
\ checker in charge.
: OPEN-TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: target operation missing" BUILD-RC die
   then
   XREF-START OPEN-CODE ;

: TARGET-ENGINE-ROOT ( -- )
   SOURCE-ROOT:ENGINE$
   s" SOURCE-ROOT:ENGINE!" OPEN-TARGET-XT SOURCE-ENGINE-XT execute ;

\ Find a private word of a package in the FRESHLY LOADED target's pool, which
\ did not exist when this file was compiled: the package's namespace record,
\ then the word in its private wordlist. The alternatives are a public wrapper
\ or a fixed engine cell for the token. Two words are found this way.
\ NSTR:IMPORT-ROWS, the literal row importer, after the window closes
\ (TARGET-IMPORTER): a public wrapper would be a well-typed way for any source
\ to invoke literal ownership import, which test/compiler/native-string.f
\ forbids, and the capture keeps its name for this lookup
\ (src/habu/aot-capture.f ACAP-KEEP?).
\ NPUB:IMPORT-UNIT, the package unit importer, while the window is open, at a
\ unit cut (src/compiler/native/publish.f): a public one would let any
\ source publish code bytes and dictionary rows, and the lookup precedes the
\ capture, so the capture keeps no name for it.
: PRIVATE-TARGET-XT ( ptr u8 n ptr u8 n -- n )
   {: pkg:ptr pkgu:n name:ptr nameu:n :}
   pkg pkgu XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if
      drop s" native-build: literal owner missing" 76 die
   then
   XREF-PKG-PRIVATE  name nameu rot XREF-FIND-WL
   dup XREF-FOUND? 0= if
      drop s" native-build: literal importer missing" 76 die
   then
   XREF-START ;

\ A private word of a package loaded into the open target code window.
: OPEN-PRIVATE-XT ( ptr u8 n ptr u8 n -- n )
   PRIVATE-TARGET-XT OPEN-CODE ;

: PREFIX-COMPILER-FINISH ( -- )
   s" SOURCE-PRODUCER" s" INSTALL-ROLES" OPEN-PRIVATE-XT
      SOURCE-ROLES-XT {: install :}
   BUILD-PRODUCER:CHECKER$ ROLE-PTR
   BUILD-PRODUCER:COMPILER$ ROLE-PTR
   NDEF-CACHE:SUSPEND-DISPATCH
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ OPEN-CODE PREFIX-COMPILE-XT
   NDEF-CACHE:DEFINITIONS-CLOSE
   install execute ;

variable TARGET-SOURCE-BOUND
create HOST-KERNEL 32 allot
variable HOST-KERNEL-KNOWN
create HOST-COMPILER 32 allot
variable HOST-COMPILER-KNOWN
variable HOST-PREFIX-FIRST

: HOST-COMPILER$ ( -- ptr u8 n bool )
   HOST-COMPILER HOST-COMPILER-KNOWN @ if 32 else 0 then
      HOST-COMPILER-KNOWN @ 0<> ;

\ Resolve before LOGICAL-RESET, while the donor dictionary still owns this
\ name. A source-defined namesake cannot authenticate the donor's baked role.
: HOST-ACTIVE-COMPILER$ ( -- ptr u8 n bool )
   s" SOURCE-PRODUCER:ACTIVE-COMPILER$" XREF-FIND
   dup XREF-FOUND? 0= if drop NULL-PTR 0 false exit then
   dup XREF-RETIRED? if drop NULL-PTR 0 false exit then
   XREF-START dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if
      drop NULL-PTR 0 false exit
   then
   ACTIVE-COMPILER-XT execute ;

\ The host's source boundary and producer roles belong to its dictionary.
\ Save them before LOGICAL-RESET retires that dictionary, then bind the
\ matching producers to code-bearing seed rows in the replacement window.
: RETAIN-HOST-PRODUCERS ( -- )
   NDEF-CACHE:RETAIN-HOST-CHECKER
   CORE-PREFIX:FIRST-RECORD HOST-PREFIX-FIRST !
   SOURCE-PRODUCER:KERNEL SOURCE-PRODUCER:ROLE$
      {: digest:ptr size:n known:bool :}
   known if
      size 32 <> if
         s" native-build: invalid host kernel producer" BUILD-RC die
      then
      digest HOST-KERNEL 32 BYTE-COPY
   then
   known HOST-KERNEL-KNOWN !
   HOST-ACTIVE-COMPILER$
      {: compiler:ptr compileru:n compiler-known:bool :}
   compiler-known if
      compileru 32 <> if
         s" native-build: invalid host compiler producer" BUILD-RC die
      then
      compiler HOST-COMPILER 32 BYTE-COPY
   then
   compiler-known HOST-COMPILER-KNOWN !
   ['] HOST-COMPILER$ NUNIT-DEP:USE-COMPILER-ROLE ;

TYPED-VARIABLE SOURCE-BIND [ -- ]
TYPED-VARIABLE SOURCE-CHECK [ -- ]
TYPED-VARIABLE SOURCE-CLOSE [ -- ]
TYPED-VARIABLE SOURCE-TAIL [ -- ]

: SOURCE-NOOP ( -- ) ;

: SOURCE-POLICY! ( [ -- ] [ -- ] [ -- ] -- ) {: bind check close :}
   bind SOURCE-BIND !
   check SOURCE-CHECK !
   close SOURCE-CLOSE !
   [: SOURCE-NOOP ;] SOURCE-TAIL ! ;

: SOURCE-TAIL! ( [ -- ] -- ) SOURCE-TAIL ! ;

: DEFAULT-SOURCE-BIND ( -- )
   s" REQUIRE-BOOT-OPEN" OPEN-TARGET-XT SOURCE-RESET-XT execute ;

\ The ordinary loader's completed child is the selected source boundary.
\ Keep a custom source policy's loader inside the completion callback.
TYPED-VARIABLE HOST-UNIT-PRIOR [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
variable HOST-UNIT-BOUND

: HOST-UNIT-ORDINARY ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   0 sourceu q execute ;

: HOST-UNIT-RUN ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ptr u8 n ptr u8 n ptr u8 n [ n n -- ] )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu root rootu source sourceu q HOST-UNIT-PRIOR @ execute
   path pathu root rootu source sourceu q ;

\ The retained loader predates the new frame publisher. Its execution source
\ is valid only while the prior unit policy runs, including nested loads.
: HOST-UNIT ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   data-base OWNED-SOURCE-CELL + 0 ptr-field @ {: old:ptr :}
   data-base OWNED-SOURCE-SIZE-CELL + @ {: oldu:n :}
   source NULL-PTR - data-base OWNED-SOURCE-CELL + !
   sourceu data-base OWNED-SOURCE-SIZE-CELL + !
   path pathu root rootu source sourceu q
      [: HOST-UNIT-RUN ;] catch {: rc:n :}
   2drop 2drop 2drop drop
   old NULL-PTR - data-base OWNED-SOURCE-CELL + !
   oldu data-base OWNED-SOURCE-SIZE-CELL + !
   rc 0<> if rc throw then ;

: BIND-HOST-UNIT ( -- )
   SOURCE-UNIT:CURRENT HOST-UNIT-PRIOR !
   [: HOST-UNIT ;] SOURCE-UNIT:USE
   1 HOST-UNIT-BOUND ! ;

: RESTORE-HOST-UNIT ( -- )
   HOST-UNIT-BOUND @ 0= if exit then
   HOST-UNIT-PRIOR @ SOURCE-UNIT:USE
   ['] HOST-UNIT-ORDINARY HOST-UNIT-PRIOR !
   0 HOST-UNIT-BOUND ! ;

: BIND-SEED-PRODUCERS ( -- )
   \ The seed ends at this frontier; later FILEs bind their own source role.
   0 HOST-PREFIX-FIRST @ HOST-PREFIX-FIRST @
   HOST-KERNEL HOST-KERNEL-KNOWN @ if 32 else 0 then
   HOST-KERNEL-KNOWN @ 0<>
   HOST-KERNEL
   NFILE-REF:BIND-SEED-PRODUCER ;

: DEFAULT-SOURCE-POLICY ( -- )
   ['] DEFAULT-SOURCE-BIND ['] SOURCE-NOOP ['] SOURCE-NOOP SOURCE-POLICY! ;

: RESET-TARGET-SOURCE ( -- )
   TARGET-SOURCE-BOUND @ if
      s" SOURCE-INPUT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
      s" SOURCE-UNIT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
      0 TARGET-SOURCE-BOUND !
   then ;

TYPED-VARIABLE PREFIX-DISPATCH [ ptr u8 n -- ]
CAST: PREFIX-TAKE-XT ( n -- [ ptr u8 n n -- ] )
CAST: PREFIX-IMPORT-XT ( n -- [ n n n n ptr u8 n ptr u8 n -- ] )
CAST: PREFIX-RAW-XT ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: PREFIX-RAW-N ( [ ptr u8 n ptr u8 n -- ] -- n )
variable PREFIX-TAKE
PTR-VARIABLE PREFIX-RAW-OWNER
variable PREFIX-RAW-PRIOR

\ Before the early recorder loads, keep only accepted definition coordinates.
\ Source and path are validated while the loader still owns their bytes.
7 constant PRE-CELLS
DYNAMIC-BUFFER PRE-BYTES u8
DYNAMIC-BUFFER PRE-ROWS n
variable PRE-BYTES-N
variable PRE-ROWS-N

: PRE-ROW@ ( n n -- n ) {: id:n field:n :}
   id PRE-CELLS * field + PRE-ROWS @ ;

: PRE-ROW! ( n n n -- ) {: value:n id:n field:n :}
   value id PRE-CELLS * field + PRE-ROWS ! ;

: PRE-BYTES+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   PRE-BYTES-N @ {: off:n :}
   off u + 1 max PRE-BYTES-RESERVE
   u 0 > if a off PRE-BYTES u BYTE-COPY then
   off u + PRE-BYTES-N !
   off ;

: PRE-ROW+ ( ptr u8 n n ptr u8 -- )
   {: name:ptr nameu:n raw:n token:ptr :}
   raw 0= nameu 0 <= or if exit then
   PRE-ROWS-N @ {: id:n :}
   id 1+ PRE-CELLS * PRE-ROWS-RESERVE
   raw XREF-ADDR>INDEX id 0 PRE-ROW!
   0 id 1 PRE-ROW!  0 id 2 PRE-ROW!
   0 id 3 PRE-ROW!  0 id 4 PRE-ROW!
   data-base SRCLOC:PATH-CELL + 0 ptr-field @ {: path:ptr :}
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   data-base OWNED-SOURCE-CELL + 0 ptr-field @ {: source:ptr :}
   data-base OWNED-SOURCE-SIZE-CELL + @ {: sourceu:n :}
   token source - {: at:n :}
   pathu 0 > sourceu 0 > and
   at 0 >= at sourceu <= and and
   nameu sourceu at - <= and if
      token nameu name nameu CORE-STR= if
         at id 1 PRE-ROW!  at nameu + id 2 PRE-ROW!
         path pathu PRE-BYTES+ id 3 PRE-ROW!
         pathu id 4 PRE-ROW!
      then
   then
   name nameu PRE-BYTES+ id 5 PRE-ROW!
   nameu id 6 PRE-ROW!
   id 1+ PRE-ROWS-N ! ;

: PREFIX-RECORD ( ptr u8 n n ptr u8 -- )
   PREFIX-TAKE @ dup 0= if drop PRE-ROW+ exit then
   {: target:n :}
   drop target PREFIX-TAKE-XT execute ;

: PREFIX-TOKEN ( -- ptr u8 )
   data-base PEND-CELL + @ 0= if
      data-base DEF-TKA-CELL + 0 ptr-field @ exit
   then
   data-base PENDTKA-CELL + 0 ptr-field @ ;

: DEF-TOKEN ( -- ptr u8 ) data-base DEF-TKA-CELL + 0 ptr-field @ ;

\ The active source owner remains the retained checker until handover.
\ Its raw registrar runs after a direct definer publishes LASTC.
: PREFIX-RAW ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sig:ptr sigu:n :}
   name nameu sig sigu PREFIX-RAW-PRIOR @ PREFIX-RAW-XT execute
   data-base LASTC-CELL + @ {: raw:n :}
   raw 0= if exit then
   raw XREF-ADDR>REC {: rec:ptr :}
   rec XREF-WORDLIST 0<> if exit then
   rec XREF-NAME$ name nameu CORE-STR=CI 0= if exit then
   name nameu raw DEF-TOKEN PREFIX-RECORD ;

: BIND-PREFIX-RAW ( -- )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ {: owner:ptr :}
   owner PREFIX-RAW-OWNER !
   owner NCOMP-DISPATCH:DECL-RAW-OFF + CELL-VIEW @ PREFIX-RAW-PRIOR !
   ['] PREFIX-RAW PREFIX-RAW-N
      owner NCOMP-DISPATCH:DECL-RAW-OFF + CELL-VIEW xt! ;

: RESTORE-PREFIX-RAW ( -- )
   PREFIX-RAW-OWNER @ dup NULL-PTR = if drop exit then
   {: owner:ptr :}
   PREFIX-RAW-PRIOR @ owner NCOMP-DISPATCH:DECL-RAW-OFF + CELL-VIEW xt!
   NULL-PTR PREFIX-RAW-OWNER !  0 PREFIX-RAW-PRIOR ! ;

: PRE-RELEASE ( -- )
   PRE-BYTES-RELEASE PRE-ROWS-RELEASE
   0 PRE-BYTES-N !  0 PRE-ROWS-N ! ;

: PRE-FLUSH ( -- )
   s" ENGINE-ORIGIN-EARLY:FILE-IMPORT" OPEN-TARGET-XT
      PREFIX-IMPORT-XT {: import :}
   PRE-ROWS-N @ 0 ?do
      i 0 PRE-ROW@ 0 i 1 PRE-ROW@ i 2 PRE-ROW@
      i 3 PRE-ROW@ PRE-BYTES i 4 PRE-ROW@
      i 5 PRE-ROW@ PRE-BYTES i 6 PRE-ROW@
      import execute
   loop
   PRE-RELEASE ;

: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else false then while 1+ repeat
   a swap ;

\ The borrowed compiler finishes before the engine publishes the pending
\ record. Keep only its exact global name token; the engine's subsequent
\ publication makes that same record live for the origin owner to drain.
: PREFIX-ACCEPT ( ptr u8 n -- )
   {: a:ptr u:n :}
   data-base PEND-CELL + @ {: raw:n :}
   raw 0= if exit then
   raw XREF-ADDR>REC {: rec:ptr :}
   rec XREF-WORDLIST 0<> if exit then
   a u FIRST-TOKEN {: name:ptr nameu:n :}
   rec XREF-NAME$ name nameu CORE-STR=CI 0= if exit then
   name nameu raw PREFIX-TOKEN PREFIX-RECORD ;

: PREFIX-ARM ( ptr u8 n -- )
   {: a:ptr u:n :}
   ['] PREFIX-ACCEPT NDEF-CACHE:ARM-NATIVE-ACCEPT 0= if
      a u PREFIX-ACCEPT
   then ;

: PREFIX-COMPILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u NCOMP:COMPILE-CHECKED
   a u PREFIX-ARM ;

: RESTORE-PREFIX-DISPATCH ( -- )
   PREFIX-DISPATCH @ NCOMP:DISPATCH! ;

\ Borrow the retained compiler only while the target has no check hook. The
\ target's own NCOMP:INSTALL later replaces this dispatch after its source load.
: LOAD-CHECKED-PREFIX ( -- )
   0 PREFIX-TAKE !
   BIND-PREFIX-RAW
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker-fetch-abi.f" included
   s" src/core/checker-owner-extension.f" included
   s" src/core/checker-owner-abi.f" included
   s" src/habu/prims.f" included
   s" src/core/does-clause.f" included
   s" src/core/checker.f" included
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/habu/native-body-abi.f" included
   s" src/habu/engine-origin-early.f" included
   s" ENGINE-ORIGIN-EARLY:PREFIX-TAKE" OPEN-TARGET-XT PREFIX-TAKE !
   PRE-FLUSH
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" src/core/render.f" included
   s" src/core/sumtype.f" included
   s" src/core/layout-buffer.f" included
   s" src/core/layout-valid.f" included
   NDEF-CACHE:HANDOVER-PREPARE
   RESTORE-PREFIX-RAW
   s" CHECKER-REG:HANDOVER" OPEN-TARGET-XT SOURCE-RESET-XT execute
   NDEF-CACHE:HANDOVER-BIND
   \ The handover, the same entry tools/bootstrap.sh's recovery reload calls,
   \ publishes the complete replacement owner. Earlier pre-hook code still needs
   \ the retained compiler's paired checker; later calls use this one.
   CHECKER-OWNER:CAPTURE-PREPARE
   CHECKER-OWNER AOT-ARM:PAYLOAD-PERSISTENT
   s" src/core/check-hook.f" included
   0 PREFIX-TAKE !
   s" src/habu/native-body.f" included
   s" src/habu/engine-origin-checked.f" included
   s" ENGINE-ORIGIN-EARLY:INSTALL" OPEN-TARGET-XT SOURCE-RESET-XT execute ;

: CHECKED-PREFIX ( -- )
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ PREFIX-COMPILE-XT
      PREFIX-DISPATCH !
   ['] PREFIX-COMPILE ['] PREFIX-ARM NDEF-CACHE:BORROW
   [: LOAD-CHECKED-PREFIX ;]
      [: RESTORE-PREFIX-RAW
         PRE-RELEASE
         NDEF-CACHE:RETURN-BORROW RESTORE-PREFIX-DISPATCH ;] finally ;

: LOAD-TARGET ( -- )
   BIND-SEED-PRODUCERS
   CHECKED-PREFIX
   BUILD-PRODUCER:FREEZE-CHECKER
   s" src/core/roles.f" included
   s" src/core/cell-effects.f" included
   s" src/core/declaration-transaction.f" included
   s" src/core/generated-declaration.f" included
   s" src/core/decl-event.f" included
   s" src/core/structure-make.f" included
   s" src/core/structure-decl.f" included
   s" src/core/enum-decl.f" included
   s" src/core/structures.f" included
   s" src/core/bytes.f" included
   NB-TARGET-CORE-FILES
   s" src/habu/stack-abi.f" included
   s" src/habu/layout.f" included
   s" src/os/env-base.f" included
   s" src/core/quotation-storage.f" included
   s" src/core/include.f" included
   RESTORE-HOST-UNIT
   \ The fresh loader needs its canon callback before it can accept the host root.
   s" SOURCE-INPUT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
   TARGET-ENGINE-ROOT
   SOURCE-BIND @ execute
   NDEF-CACHE:REBIND
   1 TARGET-SOURCE-BOUND !
   s" src/habu/native-runtime.f"
      s" included" OPEN-TARGET-XT SOURCE-INCLUDED-XT execute
   PREFIX-COMPILER-FINISH
   NDEF-CACHE:REBIND
   SOURCE-TAIL @ execute ;

: OPEN-FILE-AND-COMPILE ( -- )
   \ APP-IMAGE preserves the exact DATA cursor, including a trailing byte field.
   \ The captured window starts on the cell grid used by address relocation.
   align
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   NSTR:WINDOW-OPEN
   [: ['] LOAD-TARGET NDEF-CACHE:RUN ;] SOURCE-ROOT:WITH-CWD
   AOT-ARM:WINDOW-CLOSE ;

\ Resolve a target operation without compiling another definition or opening a
\ source frame. It must be part of the frozen target code window.
: TARGET-CODE ( n -- n ) {: xt:n :}
   xt AOT-ARM:B0 @ < xt AOT-ARM:B1 @ >= or if
      s" native-build: operation does not belong to target" 76 die
   then
   xt ;

: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: target operation missing" 76 die
   then
   XREF-START TARGET-CODE ;

\ The target's row importer, a private word (PRIVATE-TARGET-XT).
: TARGET-IMPORTER ( -- n )
   s" NSTR" s" IMPORT-ROWS" PRIVATE-TARGET-XT TARGET-CODE ;

: TARGET-PRIVATE-XT ( ptr u8 n ptr u8 n -- n )
   PRIVATE-TARGET-XT TARGET-CODE ;

\ TARGET-XT and TARGET-IMPORTER hand target entries over as code address integers.
CAST: PREPARE-XT ( n -- [ -- ] )
CAST: SEAL-XT ( n -- [ n -- ] )
CAST: LITERAL-IMPORT-XT ( n -- [ ptr u8 n ptr n ptr n -- ] )
CAST: PLAN-COUNT-XT ( n -- [ -- n ] )
CAST: PLAN-PAIR-XT ( n -- [ n -- n n ] )
CAST: PLAN-APPLY-XT ( n -- [ ptr n n -- ] )

\ Every segment's arena is checked before the target imports any of them.
: TRANSFER-LITERALS ( -- )
   NSTR:SEGMENTS 0 ?do i CHECK-LITERAL-SPAN loop
   TARGET-IMPORTER {: importer:n :}
   NSTR:SEGMENTS 0 ?do
      i LITERAL-SOURCE-ROWS importer LITERAL-IMPORT-XT execute
   loop ;

\ Target cleanup executes against the live donor registry. Its own header is
\ at a different offset when REGION or another reserved band grew. The host
\ constant was bound before the target window replaced layout.f. Each cleanup
\ scope ends before its capture reads the target layout.
CAST: HOST-HEADER-XT ( n -- [ n [ -- ] -- ] )

: WITH-DONOR-HEADER ( [ -- ] -- )
   SNAP-RELOC:XTCELL-N-CELL swap
   s" ADDRESS-CELLS" s" WITH-HOST-HEADER" TARGET-PRIVATE-XT
   HOST-HEADER-XT execute ;

\ The package seal comes first: CAPTURE-PREPARE's checker sweep reads the
\ protected bits it sets (src/core/checker-surface.f KEEP?).
: PREPARE-WINDOW-BODY ( -- )
   s" NATIVE-RUNTIME:CAPTURE-PREPARE" TARGET-XT PREPARE-XT execute
   s" CHECKER-STORAGE-UNBIND-ALL" TARGET-XT PREPARE-XT execute
   AOT-ARM:HERE-N AOT-ARM:D1 ! ;

: PREPARE-WINDOW ( -- )
   ['] PREPARE-WINDOW-BODY WITH-DONOR-HEADER ;

: PREPARE-TARGET-BODY ( -- )
   AOT-ARM:R0 @ s" ENGINE-INTERNAL:SEAL-PACKAGES" TARGET-XT SEAL-XT execute
   PREPARE-WINDOW-BODY ;

: PREPARE-TARGET ( -- )
   ['] PREPARE-TARGET-BODY WITH-DONOR-HEADER ;

\ Keep the source-time classifier's exact answers outside target DATA. The
\ first capture releases target dynamic buffers before the sealed one runs.
: COPY-SEAL-PLAN ( -- ptr n n )
   s" ENGINE-INTERNAL" s" PLAN-N" TARGET-PRIVATE-XT PLAN-COUNT-XT execute
      {: count:n :}
   count 2 * >COUNT MEM-ALLOC-CELLS {: plan:ptr :}
   s" ENGINE-INTERNAL" s" PLAN-PAIR" TARGET-PRIVATE-XT PLAN-PAIR-XT
      {: pair :}
   count 0 ?do
      i pair execute {: rec:n min:n :}
      rec plan i 2 * cells + !
      min plan i 2 * 1+ cells + !
   loop
   plan count ;

: APPLY-SEAL-PLAN ( ptr n n -- )
   s" ENGINE-INTERNAL" s" APPLY-PLAN" TARGET-PRIVATE-XT
      PLAN-APPLY-XT execute ;

\ A retained driver cell is not a fixed engine slot. Registering a target XT
\ into one after reset would make it look like a load-time engine hook.
: CHECK-FIXED-ROWS ( -- )
   AOT-WINDOW:XTOFF-N @ 0 ?do
      \ The first four bytes of each eight-byte row encode its location.
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + CELL-VIEW @ $FFFFFFFF and
      dup AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if
         HOST-HEAP-START CELL - > if
            s" native-build: captured fixed address belongs to the retired host heap" BUILD-RC die
         then
      else drop then
   loop ;

\ Accepted source paths must be borrowed before capture strips checker
\ symbols. Keep only paths for the target window's record indices.
DYNAMIC-BUFFER NAMES-SOURCE u8
DYNAMIC-BUFFER NAMES-SRC-OFF n
DYNAMIC-BUFFER NAMES-SRC-LEN n
DYNAMIC-BUFFER NAMES-CAP-OFF n
DYNAMIC-BUFFER NAMES-CAP-LEN n
variable NAMES-SOURCE-U
variable NAMES-SRC-N
TYPED-VARIABLE NAMES-CAP-OP [ n -- n ]
CAST: NAMES-SOURCE-XT ( n -- [ [ ptr n ptr u8 n -- ] -- ] )
CAST: NAMES-CAP-XT ( n -- [ n -- n ] )
s" AOT-CAPTURE" s" ACAP-DICT>CAP" PRIVATE-TARGET-XT
   NAMES-CAP-XT NAMES-CAP-OP !

: NAMES-SOURCE-TAKE ( ptr n ptr u8 n -- )
   {: rec:ptr a:ptr u:n :}
   rec BYTE-VIEW NULL-PTR BYTE-VIEW - XREF-ADDR>INDEX {: idx:n :}
   idx AOT-ARM:R0 @ < idx AOT-ARM:R1 @ >= or if exit then
   idx AOT-ARM:R0 @ - {: row:n :}
   row NAMES-SRC-LEN @ -1 <> if exit then
   NAMES-SOURCE-U @ row NAMES-SRC-OFF !
   u row NAMES-SRC-LEN !
   u 0= if exit then
   NAMES-SOURCE-U @ u + NAMES-SOURCE-RESERVE
   a 0 NAMES-SOURCE BYTE-VIEW NAMES-SOURCE-U @ + u BYTE-COPY
   NAMES-SOURCE-U @ u + NAMES-SOURCE-U ! ;

: NAMES-SOURCES ( -- )
   AOT-ARM:R1 @ AOT-ARM:R0 @ - NAMES-SRC-N !
   NAMES-SRC-N @ NAMES-SRC-OFF-RESERVE
   NAMES-SRC-N @ NAMES-SRC-LEN-RESERVE
   0 NAMES-SOURCE-U !
   NAMES-SRC-N @ 0 ?do
      0 i NAMES-SRC-OFF ! -1 i NAMES-SRC-LEN !
   loop
   ['] NAMES-SOURCE-TAKE
      s" ENGINE-ORIGIN" s" MAP-SOURCES" TARGET-PRIVATE-XT
      NAMES-SOURCE-XT execute
   NAMES-SRC-N @ 0 ?do
      i NAMES-SRC-LEN @ 0 < if 0 i NAMES-SRC-LEN ! then
   loop ;

\ Capture omits retired dictionary rows. Index the accepted paths by its own
\ dictionary-to-record map without changing the source rows: a compiler bundle
\ captures the same dictionary twice after the private checker rows are gone.
: NAMES-SOURCE-ALIGN ( -- )
   NAMES-CAP-OP @ {: to-cap :}
   AOT-CAPTURE:MAP-N NAMES-CAP-OFF-RESERVE
   AOT-CAPTURE:MAP-N NAMES-CAP-LEN-RESERVE
   AOT-CAPTURE:MAP-N 0 ?do
      0 i NAMES-CAP-OFF ! 0 i NAMES-CAP-LEN !
   loop
   NAMES-SRC-N @ 0 ?do
      AOT-ARM:R0 @ i + to-cap execute {: k:n :}
      k 0 >= if
         i NAMES-SRC-OFF @ k NAMES-CAP-OFF !
         i NAMES-SRC-LEN @ k NAMES-CAP-LEN !
      then
   loop ;

: CAPTURE-START ( -- )
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
   s" NHOST:INSTALL" AOT-CAPTURE:BOOTRUN+
   s" ENGINE-ORIGIN:INSTALL" AOT-CAPTURE:BOOTRUN+ ;

\ An x86-64 target's routines are the shadow the pre-window writer opened
\ (NATIVE-EMIT:OPEN-SHADOW). A native shadow reuses the placed emission, so
\ its capture walks the shadow's reach. An ARM64 host's dual-emission shadow
\ goes through TARGET-CAPTURE, which names a live emitted private routine with
\ its private WID before compaction; x86-only routines travel unnamed. With no
\ shadow open this is the plain capture. The shadow closes with the capture.
: CAPTURE ( -- )
   CAPTURE-START
   NSHADOW:NATIVE? if
      AOT-ARM:WINDOW$
      ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
      AOT-CAPTURE:CAPTURE-NATIVE
      AOT-CAPTURE:SHADOW-CAPTURE
   else
      NSHADOW:OPEN? if
         AOT-ARM:WINDOW$ AOT-CAPTURE:TARGET-CAPTURE
      else
         AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
      then
   then
   NSHADOW:CLOSE
   NAMES-SOURCE-ALIGN
   \ The snapshot has its armed cell. The builder still uses the old checker
   \ owner for the source writer compiled below, so stop observing in it.
   s" ENGINE-ORIGIN:DISARM" TARGET-XT PREPARE-XT execute
   CHECK-FIXED-ROWS ;

: CAPTURE-NAMED ( bool -- )
   {: all:bool :}
   CAPTURE-START
   AOT-ARM:WINDOW$ all AOT-CAPTURE:CAPTURE-WITH-NAMES
   NAMES-SOURCE-ALIGN
   CHECK-FIXED-ROWS ;

FS-PATH-CAP constant OUTPUT-CAP
create OUTPUT-BUF OUTPUT-CAP allot
variable OUTPUT-U
create TEMP-BUF OUTPUT-CAP allot
variable TEMP-U
create SMOKE-DIR OUTPUT-CAP allot
variable SMOKE-DIR-U

: OUTPUT$ ( -- ptr u8 n ) OUTPUT-BUF OUTPUT-U @ ;
: TEMP$ ( -- ptr u8 n ) TEMP-BUF TEMP-U @ ;
: TEMP-SUFFIX$ ( -- ptr u8 n ) s" .native-build.tmp" ;

: ABSOLUTE-OUTPUT! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size OUTPUT-CAP > or if E-FS-PATH throw then
   path c@ 47 = if
      path OUTPUT-BUF size BYTE-COPY size OUTPUT-U ! exit
   then
   SOURCE-ROOT:CWD$ path size OUTPUT-BUF JOIN-PATH OUTPUT-U ! ;

: OUTPUT! ( ptr u8 n -- ) {: path:ptr size:n :}
   path size ABSOLUTE-OUTPUT!
   OUTPUT-U @ {: absoluteu:n :}
   absoluteu OUTPUT-CAP TEMP-SUFFIX$ nip - > if E-FS-PATH throw then
   OUTPUT-BUF TEMP-BUF absoluteu BYTE-COPY
   TEMP-SUFFIX$ TEMP-BUF absoluteu + swap BYTE-COPY
   absoluteu TEMP-SUFFIX$ nip + TEMP-U ! ;

: SMOKE-DIR! ( -- )
   s" hb-native-smoke" HB-TMP-MKDIR {: path:ptr size:n :}
   path size CLEANUP-TREE+
   path SMOKE-DIR size BYTE-COPY size SMOKE-DIR-U ! ;

\ XREF-START answers the source writer's entry as a code address integer, and
\ this private cast states its exact compiled ABI. Its capture is a multi-cell
\ value, which cannot be passed through interpret-mode evaluate. The x86-64
\ writer's name-span query and shadow opener are cast the same way.
CAST: WRITER-XT ( n -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
CAST: NAME-SPAN-XT ( n -- [ n -- n n ] )
TYPED-VARIABLE WRITER-NAME-SPAN [ n -- n n ]

: SOURCE-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: source callback missing" BUILD-RC die
   then
   dup XREF-RETIRED? if
      drop s" native-build: source callback retired" BUILD-RC die
   then
   XREF-START {: xt:n :}
   \ A source callback belongs above the captured target window, not to the
   \ retained host or the target that the image will boot.
   xt 0= xt AOT-ARM:B1 @ < or xt cp@ >= or if
      s" native-build: callback does not belong to the source load" BUILD-RC die
   then
   xt ;

: SOURCE-WRITER-NAMED ( ptr u8 n -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
   SOURCE-XT WRITER-XT {: writer :}
   \ A source-loaded x86 writer links through its own X64LINK instance. Pair
   \ the sidecar query with that instance before the writer uses its placement
   \ storage; the pre-window callback belongs only to the pre-window writer.
   X64-TARGET? if
      s" X64LINK:NAMES-SPAN" SOURCE-XT NAME-SPAN-XT WRITER-NAME-SPAN !
   then
   writer ;

: SOURCE-WRITER ( -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
   s" NATIVE-EMIT:WRITE-C2" SOURCE-WRITER-NAMED ;

\ SOURCE LOADED AFTER THE CAPTURE COMPILES FOR THE WINDOW'S MACHINE. The
\ window's last act, src/habu/native-runtime.f CHECKER-REG:SEAL, runs
\ NCOMP:INSTALL, which puts the window's compiler in the dispatch cell this
\ process shares, and that compiler lowers for the binding the window's
\ target.f names. A window for this engine's machine compiles the writer into
\ code this engine runs. A window for another machine cannot: the linux-x86-64
\ window's compiler lowered the writer's first new file, src/arch/arm64/icode.f,
\ for x86-64 inside this AArch64 engine and refused the file's first definition
\ with E-X64EMIT-PLACE. Such a window's writer has to be compiled before the window
\ opens, and none is. The ordinary entry checks before opening the window,
\ so it cannot load target sources that the foreign machine does not support.
\ Other source-writer callers still check here before writing.
\ NABI:BINDING, compiled with this file, answers for the building engine.
: OUTPUT-LAUNCH-CK ( -- )
   BUILD-TARGET:ACTION@ {: action:RTARGET:build-action :}
   action RTARGET:EXECUTION@ action RTARGET:BODY-TARGET@
      RTARGET:EXECUTABLE-HERE? if exit then
   s" native-build: target process ABI or image format is not executable here" type cr
   BUILD-RC throw ;

: FOREIGN? ( -- bool )
   TARGET-ARCH NABI:BINDING CBIND:TARGET@ CTARGET:ARCH@ CTARGET-ARCH:EQ 0= ;

: WRITER-MACHINE-CK ( -- )
   FOREIGN? if
      s" native-build: no writer is loaded for a --target on another machine; a writer loaded after the capture would compile for the window's machine" BUILD-RC die
   then
   OUTPUT-LAUNCH-CK ;

CAST: OPENER-XT ( n -- [ bool -- ] )
TYPED-VARIABLE HELD-WRITER [ AOT-OWNED:capture ptr n n ptr u8 n -- ]

: HELD-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: x86-64 writer not loaded before the target window" BUILD-RC die
   then
   XREF-START ;

\ The retained host binding and the selected output action are both fixed
\ before the target window replaces the compiler dispatch. Only an executable
\ same-binding build can reuse its placed native emission.
: NATIVE-X64? ( -- bool )
   BUILD-TARGET:ACTION@ {: action:RTARGET:build-action :}
   action RTARGET:EXECUTION@ action RTARGET:BODY-TARGET@
      RTARGET:EXECUTABLE-HERE?
   NABI:BINDING action RTARGET:BODY-TARGET@ RTARGET:NATIVE-BINDING
      CBIND:SAME? and ;

: HOLD-X64 ( -- )
   s" NATIVE-EMIT:WRITE-C2" HELD-XT WRITER-XT HELD-WRITER !
   s" X64LINK:NAMES-SPAN" HELD-XT NAME-SPAN-XT WRITER-NAME-SPAN !
   NATIVE-X64?
   s" NATIVE-EMIT:OPEN-SHADOW" HELD-XT OPENER-XT execute ;

\ The reader's capture has its own bytes. Loading a writer may allocate and
\ compile freely; none of those definitions or mutations enters that value.
: SOURCE-WRITER-DISPATCH ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   X64-TARGET? if HELD-WRITER @ execute exit then
   WRITER-MACHINE-CK
   SOURCE-WRITER execute ;

DYNAMIC-BUFFER BUILD-INPUTS u8
variable BUILD-INPUTS-U
variable BUILD-INPUTS-MARK
TYPED-VARIABLE BUILD-INPUTS-PATH ptr u8
variable BUILD-INPUTS-PATH-U

: BUILD-INPUTS-APPEND ( ptr u8 n -- )
   {: rows:ptr rowsu:n :}
   rowsu 0= if exit then
   BUILD-INPUTS-U @ rowsu + BUILD-INPUTS-RESERVE
   rows 0 BUILD-INPUTS BUILD-INPUTS-U @ + rowsu BYTE-COPY
   rowsu BUILD-INPUTS-U +! ;

: FILE-INPUTS-SNAPSHOT ( -- )
   BUILD-INPUTS-PATH-U @ 0= if 0 BUILD-INPUTS-U ! exit then
   BUILD-INPUTS-MARK @ NFILE-INPUTS:BYTES-FROM
      {: rows:ptr rowsu:n :}
   rowsu 0= if s" native-build: no engine source inputs" 76 die then
   0 BUILD-INPUTS-U !
   rows rowsu BUILD-INPUTS-APPEND ;

: READ-SOURCE-WRITER ( -- )
   s" tools/native-emit.f" s" required" OPEN-TARGET-XT
      SOURCE-INCLUDED-XT execute ;

: WRITER-INPUTS-RETAIN ( -- )
   BUILD-INPUTS-U @ 0= if exit then
   NFILE-INPUTS:BYTES$ BUILD-INPUTS-APPEND ;

\ The post-capture writer loads through the fresh target's SOURCE-INPUT.
\ Its nested file-cache session starts a new input buffer, so bind that loader
\ to the same query and consumed-byte recorder before the first writer require.
: WRITER-INPUTS ( -- )
   ['] REWIND-CANON
   s" SOURCE-ROOT:READ-OS" OPEN-TARGET-XT SOURCE-READ-XT
   s" SOURCE-INPUT:USE" OPEN-TARGET-XT SOURCE-INPUT-USE-XT execute
   ['] PREFIX-INPUT ['] PREFIX-NOOP
   s" SOURCE-INPUT:USE-OBSERVER" OPEN-TARGET-XT
      SOURCE-INPUT-OBSERVER-XT execute ;

: WRITER-INPUTS-RESET ( -- )
   s" SOURCE-INPUT:OBSERVER-DEFAULT" OPEN-TARGET-XT
      SOURCE-RESET-XT execute ;

: READ-WRITER ( -- )
   \ Snapshot preparation cleared the target root from the captured image.
   TARGET-ENGINE-ROOT
   s" engine"
      [: NDEF-CACHE:REBIND
         WRITER-INPUTS
         [: READ-SOURCE-WRITER WRITER-INPUTS-RETAIN ;]
            [: WRITER-INPUTS-RESET ;] finally ;]
      NDEF-CACHE:WITH-QUIET ;

\ The owned capture is already frozen. Only this uncaptured writer selects
\ the host JIT; a caller-owned outer native scope still refuses that selection.
variable WRITER-TIER
: LOAD-SOURCE-WRITER ( -- )
   tier@ WRITER-TIER !
   [: HB-TARGET-LINUX-X86-64? if 1 else 0 then TIER:SELECT READ-WRITER ;]
      [: WRITER-TIER @ TIER:SELECT ;] finally ;

\ A caller-supplied writer (RUN-PATH-WRITER-RC) runs after the source writer
\ loads, so it can bind that writer by name (SOURCE-WRITER-NAMED).
TYPED-VARIABLE WRITER-OVERRIDE [ AOT-OWNED:capture ptr n n ptr u8 n -- ]
variable WRITER-OVERRIDE?

\ An x86-64 target's writer was loaded before the window (HOLD-X64).
: WRITE-OWNED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   WRITER-OVERRIDE? @ 0<> X64-TARGET? 0= or if LOAD-SOURCE-WRITER then
   WRITER-OVERRIDE? @ if
      dup NATIVE-LAYOUT:CURRENT TEMP$ WRITER-OVERRIDE @ execute exit
   then
   dup NATIVE-LAYOUT:CURRENT TEMP$ SOURCE-WRITER-DISPATCH ;

: EMIT-TEMP ( AOT-OWNED:capture -- )
   {: owned:AOT-OWNED:capture :}
   owned ['] WRITE-OWNED catch {: rc:n :}
   drop
   owned AOT-OWNED:CLOSE
   rc 0<> if rc throw then
   TEMP$ CHMOD-X ;

: SMOKE-RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF
         PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF
         PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N
      ENDOF
   ;MATCH ;

\ `.` ends its number with a newline of its own and `cr` adds the second, so one
\ printed value is "<digits>\n\n". The smoke program prints two: the arithmetic
\ that proves the image runs, then the class the image says it is.
: SMOKE-PROGRAM$ ( -- ptr u8 n )
   S\" : X ( -- n ) 42 ; X . cr ENGINE-INTERNAL:IMAGE-CLASS . cr\n" ;

: SMOKE-EXPECT$ ( -- ptr u8 n )
   CLASS-WANTED @ CLASS-WHITEBOX = if S\" 42\n\n1\n\n" exit then
   S\" 42\n\n0\n\n" ;

: SMOKE-OTHER$ ( -- ptr u8 n )
   CLASS-WANTED @ CLASS-WHITEBOX = if S\" 42\n\n0\n\n" exit then
   S\" 42\n\n1\n\n" ;

\ The one place a whitebox image is stopped from becoming a product. It runs
\ before PROMOTE, so the refusal costs a temp file and nothing that was named.
: SMOKE-CLASS-REFUSE ( -- )
   CLASS-WANTED @ CLASS-WHITEBOX = if
      S\" native-build: `whitebox` was asked for and the image came back sealed; the target load needs HABU_WHITEBOX_IMAGE=1\n" type
      BUILD-RC throw
   then
   S\" native-build: refusing to promote a whitebox image as the product - the seal pass stood down for this build, so HABU_WHITEBOX_IMAGE=1 was set in its environment. Pass `whitebox` after the output path to build one on purpose.\n" type
   BUILD-RC throw ;

: SMOKE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   TEMP$ >LEN
   SMOKE-DIR SMOKE-DIR-U @ >LEN
   SMOKE-PROGRAM$ >LEN
   SMOKE-OUT SMOKE-CAP >LEN SMOKE-ERR SMOKE-CAP >LEN SMOKE-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE SMOKE-RESULT {: outu:n erru:n rc:n :}
   rc 0<> if
      SMOKE-ERR erru type
      BUILD-RC throw
   then
   erru 0<> if
      SMOKE-ERR erru type
      BUILD-RC throw
   then
   SMOKE-OUT outu SMOKE-EXPECT$ ENDS-WITH? 0= if
      SMOKE-OUT outu SMOKE-OTHER$ ENDS-WITH? if SMOKE-CLASS-REFUSE then
      BUILD-RC throw
   then ;

: SIGN-TEMP ( -- )
   TEMP$ CODESIGN:ENSURE ;

: PROMOTE ( -- )
   TEMP$ OUTPUT$ RENAME-FILE ;

\ ---- the build-side name map ---------------------------------------------------
\ The image keeps a name only for the records something can ask for by name and
\ strips the rest (src/habu/aot-capture.f ACAP-NAMED?). The code is all still
\ there, so a tool that reads this image - imgdump, a debugger, an xref over a
\ shipped engine - can still be handed a code address it cannot name. The build
\ therefore writes the COMPLETE map beside the engine, at <image>.map: every
\ record the capture saw, stripped ones included, with the span the image kept
\ and the name it did not.
\
\ FORMAT. A version line, a `columns` line naming the fields in order, then one
\ row per record with fields separated by one space. The columns line is the
\ extension point: a reader keys on the names, so a later column appends without
\ breaking a reader that does not know it. A Habu name carries no space, so the
\ name is last and needs no quoting.
\    rec    capture-order index, 0-based and dense
\    named  1 when the image kept this name, 0 when it stripped it
\    start  code offset in the image's payload blob (x64: REGION code band),
\           or, on a package row, its public wordlist as the image carries it
\    len    raw CODE-SPAN length: bit 31 means an exact byte span; otherwise
\           the final instruction follows the body (decode with CODE-SPAN:BYTES)
\           - or, on a package row, its private wordlist, 0 when it has none
\    wid    the record's wordlist as the image carries it (ARM: window-relative;
\           x64: placed image ID), 0 for global, or -1 for a package row
\    name   the definition's name, as the capture saw it
create NAMES-PATH OUTPUT-CAP allot
variable NAMES-PATH-U
create NAMES-HASH-CTX SHA256-FILE-CTX-BYTES allot
create NAMES-HASH-HEX 64 allot
create NAMES-ONE 1 allot
TYPED-VARIABLE NAMES-A ptr u8
variable NAMES-CAP
variable NAMES-U
variable NAMES-HASH-OFF
DYNAMIC-BUFFER NAMES-ALIAS-FLAGS u8
DYNAMIC-BUFFER NAMES-ORDER n
variable NAMES-ORDER-N
DYNAMIC-BUFFER NAMES-SHADOW n           \ capture record -> placed shadow row
DYNAMIC-BUFFER NAMES-NAMED n            \ shipped named row -> capture record
variable NAMES-NAMED-N

: NAMES-SUFFIX$ ( -- ptr u8 n ) s" .map" ;

: NAMES-PATH! ( -- )
   OUTPUT$ {: a:ptr u:n :}
   u NAMES-SUFFIX$ nip + OUTPUT-CAP > if E-FS-PATH throw then
   a NAMES-PATH u BYTE-COPY
   NAMES-SUFFIX$ NAMES-PATH u + swap BYTE-COPY
   u NAMES-SUFFIX$ nip + NAMES-PATH-U ! ;

: NAMES-PATH$ ( -- ptr u8 n ) NAMES-PATH NAMES-PATH-U @ ;

\ The buffer address is held in a typed cell: a plain variable would hand
\ BYTE-COPY a bare n where it wants a byte pointer.
: NAMES-A@ ( -- ptr u8 ) NAMES-A @ ;
: NAMES-A! ( ptr u8 -- ) NAMES-A ! ;

: NAMES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAMES-U @ u + NAMES-CAP @ > if
      s" native-build: name map buffer overflow" BUILD-RC die
   then
   a  NAMES-A@ NAMES-U @ +  u BYTE-COPY
   NAMES-U @ u + NAMES-U ! ;

\ Its own decimal renderer rather than FMT's: the build runs inside a rewound
\ prefix where the shared string builder is not in scope, and a map row needs
\ nothing more than digits.
20 constant NAMES-NCAP
create NAMES-NBUF NAMES-NCAP allot
variable NAMES-NI

: NAMES-DIGITS+ ( n -- ) {: v:n :}                    \ v >= 0, digits built tail first
   v 0= if s" 0" NAMES+ exit then
   NAMES-NCAP NAMES-NI !
   v begin dup 0 > while
      dup 10 mod 48 +
      NAMES-NI @ 1- NAMES-NI !
      NAMES-NBUF NAMES-NI @ + c!
      10 /
   repeat drop
   NAMES-NBUF NAMES-NI @ +  NAMES-NCAP NAMES-NI @ -  NAMES+ ;

: NAMES-INT+ ( n -- ) {: v:n :}
   v 0 < if s" -" NAMES+ 0 v - NAMES-DIGITS+ exit then
   v NAMES-DIGITS+ ;

: NAMES-CHAR+ ( n -- ) NAMES-ONE c! NAMES-ONE 1 NAMES+ ;

: NAMES-HEX+ ( n -- )
   dup 10 < if 48 + else 55 + then NAMES-CHAR+ ;

: NAMES-ESC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if s" -" NAMES+ exit then
   u 0 ?do
      a i + c@ {: c:n :}
      c 32 <= c 37 = or if
         37 NAMES-CHAR+ c 4 rshift NAMES-HEX+ c 15 and NAMES-HEX+
      else c NAMES-CHAR+ then
   loop ;

: NAMES-SRC$ ( n -- ptr u8 n ) {: k:n :}
   k AOT-CAPTURE:MAP-N >= if NULL$ exit then
   0 NAMES-SOURCE BYTE-VIEW k NAMES-CAP-OFF @ + k NAMES-CAP-LEN @ ;

: NAMES-SPAN< ( n n -- bool )
   {: a:n b:n :}
   a AOT-CAPTURE:MAP-START {: start:n :}
   b AOT-CAPTURE:MAP-START {: next:n :}
   start next <> if start next < exit then
   a AOT-CAPTURE:MAP-LEN {: size:n :}
   b AOT-CAPTURE:MAP-LEN {: other:n :}
   size other <> if size other < exit then
   a b < ;

: NAMES-SAME-SPAN? ( n n -- bool )
   {: a:n b:n :}
   a AOT-CAPTURE:MAP-START b AOT-CAPTURE:MAP-START =
   a AOT-CAPTURE:MAP-LEN b AOT-CAPTURE:MAP-LEN = and ;

\ Preserve capture order within each exact code span. Package rows and negative
\ starts never classify as aliases and cannot establish an earlier owner.
: NAMES-INDEX-ALIASES ( -- )
   AOT-CAPTURE:MAP-N NAMES-ALIAS-FLAGS-RESERVE
   AOT-CAPTURE:MAP-N NAMES-ORDER-RESERVE
   0 NAMES-ORDER-N !
   AOT-CAPTURE:MAP-N 0 ?do
      0 i NAMES-ALIAS-FLAGS c!
      i AOT-CAPTURE:MAP-PKG? 0= i AOT-CAPTURE:MAP-START 0 >= and if
         i NAMES-ORDER-N @ NAMES-ORDER !
         1 NAMES-ORDER-N +!
      then
   loop
   NAMES-ORDER-N @ 1 <= if exit then
   0 NAMES-ORDER NAMES-ORDER-N @ [: NAMES-SPAN< ;] SORT:SORT!
   NAMES-ORDER-N @ 1 ?do
      i 1- NAMES-ORDER @ i NAMES-ORDER @ NAMES-SAME-SPAN? if
         1 i NAMES-ORDER @ NAMES-ALIAS-FLAGS c!
      then
   loop ;

: NAMES-ALIAS? ( n -- bool ) NAMES-ALIAS-FLAGS c@ 0<> ;

: NAMES-KIND$ ( n -- ptr u8 n ) {: k:n :}
   k AOT-CAPTURE:MAP-PKG? if s" namespace" exit then
   k NAMES-ALIAS? if s" alias" exit then
   s" word" ;

: NAMES-SH-KEY ( n -- n ) {: row:n :}
   AOT-SHADOW:REC-BUF@ row AOT-SHADOW:REC-ROW * + LE:U32@ ;

\ The source capture keeps every record for the sidecar, while the payload
\ keeps only named records and private live code. Pair the two once; a shadow
\ row's anonymous key carries its capture index without shipping a name.
: NAMES-X64-PLAN ( -- )
   AOT-CAPTURE:MAP-N 1 max NAMES-SHADOW-RESERVE
   AOT-CAPTURE:MAP-N 0 ?do -1 i NAMES-SHADOW ! loop
   AOT-BUF:AOT-REC-N @ 1 max NAMES-NAMED-RESERVE
   0 NAMES-NAMED-N !
   AOT-CAPTURE:MAP-N 0 ?do
      i AOT-CAPTURE:MAP-NAMED 0<> if
         i NAMES-NAMED-N @ NAMES-NAMED !
         1 NAMES-NAMED-N +!
      then
   loop
   NAMES-NAMED-N @ AOT-BUF:AOT-REC-N @ <> if
      s" native-build: capture and x64 named rows differ" BUILD-RC die then
   AOT-SHADOW:REC-N @ 0 ?do
      i NAMES-SH-KEY {: key:n :}
      key AOT-SHADOW:ANON-REC and 0<> if
         key AOT-SHADOW:RETIRED-REC <> if
            key AOT-SHADOW:ANON-REC invert and {: k:n :}
            k AOT-CAPTURE:MAP-N >= if
               s" native-build: anonymous shadow row has no capture record" BUILD-RC die then
            k NAMES-SHADOW @ 0 >= if
               s" native-build: two shadow rows name one capture record" BUILD-RC die then
            i k NAMES-SHADOW !
         then
      else
         key NAMES-NAMED-N @ >= if
            s" native-build: named shadow row has no capture record" BUILD-RC die then
         key NAMES-NAMED @ {: k:n :}
         k NAMES-SHADOW @ 0 >= if
            s" native-build: two shadow rows name one capture record" BUILD-RC die then
         i k NAMES-SHADOW !
      then
   loop ;

: NAMES-X64-WID ( n -- n ) {: wid:n :}
   wid 0= if 0 exit then
   wid AOT-BUF:WID-REL-BASE - FIRST-DYNAMIC-WID + ;

: NAMES-X64-FIELDS ( n -- n n n ) {: k:n :}
   k AOT-CAPTURE:MAP-WID {: wid:n :}
   wid -1 = if
      k AOT-CAPTURE:MAP-START NAMES-X64-WID
      k AOT-CAPTURE:MAP-LEN NAMES-X64-WID
      -1 exit
   then
   k NAMES-SHADOW @ {: row:n :}
   row 0 < if -1 0 wid NAMES-X64-WID exit then
   row WRITER-NAME-SPAN @ execute
   wid NAMES-X64-WID ;

: NAMES-ROW ( n -- ) {: k:n :}
   k NAMES-INT+                          s"  " NAMES+
   k AOT-CAPTURE:MAP-NAMED NAMES-INT+    s"  " NAMES+
   X64-TARGET? if k NAMES-X64-FIELDS else
      k AOT-CAPTURE:MAP-START k AOT-CAPTURE:MAP-LEN k AOT-CAPTURE:MAP-WID
   then {: start:n len:n wid:n :}
   start NAMES-INT+                      s"  " NAMES+
   len NAMES-INT+                        s"  " NAMES+
   wid NAMES-INT+                        s"  " NAMES+
   k AOT-CAPTURE:MAP-NAME$ NAMES+       s"  " NAMES+
   k NAMES-KIND$ NAMES+               s"  baked " NAMES+
   k NAMES-SRC$ NAMES-ESC+
   S\" \n" NAMES+ ;

: NAMES-U32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1+ c@ 8 lshift or
   a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or ;

: NAMES-SPAN ( n -- ) {: k:n :}
   AOT-SPAN:BUF@ k AOT-SPAN:ROW * + {: row:ptr :}
   AOT-CAPTURE:MAP-N k + NAMES-INT+ s"  0 " NAMES+
   row NAMES-U32@ NAMES-INT+ s"  " NAMES+
   row 4 + NAMES-U32@ NAMES-INT+
   S\"  -1 - word baked -\n" NAMES+ ;

\ Source tokens can expand threefold when escaped. The record and span ceilings
\ are bounded by capture, so reserve for the actual accepted path bytes.
: NAMES-ALLOCATE ( -- )
   AOT-CAPTURE:MAP-N 340 * NAMES-SOURCE-U @ 3 * +
   AOT-SPAN:N @ 80 * + $1000 + MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   buf NAMES-A!  cap NAMES-CAP !  0 NAMES-U ! ;

: BUILD-NAMES ( -- )
   NAMES-INDEX-ALIASES
   NAMES-ALLOCATE
   S\" habu-names 1\n" NAMES+
   S\" columns rec named start len wid name kind origin source\n" NAMES+
   s" image-sha256 " NAMES+
   NAMES-U @ NAMES-HASH-OFF !
   64 0 ?do 48 NAMES-CHAR+ loop
   10 NAMES-CHAR+
   AOT-CAPTURE:MAP-N 0 ?do i NAMES-ROW loop
   AOT-SPAN:N @ 0 ?do i NAMES-SPAN loop ;

: WRITE-NAMES ( -- )
   NAMES-PATH!
   NAMES-HASH-CTX OUTPUT$ NAMES-HASH-HEX SHA256-FILE-HEX-IN
      dup 0<> if throw then drop
   NAMES-HASH-HEX NAMES-A@ NAMES-HASH-OFF @ + 64 BYTE-COPY
   NAMES-PATH$ NAMES-A@ NAMES-U @ WRITE-ALL ;

\ The suite builder's private directory is absolute. Each capture retains its
\ rows until the corresponding final image exists and can bind their hash.
create BUNDLE-PATH OUTPUT-CAP allot
variable BUNDLE-PATH-U
TYPED-VARIABLE BUNDLE-DIR ptr u8
variable BUNDLE-DIR-U
TYPED-VARIABLE WHITE-NAMES-A ptr u8
variable WHITE-NAMES-U
TYPED-VARIABLE SEALED-NAMES-A ptr u8
variable SEALED-NAMES-U

: BUNDLE-OUTPUT ( ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n name:ptr nameu:n :}
   dir diru name nameu BUNDLE-PATH JOIN-PATH BUNDLE-PATH-U !
   BUNDLE-PATH BUNDLE-PATH-U @ OUTPUT! ;

: OWN-CAPTURE ( -- AOT-OWNED:capture )
   AOT-CAPTURE:CODE-WINDOW [: code-origin ;] false AOT-FILE:OWN-WINDOW ;

: WRITE-BUNDLE-CAPTURE ( AOT-OWNED:capture [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- )
   {: writer :}
   NATIVE-LAYOUT:CURRENT OUTPUT$ writer execute
   OUTPUT$ CHMOD-X ;

: BUNDLE-PREPARE ( -- )
   BUILD-TARGET:CURRENT RTARGET:CORE MATCH RTARGET:core-result
      supported OF drop ENDOF
      unsupported OF RTARGET:E-UNSUPPORTED throw ENDOF
   ;MATCH
   CHECK-HOST-LAYOUT
   RETAIN-HOST-PRODUCERS ;

: FILE-LOAD ( -- )
   \ The manifest destination is driver transport, read before source recording.
   s" HABU_ENGINE_INPUTS" GETENV BUILD-INPUTS-PATH-U ! BUILD-INPUTS-PATH !
   NFILE-INPUTS:MARK BUILD-INPUTS-MARK !
   ['] PREFIX-INPUT ['] PREFIX-NOOP SOURCE-INPUT:USE-OBSERVER
   CHECKER-OWNER LOGICAL-RESET
   BIND-HOST-UNIT
   PREFIX-START
   OPEN-FILE-AND-COMPILE
   FILE-INPUTS-SNAPSHOT ;

: BUNDLE-CAPTURE ( -- AOT-OWNED:capture AOT-OWNED:capture ptr n n )
   SOURCE-CHECK @ execute
   RESET-TARGET-SOURCE
   SOURCE-CLOSE @ execute
   TRANSFER-LITERALS
   COPY-SEAL-PLAN {: plan:ptr count:n :}
   AOT-CAPTURE:PAYLOAD-CAPTURE
   NAMES-SOURCES
   PREPARE-WINDOW
   true CAPTURE-NAMED
   BUILD-NAMES
   NAMES-A@ WHITE-NAMES-A !  NAMES-U @ WHITE-NAMES-U !
   OWN-CAPTURE {: white:AOT-OWNED:capture :}
   plan count APPLY-SEAL-PLAN
   PREPARE-TARGET
   false CAPTURE-NAMED
   BUILD-NAMES
   NAMES-A@ SEALED-NAMES-A !  NAMES-U @ SEALED-NAMES-U !
   OWN-CAPTURE {: sealed:AOT-OWNED:capture :}
   white sealed plan count ;

: BUNDLE-WRITE ( AOT-OWNED:capture AOT-OWNED:capture ptr n n -- AOT-OWNED:capture AOT-OWNED:capture ptr n n )
   {: white:AOT-OWNED:capture sealed:AOT-OWNED:capture plan:ptr count:n :}
   BUNDLE-DIR @ BUNDLE-DIR-U @ {: dir:ptr diru:n :}
   WRITER-MACHINE-CK
   LOAD-SOURCE-WRITER
   dir diru s" hb-whitebox-source" BUNDLE-OUTPUT
   white s" NATIVE-EMIT:WRITE-C2" SOURCE-WRITER-NAMED WRITE-BUNDLE-CAPTURE
   WHITE-NAMES-A @ NAMES-A!  WHITE-NAMES-U @ NAMES-U !
   WRITE-NAMES
   dir diru s" hb-root" BUNDLE-OUTPUT
   sealed s" NATIVE-EMIT:WRITE-C2" SOURCE-WRITER-NAMED WRITE-BUNDLE-CAPTURE
   SEALED-NAMES-A @ NAMES-A!  SEALED-NAMES-U @ NAMES-U !
   WRITE-NAMES
   dir diru s" hb-ordinary" BUNDLE-OUTPUT
   sealed s" NATIVE-EMIT:WRITE" SOURCE-WRITER-NAMED WRITE-BUNDLE-CAPTURE
   WRITE-NAMES
   white sealed plan count ;

: BUNDLE-FINISH ( -- )
   ['] BUNDLE-CAPTURE EXECUTABLE-BUILD:WITH
   {: white:AOT-OWNED:capture sealed:AOT-OWNED:capture plan:ptr count:n :}
   white sealed plan count ['] BUNDLE-WRITE catch {: rc:n :}
   2drop drop drop
   white AOT-OWNED:CLOSE
   sealed AOT-OWNED:CLOSE
   plan count 2 * cells munmap 0<> if E-MEM-UNMAP throw then
   rc 0<> if rc throw then ;

: REMOVE-STALE-TEMP ( -- )
   TEMP$ 2dup SYMLINK? if REMOVE-FILE exit then
   2dup EXISTS? if REMOVE-FILE else 2drop then ;

: IMAGE-CAPTURE ( [ n n -- n ] bool -- AOT-OWNED:capture )
   {: query bootstrap:bool :}
   SOURCE-CHECK @ execute
   RESET-TARGET-SOURCE
   SOURCE-CLOSE @ execute
   TRANSFER-LITERALS
   AOT-CAPTURE:PAYLOAD-CAPTURE
   \ Private source identities are still live before the checker sweep.
   NAMES-SOURCES
   PREPARE-TARGET
   CAPTURE
   \ An x86-64 map row reads the placement the held writer makes, so its rows
   \ are paired with the shadow here and built once the image is written.
   X64-TARGET? if NAMES-X64-PLAN else BUILD-NAMES then
   AOT-CAPTURE:CODE-WINDOW query bootstrap AOT-FILE:OWN-WINDOW ;

: IMAGE-INSTALL ( -- )
   X64-TARGET? if BUILD-NAMES then
   SIGN-TEMP
   FOREIGN? 0= if OUTPUT-LAUNCH-CK SMOKE then
   PROMOTE
   WRITE-NAMES ;

: IMAGE-FINISH ( [ n n -- n ] bool -- )
   ['] IMAGE-CAPTURE EXECUTABLE-BUILD:WITH
   EMIT-TEMP
   IMAGE-INSTALL ;

\ A failed load reaches its policy close without passing IMAGE-CAPTURE.
: CLEANUP-SOURCE ( -- )
   [: RESET-TARGET-SOURCE SOURCE-CLOSE @ execute ;] EXECUTABLE-BUILD:WITH ;

TYPED-VARIABLE READY-QUERY [ n n -- n ]
variable READY-BOOTSTRAP

: FILE-PREPARE ( -- )
   BUILD-TARGET:CURRENT RTARGET:CORE MATCH RTARGET:core-result
      supported OF drop ENDOF
      unsupported OF RTARGET:E-UNSUPPORTED throw ENDOF
   ;MATCH
   CHECK-HOST-LAYOUT
   \ An x86-64 target's writer runs in this host, so it may be another
   \ machine's: bind it and open its shadow before the window loads.
   X64-TARGET? if
      FOREIGN? 0= if OUTPUT-LAUNCH-CK then
      HOLD-X64
   else WRITER-MACHINE-CK then
   RETAIN-HOST-PRODUCERS ;

: FILE-FINISH ( -- )
   READY-QUERY @ READY-BOOTSTRAP @ 0<> IMAGE-FINISH
   BUILD-INPUTS-U @ 0 > if
      BUILD-INPUTS-PATH @ BUILD-INPUTS-PATH-U @
         0 BUILD-INPUTS BUILD-INPUTS-U @ ATOMIC-WRITE-FILE
   then ;

: RUN-FILE-INNER ( -- n )
   BUILD-TARGET:IDLE-CK
   CLEANUP-RESET
   0 TARGET-SOURCE-BOUND !
   REMOVE-STALE-TEMP
   TEMP$ CLEANUP+
   SMOKE-DIR!
   \ The load owns WINDOW-OPEN through WINDOW-CLOSE. Cache session setup and
   \ reset outside it only bind loader state; finish audits and owns the window.
   s" engine" RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      [: ['] FILE-PREPARE EXECUTABLE-BUILD:WITH ;]
      [: ['] FILE-LOAD EXECUTABLE-BUILD:WITH ;] ['] FILE-FINISH
      BUILD-CORE:WITH {: rc:n :}
   RESTORE-HOST-UNIT
   CLEANUP-SOURCE
   CLEANUP-RUN
   BUILD-INPUTS-RELEASE
   rc 0<> if s" native-build: uncaught throw code " type rc . cr then
   rc ;

public

\ Test-only entry. Its source load is selected by the caller's bundle
\ environment; standalone RUN-PATH-RC keeps its product/whitebox path.
\ The suite selects its source policy before the one compiler bundle load.
\ Ordinary callers keep the default policy through RUN-BUNDLE.
: RUN-BUNDLE-READY ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   BUILD-TARGET:IDLE-CK
   dir BUNDLE-DIR !
   diru BUNDLE-DIR-U !
   s" engine" RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      [: ['] BUNDLE-PREPARE EXECUTABLE-BUILD:WITH ;]
      [: ['] FILE-LOAD EXECUTABLE-BUILD:WITH ;] ['] BUNDLE-FINISH
      ['] BUILD-CORE:WITH WITH-TARGET-ROOT {: rc:n :}
   RESTORE-HOST-UNIT
   rc 0<> if rc throw then ;

: RUN-BUNDLE ( ptr u8 n -- )
   BUILD-TARGET:IDLE-CK
   DEFAULT-SOURCE-POLICY
   RUN-BUNDLE-READY ;

\ The build proper, to an explicit output path, returning its result code.
\
\ RUN is this plus the argv convention and the exit. A driver that wants to do
\ anything AFTER the build needs one that returns, and `die` never does: it is
\ an unconditional exit_group, so `include tools/native-build.f` has no code
\ path back to its caller. tools/build-profile.f prints the profiler's report
\ after the build and is that driver.
: RUN-PATH-RC ( ptr u8 n [ n n -- n ] bool -- n )
   {: out:ptr outu:n query bootstrap:bool :}
   BUILD-TARGET:IDLE-CK
   ['] SOURCE-NOOP ['] SOURCE-NOOP ['] SOURCE-NOOP SOURCE-POLICY!
   out outu OUTPUT!
   query READY-QUERY !
   bootstrap READY-BOOTSTRAP !
   ['] RUN-FILE-INNER WITH-TARGET-ROOT ;

\ RUN-PATH-RC with the image written by `writer` instead of the source writer's
\ WRITE-C2. The x86-64 fixture writer (test/native-fixture-native.f) merges a
\ partial artifact into the runtime capture this way.
: RUN-PATH-WRITER-RC ( ptr u8 n [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- n )
   {: out:ptr outu:n query bootstrap:bool writer :}
   writer WRITER-OVERRIDE !
   -1 WRITER-OVERRIDE? !
   out outu query bootstrap RUN-PATH-RC
   0 WRITER-OVERRIDE? ! ;

\ The exit status of a finished build, for every driver that ends the process
\ with one (RUN and tools/build-profile.f): 0,
\ PROC-TIMEOUT-RC when a deadline expired (src/lib/process.f), and BUILD-RC for any
\ other failure. RUN-FILE-INNER has already printed the throw code itself.
: EXIT-RC ( n -- n )
   BUILD-RC PROC-EXIT-RC ;

\ The build says which of the two images it wrote, on the way out, so a build
\ log records the class instead of leaving it to be inferred from a size.
: REPORT-CLASS ( -- )
   \ hb-build reports the installed output path after the child build finishes.
   BUILD-INPUTS-PATH-U @ 0<> if exit then
   s" native-build OK: " type OUTPUT$ type
   s"  (" type CLASS-WANTED$ type s" )" type cr ;

: RUN ( [ n n -- n ] bool -- ) {: query bootstrap:bool :}
   BUILD-ARGS!
   0 SCRIPT-ARGV$ query bootstrap RUN-PATH-RC {: rc:n :}
   rc 0= if REPORT-CLASS then
   s" " rc EXIT-RC die ;

;package
