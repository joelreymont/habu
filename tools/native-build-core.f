\ Shared native-build driver. Entries select and own the compilation tier.

require lib/errors.f
require src/core/prefix-boundary.f
require lib/string.f
require lib/memory.f
require src/habu/address-cells.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require src/os/script-argv.f
require tools/native-build-args.f
require tools/build-target.f
require lib/codesign.f
require lib/executable-build.f

\ A cross writer runs in the build host. Load it before the target window
\ installs its own compiler, and before ARM64's global CODE/LBL definitions.
package NATIVE-BUILD
: X64-TARGET? ( -- bool ) TARGET-ARCH CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ ;
: LOAD-X64-WRITER ( -- )
   X64-TARGET? if s" tools/native-emit-x64.f" required then ;
' LOAD-X64-WRITER
;package
execute

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/compiler/native/string.f
require src/compiler/native/abi.f
require tools/native-layout.f

package NATIVE-BUILD

$1000 constant SMOKE-CAP
10000 constant SMOKE-TIMEOUT-MS

create SMOKE-OUT SMOKE-CAP allot
create SMOKE-ERR SMOKE-CAP allot

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

TRUSTED: RESET-XT ( n -- [ -- ] ) ;
TRUSTED: IMPORT-XT ( n -- [ ptr u8 -- ] ) ;

\ The retained host's literal rows come from the package that owns them, which
\ publishes them while it is open, one segment of the active pool at a time
\ (src/compiler/native/string.f SEGMENTS, SOURCE-SPAN, SOURCE-ROWS). This driver
\ used to find ARENA, ARENA-CAP, R-OFF and R-LEN by name in NSTR's PRIVATE
\ wordlist, through the namespace record, after the package had closed - four
\ private names a shipped engine had to keep alive for one caller. A call needs
\ none of them.
: LITERAL-SOURCE-ROWS ( n -- ptr u8 n ptr n ptr n )
   NSTR:SOURCE-ROWS ;

TRUSTED: LITERAL-ADDRESS ( ptr u8 -- n ) ;

: LITERAL-SPAN-REFUSE ( -- )
   s" native-build: source literal arena outside capture" BUILD-RC die ;

: CHECK-LITERAL-SPAN ( n -- )
   NSTR:SOURCE-SPAN {: arena:ptr size:n :}
   arena LITERAL-ADDRESS {: start:n :}
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

\ Logical reset clears the target declaration cell. The retained source
\ certifier stays in DECL-CELL until the fresh checker is transferred.
: SOURCE-CHECKER-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

\ These execution tokens belong to the retained/target private checker owners.
: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ RESET-XT execute ;

: TRANSFER-CHECKER ( ptr u8 -- ) {: source:ptr :}
   CHECKER-OWNER {: owner:ptr :}
   owner 0= if s" native-build: target checker owner missing" 76 die then
   source owner NCOMP-DISPATCH:DECL-TRANSFER-OFF + CELL-VIEW @ IMPORT-XT execute ;

\ The window's target seam: the build's target (tools/build-target.f, the host
\ unless `--target` named another) chooses which OS sources the window is given,
\ and an unknown target is refused rather than defaulted. From here on the
\ window's own target.f answers HB-TARGET-* for everything the window compiles,
\ while this engine's predicates stay its own.
: NB-TARGET-CORE-FILES ( -- )
   BUILD-TARGET:LINUX? if
      s" src/os/linux/target.f" included
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
      s" src/os/linux-x86-64/layout.f" included
      exit
   then
   s" native-build: unknown target" 76 die ;

\ The discarded build host remains callable through this compiled continuation,
\ but none of its dictionary records or address declarations enters the window.
TRUSTED: LOGICAL-RESET ( ptr u8 -- )
   0 set-check
   0 set-top-check
   RESET-CHECKER
   CORE-PREFIX:FIRST-RECORD seed-ndict!
   RESET-ADDRESS-ROWS ;

TRUSTED: SOURCE-RESET-XT ( n -- [ -- ] ) ;

\ Resolve the fresh loader inside the open target code window. A retained
\ SOURCE-INPUT:RESET would activate the wrong loader after LOGICAL-RESET.
: OPEN-TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: target source operation missing" BUILD-RC die
   then
   XREF-START {: xt:n :}
   xt AOT-ARM:B0 @ < xt cp@ >= or if
      s" native-build: source operation outside target window" BUILD-RC die
   then
   xt ;

variable TARGET-SOURCE-BOUND

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
   s" SOURCE-INPUT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
   s" REQUIRE-BOOT-OPEN" OPEN-TARGET-XT SOURCE-RESET-XT execute ;

: DEFAULT-SOURCE-POLICY ( -- )
   ['] DEFAULT-SOURCE-BIND ['] SOURCE-NOOP ['] SOURCE-NOOP SOURCE-POLICY! ;

: RESET-TARGET-SOURCE ( -- )
   TARGET-SOURCE-BOUND @ if
      s" SOURCE-INPUT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
      s" SOURCE-UNIT:RESET" OPEN-TARGET-XT SOURCE-RESET-XT execute
      0 TARGET-SOURCE-BOUND !
   then ;

: LOAD-TARGET ( -- )
   SOURCE-CHECKER-OWNER {: source:ptr :}
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker-fetch-abi.f" included
   s" src/core/checker-owner-abi.f" included
   s" src/habu/prims.f" included
   s" src/core/does-clause.f" included
   s" src/core/checker.f" included
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" src/core/render.f" included
   s" src/core/sumtype.f" included
   s" src/core/layout-buffer.f" included
   s" src/core/layout-valid.f" included
   source TRANSFER-CHECKER
   \ Transfer publishes the complete replacement owner. Earlier pre-hook code
   \ still needs the retained compiler's paired checker; later calls use this one.
   CHECKER-OWNER:CAPTURE-PREPARE
   CHECKER-OWNER AOT-ARM:PAYLOAD-PERSISTENT
   s" src/core/check-hook.f" included
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
   s" src/core/include.f" included
   SOURCE-BIND @ execute
   s" src/habu/native-runtime.f" included
   SOURCE-TAIL @ execute ;

: OPEN-AND-COMPILE ( -- )
   \ APP-IMAGE preserves the exact DATA cursor, including a trailing byte field.
   \ The captured window starts on the cell grid used by address relocation.
   align
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   NSTR:WINDOW-OPEN
   [: LOAD-TARGET ;] SOURCE-ROOT:WITH-CWD
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

\ The target's row importer is the one private word this driver still finds by
\ name: it belongs to the FRESHLY LOADED target's pool, which did not exist when
\ this file was compiled, and the alternatives are a public wrapper - a
\ well-typed way for any source to invoke literal ownership import, which
\ test/compiler/native-string.f forbids - or a fixed engine cell for the token.
\ The capture keeps its name for this lookup: src/habu/aot-capture.f ACAP-KEEP?.
: TARGET-IMPORTER ( -- n )
   s" NSTR" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if
      drop s" native-build: literal owner missing" 76 die
   then
   XREF-PKG-PRIVATE  s" IMPORT-ROWS" rot XREF-FIND-WL
   dup XREF-FOUND? 0= if
      drop s" native-build: literal importer missing" 76 die
   then
   XREF-START TARGET-CODE ;

TRUSTED: PREPARE-XT ( n -- [ -- ] ) ;
\ TARGET-XT hands the target's package seal over as a code address integer.
CAST: SEAL-XT ( n -- [ n -- ] )
TRUSTED: LITERAL-IMPORT-XT ( n -- [ ptr u8 n ptr n ptr n -- ] ) ;

\ Every segment's arena is checked before the target imports any of them.
: TRANSFER-LITERALS ( -- )
   NSTR:SEGMENTS 0 ?do i CHECK-LITERAL-SPAN loop
   TARGET-IMPORTER {: importer:n :}
   NSTR:SEGMENTS 0 ?do
      i LITERAL-SOURCE-ROWS importer LITERAL-IMPORT-XT execute
   loop ;

\ The package seal comes first: CAPTURE-PREPARE's checker sweep reads the
\ protected bits it sets (src/core/checker-surface.f KEEP?).
: PREPARE-TARGET ( -- )
   AOT-ARM:R0 @ s" ENGINE-INTERNAL:SEAL-PACKAGES" TARGET-XT SEAL-XT execute
   s" NATIVE-RUNTIME:CAPTURE-PREPARE" TARGET-XT PREPARE-XT execute
   s" CHECKER-STORAGE-UNBIND-ALL" TARGET-XT PREPARE-XT execute
   AOT-ARM:HERE-N AOT-ARM:D1 ! ;

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

: CAPTURE ( -- )
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
   NSHADOW:NATIVE? if
      AOT-ARM:WINDOW$
      ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
      AOT-CAPTURE:CAPTURE-NATIVE
   else
      AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   then
   AOT-CAPTURE:SHADOW-CAPTURE
   NSHADOW:CLOSE
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

\ This private boundary has the source writer's exact compiled ABI. Its capture
\ is a multi-cell value, which cannot be passed through interpret-mode evaluate.
TRUSTED: WRITER-XT ( n -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] ) ;
TRUSTED: NAME-SPAN-XT ( n -- [ n -- n n ] ) ;
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
   then ;

TRUSTED: OPENER-XT ( n -- [ bool -- ] ) ;
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
   s" tools/native-emit.f" required
   SOURCE-WRITER execute ;

: WRITE-TARGET ( AOT-OWNED:capture [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- )
   {: writer :}
   NATIVE-LAYOUT:CURRENT TEMP$ writer execute ;

: WRITE-OWNED ( AOT-OWNED:capture [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- AOT-OWNED:capture [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
   {: writer :}
   dup writer WRITE-TARGET writer ;

: EMIT-TEMP ( [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- )
   {: query bootstrap:bool writer :}
   AOT-CAPTURE:CODE-WINDOW query bootstrap AOT-FILE:OWN-WINDOW writer
   ['] WRITE-OWNED catch {: rc:n :}
   drop
   AOT-OWNED:CLOSE
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
   SMOKE-OUT outu SMOKE-EXPECT$ STR= 0= if
      SMOKE-OUT outu S\" 42\n\n" STARTS-WITH? if SMOKE-CLASS-REFUSE then
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
\ therefore writes the COMPLETE map beside the engine, at <image>.names: every
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
TYPED-VARIABLE NAMES-A ptr u8
variable NAMES-CAP
variable NAMES-U
DYNAMIC-BUFFER NAMES-SHADOW n           \ capture record -> placed shadow row
DYNAMIC-BUFFER NAMES-NAMED n            \ shipped named row -> capture record
variable NAMES-NAMED-N

: NAMES-SUFFIX$ ( -- ptr u8 n ) s" .names" ;

: NAMES-PATH! ( -- )
   OUTPUT$ {: a:ptr u:n :}
   u NAMES-SUFFIX$ nip + OUTPUT-CAP > if E-FS-PATH throw then
   a NAMES-PATH u BYTE-COPY
   NAMES-SUFFIX$ NAMES-PATH u + swap BYTE-COPY
   u NAMES-SUFFIX$ nip + NAMES-PATH-U ! ;

: NAMES-PATH$ ( -- ptr u8 n ) NAMES-PATH NAMES-PATH-U @ ;

\ The buffer address is held in a typed cell: a plain variable would hand
\ BYTE-COPY a bare n where it wants a byte pointer, the same reason env-base.f
\ keeps ENV-QA behind a ptr-field.
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
   k AOT-CAPTURE:MAP-NAME$ NAMES+
   S\" \n" NAMES+ ;

\ 64 bytes a row is the budget: five decimal fields and their separators cannot
\ reach 40, and ACAP-POOL-ADD already refuses a name over 255, so the only way
\ past this is a record count the capture itself would have refused first.
: NAMES-ALLOCATE ( -- )
   AOT-CAPTURE:MAP-N 64 * $1000 + MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   buf NAMES-A!  cap NAMES-CAP !  0 NAMES-U ! ;

: WRITE-NAMES ( -- )
   NAMES-PATH!
   NAMES-ALLOCATE
   S\" habu-names 1\n" NAMES+
   S\" columns rec named start len wid name\n" NAMES+
   AOT-CAPTURE:MAP-N 0 ?do i NAMES-ROW loop
   NAMES-PATH$ NAMES-A@ NAMES-U @ WRITE-ALL ;

: REMOVE-STALE-TEMP ( -- )
   TEMP$ 2dup SYMLINK? if REMOVE-FILE exit then
   2dup EXISTS? if REMOVE-FILE else 2drop then ;

: DRIVE ( [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
   {: query bootstrap:bool writer :}
   BUILD-TARGET:CURRENT RTARGET:CORE MATCH RTARGET:core-result
      supported OF drop ENDOF
      unsupported OF RTARGET:E-UNSUPPORTED throw ENDOF
   ;MATCH
   FOREIGN? 0= if OUTPUT-LAUNCH-CK then
   CHECK-HOST-LAYOUT
   X64-TARGET? 0= if WRITER-MACHINE-CK then
   X64-TARGET? if HOLD-X64 then
   CHECKER-OWNER LOGICAL-RESET
   OPEN-AND-COMPILE
   SOURCE-CHECK @ execute
   RESET-TARGET-SOURCE
   SOURCE-CLOSE @ execute
   TRANSFER-LITERALS
   AOT-CAPTURE:PAYLOAD-CAPTURE
   PREPARE-TARGET
   CAPTURE
   X64-TARGET? if NAMES-X64-PLAN then
   query bootstrap writer EMIT-TEMP
   SIGN-TEMP
   FOREIGN? 0= if OUTPUT-LAUNCH-CK SMOKE then
   PROMOTE
   WRITE-NAMES
   query bootstrap writer ;

\ The build proper, to an explicit output path, returning its result code.
\
\ RUN is this plus the argv convention and the exit. A driver that wants to do
\ anything AFTER the build needs one that returns, and `die` never does: it is
\ an unconditional exit_group, so `include tools/native-build.f` has no code
\ path back to its caller. tools/build-profile.f prints the profiler's report
\ after the build and is that driver.
: RUN-READY-INNER ( [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- n )
   {: query bootstrap:bool writer :}
   CLEANUP-RESET
   0 TARGET-SOURCE-BOUND !
   REMOVE-STALE-TEMP
   TEMP$ CLEANUP+
   SMOKE-DIR!
   query bootstrap writer [: DRIVE ;] catch {: rc:n :}
   drop 2drop
   RESET-TARGET-SOURCE
   SOURCE-CLOSE @ execute
   CLEANUP-RUN
   rc 0<> if s" native-build: uncaught throw code " type rc . cr then
   rc ;

TYPED-VARIABLE READY-QUERY [ n n -- n ]
variable READY-BOOTSTRAP
TYPED-VARIABLE READY-WRITER [ AOT-OWNED:capture ptr n n ptr u8 n -- ]
variable READY-RC

: READY-ACTION ( RTARGET:build-action -- )
   drop
   READY-QUERY @ READY-BOOTSTRAP @ 0<> READY-WRITER @
      RUN-READY-INNER READY-RC ! ;

: RUN-READY-RC ( [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- n )
   {: query bootstrap:bool writer :}
   BUILD-TARGET:IDLE-CK
   query READY-QUERY !
   bootstrap READY-BOOTSTRAP !
   writer READY-WRITER !
   RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      [: READY-ACTION ;] BUILD-TARGET:WITH
   READY-RC @ ;

public

: RUN-PATH-RC ( ptr u8 n [ n n -- n ] bool -- n )
   {: out:ptr outu:n query bootstrap:bool :}
   BUILD-TARGET:IDLE-CK
   DEFAULT-SOURCE-POLICY
   out outu OUTPUT!
   query bootstrap ['] SOURCE-WRITER-DISPATCH RUN-READY-RC ;

\ The exit status of a finished build, for every driver that ends the process
\ with one (RUN, RUN-IMAGE, RUN-UNIT and tools/build-profile.f): 0,
\ PROC-TIMEOUT-RC when a deadline expired (lib/process.f), and BUILD-RC for any
\ other failure. RUN-READY-RC has already printed the throw code itself.
: EXIT-RC ( n -- n )
   BUILD-RC PROC-EXIT-RC ;

\ The build says which of the two images it wrote, on the way out, so a build
\ log records the class instead of leaving it to be inferred from a size.
: REPORT-CLASS ( -- )
   s" native-build OK: " type OUTPUT$ type
   s"  (" type CLASS-WANTED$ type s" )" type cr ;

: RUN ( [ n n -- n ] bool -- ) {: query bootstrap:bool :}
   BUILD-ARGS!
   0 SCRIPT-ARGV$ query bootstrap RUN-PATH-RC {: rc:n :}
   rc 0= if REPORT-CLASS then
   s" " rc EXIT-RC die ;

\ APP-ENTRY changes the argv slice. Own both arguments before clearing it and
\ enter the same protected build scope used by the source-loaded driver.
: RUN-IMAGE ( [ n n -- n ] bool [ AOT-OWNED:capture ptr n n ptr u8 n -- ] -- )
   {: query bootstrap:bool writer :}
   BUILD-ARGS!
   DEFAULT-SOURCE-POLICY
   0 SCRIPT-ARGV$ OUTPUT!
   \ The saved image already registered this cell; zero adds no code reference.
   0 data-base APP-ENTRY:XT-CELL + !
   query bootstrap writer [: RUN-READY-RC ;] EXECUTABLE-BUILD:WITH {: rc:n :}
   rc 0= if REPORT-CLASS then
   s" " rc EXIT-RC die ;

\ A saved builder uses the same target writer as a source build. DRIVE binds
\ the x86 writer before the target window, while an ARM image has its C2
\ writer already loaded, so the dispatch also preserves RUN-IMAGE's clearing
\ of the saved APP-ENTRY before emitting the next product.
: RUN-IMAGE-DEFAULT ( [ n n -- n ] bool -- )
   ['] SOURCE-WRITER-DISPATCH RUN-IMAGE ;

;package
