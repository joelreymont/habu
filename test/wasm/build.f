\ build.f - tools/wasm-build.f run by its command line, each module it writes
\ validated by wasm-tools and run by bun (test/wasm/harness.f). Both tools must
\ be on PATH, so this is no row of the ordinary gate: test/wasm/device.f runs
\ it, as does `bin/hb --load test/wasm/build.f` from the tree's root.
\
\ `: MAIN ( -- ) 2 3 + . ;` builds into a module that validates and prints 5
\ and a newline. A command line without the output path is refused with 64 and
\ the usage line; an entry that takes a cell, and an output path in a missing
\ directory, with 74 and WASMLINK's sentence.
\
\ A code cell holding execute's xt, or catch's, holds its runtime function's
\ slot: a word run through the cell prints what the engine prints, 31 through
\ execute and the code -37 through catch.
\
\ The checked-memory rows test/wasm/select.f proves on their selection run
\ here: a cell at an upper-bit pointer and a byte in the null region fail their
\ check and trap, the last byte of the module's memory reads, and a cell whose
\ last byte is one past that end passes the check and traps in the engine's
\ bounds check. The memory ends where the module's memory section says, at the
\ pages WLINK sizes to the image. The upper-bit row's module gives that end;
\ the last two rows' sources differ from its source only in code, so their
\ windows hold the same data and their memories end there too.
\
\ The sources and modules stay in the printed directory.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
require src/arch/wasm/leb.f
require test/wasm/harness.f

package WASM-BUILD-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable ERR-U
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: SRC
variable SRC-U
PATH-CAP BUFFER: MOD
variable MOD-U
DYNAMIC-BUFFER MODULE u8

: SETUP ( -- )
   s" wasm-build" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm build: sources and modules in " type  DIR u type cr ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ bin/hb --load tools/wasm-build.f -- and the arguments staged after it;
\ answers its exit status.
: DRIVER ( -- n )
   s" bin/hb" >LEN  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   erru LEN>N ERR-U !
   rc ;

: COMMAND ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+ ;

\ text written as the file name in DIR, the source the next build reads.
: SOURCE! ( ptr u8 n ptr u8 n -- )
   {: text:ptr tu:n name:ptr nu:n :}
   DIR DIR-U @ name nu SRC FS:JOIN-PATH SRC-U !
   SRC SRC-U @ text tu FS:WRITE-ALL ;

\ The source built into the module file name in DIR, whose run calls entry;
\ answers the driver's status.
: BUILD ( ptr u8 n ptr u8 n -- n )
   {: e:ptr eu:n name:ptr nu:n :}
   DIR DIR-U @ name nu MOD FS:JOIN-PATH MOD-U !
   COMMAND  SRC SRC-U @ ARG+  e eu ARG+  MOD MOD-U @ ARG+
   DRIVER ;

\ The module just built validates; answers its run's status.
: RAN ( -- n )
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ WASM-HARNESS:RUN ;

: MAIN-CASE ( -- )
   s" : MAIN ( -- ) 2 3 + . ;" s" main.f" SOURCE!
   s" `: MAIN ( -- ) 2 3 + . ;` builds into a module that validates and runs to status 0" T-LABEL
   s" MAIN" s" main.wasm" BUILD 0 T=
   RAN 0 T=
   s" it prints 5 and a newline" T-LABEL
   WASM-HARNESS:OUT$ S\" 5\n" T$= ;

: UNUSED-CASE ( -- )
   s" : DEAD ( -- ) here drop ; : MAIN ( -- ) 7 . ;" s" unused.f" SOURCE!
   s" an unreachable definition with an unsupported call is omitted" T-LABEL
   s" MAIN" s" unused.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 7\n" T$= ;

: FATAL-CASE ( -- )
   S\" : FATAL ( -- ) s\" fatal\" 77 die ; : MAIN ( -- ) ['] FATAL catch . ;"
   s" fatal.f" SOURCE!
   s" die prints its message and traps through catch, with no continuation" T-LABEL
   s" MAIN" s" fatal.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ s" fatal" T$= ;

: NEGATIVE-BYTE-CASE ( -- )
   S\" 8 BUFFER: B : BAD ( -- ) B B -1 BYTE-COPY ; : MAIN ( -- ) ['] BAD catch . ; MAIN"
   s" negative-native.f" SOURCE!
   s" native BYTE-COPY dies through catch on a negative length" T-LABEL
   PROC-ARGV-RESET  s" --load" ARG+  SRC SRC-U @ ARG+
   DRIVER 76 T=
   ERR ERR-U @ RTRIM s" byte: negative length" T$=
   S\" 8 BUFFER: B : BAD ( -- ) B B -1 BYTE-COPY ; : MAIN ( -- ) ['] BAD catch . ;"
   s" negative-wasm.f" SOURCE!
   s" Wasm BYTE-COPY prints the same diagnostic and traps through catch" T-LABEL
   s" MAIN" s" negative.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ s" byte: negative length" T$= ;

: USAGE-CASE ( -- )
   s" a command line without the output path is refused with 64 and the usage line" T-LABEL
   COMMAND  SRC SRC-U @ ARG+  s" MAIN" ARG+
   DRIVER 64 T=
   ERR ERR-U @ RTRIM
   s" usage: bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>" T$= ;

: REFUSAL-CASES ( -- )
   s" : MAIN ( -- ) 1 ;" s" unchecked.f" SOURCE!
   s" authored source is still checked after the prefix rewind" T-LABEL
   s" MAIN" s" unchecked.wasm" BUILD 70 T=
   ERR ERR-U @ s" hook: non-certified definition: main" CONTAINS? TTRUE
   s" require src/lib/span.f 8 SPAN-BUFFER: B : MAIN ( -- ) B SPAN:LEN . ;"
   s" span-buffer.f" SOURCE!
   s" a reached static byte-span producer is refused without a Wasm routine" T-LABEL
   s" MAIN" s" span-buffer.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: a code record the capture's shadow carries no routine for" T$=
   s" require src/lib/span.f : MAIN ( -- ) 8 SPAN-BUFFER: ;"
   s" reached-definer.f" SOURCE!
   s" a runtime-reached does> definer is refused before writing a module" T-LABEL
   s" MAIN" s" reached-definer.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: a name WKERNEL's map does not answer" T$=
   MOD MOD-U @ FS:EXISTS? TFALSE
   s" package ADV public : MAKER ( -- ) create 0 , does> ( -- ) drop ; ;package"
   s" companion-entry.f" SOURCE!
   s" a direct does> companion entry is refused before writing a module" T-LABEL
   s" ADV:MAKER;does" s" companion-entry.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: runtime-reached does> companion has no Wasm routine" T$=
   MOD MOD-U @ FS:EXISTS? TFALSE
   S\" package ADV\nprivate\nvariable ACTION\npublic\n: MAKER ( -- ) create 0 , does> ( -- ) drop ;\n: MAIN ( -- ) ;\n' MAKER;does ACTION xt!\n;package\n"
   s" companion-cell.f" SOURCE!
   s" a code cell reaching a does> companion is refused before output" T-LABEL
   s" ADV:MAIN" s" companion-cell.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: runtime-reached does> companion has no Wasm routine" T$=
   MOD MOD-U @ FS:EXISTS? TFALSE
   s" require src/lib/span.f 2 SPAN-CELLS: C : MAIN ( -- ) C SPAN:CELL-LEN . ;"
   s" span-cells.f" SOURCE!
   s" a reached static cell-span producer is refused without a Wasm routine" T-LABEL
   s" MAIN" s" span-cells.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: a code record the capture's shadow carries no routine for" T$=
   s" package NUM ;package : MAIN ( -- ) ;" s" sealed.f" SOURCE!
   s" authored source cannot reopen the numeric package" T-LABEL
   s" MAIN" s" sealed.wasm" BUILD ENGINE-ERROR:SEAL-PACKAGE T=
   s" : W ( n -- ) drop ;" s" cells.f" SOURCE!
   s" an entry that takes a cell is refused with 74 and WASMLINK's sentence" T-LABEL
   s" W" s" cells.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM
   s" wasmlink: an entry that takes or leaves cells, which run cannot call" T$=
   s" : MAIN ( -- ) ;" s" unwritten.f" SOURCE!
   s" an output path in a missing directory is refused with 74 and WASMLINK's sentence" T-LABEL
   s" MAIN" s" missing/unwritten.wasm" BUILD 74 T=
   ERR ERR-U @ RTRIM  s" wasmlink: an output path that cannot be written" T$= ;

\ ---- execute and catch in a code cell -----------------------------------------
\ These primitives have no stored effect. The getter states this fixture's
\ calling convention while returning the primitive's own entry for the table.
: EXECUTE-CELL! ( -- )
   SB-RESET
   S\" package PROBE\nprivate\n: CODE-CELL: ( -- ) create 0 , does> ( -- ptr [ [ -- ] -- ] ) ;\n" SB-APPEND
   S\" CODE-CELL: ACTION\nTRUSTED: ACTION-XT ( -- [ [ -- ] -- ] ) ['] execute ;\npublic\n: TARGET ( -- ) 31 . ;\n" SB-APPEND
   S\" : MAIN ( -- ) ['] TARGET ACTION @ execute ;\nACTION-XT ACTION xt!\n;package\n" SB-APPEND
   SB$ s" execute-cell.f" SOURCE! ;

: CATCH-CELL! ( -- )
   SB-RESET
   S\" package PROBE\nprivate\n: CODE-CELL: ( -- ) create 0 , does> ( -- ptr [ [ -- ] -- n ] ) ;\n" SB-APPEND
   S\" CODE-CELL: ACTION\nTRUSTED: ACTION-XT ( -- [ [ -- ] -- n ] ) ['] catch ;\npublic\n: TARGET ( -- ) -37 throw ;\n" SB-APPEND
   S\" : MAIN ( -- ) ['] TARGET ACTION @ execute . ;\nACTION-XT ACTION xt!\n;package\n" SB-APPEND
   SB$ s" catch-cell.f" SOURCE! ;

: CELL-CASES ( -- )
   EXECUTE-CELL!
   s" a word run through a code cell holding execute's xt prints 31, as natively" T-LABEL
   s" PROBE:MAIN" s" execute-cell.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 31\n" T$=
   CATCH-CELL!
   s" catch through a code cell holding catch's xt prints the code -37, as natively" T-LABEL
   s" PROBE:MAIN" s" catch-cell.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" -37\n" T$= ;

\ ---- checked memory ---------------------------------------------------------
65536 constant PAGE-BYTES
5 constant SEC-MEMORY
variable POS                         \ where the walk of the module reads next

\ The source of a row: WASM-MEMORY:ACCESS loads a cell at a, or a byte, and
\ drops it.
: ACCESS! ( n bool ptr u8 n -- )
   {: a:n cell:bool name:ptr nu:n :}
   SB-RESET
   S\" package WASM-MEMORY\nprivate\nCAST: >CELL ( n -- ptr n )\n" SB-APPEND
   S\" CAST: >BYTE ( n -- ptr u8 )\npublic\n: ACCESS ( -- ) " SB-APPEND
   a FMT:SB-U
   cell if s"  >CELL @" else s"  >BYTE c@" then SB-APPEND
   S\"  drop ;\n;package\n" SB-APPEND
   SB$ name nu SOURCE! ;

\ The row's module built into the file name, valid; answers its run's status.
: ACCESS ( ptr u8 n -- n )
   {: name:ptr nu:n :}
   s" WASM-MEMORY:ACCESS" name nu BUILD 0 T=
   RAN ;

\ The LEB at POS of the module m of u bytes, POS moved past it.
: U32> ( ptr u8 n -- n )
   {: m:ptr u:n :}
   m POS @ +  u POS @ -  WLEB:U32@ POS +! ;

\ The end of the memory of the module just built: the pages its memory section
\ states, read past the magic, the version and the sections before it.
: MEMORY-END ( -- n )
   MOD MOD-U @ FS:FILE-SIZE {: u:n :}
   u MODULE-RESERVE
   MOD MOD-U @ 0 MODULE u FS:READ-ALL u T=
   0 MODULE {: m:ptr :}
   8 POS !
   begin m POS @ + c@ SEC-MEMORY <> while
      1 POS +!  m u U32> POS +!
   repeat
   1 POS +!  m u U32> drop  m u U32> drop  1 POS +!
   m u U32> PAGE-BYTES * ;

: MEMORY-CASES ( -- )
   s" a cell at an upper-bit pointer, $100031000, fails its check and traps" T-LABEL
   $100031000 true s" upper.f" ACCESS!
   s" upper.wasm" ACCESS 2 T=
   MEMORY-END {: end:n :}
   s" a byte in the null region, $FFFF, fails its check and traps" T-LABEL
   $FFFF false s" null.f" ACCESS!
   s" null.wasm" ACCESS 2 T=
   s" the last byte of the module's memory reads" T-LABEL
   end 1- false s" last.f" ACCESS!
   s" last.wasm" ACCESS 0 T=
   s" a cell whose last byte is one past the memory's end traps in the engine" T-LABEL
   end 7 - true s" past.f" ACCESS!
   s" past.wasm" ACCESS 2 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   MAIN-CASE
   UNUSED-CASE
   FATAL-CASE
   NEGATIVE-BYTE-CASE
   USAGE-CASE
   REFUSAL-CASES
   CELL-CASES
   MEMORY-CASES
   T-REPORT ;

;package

WASM-BUILD-TEST:RUN
