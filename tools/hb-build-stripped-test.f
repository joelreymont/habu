\ hb-build-stripped-test.f - checked fixture for tools/hb-build-lib.f: what a
\ stripped image may own and reach (library state, claimed engine cells, fixed
\ quotation fields, a run-time pointer-cell mark that preserves data and stack)
\ and the refusal of a missing entry. tools/hb-build-test-lib.f lists the other
\ rows.
\ Run: bin/hb --load tools/hb-build-stripped-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require test/aot-builder-engine.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-LIB-SRC ( -- ptr u8 n )
   HBT-LIB-SRC-BUF HBT-LIB-SRC-U @ ;

: HBT-LIB-OUT ( -- ptr u8 n )
   HBT-LIB-OUT-BUF HBT-LIB-OUT-U @ ;

: HBT-LIB-DIR ( -- ptr u8 n )
   HBT-LIB-DIR-BUF HBT-LIB-DIR-U @ ;

: HBT-CELLS-SRC ( -- ptr u8 n )
   HBT-CELLS-SRC-BUF HBT-CELLS-SRC-U @ ;

: HBT-CELLS-OUT ( -- ptr u8 n )
   HBT-CELLS-OUT-BUF HBT-CELLS-OUT-U @ ;

: HBT-PMK-SRC ( -- ptr u8 n )
   HBT-PMK-SRC-BUF HBT-PMK-SRC-U @ ;

\ An application that touches a persistent cell of each library it requires
\ (src/lib/string.f's builder, src/lib/fs-mutate.f's COPY-BUF) and walks a tree
\ with FS:WALK-FILES, which maps its own context for the call: a maker that
\ loaded those libraries before opening the capture window would put the cells
\ below the span and refuse the image. The walked directory is a literal because
\ a stripped image reads no argv here. All seven generated Unicode range tables
\ are read: raw comma-built rows must not look like mapped pointers.
: HBT-LIB-SRC-HEAD$ ( -- ptr u8 n )
   S\" require src/lib/string.f\nrequire src/lib/fs.f\nrequire src/lib/fs-mutate.f\nrequire src/lib/unicode/class.f\n\npackage HBT-SLIB\nprivate\nvariable HITS\ncreate P1 PATH-CAP allot  variable P1U\ncreate P2 PATH-CAP allot  variable P2U\n: DIR$ ( -- ptr u8 n ) s\" " ;

: HBT-LIB-SRC-TAIL$ ( -- ptr u8 n )
   S\" \" ;\n: JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}\n   SB-RESET DIR$ SB-APPEND s\" /\" SB-APPEND name nameu SB-APPEND\n   SB$ {: a:ptr u:n :} a dst u BYTE-COPY u lenp ! ;\n: P1$ ( -- ptr u8 n ) P1 P1U @ ;\n: P2$ ( -- ptr u8 n ) P2 P2U @ ;\npublic\n: RUN ( -- )\n   SB-RESET s\" sb=\" SB-APPEND s\" ok\" SB-APPEND SB$ type cr\n   s\" a.txt\" P1 P1U JOIN!\n   s\" b.txt\" P2 P2U JOIN!\n   P1$ P2$ FS:COPY-FILE-STREAM\n   P2$ FS:FILE? if s\" copy=ok\" type cr then\n   0 HITS !\n   DIR$ [: 2drop HITS @ 1 + HITS ! ;] FS:WALK-FILES\n   HITS @ 3 = if s\" files=3\" type cr then\n   248 UNICODE-CLASS:LETTER? 705 UNICODE-CLASS:LETTER? and\n   706 UNICODE-CLASS:LETTER? 0= and\n   $1D7CE UNICODE-CLASS:NUMBER? and\n   $3000 UNICODE-CLASS:WHITE-SPACE? and\n   $2160 UNICODE-CLASS:ALPHABETIC? and\n   $5B0 UNICODE-CLASS:ALPHABETIC? and\n   $10400 UNICODE-CLASS:UPPERCASE? and\n   $24B6 UNICODE-CLASS:UPPERCASE? and\n   if s\" unicode=ok\" type cr then ;\n;package\n: MAIN ( -- ) HBT-SLIB:RUN ;\n" ;

\ Written in three pieces: the directory is as long as the scratch root makes
\ it, so the source goes to the file without passing through a fixed buffer.
: HBT-LIB-SRC! ( -- )
   HBT-LIB-SRC HBT-LIB-SRC-HEAD$ FS:WRITE-ALL
   HBT-LIB-SRC HBT-LIB-DIR FS:APPEND-FILE
   HBT-LIB-SRC HBT-LIB-SRC-TAIL$ FS:APPEND-FILE ;

: HBT-LIB-EXPECTED$ ( -- ptr u8 n )
   S\" sb=ok\ncopy=ok\nfiles=3\nunicode=ok\n" ;

\ The engine runtime cells a stripped image owns, in one program: the
\ environment (one variable set explicitly, one inherited, both through GETENV,
\ which reaches the environment through data-base's fixed ENVP cell, published
\ from the register by the stripped entry, aot-link-arm.f EMIT-OWNED-CELLS),
\ the kernel's argv, and a src/lib/memory.f WITH-BYTES scope, whose head is the
\ MEM-ABI band of the image's own fresh DATA region and needs no claim. Each
\ engine cell below the window is named in src/habu/aot-owned-cells.f; nothing
\ is admitted for being scratch. The image's own arguments start at argv[1]
\ because the stripped entry publishes its claim on APP-ENTRY:XT-CELL; numbered
\ lines make a dropped first argument show as a shifted index.
: HBT-CELLS-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require src/lib/memory.f\n: SHOW ( ptr u8 NUM:alloc-byte-len -- ) drop {: a:ptr :}\n" SB-APPEND
   S\"    $6F a c!  $6B a 1 + c!  a 2 type cr ;\n: MAIN ( -- )\n" SB-APPEND
   S\"    s\" HBT_EXPLICIT\" GETENV type cr\n   s\" HOME\" GETENV type cr\n" SB-APPEND
   S\"    SCRIPT-ARGC 0 ?do s\" arg\" type 48 i + emit s\" =\" type i SCRIPT-ARGV$ type cr loop\n" SB-APPEND
   S\"    4096 MEM:BYTES-ALLOC-LEN [: SHOW ;] MEM:WITH-BYTES ;\n" SB-APPEND
   SB$ ;

\ A PROGRAM THAT REGISTERS A POINTER CELL AT RUN TIME. The definer's own
\ `ptr-cell-mark` runs at DEFINITION time, on the build host; this program calls
\ it and scratch-cell-mark inside MAIN, after storing the address of its own data
\ in a persisted cell. The linker removes the compiler-only registration calls
\ while preserving their stack effect and the application pointer. The same image
\ carries test/stripped-quotation-field-subject.f's fixed quotation fields,
\ replaced by whole-record stores as it loads, and checks them first.
: HBT-PMK-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require test/stripped-quotation-field-subject.f\n" SB-APPEND
   S\" create PMK-OWN 111 c, 107 c,\nPERSISTED-PTR-VARIABLE PMK-CELL\nPTR-VARIABLE PMK-SCRATCH\n" SB-APPEND
   S\" : MAIN ( -- )\n   STRIPPED-QUOT-FIELD:CHECK\n" SB-APPEND
   S\"    PMK-OWN PMK-CELL !\n   PMK-CELL ptr-cell-mark\n   PMK-SCRATCH scratch-cell-mark\n" SB-APPEND
   S\"    depth 0 <> if s\" ptrmark: stack\" 74 die then\n   PMK-CELL @ 2 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-LIB-FILE! ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu body:ptr bodyu :}
   SB-RESET HBT-LIB-DIR SB-APPEND s" /" SB-APPEND name nameu SB-APPEND
   SB$ body bodyu FS:WRITE-ALL ;

\ The window invariant: nothing the application can require is loaded when the
\ capture window opens, so its own require closure is the only library content
\ in the restored span.
: HBT-STRIPPED-LIB-STATE ( -- )
   HBT-LIB-DIR FS:MAKE-DIR
   s" a.txt" s" one" HBT-LIB-FILE!
   s" c.txt" s" two" HBT-LIB-FILE!
   HBT-LIB-SRC!
   HBT-LIB-OUT HBT-REMOVE-FILE?
   HBT-LIB-SRC HBT-LIB-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   HBT-LIB-OUT FS:FILE? TTRUE
   HBT-LIB-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LIB-EXPECTED$ T$= ;

\ The expected output of HBT-CELLS-SRC$, BUILT AND NOT SPELLED: the second line is
\ this process's own HOME, which is exactly what PROC-ENV-INHERIT-MISSING hands
\ the child, so the assertion reads the inherited value back through the stripped
\ image rather than hard-coding a machine's.
: HBT-CELLS-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" explicit-ok" SB-APPEND 10 SB-APPEND-C
   s" HOME" GETENV SB-APPEND 10 SB-APPEND-C
   s" arg0=one" SB-APPEND 10 SB-APPEND-C
   s" arg1=two" SB-APPEND 10 SB-APPEND-C
   s" ok" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ ... and the same image started with NO arguments prints no argument line at
\ all: SCRIPT-ARGC is 0 and the loop body never runs.
: HBT-CELLS-NOARG-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" explicit-ok" SB-APPEND 10 SB-APPEND-C
   s" HOME" GETENV SB-APPEND 10 SB-APPEND-C
   s" ok" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ One variable set for the child and the rest of this process's environment
\ inherited - src/lib/process-env.f's inherited path, which is how every application
\ image is actually started.
: HBT-CELLS-CHILD-ENV ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HBT_EXPLICIT" >LEN s" explicit-ok" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-CELLS-CHILD-ARGV-ENV ( -- )
   HBT-CELLS-CHILD-ENV
   s" one" >LEN PROC-ARGV+
   s" two" >LEN PROC-ARGV+ ;

\ The image built above, started a second time with no arguments at all: the
\ empty vector is the boundary of the application convention, where ARGC is 1,
\ the start offset is 1 and SCRIPT-ARGC answers 0 from the subtraction itself -
\ no argument line is printed, and none is clamped away either.
: HBT-CELLS-NOARG-RUN ( -- )
   HBT-CELLS-CHILD-ENV
   HBT-CELLS-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CELLS-NOARG-EXPECTED$ T$= ;

\ The walk reaches engine words the build stripped through the payload's span
\ table.
: HBT-STRIPPED-ENGINE-CELLS ( -- )
   HBT-CELLS-SRC HBT-CELLS-SRC$ FS:WRITE-ALL
   HBT-CELLS-OUT HBT-REMOVE-FILE?
   HBT-CELLS-SRC HBT-CELLS-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   HBT-CELLS-OUT FS:FILE? TTRUE
   HBT-CELLS-CHILD-ARGV-ENV
   HBT-CELLS-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CELLS-EXPECTED$ T$=
   HBT-CELLS-NOARG-RUN ;

\ Private and public MAINs, no global one: tools/hb-build.f passes the link's
\ exit 74 and stderr through (aot-closure.f NO-ENTRY-DIE) and installs no image.
: HBT-STRIPPED-NO-ENTRY ( -- )
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC
   S\" package STRIPPED-ENTRY-NO-GLOBAL\nprivate\n: MAIN ( -- ) ;\npublic\n: MAIN ( -- ) ;\n;package\n"
   FS:WRITE-ALL
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 74 T=
   outu 0 T=
   HBT-ERR erru S\" aot: entry word not found: MAIN\naot: no entry\n" T$=
   HBT-AOT-OUT FS:FILE? TFALSE ;

\ The linker keeps the pointer store and replaces the stripped registrar call
\ with a stack pop. The image must read its restored bytes with balanced stack.
: HBT-STRIPPED-PTR-MARK ( -- )
   HBT-PMK-SRC HBT-PMK-SRC$ FS:WRITE-ALL
   HBT-PMK-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   HBT-AOT-OUT FS:FILE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn S\" quotation fields=ok\nok\n" T$= ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-MAIN ( -- )
   T-RESET
   AOT-BUILDER-ENGINE:ENSURE ENGINE-CANDIDATE:PATH$ HBT-KEYED!
   HBT-PREPARE
   HBT-STRIPPED-LIB-STATE
   HBT-STRIPPED-ENGINE-CELLS
   HBT-STRIPPED-NO-ENTRY
   HBT-STRIPPED-PTR-MARK
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-MAIN
