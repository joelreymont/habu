\ hb-build-stripped-test.f - checked fixture for tools/hb-build-lib.f: what a
\ stripped image may own and reach - library state and the claimed engine
\ cells - and what it is refused: an entry the link cannot find, as the CLI
\ reports it, an engine cell nothing claims and a run-time pointer-cell mark.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-test.f

require tools/hb-build-test-lib.f
require test/preloaded-engine.f

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

: HBT-UNOWNED-SRC ( -- ptr u8 n )
   HBT-UNOWNED-SRC-BUF HBT-UNOWNED-SRC-U @ ;

: HBT-PMK-SRC ( -- ptr u8 n )
   HBT-PMK-SRC-BUF HBT-PMK-SRC-U @ ;

\ An application that touches a PERSISTENT CELL of each library it requires:
\ lib/string.f's builder, lib/fs-mutate.f's copy buffer (FS-MUT-COPY-BUF) and
\ lib/fs.f's static walk context (FS-WALK-CTX0). Those cells are what the maker
\ used to own before the application was read - it required app-image.f, and so
\ lib/fs.f and lib/fs-mutate.f, before opening the capture window - which put them
\ below the span and refused the image. The walked directory is spliced in as a
\ literal because a stripped image reads no argv here.
: HBT-LIB-SRC-HEAD$ ( -- ptr u8 n )
   S\" require lib/string.f\nrequire lib/fs.f\nrequire lib/fs-mutate.f\n\npackage HBT-SLIB\nprivate\nvariable HITS\ncreate P1 FS-PATH-CAP allot  variable P1U\ncreate P2 FS-PATH-CAP allot  variable P2U\n: DIR$ ( -- ptr u8 n ) s\" " ;

: HBT-LIB-SRC-TAIL$ ( -- ptr u8 n )
   S\" \" ;\n: JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}\n   SB-RESET DIR$ SB-APPEND s\" /\" SB-APPEND name nameu SB-APPEND\n   SB$ {: a:ptr u:n :} a dst u BYTE-COPY u lenp ! ;\n: P1$ ( -- ptr u8 n ) P1 P1U @ ;\n: P2$ ( -- ptr u8 n ) P2 P2U @ ;\npublic\n: RUN ( -- )\n   SB-RESET s\" sb=\" SB-APPEND s\" ok\" SB-APPEND SB$ type cr\n   s\" a.txt\" P1 P1U JOIN!\n   s\" b.txt\" P2 P2U JOIN!\n   P1$ P2$ COPY-FILE-STREAM\n   P2$ FILE? if s\" copy=ok\" type cr then\n   0 HITS !\n   DIR$ [: 2drop HITS @ 1 + HITS ! ;] WALK-FILES\n   HITS @ 3 = if s\" files=3\" type cr then ;\n;package\n: MAIN ( -- ) HBT-SLIB:RUN ;\n" ;

\ Written in three pieces: the directory is as long as the scratch root makes
\ it, so the source goes to the file without passing through a fixed buffer.
: HBT-LIB-SRC! ( -- )
   HBT-LIB-SRC HBT-LIB-SRC-HEAD$ WRITE-ALL
   HBT-LIB-SRC HBT-LIB-DIR APPEND-FILE
   HBT-LIB-SRC HBT-LIB-SRC-TAIL$ APPEND-FILE ;

: HBT-LIB-EXPECTED$ ( -- ptr u8 n )
   S\" sb=ok\ncopy=ok\nfiles=3\n" ;

\ THE ENGINE RUNTIME CELLS A STRIPPED IMAGE OWNS, all in one program: the
\ environment (an explicitly set variable and an inherited one, both through
\ GETENV, whose ENV-QA/ENV-QU/ENV-DATA-PTR are baked engine cells below every
\ window), the kernel's argv, and lib/memory.f's WITH-BYTES scope over the baked
\ DYNAMIC-STORAGE registry. Each cell it reaches is named in
\ src/habu/aot-owned-cells.f, so the entry publishes or zeroes it by declaration
\ and the closure walker admits it; nothing here is admitted for being scratch.
\ The argv lines pin the APPLICATION convention: the image's own arguments start
\ at argv[1], every one of them, because the stripped entry publishes its claim
\ on APP-ENTRY:XT-CELL. The program prints them numbered, so a dropped first
\ argument (the defect while that cell read zero and SCRIPT-ARG-START took the
\ engine's source-list branch) shows up as a shifted index and not just a
\ different word.
: HBT-CELLS-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/memory.f\n: SHOW ( ptr u8 NUM:alloc-byte-len -- ) drop {: a:ptr :}\n" SB-APPEND
   S\"    $6F a c!  $6B a 1 + c!  a 2 type cr ;\n: MAIN ( -- )\n" SB-APPEND
   S\"    s\" HBT_EXPLICIT\" GETENV type cr\n   s\" HOME\" GETENV type cr\n" SB-APPEND
   S\"    SCRIPT-ARGC 0 ?do s\" arg\" type 48 i + emit s\" =\" type i SCRIPT-ARGV$ type cr loop\n" SB-APPEND
   S\"    4096 MEM:BYTES-ALLOC-LEN [: SHOW ;] MEM:WITH-BYTES ;\n" SB-APPEND
   SB$ ;

\ ... and an engine cell NOTHING claims is refused exactly as before. TMP-PATH is
\ src/os/env-base.f, the same baked file as the admitted environment cells and
\ just as transient, but its cursors are on no list - so the refusal is about the
\ declaration and not about the file, the value or the address.
: HBT-UNOWNED-SRC$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) s\" x\" TMP-PATH type cr ;\n" ;

\ A PROGRAM THAT REGISTERS A POINTER CELL AT RUN TIME. The definer's own
\ `ptr-cell-mark` runs at DEFINITION time, on the build host; this program calls
\ the primitive itself inside MAIN, after storing the address of its own data in
\ a persisted cell - the one shape in which a stripped image would have to carry
\ the registrar.
: HBT-PMK-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" create PMK-OWN 111 c, 107 c,\nPERSISTED-PTR-VARIABLE PMK-CELL\n" SB-APPEND
   S\" : MAIN ( -- )\n   PMK-OWN PMK-CELL !\n   PMK-CELL ptr-cell-mark\n" SB-APPEND
   S\"    PMK-CELL @ 2 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-LIB-FILE! ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu body:ptr bodyu :}
   SB-RESET HBT-LIB-DIR SB-APPEND s" /" SB-APPEND name nameu SB-APPEND
   SB$ body bodyu WRITE-ALL ;

\ THE WINDOW INVARIANT, end to end: nothing the application can require is loaded
\ when the capture window opens, so the application's own require closure is the
\ only library content inside the restored span. Measured before this held:
\ `caller=WALK-FILES target=FS-DEPTH` and `caller=COPY-FILE-STREAM
\ target=FS-MUT-COPY-IN` refused this very program - the copy descriptors are
\ locals of the call now, and the copy BUFFER is the cell that stands there.
\ That order is the claim, so this build compiles the linker above the program.
: HBT-STRIPPED-LIB-STATE ( -- )
   HBT-LIB-DIR MAKE-DIR
   s" a.txt" s" one" HBT-LIB-FILE!
   s" c.txt" s" two" HBT-LIB-FILE!
   HBT-LIB-SRC!
   HBT-LIB-OUT HBT-REMOVE-FILE?
   HBT-LIB-SRC HBT-LIB-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   HBT-LIB-OUT FILE? TTRUE
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
\ inherited - lib/process-env.f's inherited path, which is how every application
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

\ ... and the engine runtime cells the stripped entry OWNS are readable in the
\ image: the environment (explicit and inherited), argv, and an allocation
\ through the baked dynamic-storage registry. Before src/habu/aot-owned-cells.f
\ this very program was refused with
\ `outside the restored span caller=GETENV target=ENV-QU`. The walk reaches
\ engine words the build stripped through the payload's span table, which the
\ keyed linker image this build runs on reads at the address its own boot
\ published (src/habu/habu2.f EM-SNAPSHOT-RESTORE).
: HBT-STRIPPED-ENGINE-CELLS ( -- )
   HBT-CELLS-SRC HBT-CELLS-SRC$ WRITE-ALL
   HBT-CELLS-OUT HBT-REMOVE-FILE?
   HBT-CELLS-SRC HBT-CELLS-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-CELLS-OUT FILE? TTRUE
   HBT-CELLS-CHILD-ARGV-ENV
   HBT-CELLS-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CELLS-EXPECTED$ T$=
   HBT-CELLS-NOARG-RUN ;

\ A stripped build's refusal as the CLI reports it. The link refuses an entry
\ it cannot find with exit 74 and two lines on stderr, `aot: entry word not
\ found: NAME` and its die message `aot: no entry` (src/habu/aot-closure.f
\ NO-ENTRY-DIE); tools/hb-build.f exits with the maker's code, carries that
\ stderr through byte for byte, prints nothing on stdout and installs no image,
\ so this case fails if the tool swallows, rewrites, adds to or drops any of
\ the maker's exit or diagnostic. tools/hb-build-cli-errors-test.f
\ HBT-REFUSE-MAIN-CLI is the REPL path's. The refusals below ask the maker
\ directly (HBT-RUN-MAKER).
: HBT-STRIPPED-NO-ENTRY ( -- )
   HBT-REMOVE-AOT-OUT
   HBT-ARGV-BASE
   s" --preseed-entry" >LEN PROC-ARGV+
   s" HBT-NO-ENTRY" >LEN PROC-ARGV+
   s" --preseed-seed" >LEN PROC-ARGV+
   s" 000000000000002a" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 74 T=
   outu 0 T=
   HBT-ERR erru S\" aot: entry word not found: HBT-NO-ENTRY\naot: no entry\n" T$=
   HBT-AOT-OUT FILE? TFALSE ;

\ ... while an engine cell on no list is still refused, with its own diagnostic.
: HBT-STRIPPED-UNOWNED-CELL ( -- )
   HBT-UNOWNED-SRC HBT-UNOWNED-SRC$ WRITE-ALL
   HBT-UNOWNED-SRC HBT-RUN-MAKER {: nout:n nerr:n nrc:n :}
   nrc 0 <> TTRUE
   HBB-ERR-BUF nerr s" outside the restored span" CONTAINS? TTRUE
   HBB-ERR-BUF nerr s" caller=TMP-PATH" CONTAINS? TTRUE
   HBB-ERR-BUF nerr s" target=TPU" CONTAINS? TTRUE ;

\ ... and the relocator's own arm refuses by name too. `ptr-cell-mark` has a body
\ of its own (a deref-form prim, src/habu/habu2.f BPTRCELLMARK) holding a BL to
\ LPTRMARK, an entry INSIDE the (MARK) body and not that record's code ENTRY - so
\ FINDADDR-PTR resolves nothing, DECLARATION-TARGET? is false, and MAP-TARGET!
\ (src/habu/aot-lib.f) dies naming the primitive as the site and, through
\ ADDRESS-OWNER's recorded span, (MARK) as the body the target lands in. Only a
\ branch to the (MARK) ENTRY - what `xt!` compiles - is dropped as a declaration.
: HBT-STRIPPED-PTR-MARK ( -- )
   HBT-PMK-SRC HBT-PMK-SRC$ WRITE-ALL
   HBT-PMK-SRC HBT-RUN-MAKER {: mout:n merr:n mrc:n :}
   mrc 0 <> TTRUE
   HBB-ERR-BUF merr s" PC-relative target removed or outside closure" CONTAINS? TTRUE
   HBB-ERR-BUF merr s" site=ptr-cell-mark" CONTAINS? TTRUE
   HBB-ERR-BUF merr s" target-word=(MARK)" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-MAIN ( -- )
   T-RESET
   PRELOADED-ENGINE:LINKER$ APP-IMAGE-ENGINE:PATH$ HBT-KEYED!
   HBT-PREPARE
   HBT-STRIPPED-LIB-STATE
   HBT-STRIPPED-ENGINE-CELLS
   HBT-STRIPPED-NO-ENTRY
   HBT-STRIPPED-UNOWNED-CELL
   HBT-STRIPPED-PTR-MARK
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-MAIN
