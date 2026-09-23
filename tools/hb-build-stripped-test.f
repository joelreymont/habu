\ hb-build-stripped-test.f - checked fixture for tools/hb-build-lib.f: what a
\ stripped image may own and reach - library state, the claimed engine cells,
\ the closure chain, the open path, the lifecycle registry and number parsing.
\ The carried DATA cells are tools/hb-build-stripped-cells-test.f and the
\ build, CLI and AOT groups are tools/hb-build-test.f; each is a gate row of
\ its own, because one row running all three took 353-355 s of the gate's
\ 360 s child timeout.
\ Run: bin/hb --load tools/hb-build-stripped-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this half reopens it the way tools/hb-build-test-lib.f does.
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

: HBT-UNOWNED-OUT ( -- ptr u8 n )
   HBT-UNOWNED-OUT-BUF HBT-UNOWNED-OUT-U @ ;

: HBT-PMK-SRC ( -- ptr u8 n )
   HBT-PMK-SRC-BUF HBT-PMK-SRC-U @ ;

: HBT-PMK-OUT ( -- ptr u8 n )
   HBT-PMK-OUT-BUF HBT-PMK-OUT-U @ ;

: HBT-PPH-SRC ( -- ptr u8 n )
   HBT-PPH-SRC-BUF HBT-PPH-SRC-U @ ;

: HBT-PPH-OUT ( -- ptr u8 n )
   HBT-PPH-OUT-BUF HBT-PPH-OUT-U @ ;

: HBT-CHAIN-SRC ( -- ptr u8 n )
   HBT-CHAIN-SRC-BUF HBT-CHAIN-SRC-U @ ;

: HBT-CHAIN-OUT ( -- ptr u8 n )
   HBT-CHAIN-OUT-BUF HBT-CHAIN-OUT-U @ ;

: HBT-OPENP-SRC ( -- ptr u8 n )
   HBT-OPENP-SRC-BUF HBT-OPENP-SRC-U @ ;

: HBT-OPENP-OUT ( -- ptr u8 n )
   HBT-OPENP-OUT-BUF HBT-OPENP-OUT-U @ ;

: HBT-LIFE-SRC ( -- ptr u8 n )
   HBT-LIFE-SRC-BUF HBT-LIFE-SRC-U @ ;

: HBT-LIFE-OUT ( -- ptr u8 n )
   HBT-LIFE-OUT-BUF HBT-LIFE-OUT-U @ ;

: HBT-HOOK-SRC ( -- ptr u8 n )
   HBT-HOOK-SRC-BUF HBT-HOOK-SRC-U @ ;

: HBT-HOOK-OUT ( -- ptr u8 n )
   HBT-HOOK-OUT-BUF HBT-HOOK-OUT-U @ ;

: HBT-NUMP-SRC ( -- ptr u8 n )
   HBT-NUMP-SRC-BUF HBT-NUMP-SRC-U @ ;

: HBT-NUMP-OUT ( -- ptr u8 n )
   HBT-NUMP-OUT-BUF HBT-NUMP-OUT-U @ ;

\ An application that touches a PERSISTENT CELL of each library it requires:
\ lib/string.f's builder, lib/fs-mutate.f's copy buffer (FS-MUT-COPY-BUF) and
\ lib/fs.f's walk stacks (FS-DEPTH, FS-WALK-BUF). Those cells are what the maker
\ used to own before the application was read - it required app-image.f, and so
\ lib/fs.f and lib/fs-mutate.f, before opening the capture window - which put them
\ below the span and refused the image. The walked directory is spliced in as a
\ literal because a stripped image reads no argv here.
: HBT-LIB-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/string.f\nrequire lib/fs.f\nrequire lib/fs-mutate.f\n\npackage HBT-SLIB\nprivate\nvariable HITS\ncreate P1 FS-PATH-CAP allot  variable P1U\ncreate P2 FS-PATH-CAP allot  variable P2U\n: DIR$ ( -- ptr u8 n ) s\" " SB-APPEND
   HBT-LIB-DIR SB-APPEND
   S\" \" ;\n: JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}\n   SB-RESET DIR$ SB-APPEND s\" /\" SB-APPEND name nameu SB-APPEND\n   SB$ {: a:ptr u:n :} a dst u BYTE-COPY u lenp ! ;\n: P1$ ( -- ptr u8 n ) P1 P1U @ ;\n: P2$ ( -- ptr u8 n ) P2 P2U @ ;\npublic\n: RUN ( -- )\n   SB-RESET s\" sb=\" SB-APPEND s\" ok\" SB-APPEND SB$ type cr\n   s\" a.txt\" P1 P1U JOIN!\n   s\" b.txt\" P2 P2U JOIN!\n   P1$ P2$ COPY-FILE-STREAM\n   P2$ FILE? if s\" copy=ok\" type cr then\n   0 HITS !\n   DIR$ [: 2drop HITS @ 1 + HITS ! ;] WALK-FILES\n   HITS @ 3 = if s\" files=3\" type cr then ;\n;package\n: MAIN ( -- ) HBT-SLIB:RUN ;\n" SB-APPEND
   SB$ ;

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

\ THE THREE BAKED CONSTANTS AN ORDINARY PROGRAM READS, in one program: printing
\ an integer reaches lib/fmt.f INT>NUM and its copy of STR-MIN-I64$, parsing one
\ reaches STR-PARSE-POS and STR-MAX-I64$, hashing reaches SHA-256's KK and HH0
\ and every scratch cell in src/core/sha256.f. All of those live in baked files,
\ below every capture window, and each one of them refused this program before
\ src/habu/aot-owned-cells.f named them (measured: caller=INT>NUM
\ target=STR-MIN-I64$, caller=STR-PARSE-POS target=STR-MAX-I64$, caller=SHA256
\ target=SHA-U). The digest is SHA-256 of "abc" from FIPS-180, so the pinned
\ stdout below proves the CARRIED bytes arrived, not merely that the image ran:
\ a zeroed KK or HH0 answers a different digest, and a zeroed bound table parses
\ nothing.
: HBT-PPH-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/fmt.f\ncreate PPH-DIG $20 allot\ncreate PPH-HEX $40 allot\n" SB-APPEND
   S\" : MAIN ( -- )\n   42 FMT:.INT cr\n   s\" 123\" STR-PARSE-POS MATCH option\n" SB-APPEND
   S\"      none OF s\" none\" type cr ENDOF\n     some OF FMT:.INT cr ENDOF\n   ;MATCH\n" SB-APPEND
   S\"    s\" abc\" PPH-DIG SHA256\n   PPH-DIG PPH-HEX SHA256>HEX\n   PPH-HEX $40 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-PPH-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 42" SB-APPEND 10 SB-APPEND-C
   s" 123" SB-APPEND 10 SB-APPEND-C
   s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ OPENING A FILE BY PATH, the smallest program that reaches the engine's path
\ scratch: PATH0 (src/core/util.f) zero-terminates the path into PZB and hands
\ `open` that buffer, so PATH0's compiled code spells out an address below every
\ window. Reported from Tender, where it refused every entry point before
\ anything else - `caller=PATH0 target=PZB` - and answered by the FRESH-BYTES
\ claim in src/habu/aot-owned-cells.f. The printed line runs AFTER the close, so
\ the pinned stdout proves the image opened and closed the file rather than
\ merely exiting zero.
: HBT-OPENP-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" variable FD\n: MAIN ( -- )\n   s\" /dev/null\" PATH0 0 0 open FD !\n" SB-APPEND
   S\"    FD @ 0 < IF s\" openpath: cannot open /dev/null\" 74 die THEN\n" SB-APPEND
   S\"    FD @ close\n   s\" opened\" type cr ;\n" SB-APPEND
   SB$ ;

: HBT-OPENP-EXPECTED$ ( -- ptr u8 n )
   S\" opened\n" ;

\ READING THE IMAGE-LIFECYCLE REGISTRY, the second site a stripped image is
\ refused at and the smallest program that reaches it: IMAGE-LIFECYCLE:COUNT
\ takes the registry's private lock and reads both hook counters, so its
\ compiled code spells three baked cells below every window. Before the fresh
\ claims in src/habu/aot-owned-cells.f this program - and every image whose
\ libraries register a cleanup hook on first use, which is how Tender's server
\ and scraper reached it - was refused at `value=13964713400` with neither name
\ (the refusal now reads `caller=STORE+748 target=COUNT+8`, the neighbour form).
\ The printed count is the proof the claims are right and not merely quiet: a
\ new process has registered nothing, and the lock the count is read under has
\ to be free for the image to print at all.
: HBT-LIFE-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/image-lifecycle.f\n: MAIN ( -- )\n" SB-APPEND
   S\"    IMAGE-LIFECYCLE:COUNT 0 = if s\" hooks=0\" type cr then ;\n" SB-APPEND
   SB$ ;

: HBT-LIFE-EXPECTED$ ( -- ptr u8 n )
   S\" hooks=0\n" ;

\ ... and REGISTERING one, which is the store the read above only counted.
\ IMAGE-LIFECYCLE:REGISTER appends a quotation to the HOOKS buffer and
\ REGISTER-PERSISTENT to the PERSISTENT table; both are quotation stores into a
\ declared cell, which the optimizing tier lowers through QUOTATION-STORAGE:STORE
\ and so through `xt!`. `xt!` stores the token and then calls the engine's
\ address-cell registrar, and that call is what refused this program: with the
\ two table bases unclaimed at `caller=STORE+424 target=DICT+56`, and with them
\ claimed at `aot: PC-relative target removed or outside closure site=xt!`. A
\ stripped image has no reader for the address-cell table, so the linker drops
\ the declaration and keeps the store (src/habu/aot-closure.f AOT-DECLARATION?).
\ PROC-ARGV-BUF is a REAL first-use registrant - lib/process-argv.f registers its
\ RELEASE hook the first time the argv buffer is taken - so the program reaches
\ the store the way Tender's server and scraper do, and not only through its own
\ two calls.
\ THE PINNED ORDER IS WHAT PREPARE PRODUCES: the HOOKS buffer from the last
\ registration down to the first (`hook=b` before `hook=a`, with the silent
\ RELEASE hook ahead of both), then the PERSISTENT table the same way, because
\ reverse order releases dependents before what they depend on. `count=4` is
\ three of the image's own registrations plus that RELEASE hook, out of a
\ registry that started empty. Nothing else runs PREPARE in a stripped image -
\ the entry is `bl MAIN; exit(0)` - so MAIN calls it, after printing `exit`.
: HBT-HOOK-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/image-lifecycle.f\nrequire lib/process-argv.f\n" SB-APPEND
   S\" require lib/fmt.f\n: HOOK-A ( -- ) s\" hook=a\" type cr ;\n" SB-APPEND
   S\" : HOOK-B ( -- ) s\" hook=b\" type cr ;\n" SB-APPEND
   S\" : HOOK-P ( -- ) s\" hook=p\" type cr ;\n: MAIN ( -- )\n" SB-APPEND
   S\"    [: HOOK-A ;] IMAGE-LIFECYCLE:REGISTER\n" SB-APPEND
   S\"    [: HOOK-B ;] IMAGE-LIFECYCLE:REGISTER\n" SB-APPEND
   S\"    [: HOOK-P ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT\n" SB-APPEND
   S\"    PROC-ARGV-BUF drop\n" SB-APPEND
   S\"    s\" count=\" type IMAGE-LIFECYCLE:COUNT FMT:.INT cr\n" SB-APPEND
   S\"    s\" exit\" type cr\n   IMAGE-LIFECYCLE:PREPARE ;\n" SB-APPEND
   SB$ ;

: HBT-HOOK-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" count=4" SB-APPEND 10 SB-APPEND-C
   s" exit" SB-APPEND 10 SB-APPEND-C
   s" hook=b" SB-APPEND 10 SB-APPEND-C
   s" hook=a" SB-APPEND 10 SB-APPEND-C
   s" hook=p" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ PARSING A NUMBER AT RUN TIME. `num-parse` is `bl LNUM` (src/habu/habu1.f
\ BNUMPARSE) and LNUM is the engine's own number reader, which had no
\ dictionary record: the closure walk follows a direct branch only to a
\ record's exact entry, so this program was refused
\ `aot: PC-relative target removed or outside closure site=num-parse
\ target=4293656 target-word=<unknown>` (exit 74, measured on engine
\ ec37691e, the refusal Tender's stripped server stopped at). The reader is
\ now the sealed (NUM) engine helper and is CARRIED, not dropped like (MARK):
\ its body branches only within itself and touches only the caller's bytes,
\ so the image gets the 480-byte record and parses at run time.
\ THE THREE ANSWERS ARE THE READER'S OWN: `42` is the value with the float
\ flag clear, `1.5` sets it, and `12a` - a spelling the reader refuses - is
\ the pair of ANDs in BNUMPARSE answering zero with both flags false, which
\ is why the last line is `0` and not `12`.
: HBT-NUMP-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/fmt.f\n: MAIN ( -- )\n" SB-APPEND
   S\"    s\" 42\" num-parse {: v:n flt:bool ok:bool :}\n" SB-APPEND
   S\"    ok if v FMT:.INT cr then\n" SB-APPEND
   S\"    flt if s\" float\" type cr else s\" int\" type cr then\n" SB-APPEND
   S\"    s\" 1.5\" num-parse {: v2:n f2:bool ok2:bool :}\n" SB-APPEND
   S\"    f2 if s\" float\" type cr else s\" int\" type cr then\n" SB-APPEND
   S\"    s\" 12a\" num-parse {: v3:n f3:bool ok3:bool :}\n" SB-APPEND
   S\"    ok3 if s\" num\" type cr else v3 FMT:.INT cr then ;\n" SB-APPEND
   SB$ ;

: HBT-NUMP-EXPECTED$ ( -- ptr u8 n )
   S\" 42\nint\nfloat\n0\n" ;

\ A PROGRAM WHOSE CLOSURE IS ITS OWN SIZE. HBT-CHAIN-N words, each calling the
\ next and reaching no library, are exactly HBT-CHAIN-N + 1 closure members, so
\ this fixture measures the table sizing and nothing else. 1100 is past the 1024
\ rows the tables were once cut to by a constant - the wall that refused every
\ Tender entry point (dot habu-size-the-stripped-b2932715) and, measured on the
\ engine before the sizing landed, this very chain at `last_added_word='CW76'`.
\ The image prints the chain's value rather than merely exiting, so a closure
\ that built but lost a member is a wrong number and not a silent pass. The
\ source is generated here because 1101 definitions is 50 KB of fixture; the
\ builder holds one line at a time (SB-CAP is 1 KB).
1100 constant HBT-CHAIN-N
: HBT-SB-U+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + SB-APPEND-C ;

: HBT-CHAIN-WORD! ( n -- ) {: i:n :}
   SB-RESET
   s" : CW" SB-APPEND i HBT-SB-U+ s"  ( n -- n ) " SB-APPEND
   i 0 > if s" CW" SB-APPEND i 1- HBT-SB-U+ s"  " SB-APPEND then
   s" dup 0 < if drop 0 then 1 + ;" SB-APPEND 10 SB-APPEND-C
   HBT-CHAIN-SRC SB$ APPEND-FILE ;

: HBT-CHAIN-SRC! ( -- )
   HBT-CHAIN-SRC s" " WRITE-ALL
   HBT-CHAIN-N 0 ?do i HBT-CHAIN-WORD! loop
   SB-RESET
   s" : MAIN ( -- ) 0 CW" SB-APPEND HBT-CHAIN-N 1- HBT-SB-U+
   s"  " SB-APPEND HBT-CHAIN-N HBT-SB-U+
   s\"  = if s\" chain=ok\" type cr then ;" SB-APPEND 10 SB-APPEND-C
   HBT-CHAIN-SRC SB$ APPEND-FILE ;

: HBT-CHAIN-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" chain=ok" SB-APPEND 10 SB-APPEND-C
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
: HBT-STRIPPED-LIB-STATE ( -- )
   HBT-LIB-DIR MAKE-DIR
   s" a.txt" s" one" HBT-LIB-FILE!
   s" c.txt" s" two" HBT-LIB-FILE!
   HBT-LIB-SRC HBT-LIB-SRC$ WRITE-ALL
   HBT-LIB-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LIB-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LIB-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
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
\ `outside the restored span caller=GETENV target=ENV-QU`.
: HBT-STRIPPED-ENGINE-CELLS ( -- )
   HBT-CELLS-SRC HBT-CELLS-SRC$ WRITE-ALL
   HBT-CELLS-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-CELLS-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-CELLS-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-CELLS-OUT FILE? TTRUE
   HBT-CELLS-CHILD-ARGV-ENV
   HBT-CELLS-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CELLS-EXPECTED$ T$=
   HBT-CELLS-NOARG-RUN ;

\ ... while an engine cell on no list is still refused, with its own diagnostic.
: HBT-STRIPPED-UNOWNED-CELL ( -- )
   HBT-UNOWNED-SRC HBT-UNOWNED-SRC$ WRITE-ALL
   HBT-UNOWNED-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-UNOWNED-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-UNOWNED-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: nout:n nerr:n nrc:n :}
   nrc 0 <> TTRUE
   HBT-ERR nerr s" outside the restored span" CONTAINS? TTRUE
   HBT-ERR nerr s" caller=TMP-PATH" CONTAINS? TTRUE
   HBT-ERR nerr s" target=TPU" CONTAINS? TTRUE
   HBT-UNOWNED-OUT FILE? TFALSE ;

\ ... and the relocator's own arm refuses by name too. `ptr-cell-mark` has a body
\ of its own (a deref-form prim, src/habu/habu2.f BPTRCELLMARK) holding a BL to
\ LPTRMARK, an entry INSIDE the (MARK) body and not that record's code ENTRY - so
\ FINDADDR-PTR resolves nothing, DECLARATION-TARGET? is false, and MAP-TARGET!
\ (src/habu/aot-lib.f) dies naming the primitive as the site and, through
\ ADDRESS-OWNER's recorded span, (MARK) as the body the target lands in. Only a
\ branch to the (MARK) ENTRY - what `xt!` compiles - is dropped as a declaration.
: HBT-STRIPPED-PTR-MARK ( -- )
   HBT-PMK-SRC HBT-PMK-SRC$ WRITE-ALL
   HBT-PMK-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-PMK-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-PMK-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: mout:n merr:n mrc:n :}
   mrc 0 <> TTRUE
   HBT-ERR merr s" PC-relative target removed or outside closure" CONTAINS? TTRUE
   HBT-ERR merr s" site=ptr-cell-mark" CONTAINS? TTRUE
   HBT-ERR merr s" target-word=(MARK)" CONTAINS? TTRUE
   HBT-PMK-OUT FILE? TFALSE ;

\ ... a program that PRINTS an integer, PARSES one and HASHES a string builds
\ stripped, because every baked constant those three reach is carried by name,
\ and the image's stdout is the proof that the bytes travelled.
: HBT-STRIPPED-PRINT-PARSE-HASH ( -- )
   HBT-PPH-SRC HBT-PPH-SRC$ WRITE-ALL
   HBT-PPH-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-PPH-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-PPH-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-PPH-OUT FILE? TTRUE
   HBT-PPH-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-PPH-EXPECTED$ T$= ;

\ ... and a program whose closure is larger than any constant the linker used to
\ carry builds and runs: the tables are sized from the program (aot-closure.f
\ CLO-CAPACITY).
: HBT-STRIPPED-CHAIN ( -- )
   HBT-CHAIN-SRC!
   HBT-CHAIN-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-CHAIN-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-CHAIN-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-CHAIN-OUT FILE? TTRUE
   HBT-CHAIN-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CHAIN-EXPECTED$ T$= ;

\ ... a stripped image OPENS A FILE BY PATH, because the path scratch PATH0
\ terminates into is claimed fresh with its byte length. Before that claim this
\ program - Tender's minimal reproducer - was refused with `outside the restored
\ span caller=PATH0 target=PZB`, and so was every entry point that reaches a file.
: HBT-STRIPPED-OPEN-PATH ( -- )
   HBT-OPENP-SRC HBT-OPENP-SRC$ WRITE-ALL
   HBT-OPENP-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-OPENP-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-OPENP-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-OPENP-OUT FILE? TTRUE
   HBT-OPENP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-OPENP-EXPECTED$ T$=
   HBT-OPENP-OUT HBT-REMOVE-FILE? ;

\ ... a stripped image READS THE IMAGE-LIFECYCLE REGISTRY, because its lock and
\ its two hook counters are claimed fresh: a new process has registered nothing.
: HBT-STRIPPED-LIFECYCLE-REGISTRY ( -- )
   HBT-LIFE-SRC HBT-LIFE-SRC$ WRITE-ALL
   HBT-LIFE-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LIFE-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LIFE-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-LIFE-OUT FILE? TTRUE
   HBT-LIFE-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LIFE-EXPECTED$ T$=
   HBT-LIFE-OUT HBT-REMOVE-FILE? ;

\ ... and REGISTERS a hook, runs it at exit and prints from it. The refusals this
\ replaced, the reason the declaration half of `xt!` is dropped and the reason
\ the printed order is the one pinned are all with HBT-HOOK-SRC$ above.
: HBT-STRIPPED-LIFECYCLE-HOOK ( -- )
   HBT-HOOK-SRC HBT-HOOK-SRC$ WRITE-ALL
   HBT-HOOK-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-HOOK-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-HOOK-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-HOOK-OUT FILE? TTRUE
   HBT-HOOK-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-HOOK-EXPECTED$ T$=
   HBT-HOOK-OUT HBT-REMOVE-FILE? ;

\ ... and PARSES A NUMBER, carrying the engine's number reader. The refusal
\ this replaced and the three pinned answers are with HBT-NUMP-SRC$ above.
: HBT-STRIPPED-NUM-PARSE ( -- )
   HBT-NUMP-SRC HBT-NUMP-SRC$ WRITE-ALL
   HBT-NUMP-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-NUMP-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-NUMP-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-NUMP-OUT FILE? TTRUE
   HBT-NUMP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-NUMP-EXPECTED$ T$=
   HBT-NUMP-OUT HBT-REMOVE-FILE? ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-LIB-STATE
   HBT-STRIPPED-ENGINE-CELLS
   HBT-STRIPPED-UNOWNED-CELL
   HBT-STRIPPED-PTR-MARK
   HBT-STRIPPED-PRINT-PARSE-HASH
   HBT-STRIPPED-CHAIN
   HBT-STRIPPED-OPEN-PATH
   HBT-STRIPPED-LIFECYCLE-REGISTRY
   HBT-STRIPPED-LIFECYCLE-HOOK
   HBT-STRIPPED-NUM-PARSE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-MAIN
