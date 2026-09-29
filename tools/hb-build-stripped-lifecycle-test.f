\ hb-build-stripped-lifecycle-test.f - checked fixture for tools/hb-build-lib.f:
\ the image-lifecycle registry a stripped image reads and registers hooks in,
\ and the engine's number reader it carries. tools/hb-build-test-lib.f lists
\ the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-lifecycle-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

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
: HBT-STRIPPED-LIFECYCLE-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-LIFECYCLE-REGISTRY
   HBT-STRIPPED-LIFECYCLE-HOOK
   HBT-STRIPPED-NUM-PARSE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-lifecycle-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-LIFECYCLE-MAIN
