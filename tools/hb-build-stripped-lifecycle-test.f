\ hb-build-stripped-lifecycle-test.f - checked fixture for tools/hb-build-lib.f:
\ the image-lifecycle registry a stripped image reads and registers hooks in,
\ and the engine's number reader it carries. tools/hb-build-test-lib.f lists
\ the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-lifecycle-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require test/linker-builder-engine.f
require test/aot-builder-engine.f

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

\ READING THE IMAGE-LIFECYCLE REGISTRY, the smallest program that reaches the
\ second site a stripped image is refused at: IMAGE-LIFECYCLE:COUNT takes the
\ registry's private lock and reads both hook counters, three baked cells below
\ every window, claimed fresh in src/habu/aot-owned-cells.f. Every image whose
\ libraries register a cleanup hook on first use reaches them. The printed count
\ proves the claims right, not merely quiet: a new process has registered
\ nothing, and the lock must be free for the image to print at all.
: HBT-LIFE-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require src/lib/image-lifecycle.f\n: MAIN ( -- )\n" SB-APPEND
   S\"    IMAGE-LIFECYCLE:COUNT 0 = if s\" hooks=0\" type cr then ;\n" SB-APPEND
   SB$ ;

: HBT-LIFE-EXPECTED$ ( -- ptr u8 n )
   S\" hooks=0\n" ;

\ ... and REGISTERING one. IMAGE-LIFECYCLE:REGISTER and REGISTER-PERSISTENT store
\ a quotation into a declared cell, which the optimizing tier lowers through
\ QUOTATION-STORAGE:STORE to `xt!`, and `xt!` calls the engine's address-cell
\ registrar. A stripped image has no reader for the address-cell table, so the
\ linker drops the declaration and keeps the store (src/habu/aot-closure.f
\ AOT-DECLARATION?). PROC-ARGV+ is a real first-use registrant:
\ src/lib/process-argv.f registers its RELEASE hook when an argument is staged.
\ The pinned order is PREPARE's: the HOOKS buffer from the last registration
\ down (`hook=b` before `hook=a`, the silent RELEASE hook ahead of both), then
\ the PERSISTENT table the same way, so dependents release first. `count=4` is
\ three registrations plus RELEASE. Nothing else runs PREPARE in a stripped
\ image (the entry is `bl MAIN; exit(0)`), so MAIN calls it after printing `exit`.
\ The image carries the sealed (NUM) helper behind num-parse: 42 has its float
\ flag clear, 1.5 sets it, and the refused 12a answers zero with both flags false.
: HBT-HOOK-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require src/lib/image-lifecycle.f\nrequire src/lib/process-argv.f\n" SB-APPEND
   S\" require src/lib/fmt.f\n: HOOK-A ( -- ) s\" hook=a\" type cr ;\n" SB-APPEND
   S\" : HOOK-B ( -- ) s\" hook=b\" type cr ;\n" SB-APPEND
   S\" : HOOK-P ( -- ) s\" hook=p\" type cr ;\n: MAIN ( -- )\n" SB-APPEND
   S\"    s\" 42\" num-parse {: v:n flt:bool ok:bool :}\n" SB-APPEND
   S\"    ok if v FMT:.INT cr then\n" SB-APPEND
   S\"    flt if s\" float\" type cr else s\" int\" type cr then\n" SB-APPEND
   S\"    s\" 1.5\" num-parse {: v2:n f2:bool ok2:bool :}\n" SB-APPEND
   S\"    f2 if s\" float\" type cr else s\" int\" type cr then\n" SB-APPEND
   S\"    s\" 12a\" num-parse {: v3:n f3:bool ok3:bool :}\n" SB-APPEND
   S\"    ok3 if s\" num\" type cr else v3 FMT:.INT cr then\n" SB-APPEND
   S\"    [: HOOK-A ;] IMAGE-LIFECYCLE:REGISTER\n" SB-APPEND
   S\"    [: HOOK-B ;] IMAGE-LIFECYCLE:REGISTER\n" SB-APPEND
   S\"    [: HOOK-P ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT\n" SB-APPEND
   S\"    s\" x\" >LEN PROC-ARGV+\n" SB-APPEND
   S\"    s\" count=\" type IMAGE-LIFECYCLE:COUNT FMT:.INT cr\n" SB-APPEND
   S\"    s\" exit\" type cr\n   IMAGE-LIFECYCLE:PREPARE ;\n" SB-APPEND
   SB$ ;

: HBT-HOOK-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   S\" 42\nint\nfloat\n0\n" SB-APPEND
   s" count=4" SB-APPEND 10 SB-APPEND-C
   s" exit" SB-APPEND 10 SB-APPEND-C
   s" hook=b" SB-APPEND 10 SB-APPEND-C
   s" hook=a" SB-APPEND 10 SB-APPEND-C
   s" hook=p" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ ... a stripped image READS THE IMAGE-LIFECYCLE REGISTRY, because its lock and
\ its two hook counters are claimed fresh: a new process has registered nothing.
: HBT-STRIPPED-LIFECYCLE-REGISTRY ( -- )
   HBT-LIFE-SRC HBT-LIFE-SRC$ FS:WRITE-ALL
   HBT-LIFE-OUT HBT-REMOVE-FILE?
   HBT-LIFE-SRC HBT-LIFE-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-LIFE-OUT FS:FILE? TTRUE
   HBT-LIFE-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LIFE-EXPECTED$ T$=
   HBT-LIFE-OUT HBT-REMOVE-FILE? ;

\ ... PARSES A NUMBER, REGISTERS a hook, runs it at exit and prints from it.
\ src/lib/process-argv.f and src/lib/fmt.f are modules of the linker's lib closure the
\ engine does not bake, and the linker image refuses this program at
\ PROC-ARGV+ (E-AOT-PRE-WINDOW, test/preloaded-engine.f rule 3), so it builds
\ on the engine.
: HBT-STRIPPED-LIFECYCLE-HOOK ( -- )
   HBT-HOOK-SRC HBT-HOOK-SRC$ FS:WRITE-ALL
   HBT-HOOK-OUT HBT-REMOVE-FILE?
   HBT-HOOK-SRC HBT-HOOK-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   HBT-HOOK-OUT FS:FILE? TTRUE
   HBT-HOOK-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-HOOK-EXPECTED$ T$=
   HBT-HOOK-OUT HBT-REMOVE-FILE? ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-LIFECYCLE-MAIN ( -- )
   T-RESET
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   AOT-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   HBT-STRIPPED-LIFECYCLE-REGISTRY
   HBT-STRIPPED-LIFECYCLE-HOOK
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-lifecycle-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-LIFECYCLE-MAIN
