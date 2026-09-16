\ tier0-profile.f - where tier-0 compile time goes, as a sampling profile.
\
\ Run:
\     bin/hb --load tools/tier0-profile.f -- corpus <usec> <file.f>...
\     bin/hb --load tools/tier0-profile.f -- trivial <usec> <count>
\     bin/hb --load tools/tier0-profile.f -- corpus-raw <usec> <file.f>...
\     bin/hb --load tools/tier0-profile.f -- trivial-raw <usec> <count>
\
\ Selects tier 0, arms the in-binary sampling profiler (docs/debugging.md
\ "Sampling profiler") at the given interval, compiles the workload, stops the
\ clock and prints the profiler's own report. One line of this tool's own
\ output precedes it:
\
\     P <mode> defs <n> ns <n> ns-per-def <n>
\
\ WHY THE TIER IS SELECTED BEFORE ANYTHING LOADS. `set-tier` is engine-global
\ and only code compiled after it belongs to the selected tier, so the
\ selection has to happen before the first corpus token is read - the same
\ rule tools/tier-census.f and tools/tier-bench.f state.
\
\ WHY THE CORPUS LOADS THROUGH `required`. It is the registry-aware load every
\ other program uses, so a shared dependency compiles once and a file the
\ engine already carries baked contributes nothing. A profile taken over a
\ renamed model of the corpus would answer for the model.
\
\ WHAT THE PROFILE CAN AND CANNOT SEE. The index is built by `prof-on` from
\ the dictionary as it stands, so every word the workload DEFINES lands in the
\ report's `new` bucket rather than in a row - which is right here, because
\ this profiles the compiler and not the compiled code. Top-level code in a
\ corpus file does run and is compiled after the index, so it too counts as
\ `new`. The rows are therefore the engine's own baked compiler, checker and
\ runtime, which is exactly the path under measurement.
\
\ WHY THIS FILE REQUIRES NOTHING. lib/string.f and lib/fmt.f are corpus
\ entries; a tool that loaded them for its own output would have them in the
\ dictionary before the corpus asked, and the profile would lose every word
\ their compile reaches. The decimal writer below is the price of that.
\
\ THE `-raw` MODES ARE THE CHECKER ABLATION. `0 set-check` clears the engine's
\ check hook, so the same source goes down the same tokenize/lookup/emit/publish
\ path with no checking at all. The difference between a mode and its `-raw`
\ twin is what the checker costs, measured rather than inferred from which
\ bucket a sample landed in - the profiler cannot split the engine's own
\ unregistered text (the interpreter loop, the tokenizer, the tier-0 emitters
\ and the publish step all land in `other`), and this can.
\
\ MEASURE ON A QUIET MACHINE and pin the run (`taskset -c 8`): this is wall
\ clock, and the sample counts are only comparable between runs taken at a
\ similar load average.

package TIER0-PROF
private

64 constant USAGE-RC                \ sysexits EX_USAGE
32 constant DIG-CAP
$200 constant SRC-CAP
48 constant ZERO-C
$0A constant LF
$20 constant SP

create DIG DIG-CAP allot
create CHB 1 allot                  \ EMIT-C's own byte; sharing DIG would let it
variable DEC-V                      \ overwrite the digit EMIT-DEC has not read yet
variable DEC-K

create SRC SRC-CAP allot
variable SRC-N

\ ---- the one trust boundary -------------------------------------------------
\ `set-tier` and `evaluate` are both refused inside a plain checked body. Each
\ gets one row with nothing else in it, so the unchecked surface is the two
\ primitives and not the measurement around them.

TRUSTED: SELECT-TIER ( n -- ) set-tier ;
TRUSTED: EVAL$ ( ptr u8 n -- ) evaluate ;
TRUSTED: HOOK-OFF ( -- ) 0 set-check ;   \ the name may not fold to `set-check`: UNSAFE-TOK? is a case-folded spelling test

\ ---- output -----------------------------------------------------------------

: EMIT-C ( n -- ) {: c :}
   c CHB c!
   CHB 1 type ;

: EMIT$ ( ptr u8 n -- ) {: a:ptr u :}
   u 0 ?do a i + c@ EMIT-C loop ;

: EMIT-DEC ( n -- )
   DEC-V !
   DEC-V @ 0= if ZERO-C EMIT-C exit then
   0 DEC-K !
   begin DEC-V @ 0 > while
      DEC-V @ 10 mod ZERO-C + DIG DEC-K @ + c!
      DEC-K @ 1 + DEC-K !
      DEC-V @ 10 / DEC-V !
   repeat
   begin DEC-K @ 0 > while
      DEC-K @ 1 - DEC-K !
      DIG DEC-K @ + c@ EMIT-C
   repeat ;

: EMIT-FIELD ( n -- ) SP EMIT-C EMIT-DEC ;

\ ---- the trivial-definition source, built before the clock starts -----------

: SRC-C+ ( n -- ) {: c :}
   SRC-N @ SRC-CAP >= if s" tier0-profile: source overflow" USAGE-RC die then
   c SRC SRC-N @ + c!
   SRC-N @ 1 + SRC-N ! ;

: SRC$+ ( ptr u8 n -- ) {: a:ptr u :}
   u 0 ?do a i + c@ SRC-C+ loop ;

: SRC-DEC+ ( n -- )
   DEC-V !
   DEC-V @ 0= if ZERO-C SRC-C+ exit then
   0 DEC-K !
   begin DEC-V @ 0 > while
      DEC-V @ 10 mod ZERO-C + DIG DEC-K @ + c!
      DEC-K @ 1 + DEC-K !
      DEC-V @ 10 / DEC-V !
   repeat
   begin DEC-K @ 0 > while
      DEC-K @ 1 - DEC-K !
      DIG DEC-K @ + c@ SRC-C+
   repeat ;

\ `: PVn ( n -- n ) 1 + ;` - the same trivial body tools/compile-floor.f times,
\ under names no corpus file can collide with.
: TRIVIAL-SRC ( n -- ) {: ix :}
   0 SRC-N !
   s" : PV" SRC$+  ix SRC-DEC+  s"  ( n -- n ) 1 + ; " SRC$+ ;

\ ---- arguments --------------------------------------------------------------

: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au b:ptr bu :}
   au bu <> if 0 0= 0= exit then
   0 {: bad :}
   bad au 0 ?do a i + c@ b i + c@ <> if 1 + then loop
   0= ;

: USAGE ( -- )
   s" usage: bin/hb --load tools/tier0-profile.f -- corpus|corpus-raw|trivial|trivial-raw <usec> <arg>..."
   USAGE-RC die ;

: ARG>N ( ptr u8 n -- n ) {: a:ptr u :}
   u 0= if USAGE then
   0 {: acc :}
   acc u 0 ?do
      a i + c@ {: c :}
      c ZERO-C < c ZERO-C 9 + > or if USAGE then
      10 * c ZERO-C - +
   loop ;

\ ---- the two workloads ------------------------------------------------------

: LOAD-CORPUS ( -- )
   script-argc 2 ?do i script-argv$ required loop ;

: COMPILE-TRIVIAL ( n -- ) {: n :}
   n 0 ?do i 1 + TRIVIAL-SRC  SRC SRC-N @ EVAL$ loop ;

: REPORT-HEAD ( ptr u8 n n n -- ) {: name:ptr nu defs ns :}
   s" P " EMIT$  name nu EMIT$
   s"  defs" EMIT$  defs EMIT-FIELD
   s"  ns" EMIT$  ns EMIT-FIELD
   s"  ns-per-def" EMIT$  defs 0 > if ns defs / else 0 then EMIT-FIELD
   LF EMIT-C ;

public

: MAIN ( -- )
   script-argc 3 < if USAGE then
   1 script-argv$ ARG>N {: usec :}
   usec 0 > 0= if USAGE then
   0 script-argv$ {: mode:ptr mu :}
   0 SELECT-TIER
   usec prof-rate
   mode mu s" corpus-raw" STR= mode mu s" trivial-raw" STR= or if HOOK-OFF then
   mode mu s" corpus" STR= mode mu s" corpus-raw" STR= or if
      ndict@ {: first :}
      0 prof-on
      mono-ns
      LOAD-CORPUS
      mono-ns swap - {: ns :}
      prof-off
      mode mu ndict@ first - ns REPORT-HEAD
      prof-report
      exit
   then
   mode mu s" trivial" STR= mode mu s" trivial-raw" STR= or 0= if USAGE then
   2 script-argv$ ARG>N {: n :}
   0 prof-on
   mono-ns
   n COMPILE-TRIVIAL
   mono-ns swap - {: ns :}
   prof-off
   mode mu n ns REPORT-HEAD
   prof-report ;

;package

TIER0-PROF:MAIN
