\ tier0-profile.f - where tier-0 compile time goes, as a sampling profile
\ (docs/debugging.md "Sampling profiler"). Prints `P <mode> defs <n> ns <n>
\ ns-per-def <n>`, then the profiler's own report. Measure on a quiet, pinned
\ machine (`taskset -c 8`): this is wall clock.
\ Run: bin/hb --load tools/tier0-profile.f -- corpus|corpus-raw <usec> <file.f>...
\      bin/hb --load tools/tier0-profile.f -- trivial|trivial-raw <usec> <count>

\ Nothing loads before the corpus (`set-tier` is an engine word): src/lib/fmt.f
\ is a corpus entry, and loading it first would hide every word its compile
\ reaches. The decimal writer below is the price.
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

\ `required`, the registry-aware load every program uses: a shared dependency
\ compiles once and a file the engine carries baked contributes nothing.
: LOAD-CORPUS ( -- )
   script-argc 2 ?do i script-argv$ required loop ;

: COMPILE-TRIVIAL ( n -- ) {: n :}
   n 0 ?do i 1 + TRIVIAL-SRC  SRC SRC-N @ evaluate-closed loop ;

: REPORT-HEAD ( ptr u8 n n n -- ) {: name:ptr nu defs ns :}
   s" P " EMIT$  name nu EMIT$
   s"  defs" EMIT$  defs EMIT-FIELD
   s"  ns" EMIT$  ns EMIT-FIELD
   s"  ns-per-def" EMIT$  defs 0 > if ns defs / else 0 then EMIT-FIELD
   LF EMIT-C ;

\ Whether the mode argument names a `-raw` mode; MAIN refuses a short list.
: RAW? ( -- bool )
   script-argc 1 < if 0 0= 0= exit then
   0 script-argv$ {: mode:ptr mu :}
   mode mu s" corpus-raw" STR= mode mu s" trivial-raw" STR= or ;

public

\ A `-raw` mode is the checker ablation: `0 set-check` clears the hook, so the
\ same source takes the same tokenize/lookup/emit/publish path unchecked, and
\ the difference from its twin is the checker's cost (the profiler lumps the
\ engine's unregistered text into `other`). A `-raw` mode refuses a corpus
\ declaring a SUMTYPE, PRODUCT, STRUCTURE or ENUM: their generated constructors
\ need the checker. `set-check` is a top-level boundary no checked body names;
\ this is the one trusted step.
TRUSTED: ABLATE ( -- )
   RAW? if 0 set-check then ;

: MAIN ( -- )
   script-argc 3 < if USAGE then
   1 script-argv$ ARG>N {: usec :}
   usec 0 > 0= if USAGE then
   0 script-argv$ {: mode:ptr mu :}
   \ set-tier is engine-global and only code compiled after it takes the tier,
   \ so this precedes the first corpus token (as in tools/tier-census.f).
   0 set-tier
   usec prof-rate
   mode mu s" corpus" STR= mode mu s" corpus-raw" STR= or if
      ndict@ {: first :}
      \ prof-on indexes the dictionary as it stands: every word the workload
      \ defines, and its top-level code, lands in `new`, so the rows are the
      \ engine's baked compiler, checker and runtime under measurement.
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

TIER0-PROF:ABLATE TIER0-PROF:MAIN
