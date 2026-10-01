\ gate-debug.f - checked runner for profiler/debug gate checks.

require tools/jitdump-core.f
require test/gate-common.f
require src/habu/prof-abi.f

using JITDUMP      \ JD / JIT-FIND / JIT-EVALUATE, called bare in GDB-JITDUMP

: GDB-PROFILER-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-PROFILER-BUSY-WORD ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : GO ( -- ) 100000 prof-on LONG-PROFILER-BUSY-WORD prof-report ;" GE-SRC-LINE
   s" GO" GE-SRC-LINE ;

: GDB-PROFILER ( -- )
   GE-HB-RESET
   GDB-PROFILER-SOURCE
   s" profiler long dictionary names" GE-HB-RUN-STDIN
   s" LONG-PROFILER-BUSY-WORD" s" profiler long-name output" GE-EXPECT-OUT-HAS
   s" PASS: profiler long dictionary names" type cr ;

\ --- profiler report regressions (dots habu-make-profiler-sample-4df2965e,
\ habu-bound-profiler-counter-235c5f48, habu-finish-the-profiler-4cdd3b18 and
\ habu-build-the-internal-4cd07a82) ---
\ Every report opens with one header line that states its own accounting by name:
\
\   profiler samples N words W other O new X foreign F frames ... indexed I usec U
\
\ and W + O + X + F == N is the identity the counting cases below read. They read
\ a NAMED field rather than a column, so a report that grows another column does
\ not quietly stop being checked; and a busy word whose loop body inlines its
\ primitives keeps every interrupted pc inside its own code range, so the samples
\ land in one deterministic bucket.

variable GDB-I        \ GDB-NUM-AT's cursor
variable GDB-D        \ the digit run's first byte
variable GDB-CUT      \ GDB-AFTER's cut point

\ What separates a field name from its value: a space in the text report, a
\ quote and a colon in the JSON one. Both reports are read by the same helper.
: GDB-GAP? ( n -- bool ) {: c:n :}
   c STR-SPACE = c [char] " = or c [char] : = or ;

: GDB-NUM-AT ( n ptr u8 n -- n )   \ the digit run at or after offset i, -1 when none
   {: i:n a:ptr u:n :}
   i GDB-I !
   begin GDB-I @ u < if a GDB-I @ + c@ GDB-GAP? else STR-FALSE then while
      GDB-I @ 1+ GDB-I !
   repeat
   GDB-I @ GDB-D !
   begin GDB-I @ u < if a GDB-I @ + c@ STR-DIGIT? else STR-FALSE then while
      GDB-I @ 1+ GDB-I !
   repeat
   GDB-I @ GDB-D @ = if -1 exit then
   a GDB-D @ + GDB-I @ GDB-D @ - STR>NUMBER? MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH ;

: GDB-FIELD ( ptr u8 n ptr u8 n -- n )   \ the integer printed after <name>, -1 when absent
   {: a:ptr u:n b:ptr v:n :}
   a u b v FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N v + a u GDB-NUM-AT ENDOF
   ;MATCH ;

: GDB-AFTER ( ptr u8 n ptr u8 n -- ptr u8 n )   \ the tail past the first <b>, empty when absent
   {: a:ptr u:n b:ptr v:n :}
   a u b v FIND-SUB MATCH option
     none OF a 0 ENDOF
     some OF IDX>N v + GDB-CUT !  a GDB-CUT @ +  u GDB-CUT @ -  ENDOF
   ;MATCH ;

\ The sample total, from either report: "profiler samples N" or {"samples":N}.
: GDB-SAMPLES ( ptr u8 n -- n )
   s" samples" GDB-FIELD ;

\ The header's own identity, plus the sample count the case expects (-1: any).
: GDB-ACCOUNT ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u GDB-SAMPLES {: tot:n :}
   tot 0 < if s" profiler report has no header line" GE-FAIL then
   want 0 >= tot want <> and if
      s" profiler sample total is not the limit" GE-FAIL
   then
   a u s" words" GDB-FIELD
   a u s" other" GDB-FIELD +
   a u s" new" GDB-FIELD +
   a u s" defer" GDB-FIELD +
   a u s" spill" GDB-FIELD +
   a u s" foreign" GDB-FIELD + tot <> if
      s" profiler header: the named buckets do not add up to samples" GE-FAIL
   then ;

: GDB-PROF-SRC ( n -- )   \ emit a busy word + "<n> prof-on GDB-BUSY" into GE-SRC
   GE-SRC-RESET
   s" : GDB-BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   GE-SRC-U+
   s"  prof-on GDB-BUSY" GE-SRC-LINE ;

: GDB-PROF-RUN ( -- )   \ run the accumulated GE-SRC through the candidate over stdin
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN ;

\ A package whose inner word is only ever reached through the outer one, so the
\ caller rows under the inner word's row have exactly one right answer.
: GDB-PROF-PKG-SRC ( -- )
   GE-SRC-RESET
   s" package GDBPKG" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : OUTER ( -- ) BUSY ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

\ --- dot habu-complete-queued-profiler-a5ce1ad4: the SIGALRM handler reads nothing
\ from the interrupted context's live registers. A tick inside a foreign callee
\ (libc's dlsym here; libzip's inflate for Tender) used to walk the callee's
\ reuse of x20/x26/x27 and die SIGSEGV with x0 = SIGALRM; now such a sample is
\ counted in the header's foreign field. And the profiler's total and limit no
\ longer share $1E0/$1E8 with GTOD-SCRATCH, so a clock query after prof-on cannot
\ turn the next tick into an early report + exit 99.
\ GDB-SYM-FOR calls dlsym for n ms of the monotonic clock, not a call count, so
\ the sample total follows the timer rather than the host's call speed. On spark
\ (ARM64 Linux, glibc, the default 1000 us rate) 100,000 calls took 8 or 9
\ ticks, 46% of them inside dlsym and the rest in the FFI wrapper, so an empty
\ foreign bucket came up by chance; 200 ms gave 200 ticks in each of 20 runs,
\ 64 to 86 of them (38% overall) foreign.
: GDB-PROFILER-FOREIGN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" require lib/ffi-abi.f" GE-SRC-LINE
   s" create GDB-SYM 16 allot" GE-SRC-LINE
   s" : GDB-SYM-FOR ( n -- ) 1000000 * mono-ns + {: stop:n :} begin 0 GDB-SYM FFI:DLSYM drop mono-ns stop >= until ;" GE-SRC-LINE
   S\" s\" strlen\" GDB-SYM FFI:CSTR" GE-SRC-LINE
   S\" 100000 prof-on 200 GDB-SYM-FOR s\" dlsym loop done\" type cr prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler foreign-context ticks" GE-EXPECT-OK
   s" dlsym loop done" s" profiler foreign loop completes" GE-EXPECT-OUT-HAS
   GT-OUT$ s" foreign" GDB-FIELD 0 <= if
      s" profiler foreign bucket counted nothing" GE-FAIL
   then
   GT-OUT$ -1 GDB-ACCOUNT
   s" habu-crash" s" profiler foreign no-crash" GE-EXPECT-ERR-LACKS
   s" PASS: profiler counts ticks inside foreign code as foreign instead of dying" type cr ;

: GDB-PROFILER-CLOCK ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDB-BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" 100000 prof-on epoch-seconds drop GDB-BUSY prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler clock query" GE-EXPECT-OK
   s" GDB-BUSY" s" profiler output after a clock query" GE-EXPECT-OUT-HAS
   GT-OUT$ -1 GDB-ACCOUNT
   s" PASS: profiler state survives a clock query" type cr ;

: GDB-PROFILER-LIMIT1 ( -- )   \ limit 1 must attribute (not drop) the only sample
   GE-HB-RESET
   1 GDB-PROF-SRC
   GDB-PROF-RUN
   99 s" profiler limit 1 auto-report exit(99)" GE-EXPECT-RC
   GT-OUT$ 1 GDB-ACCOUNT
   s" PASS: profiler limit 1 attributes the interrupted sample" type cr ;

: GDB-PROFILER-EXACT ( n -- )   \ words + other + new + foreign == limit at the auto-report
   {: lim:n :}
   GE-HB-RESET
   lim GDB-PROF-SRC
   GDB-PROF-RUN
   99 s" profiler exact-totals auto-report exit(99)" GE-EXPECT-RC
   GT-OUT$ lim GDB-ACCOUNT ;

: GDB-PROFILER-EXACT-ALL ( -- )
   1 GDB-PROFILER-EXACT
   2 GDB-PROFILER-EXACT
   5 GDB-PROFILER-EXACT
   s" PASS: profiler sample totals are exact (sum == limit)" type cr ;

\ Words defined INSIDE the profiled phase: every one of them is compiled after
\ prof-on built the index, so the handler can only keep the raw pc and the report
\ has to rebuild and replay to name them. BUSY calls LEAF and then loops, so once
\ LEAF has returned the interrupted x30 points back inside BUSY - which is the one
\ construction that deterministically leaves a deferred sample with no caller.
\ Tier 1 on purpose: at tier 0 these words are interpreted and the samples land
\ in the primitives the interpreter runs, not in the words the case is about.
: GDB-PROF-LATE-SRC ( -- )
   GE-SRC-RESET
   s" 1 set-tier" GE-SRC-LINE
   s" 0 prof-on" GE-SRC-LINE
   s" package GDBLATE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : LEAF ( n -- n ) 1+ ;" GE-SRC-LINE
   s" : BUSY ( -- ) 80000000 begin 1- LEAF 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : OUTER ( -- ) BUSY ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GDBLATE:OUTER prof-off prof-report" GE-SRC-LINE ;

: GDB-PROFILER-DEFER ( -- )
   GE-HB-RESET
   GDB-PROF-LATE-SRC
   GDB-PROF-RUN
   s" profiler deferred attribution" GE-EXPECT-OK
   s" GDBLATE:LEAF" s" profiler did not name a word compiled after prof-on" GE-EXPECT-OUT-HAS
   s" GDBLATE:BUSY" s" profiler did not name its caller compiled after prof-on" GE-EXPECT-OUT-HAS
   GT-OUT$ -1 GDB-ACCOUNT
   GT-OUT$ s" defer" GDB-FIELD 0 <> if
      s" prof-report left deferred samples unattributed" GE-FAIL
   then
   GT-OUT$ s" spill" GDB-FIELD 0 <> if
      s" the deferred buffer overflowed on a one-word phase" GE-FAIL
   then
   GT-OUT$ s" new" GDB-FIELD  GT-OUT$ GDB-SAMPLES 100 / > if
      s" more than one percent of samples stayed unnamed after the sync" GE-FAIL
   then
   s" (unknown)" s" profiler dropped the samples whose caller it cannot establish" GE-EXPECT-OUT-HAS
   s" PASS: a word compiled after prof-on is named by the report, not bucketed" type cr
   s" PASS: a sample with no establishable caller keeps an explicit (unknown) row" type cr ;

: GDB-PROF-RATE-RUN ( ptr u8 n -- )   \ run GDB-BUSY under one prof-on line
   {: arm:ptr armu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDB-BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   arm armu GE-SRC-LINE
   GDB-PROF-RUN ;

: GDB-PROFILER-RATE ( -- )
   s" 100 prof-rate 0 prof-on GDB-BUSY prof-off prof-report" GDB-PROF-RATE-RUN
   s" profiler non-default rate" GE-EXPECT-OK
   GT-OUT$ s" usec" GDB-FIELD 100 <> if
      s" prof-rate did not take: the report names another interval" GE-FAIL
   then
   GT-OUT$ GDB-SAMPLES {: fast:n :}
   s" 0 prof-on GDB-BUSY prof-off prof-report" GDB-PROF-RATE-RUN
   s" profiler default rate" GE-EXPECT-OK
   GT-OUT$ s" usec" GDB-FIELD 1000 <> if
      s" the default sampling interval is not 1000 us" GE-FAIL
   then
   GT-OUT$ GDB-SAMPLES {: slow:n :}
   fast slow 2 * <= if
      s" a ten-times shorter interval did not take more samples" GE-FAIL
   then
   s" PASS: prof-rate sets the interval the next prof-on arms" type cr ;

\ --- dot habu-stop-the-clock-83ae936a: prof-on arms an interval of a second or
\ more, prof-reset stops the clock for its clears, prof-rate throws a
\ negative interval back to its caller, and a refused arm is named and fatal.
\ GDB-DELAY ( n -- ): spin until n more nanoseconds have passed by mono-ns.
: GDB-DELAY-SRC ( -- )
   s" : GDB-DELAY ( n -- ) mono-ns + begin dup mono-ns <= until drop ;" GE-SRC-LINE ;

\ A phase at 1.5 s over four seconds of wall clock ticks at 1.5 s and 3 s.
: GDB-PROFILER-SLOW ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GDB-DELAY-SRC
   s" 1500000 prof-rate 0 prof-on 4000000000 GDB-DELAY prof-off prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler 1.5 s interval" GE-EXPECT-OK
   GT-OUT$ s" usec" GDB-FIELD 1500000 <> if
      s" the report names another interval than 1500000 us" GE-FAIL
   then
   GT-OUT$ -1 GDB-ACCOUNT
   GT-OUT$ GDB-SAMPLES {: n:n :}
   n 2 < if s" a 1.5 s interval took fewer than two samples in four seconds" GE-FAIL then
   n 3 > if s" a 1.5 s interval sampled more often than every 1.5 s" GE-FAIL then
   s" PASS: prof-on arms an interval of a second or more" type cr ;

\ The running engine's band and arena, read as the report reads them.
: GDB-PROF-BAND-SRC ( -- )
   s" require src/habu/prof-abi.f" GE-SRC-LINE
   s" 0 SNAP-RELOC:XTCELL-OFF-MAX CELL + PROF-ABI:PROF-BAND-AT constant GDB-BAND" GE-SRC-LINE
   s" : GDB-BAND@ ( n -- n ) {: off:n :} data-base GDB-BAND + off + @ ;" GE-SRC-LINE
   s" TRUSTED: GDB>ARENA ( n -- ptr n ) ;" GE-SRC-LINE
   s" : GDB-ARENA ( n -- ptr n ) {: off:n :} PROF-ABI:PROF-ARENA GDB-BAND@ GDB>ARENA off + ;" GE-SRC-LINE ;

\ prof-reset 1000 times at 50 us, each time followed by prof-off and the
\ header's identity, words + other + new + defer + spill + foreign == samples,
\ read with the clock stopped. Each turn arms afresh, so a reset run straight
\ after the arm would always start a whole interval before the first tick;
\ a delay of 64 steps of 800 ns between the two moves the reset's start across
\ the interval. The child prints how many resets left the identity false.
: GDB-PROFILER-RESETS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GDB-PROF-BAND-SRC
   GDB-DELAY-SRC
   s" : GDB-WORDS ( -- n ) 0 ndict@ 0 ?do PROF-STATE-BYTES i cells + GDB-BAND@ + loop ;" GE-SRC-LINE
   s" : GDB-SKEW ( -- n ) GDB-WORDS PROF-ABI:ARN-NEW GDB-ARENA @ + PROF-ABI:ARN-DEFER GDB-ARENA @ +" GE-SRC-LINE
   s"    PROF-ABI:ARN-SPILL GDB-ARENA @ + PROF-ABI:PROF-OTHER GDB-BAND@ + PROF-ABI:PROF-FOREIGN GDB-BAND@ +" GE-SRC-LINE
   s"    PROF-ABI:PROF-TOT GDB-BAND@ - ;" GE-SRC-LINE
   s" variable GDB-BAD" GE-SRC-LINE
   s" : GDB-RESETS ( -- ) 1000 0 do 0 prof-on i 63 and 800 * GDB-DELAY prof-reset prof-off" GE-SRC-LINE
   s"    GDB-SKEW 0<> if 1 GDB-BAD +! then loop ;" GE-SRC-LINE
   S\" 50 prof-rate GDB-RESETS s\" skewed \" type GDB-BAD @ . cr" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler resets in a hot loop" GE-EXPECT-OK
   GT-OUT$ s" skewed" GDB-FIELD 0 <> if
      s" prof-reset left the identity false: a tick landed between its clears" GE-FAIL
   then
   s" PASS: prof-reset stops the clock, so the identity survives 1000 resets at 50 us" type cr ;

\ The engine's whole stderr for an uncaught throw of the code.
: GDB-UNCAUGHT$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET  s" hb: uncaught throw code " SB-APPEND  code FMT:SB-INT  GE-SB-LF  SB$ ;

\ A negative interval is the caller's error: prof-rate throws E-PROF-RATE
\ before it stores anything. Uncaught, the engine reports the code and exits
\ UNCAUGHT-RC before the next line runs. Caught, the code is lib/errors.f's
\ E-PROF-RATE, printed negated since GDB-FIELD reads a digit run, and the next
\ prof-on arms the 250 us set before it, which the report states.
: GDB-PROFILER-RATE-REFUSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" -1 prof-rate" GE-SRC-LINE
   S\" s\" prof-rate took -1\" type cr" GE-SRC-LINE
   GDB-PROF-RUN
   UNCAUGHT-RC s" an uncaught prof-rate -1 exits UNCAUGHT-RC" GE-EXPECT-RC
   E-PROF-RATE GDB-UNCAUGHT$ s" an uncaught prof-rate -1 reports E-PROF-RATE" GE-EXPECT-ERR
   GT-OUT$ nip 0<> if s" the line after an uncaught prof-rate -1 ran" GE-FAIL then
   S\" : GDB-REFUSE ( -- n ) [: -1 prof-rate ;] catch ; 250 prof-rate GDB-REFUSE negate s\" refused \" type . cr 0 prof-on GDB-BUSY prof-off prof-report"
   GDB-PROF-RATE-RUN
   s" a caught prof-rate -1" GE-EXPECT-OK
   GT-OUT$ s" refused" GDB-FIELD E-PROF-RATE negate <> if
      s" prof-rate -1 threw another code than E-PROF-RATE" GE-FAIL
   then
   GT-OUT$ s" usec" GDB-FIELD 250 <> if
      s" a caught prof-rate -1 changed the rate the next prof-on arms" GE-FAIL
   then
   s" PASS: prof-rate throws E-PROF-RATE for a negative interval and keeps the rate" type cr ;

\ An interval only a write past prof-rate can leave, -1 in ARN-USEC: prof-on's
\ setitimer refuses it, and the refusal is named and exits PROF-MAP-RC.
: GDB-PROFILER-ARM-REFUSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GDB-PROF-BAND-SRC
   s" 250 prof-rate -1 PROF-ABI:ARN-USEC GDB-ARENA ! 0 prof-on" GE-SRC-LINE
   S\" s\" prof-on armed -1\" type cr" GE-SRC-LINE
   GDB-PROF-RUN
   PROF-ABI:PROF-MAP-RC s" a refused arm exits PROF-MAP-RC" GE-EXPECT-RC
   PROF-ABI:PROFARMMSG$ s" a refused arm is named on fd 2" GE-EXPECT-ERR
   s" PASS: a setitimer that refuses the arm is named and fatal" type cr ;

\ A phase word - the caller that encloses the work - takes no exclusive samples
\ at all, so no exclusive ranking will ever show it. The inclusive section, the
\ complete JSON, and prof-row are the three ways to read one.
: GDB-PROF-PHASE-SRC ( -- )
   GE-SRC-RESET
   s" require lib/string.f" GE-SRC-LINE
   s" package GDBPH" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" variable GDBPH-ACC" GE-SRC-LINE
   s" : BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : PHASE ( -- ) BUSY ;" GE-SRC-LINE
   s" : FIND-REC ( ptr u8 n -- n ) {: a:ptr u:n :}" GE-SRC-LINE
   s"    -1 GDBPH-ACC !" GE-SRC-LINE
   s"    ndict@ 0 ?do i XREF-REC {: rec:ptr :}" GE-SRC-LINE
   s"       rec XREF-RETIRED? 0= if rec XREF-NAME$ a u STR= if i GDBPH-ACC ! then then" GE-SRC-LINE
   s"    loop GDBPH-ACC @ ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: GDB-PROFILER-INCLUSIVE ( -- )
   GE-HB-RESET
   GDB-PROF-PHASE-SRC
   s" 0 prof-on GDBPH:PHASE prof-off prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler inclusive section" GE-EXPECT-OK
   s" by inclusive" s" profiler printed no inclusive section" GE-EXPECT-OUT-HAS
   s" GDBPH:PHASE" s" the inclusive section did not surface the phase word" GE-EXPECT-OUT-HAS
   GT-OUT$ -1 GDB-ACCOUNT
   GT-OUT$ s" attributed" GDB-FIELD 1 < if
      s" profiler header does not count the words it attributed" GE-FAIL
   then
   s" PASS: the inclusive section shows a word no exclusive ranking would" type cr ;

: GDB-PROFILER-ROW ( -- )
   GE-HB-RESET
   GDB-PROF-PHASE-SRC
   s" 0 prof-on GDBPH:PHASE prof-off" GE-SRC-LINE
   S\" s\" PHASE\" GDBPH:FIND-REC prof-row" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler chosen row" GE-EXPECT-OK
   s" GDBPH:PHASE" s" prof-row printed no row for the word it was given" GE-EXPECT-OUT-HAS
   GT-OUT$ s" by inclusive" CONTAINS? if
      s" prof-row printed a whole report instead of one row" GE-FAIL
   then
   s" PASS: prof-row prints a chosen word whatever its rank" type cr ;

\ Every attributed word, not a top-N, and every caller edge.
: GDB-PROFILER-JSON-ALL ( -- )
   GE-HB-RESET
   GDB-PROF-PHASE-SRC
   s" 0 prof-on GDBPH:PHASE prof-off prof-json" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler json completeness" GE-EXPECT-OK
   S\" \"word\":\"GDBPH:BUSY\"" s" json is missing the hot word" GE-EXPECT-OUT-HAS
   S\" \"word\":\"GDBPH:PHASE\"" s" json is missing the phase word: it is not a top-N" GE-EXPECT-OUT-HAS
   S\" ,\"edges\":[" s" json carries no edge array" GE-EXPECT-OUT-HAS
   S\" \"caller\":\"GDBPH:PHASE\"" s" json edge does not name the caller" GE-EXPECT-OUT-HAS
   GT-OUT$ s" attributed" GDB-FIELD 1 < if
      s" json header does not count the words it attributed" GE-FAIL
   then
   \ An inclusive count above the sample total would say a word ran longer than
   \ the program did. The conservative walk can see one word twice in a sample,
   \ so the handler stamps each record with the sample serial and counts it once.
   GT-OUT$ S\" \"incl\":" GDB-FIELD  GT-OUT$ GDB-SAMPLES > if
      s" an inclusive count is greater than the sample total" GE-FAIL
   then
   s" PASS: prof-json carries every attributed word and every caller edge" type cr
   s" PASS: no inclusive count passes the sample total" type cr ;

\ A word counted during the phase and retired before the report loses its index
\ entry: the sync rebuilds the index from the dictionary as it then stands. The
\ header summed the entries, so those samples fell out of it and the identity
\ came up short - 397 of 92,013 on a self-build. The sum walks records now.
: GDB-PROFILER-RETIRED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDBRET-BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" 0 prof-on GDBRET-BUSY prof-off" GE-SRC-LINE
   s" undefine GDBRET-BUSY" GE-SRC-LINE
   s" prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler retired word" GE-EXPECT-OK
   GT-OUT$ -1 GDB-ACCOUNT
   GT-OUT$ GDB-SAMPLES 0 <= if
      s" profiler retired-word case collected no sample" GE-FAIL
   then
   s" PASS: a word retired before the report keeps its samples in the total" type cr ;

\ --- the surface the profiler dots ask for: package-qualified rows, the caller
\ under each row, a stop that keeps the counters, a reset that clears them, and
\ the same walk as JSON ---

: GDB-PROFILER-QUAL ( -- )
   GE-HB-RESET
   GDB-PROF-PKG-SRC
   s" 0 prof-on GDBPKG:OUTER prof-off prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler qualified rows" GE-EXPECT-OK
   s" GDBPKG:BUSY" s" profiler row is not package-qualified" GE-EXPECT-OUT-HAS
   s" GDBPKG:OUTER" s" profiler caller row is not package-qualified" GE-EXPECT-OUT-HAS
   s" <- " s" profiler printed no caller line" GE-EXPECT-OUT-HAS
   GT-OUT$ -1 GDB-ACCOUNT
   GT-OUT$ GDB-SAMPLES 0 <= if
      s" profiler collected no sample for the qualified-row case" GE-FAIL
   then
   s" PASS: profiler rows and callers carry the package-qualified spelling" type cr ;

: GDB-PROFILER-OFF ( -- )   \ prof-off stops the clock and keeps the counters
   GE-HB-RESET
   GDB-PROF-PKG-SRC
   s" 0 prof-on GDBPKG:OUTER prof-off prof-report GDBPKG:OUTER prof-report prof-reset prof-report" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler stop and reset" GE-EXPECT-OK
   GT-OUT$ GDB-SAMPLES {: first:n :}
   GT-OUT$ s" profiler samples" GDB-AFTER {: a:ptr u:n :}
   a u GDB-SAMPLES {: second:n :}
   a u s" profiler samples" GDB-AFTER GDB-SAMPLES {: third:n :}
   first 0 <= if s" profiler stop case collected no sample" GE-FAIL then
   first second <> if
      s" prof-off did not stop sampling: the second report moved" GE-FAIL
   then
   third 0 <> if s" prof-reset left samples behind" GE-FAIL then
   s" PASS: prof-off stops sampling, keeps the counters, prof-reset clears them" type cr ;

: GDB-PROFILER-JSON ( -- )
   GE-HB-RESET
   GDB-PROF-PKG-SRC
   s" 0 prof-on GDBPKG:OUTER prof-off prof-json" GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler json report" GE-EXPECT-OK
   S\" {\"samples\":" s" profiler json has no samples field" GE-EXPECT-OUT-HAS
   S\" \"word\":\"GDBPKG:BUSY\"" s" profiler json row is not qualified" GE-EXPECT-OUT-HAS
   S\" ,\"edges\":[" s" profiler json has no edge array" GE-EXPECT-OUT-HAS
   s" PASS: profiler json reports the same walk as the text report" type cr ;

: GDB-JITDUMP ( -- )
   GE-HB-RESET
   [: s" : JITDUMP-SMOKE ( -- i64 ) 7 ;" JIT-EVALUATE
      s" JITDUMP-SMOKE" JIT-FIND JD ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" jitdump direct core" GE-EXPECT-OK
   s" ret" s" jitdump direct core output" GE-EXPECT-OUT-HAS
   s" PASS: jitdump direct core" type cr ;

\ An engine-text target can share a page with patch32 itself: opening that
\ page RW removes execute permission from the patcher's next instruction.
\ Refusal must precede both the patch and publication of a breakpoint slot.
: GDB-BREAKPOINT-REFUSAL ( -- )
   GE-HB-RESET GE-SRC-RESET
   s" require src/habu/debug.f" GE-SRC-LINE
   s" package GDBBP" GE-SRC-LINE
   s" : VALID ( n -- n ) 1+ ; ' VALID constant VALID-XT" GE-SRC-LINE
   s" ' atomic-cas constant TARGET-XT" GE-SRC-LINE
   s" create SAVED-SLOTS MAXBP 32 * allot create SAVED-CODE 16 allot" GE-SRC-LINE
   s" : TABLE ( -- ptr u8 ) data-base BPTAB-OFF + ;" GE-SRC-LINE
   s" : TARGET ( -- ptr u8 ) TARGET-XT BP-XT>PTR ;" GE-SRC-LINE
   s" : ARM ( -- ) TARGET-XT BP+ ;" GE-SRC-LINE
   s" : CHECK ( -- ) VALID-XT BP+ TABLE SAVED-SLOTS MAXBP 32 * BYTE-COPY TARGET SAVED-CODE 16 BYTE-COPY" GE-SRC-LINE
   s" ['] ARM catch ." GE-SRC-LINE
   s" TABLE MAXBP 32 * SAVED-SLOTS MAXBP 32 * STR= if 1 else 0 then ." GE-SRC-LINE
   s" TARGET 16 SAVED-CODE 16 STR= if 1 else 0 then . VALID-XT BP- ; CHECK ;package" GE-SRC-LINE
   s" unsupported breakpoint target" GE-HB-RUN-STDIN
   S\" -9300\n1\n1\n" s" named breakpoint refusal preserves table and code" GE-EXPECT-OUT
   s" PASS: unsupported breakpoint refuses before changing slots or code" type cr ;

: GDB-BREAKPOINT-NATIVE ( -- )
   GE-HB-RESET GE-SRC-RESET
   s" 1 set-tier require src/habu/debug.f" GE-SRC-LINE
   s" package GDBNBP : TARGET ( n -- n ) 3 + ; ' TARGET BP+ 4 TARGET . 5 TARGET . ;package" GE-SRC-LINE
   s" native breakpoint resumes" GE-HB-RUN-STDIN
   S\" 7\n8\n" s" native target resumes and runs again" GE-EXPECT-OUT
   s" habu-bp:" s" native breakpoint was hit" GE-EXPECT-ERR-HAS
   s" PASS: a native breakpoint resumes its target" type cr ;

: GDB-RUN ( -- )
   s" hb-gate-debug" GT-START
   GDB-PROFILER
   GDB-PROFILER-FOREIGN
   GDB-PROFILER-CLOCK
   GDB-PROFILER-LIMIT1
   GDB-PROFILER-EXACT-ALL
   GDB-PROFILER-QUAL
   GDB-PROFILER-OFF
   GDB-PROFILER-JSON
   GDB-PROFILER-DEFER
   GDB-PROFILER-RATE
   GDB-PROFILER-SLOW
   GDB-PROFILER-RESETS
   GDB-PROFILER-RATE-REFUSED
   GDB-PROFILER-ARM-REFUSED
   GDB-PROFILER-INCLUSIVE
   GDB-PROFILER-ROW
   GDB-PROFILER-JSON-ALL
   GDB-PROFILER-RETIRED
   GDB-JITDUMP
   GDB-BREAKPOINT-REFUSAL
   GDB-BREAKPOINT-NATIVE
   GT-CLEANUP
   s" PASS: native profiler/debug tests" type cr ;

;using
