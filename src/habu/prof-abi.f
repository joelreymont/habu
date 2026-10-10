\ prof-abi.f - the sampling profiler's band and arena layout, package PROF-ABI.
\ Build-only, as data-bands.f is: runtime images need only layout.f.
\
\ The handler a target's profiler emitter writes and the report that reads its
\ samples meet only in memory: the band at the top of the DATA region and the
\ arena whose base the band holds. Every offset, size, signal fact, exit status
\ and message of that meeting is here, once, so a second target's handler reads
\ the layout the report already reads instead of a copy of it. The frame each
\ handler takes apart is its target's own and stays with its emitter
\ (src/habu/prof.f for the two aarch64 hosts). It is its own file, not rows in
\ src/habu/layout.f, because layout.f is baked into every runtime image
\ (src/habu/native-runtime.f) and none of this is read at run time.
require src/habu/layout.f

package PROF-ABI
public

\ ---- the band: PROF-STATE-BYTES of state cells, then one counter per dict record
\ The band closes the DATA region: it starts PROF-CNT-BYTES below DATA's end.
\ DATA base and size in, the band's base out, for whichever target's DATA.
: PROF-BAND-AT ( n n -- n ) {: base:n size:n :} size PROF-CNT-BYTES - base + ;
0  constant PROF-TOT        \ samples delivered
8  constant PROF-LIM        \ report + exit(PROF-LIMIT-RC) at this many, 0 = sample until prof-off
16 constant PROF-OTHER      \ Habu-code samples outside any dict word (main loop, helpers)
24 constant PROF-FOREIGN    \ samples in a context that is not Habu code
32 constant PROF-DBASE      \ the dictionary base, recorded by prof-on
40 constant PROF-ARENA      \ the profiler arena, mapped once per process
48 constant PROF-ARMED      \ the clock: ARMED-RUNS, ARMED-HELD, or 0 while stopped
1  constant ARMED-RUNS      \ the timer runs and the handler counts each tick
2  constant ARMED-HELD      \ a report, row or reset stopped the timer and starts it again
\ STOPPING THE TIMER DOES NOT STOP THE TICKS. On Darwin a tick can arrive after
\ setitimer has disarmed the timer and returned: under 48 busy loops on 12 cores
\ PROF-TOT moved in the 200 us after prof-off returned in up to 5 of 1000 stops.
\ A tick that lands while a reset clears or a reader sums the counters leaves
\ the header's identity false by one. So the aarch64 handler counts a tick only
\ at ARMED-RUNS, and every word that clears or reads the counters leaves
\ ARMED-RUNS before it stops the timer. The x86-64 twin stores 0 and ARMED-RUNS
\ and never holds: Linux delivers a raised tick before setitimer returns
\ (docs/x86-64.md, the prof-off row).

\ ---- the arena: the pc index the handler searches -----------------------------
\ THE HANDLER NEVER WALKS THE DICTIONARY. It searches a pc-sorted live-range
\ index prof-on builds, so a tick costs log2(entries) compares instead of a scan
\ that grows with the dictionary (measured 240,021 instructions per tick at
\ ndict 15,835 before this index; the walk stopped at the FIRST record holding
\ the pc, so its cost was the hot word's record number, not a constant).
\
\ The band cell PROF-ARENA holds the arena base and every table below it sits at
\ a build-time offset from that base, so the handler reaches any of them with one
\ load and no bound of its own. The arena is mapped once per process and rebuilt
\ in place by each prof-on.
\
\ WHAT THE INDEX ANSWERS, exactly, so a linear reference can state the same rule:
\ the entry with the greatest start <= pc, reported only when pc < that entry's
\ end. Entries are sorted by start with a STABLE merge, so records that share a
\ start (an alias and its original) keep dictionary order and the search lands on
\ the last of them. Records with no code of their own (a namespace row, a retired
\ row, a zero-length body) are not in the index at all.
128 constant ARN-HDR        \ arena header bytes, then the index
0  constant ARN-COUNT       \ index entries
8  constant ARN-LO          \ lowest code address in the index
16 constant ARN-HI          \ one past the highest
24 constant ARN-NDICT       \ the record count the index was built from
32 constant ARN-NEW         \ samples at or above ARN-HI: code compiled after the build
40 constant ARN-DROP        \ caller edges dropped: the probe window was full
48 constant ARN-FRAMES      \ caller frames attributed out of the machine stack
56 constant ARN-WALK        \ machine-stack cells one sample may scan
64 constant ARN-USEC        \ sampling interval in microseconds, 0 = the 1000 default
72 constant ARN-SPILL       \ new-code samples the deferred buffer had no room for
80 constant ARN-DEFER       \ deferred samples waiting for the next report to name them
88 constant ARN-OLDHI       \ the high mark before a rebuild: what the handler could name
32 constant PROF-ENT        \ index entry bytes
0  constant ENT-START
8  constant ENT-END
16 constant ENT-IDX         \ the dictionary record index, which owns the counter
24 constant ENT-INCL        \ inclusive samples since the last rebuild, folded by it

\ ---- caller edges -------------------------------------------------------------
\ One open-addressed table for every (sampled word, caller) pair, keyed on the
\ two record indices packed into 32 bits, probed at most PROF-CALL-PROBE times so
\ a tick's cost stays bounded; a pair that finds no slot is counted in ARN-DROP
\ and reported, never silently merged into another row.
$10000 constant PROF-CALL-SLOTS
$FFFF  constant PROF-CALL-MASK          \ the slot index, PROF-CALL-SLOTS wide
17 constant PROF-REC-BITS               \ a record index plus the one value above it
$1FFFF constant PROF-REC-MASK
DICT-CAP constant PROF-CALLER-NONE      \ no record index reaches it: the unknown caller
16 constant PROF-CALL-ENT               \ key+1, then the count
8  constant PROF-CALL-PROBE
$9E3779B97F4A7C15 constant PROF-HASH    \ golden-ratio multiplier; the top 16 bits index
64 constant PROF-WALK-CELLS             \ default machine-stack scan, in cells
$FFF constant PROF-PAGE-MASK            \ a sample never reads past its own 4 KiB block
\ ---- deferred samples -----------------------------------------------------------
\ A tick in code compiled after prof-on has no index entry to name it. The handler
\ keeps the pc and the interrupted return address - two cells at a fixed stride,
\ no walk, no allocation - and the next report rebuilds the index from the
\ dictionary as it then stands and replays them, which is the only point at which
\ every word the phase compiled exists. The slot count is sized from the
\ measurement that opened the dot: a 92-second self-build at 1 kHz put 13,991 of
\ 92,000 samples in this bucket, so $40000 slots hold about eighteen such builds;
\ past that a sample is counted in ARN-SPILL and reported, never dropped in
\ silence.
16 constant PROF-DEFER-ENT              \ the sample's pc, then its return address (aarch64: x30)
$40000 constant PROF-DEFER-SLOTS
64 constant PROF-ROWS                   \ rows one report prints: enough that a compiler phase's roots,
                                        \ which carry a large inclusive share on a small exclusive one, reach the report
5  constant PROF-CALLERS                \ caller lines under each row: the top few, not every one
DICT-CAP PROF-ENT * constant ARN-IDX-BYTES
PROF-CALL-SLOTS PROF-CALL-ENT * constant ARN-CALL-BYTES
ARN-HDR constant ARN-IDX                       \ the index itself
ARN-IDX ARN-IDX-BYTES + constant ARN-SCR       \ the merge sort's second half
ARN-SCR ARN-IDX-BYTES + constant ARN-CALL      \ the caller table
DICT-CAP cells constant ARN-INCL-BYTES
PROF-DEFER-SLOTS PROF-DEFER-ENT * constant ARN-DEF-BYTES
ARN-CALL ARN-CALL-BYTES + constant ARN-INCL    \ inclusive samples, one per RECORD
ARN-INCL ARN-INCL-BYTES + constant ARN-STAMP   \ the sample serial each record was last counted in
ARN-STAMP ARN-INCL-BYTES + constant ARN-DEF    \ the deferred samples
ARN-DEF ARN-DEF-BYTES + constant ARN-BYTES
1000000 constant USEC-PER-SEC   \ setitimer refuses a tv_usec at or above it: prof-on splits the interval
14  constant SIGALRM
\ SA_ONSTACK: the handler runs on the thread's alternate signal stack, the one
\ the crash handler runs on (src/habu/crash.f C-SIGNAL-STACK, its x86-64 twin
\ src/habu/boot-x64.f SIGNAL-STACK,).
$18000004 constant LINUX-SA-PROF-FLAGS   \ SA_SIGINFO | SA_ONSTACK | SA_RESTART
40 constant DICT-WL-OFF                   \ the record's wordlist cell (habu1.f BSWL reads the same 40)

\ ---- the refusals and the limit's exit -------------------------------------------
\ A resource prof-on cannot get is fatal: prof-on writes its message to fd 2 and
\ exits PROF-MAP-RC. The clock is such a resource: a refused setitimer would
\ leave a phase that samples nothing and reports zeros, so the arm is refused by
\ name too. The handler's sample that reaches a non-zero PROF-LIM prints the
\ report and exits PROF-LIMIT-RC.
78 constant PROF-MAP-RC
99 constant PROF-LIMIT-RC
: PROFARNMSG$ ( -- ptr u8 n ) S\" hb: prof-on: cannot map the profiler arena\n" ;
: PROFARMMSG$ ( -- ptr u8 n ) S\" hb: prof-on: cannot arm the interval timer\n" ;

\ A negative interval is the caller's error, not a resource the process lacks:
\ prof-rate throws E-PROF-RATE before it maps or stores anything, so a caller
\ that catches it keeps the rate it had. lib/errors.f owns the code; prof.f
\ compiles in the engine-build window, before any lib/ file exists, so the same
\ (code, name) pair is re-registered here -- the one form
\ tools/error-code-lint.f admits -- and test/gate-debug-lib.f
\ GDB-PROFILER-RATE-REFUSED keeps the two spellings equal. Qualify it: a bare
\ E-PROF-RATE under `using PROF-ABI` meets lib/errors.f's global.
-3803 constant E-PROF-RATE

;package
