\ prof-abi.f - the sampling profiler's band and arena layout, package PROF-ABI.
\ Build-only, as data-claims.f is: layout.f is baked into every runtime image
\ and none of this is read at run time. Each target's handler and the report
\ meet only in memory (the band at the top of DATA, the arena whose base the
\ band holds); every offset, size, signal fact, exit status and message of that
\ meeting is here once. Each handler's frame layout stays with its emitter.
require src/habu/layout.f

package PROF-ABI
public
\ The seed rewind body is emitted before prof.f loads. Its helper label must
\ therefore be declared in this early ABI package and assigned by habu2.f.
variable LPROFREWIND
variable LPROFTRIM
variable LPROFRECLAIM

\ The band: PROF-STATE-BYTES of state cells, then one counter per dict record.
\ The band closes the DATA region: it starts PROF-CNT-BYTES below DATA's end.
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
\ A tick during a reset or a counter sum leaves the header's identity false by
\ one. The aarch64 handler counts a tick only at ARMED-RUNS, and every word that
\ clears or reads the counters leaves ARMED-RUNS before it stops the timer. The
\ x86-64 twin stores 0 and ARMED-RUNS and never holds: Linux delivers a raised
\ tick before setitimer returns (src/habu/prof-x64.f, prof-off).

\ The arena holds the pc index the handler searches.
\ THE HANDLER NEVER WALKS THE DICTIONARY: it searches a pc-sorted live-range
\ index prof-on builds, log2(entries) compares per tick (a walk measured 240,021
\ instructions per tick at ndict 15,835). PROF-ARENA holds the arena base; every
\ table sits at a build-time offset from it, reached with one load and no bound.
\ Mapped once per process, rebuilt in place by each prof-on.
\
\ The index answers the entry with the greatest start <= pc, only when pc < that
\ entry's end. A STABLE merge by start keeps records sharing a start (an alias
\ and its original) in dictionary order; the search lands on the last of them.
\ Records with no code (a namespace row, a retired row, a zero-length body) are
\ not in the index.
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
96 constant ARN-IDS         \ profile identities issued this session, at most DICT-CAP
104 constant ARN-CHUNKS      \ linked chunks holding copied names
32 constant PROF-ENT        \ index entry bytes
0  constant ENT-START
8  constant ENT-END
16 constant ENT-IDX         \ stable profile ID, which owns the counter
24 constant ENT-INCL        \ inclusive samples since the last rebuild, folded by it

\ One open-addressed table for every (sampled word, caller) pair, keyed on the
\ two record indices packed into one cell, probed at most PROF-CALL-PROBE times;
\ a pair that finds no slot is counted in ARN-DROP and reported, never merged
\ into another row.
$10000 constant PROF-CALL-SLOTS
$FFFF  constant PROF-CALL-MASK          \ the slot index, PROF-CALL-SLOTS wide
18 constant PROF-REC-BITS               \ a record index plus the one value above it
$3FFFF constant PROF-REC-MASK
DICT-CAP constant PROF-CALLER-NONE      \ no record index reaches it: the unknown caller
16 constant PROF-CALL-ENT               \ key+1, then the count
8  constant PROF-CALL-PROBE
$9E3779B97F4A7C15 constant PROF-HASH    \ golden-ratio multiplier; the top 16 bits index
64 constant PROF-WALK-CELLS             \ default machine-stack scan, in cells
$FFF constant PROF-PAGE-MASK            \ a sample never reads past its own 4 KiB block
\ A tick in code compiled after prof-on has no index entry. The handler keeps the
\ pc and the interrupted return address (two cells, fixed stride, no walk, no
\ allocation); the next report rebuilds the index and replays them, the only
\ point at which every word the phase compiled exists. Sizing: a 92-second
\ self-build at 1 kHz put 13,991 of 92,000 samples here, so $40000 slots hold
\ about eighteen such builds; past that a sample is counted in ARN-SPILL.
16 constant PROF-DEFER-ENT              \ the sample's pc, then its return address (aarch64: x30)
$40000 constant PROF-DEFER-SLOTS
64 constant PROF-ROWS                   \ rows one report prints: enough that a compiler phase's roots,
                                        \ which carry a large inclusive share on a small exclusive one, reach the report
5  constant PROF-CALLERS                \ caller lines under each row: the top few, not every one
DICT-CAP PROF-ENT * constant ARN-IDX-BYTES
PROF-CALL-SLOTS PROF-CALL-ENT * constant ARN-CALL-BYTES
ARN-HDR constant ARN-IDX                       \ the index itself
ARN-IDX ARN-IDX-BYTES + constant ARN-SCR       \ the merge sort's second half
\ Before sorting, SNAP uses the scratch half to map each namespace WID to its
\ first dictionary row. A cell packs WID (low 32) and row + 1 (high 32); zero
\ is empty. At most two WIDs per row occupy at most half these slots.
ARN-IDX-BYTES CELL / constant PROF-QUAL-SLOTS
PROF-QUAL-SLOTS 1- constant PROF-QUAL-MASK
32 constant PROF-QUAL-BITS
ARN-SCR ARN-IDX-BYTES + constant ARN-CALL      \ the caller table
DICT-CAP cells constant ARN-INCL-BYTES
PROF-DEFER-SLOTS PROF-DEFER-ENT * constant ARN-DEF-BYTES
ARN-CALL ARN-CALL-BYTES + constant ARN-INCL    \ inclusive samples, one per RECORD
ARN-INCL ARN-INCL-BYTES + constant ARN-STAMP   \ the sample serial each record was last counted in
ARN-STAMP ARN-INCL-BYTES + constant ARN-DEF    \ the deferred samples
\ One identity owns its copied name and code span even after its dictionary
\ ordinal is reused. RID-OWNER is -1 when there is no current dictionary owner:
\ a nonempty stored span is seed-retained code, an empty span is excluded.
\ Ordinary retired records keep their owner but have no live index entry.
40 constant PROF-ID-ENT
0  constant RID-OWNER
8  constant RID-NAME
16 constant RID-LEN
24 constant RID-START
32 constant RID-END
DICT-CAP PROF-ID-ENT * constant ARN-RECS-BYTES
DICT-CAP cells constant ARN-LIVE-BYTES
ARN-DEF ARN-DEF-BYTES + constant ARN-RECS
ARN-RECS ARN-RECS-BYTES + constant ARN-LIVE
ARN-LIVE ARN-LIVE-BYTES + constant ARN-BYTES
\ Exact-length name copies live in mmap chunks linked from ARN-CHUNKS. A name
\ larger than the ordinary chunk gets a chunk large enough for that name.
$10000 constant PROF-NAME-CHUNK
0  constant CHUNK-NEXT
8  constant CHUNK-USED
16 constant CHUNK-LEN
24 constant CHUNK-DATA
1000000 constant USEC-PER-SEC   \ setitimer refuses a tv_usec at or above it: prof-on splits the interval
14  constant SIGALRM
\ SA_ONSTACK: the handler runs on the thread's alternate signal stack, the one
\ the crash handler runs on (src/habu/crash.f C-SIGNAL-STACK, its x86-64 twin
\ src/habu/boot-x64.f SIGNAL-STACK,).
$18000004 constant LINUX-SA-PROF-FLAGS   \ SA_SIGINFO | SA_ONSTACK | SA_RESTART
40 constant DICT-WL-OFF                   \ the record's wordlist cell (habu1.f BSWL reads the same 40)

\ A resource prof-on cannot get is fatal: its message goes to fd 2 and it exits
\ PROF-MAP-RC. A refused setitimer is such a resource: it would leave a phase
\ that samples nothing and reports zeros. The sample that reaches a non-zero
\ PROF-LIM prints the report and exits PROF-LIMIT-RC.
78 constant PROF-MAP-RC
99 constant PROF-LIMIT-RC
: PROFARNMSG$ ( -- ptr u8 n ) S\" hb: prof-on: cannot map the profiler arena\n" ;
: PROFARMMSG$ ( -- ptr u8 n ) S\" hb: prof-on: cannot arm the interval timer\n" ;
: PROFIDMSG$ ( -- ptr u8 n ) S\" hb: profiler profile IDs exhausted\n" ;
: PROFNAMEMSG$ ( -- ptr u8 n ) S\" hb: profiler cannot map name snapshot\n" ;
: PROFNAMEFREEMSG$ ( -- ptr u8 n ) S\" hb: profiler cannot release name snapshot\n" ;

\ A negative interval is the caller's error: prof-rate throws E-PROF-RATE before
\ it maps or stores anything, so a caller that catches it keeps its rate.
\ src/lib/errors.f owns the code; prof.f compiles before any src/lib/ file
\ exists, so the (code, name) pair is re-registered here, the one form
\ tools/error-code-lint.f admits, and test/gate-debug-lib.f
\ GDB-PROFILER-RATE-REFUSED keeps the spellings equal. Qualify it: a bare
\ E-PROF-RATE under `using PROF-ABI` meets src/lib/errors.f's global.
-3803 constant E-PROF-RATE

;package
