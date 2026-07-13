\ maki/store-replay-test.f - checked tests for the durable replay backing (dot cad-5).
\ SK-PUT-DURABLE writes memory + schedules.rows; STORE-REPLAY-LOAD rehydrates the
\ in-memory table from the file (latest row per key wins); the table GROWS on demand,
\ so a store with more distinct keys than the boot capacity rehydrates without a wall
\ (dot habu-maki-sk-table-59bb1d4d). The session latch (REPLAY-READY? / REPLAY-ENSURE)
\ merges the file once per process, before the first lookup and before the first
\ durable write (load-before-first-write: rehydration never clobbers a fresher
\ production row), and a missing store is a clean no-op. Writes only under the store
\ root; STORE-RESET + SK-TAB-RESET + REPLAY-RESET leave no rows, table entries, or
\ warm latch behind.

require lib/test.f
require lib/string.f
require maki/store-replay.f

package MAKI

\ ---- one gelu/relu elementwise chain over a single input (sched-key fixture) --
: SRT-BUILD ( n n -- ) {: rows:n cols:n :}
   MIR-RESET
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:RELU MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

\ ---- growth fixtures: 33 distinct synthetic keys (> the 32-entry boot table) --
: SRT-KEY$ ( n -- ptr u8 n ) {: k:n :}  SB-RESET s" sk" SB-APPEND k SB-INT SB$ ;
: SRT-DPUT ( n -- ) {: k:n :}  k SRT-KEY$ k SK-PUT-DURABLE ;
: SRT-DFILL ( n -- ) {: k:n :}  k 0 ?do i SRT-DPUT loop ;
: SRT-GET ( n -- n bool )  SRT-KEY$ SK-GET ;

T-RESET

\ ---- durable put lands in both the table and schedules.rows -----------------
STORE-RESET  SK-TAB-RESET
2 100 SRT-BUILD  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 7 SK-PUT-DURABLE
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 7 T=            \ hot table
SK-TAB-COUNT 1 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SCHED-GET drop 7 T=         \ durable file

\ ---- reload durability into a cleared table ---------------------------------
SK-TAB-RESET
SK-TAB-COUNT 0 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET nip TFALSE           \ table empty after reset
STORE-REPLAY-LOAD
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 7 T=            \ file -> memory
SK-TAB-COUNT 1 T=

\ ---- latest row per key wins on reload --------------------------------------
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 9 SK-PUT-DURABLE            \ memory update-in-place + newer file row
SK-TAB-RESET
STORE-REPLAY-LOAD
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 9 T=
SK-TAB-COUNT 1 T=                     \ still one distinct key

\ ---- session latch: merge once per process, never re-merge -------------------
REPLAY-RESET
REPLAY-READY? TFALSE
SK-TAB-RESET
REPLAY-ENSURE                                                  \ cold latch -> merge the file rows
REPLAY-READY? TTRUE
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 9 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 11 SK-PUT                   \ fresher memory-only row
REPLAY-ENSURE                                                  \ warm latch: no re-merge...
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 11 T=           \ ...so the live row survives

\ ---- load-before-first-write: a durable put is never clobbered by rehydration -
REPLAY-RESET  SK-TAB-RESET
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 11 SK-PUT-DURABLE           \ ensures (merges 9) BEFORE writing 11
REPLAY-READY? TTRUE                                            \ the durable put warmed the latch
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 11 T=           \ the fresh row wins in memory
SK-TAB-RESET  STORE-REPLAY-LOAD
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 11 T=           \ and is the latest durable row

\ ---- fail-safe miss: ENSURE without a store is a clean no-op ------------------
STORE-RESET  SK-TAB-RESET  REPLAY-RESET
REPLAY-ENSURE
REPLAY-READY? TTRUE
SK-TAB-COUNT 0 T=

\ ---- >32 distinct keys through the production write path: no capacity wall ---
\ 33 SK-PUT-DURABLE puts exceed the 32-entry boot table; the table grows (no
\ throw), every selection replays from memory, and a cleared table rehydrates
\ ALL 33 durable rows - the in-memory mirror keeps the whole store, latest wins.
STORE-RESET  SK-TAB-RESET  REPLAY-RESET
33 SRT-DFILL
SK-TAB-COUNT 33 T=
0  SRT-GET TTRUE 0  T=                     \ first key survives the growth relocation
32 SRT-GET TTRUE 32 T=                     \ the key past the boot capacity landed
32 SRT-KEY$ 77 SK-PUT-DURABLE              \ supersede one key past the boot boundary
SK-TAB-COUNT 33 T=                         \ update in place, no duplicate entry
SK-TAB-RESET  STORE-REPLAY-LOAD            \ cold rebuild: all 34 rows replay into the table
SK-TAB-COUNT 33 T=
0  SRT-GET TTRUE 0  T=
32 SRT-GET TTRUE 77 T=                     \ latest durable row wins for the superseded key

STORE-RESET  SK-TAB-RESET  REPLAY-RESET   \ leave no rows, table entries, or warm latch
T-REPORT

;package
