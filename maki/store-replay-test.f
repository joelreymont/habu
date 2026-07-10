\ maki/store-replay-test.f - checked tests for the durable replay backing (dot cad-5).
\ SK-PUT-DURABLE writes memory + schedules.rows; STORE-REPLAY-LOAD rehydrates the
\ in-memory table from the file (latest row per key wins); the load is capacity-guarded
\ (E-SK-FULL past SK-TAB-CAP). Writes only under the store root; STORE-RESET cleans up.

require lib/test.f
require lib/string.f
require maki/store-replay.f

package MAKI

\ ---- one gelu/relu elementwise chain over a single input (sched-key fixture) --
: SRT-BUILD ( n n -- ) {: rows:n cols:n :}
   MIR-RESET
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   OP-GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   OP-RELU MIR-OP-BEGIN 0 MIR-IN+        rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

\ ---- capacity fixtures: 33 distinct synthetic schedule rows (> SK-TAB-CAP) -----
: SRT-WRITE ( n -- ) {: k:n :}
   k 0 ?do  SB-RESET s" sk" SB-APPEND i SB-INT SB$ i SCHED-PUT  loop ;
: SRT-CAP-LOAD ( -- )  SK-TAB-RESET STORE-REPLAY-LOAD ;

T-RESET

\ ---- durable put lands in both the table and schedules.rows -----------------
STORE-RESET  SK-TAB-RESET
2 100 SRT-BUILD  0 MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
0 SK-KEY$ 7 SK-PUT-DURABLE
0 SK-KEY$ SK-GET drop 7 T=            \ hot table
SK-TAB-COUNT 1 T=
0 SK-KEY$ SCHED-GET drop 7 T=         \ durable file

\ ---- reload durability into a cleared table ---------------------------------
SK-TAB-RESET
SK-TAB-COUNT 0 T=
0 SK-KEY$ SK-GET nip TFALSE           \ table empty after reset
STORE-REPLAY-LOAD
0 SK-KEY$ SK-GET drop 7 T=            \ file -> memory
SK-TAB-COUNT 1 T=

\ ---- latest row per key wins on reload --------------------------------------
0 SK-KEY$ 9 SK-PUT-DURABLE            \ memory update-in-place + newer file row
SK-TAB-RESET
STORE-REPLAY-LOAD
0 SK-KEY$ SK-GET drop 9 T=
SK-TAB-COUNT 1 T=                     \ still one distinct key

\ ---- capacity-guarded load (33 distinct rows > SK-TAB-CAP=32) ---------------
STORE-RESET  SK-TAB-RESET
33 SRT-WRITE
' SRT-CAP-LOAD E-SK-FULL TTHROWS

STORE-RESET
T-REPORT

end-package
