\ code-window.f - the code region's write window (src/habu/habu1.f
\ EMIT-PROT-WINDOW). Two claims, each of which a plausible edit to the engine
\ falsifies.
\
\ 1. THE TAIL IS EXECUTE-ONLY AT REST. Above an open window there is no window,
\ and the region was flipped RX whole at the end of EM-STARTUP, so a raw store
\ anywhere above CP dies in the crash handler. The read control pins that the
\ refusal is about writability and not about the address being unmapped. The
\ narrowing this design replaced - a close that recomputed its range from the CP
\ it happened to see, and so never RXed the tail - passes the read and fails
\ every store above CP.
\
\ 2. EVERY DIRECT SPILL AT CP DECLARES ITS BYTES. Instructions reach the region
\ through LCEMIT, which grows the window itself. The string literals, the escaped
\ literals, a definition name past DNAME-INL and the defer metadata write BYTES
\ straight at CP, so each must call PROT:RESERVE first. Each sweep below parks CP
\ a fixed distance under a window page boundary and compiles one definition of
\ that kind, so the spill begins inside the open window and runs past its end.
\ The distance is SWEPT, in 4-byte steps, because what decides which distance
\ straddles is how many instructions that definer emits BEFORE its spill - the
\ sweep never has to know that number and stays correct when it changes. The
\ literal and name sweeps run to 128 bytes; the MATCH die sweep runs to 400,
\ because that spill sits about 250 bytes into the word.
\
\ Delete any one PROT:RESERVE in the engine and this file dies compiling itself:
\ each of the seven was checked that way, and the four literal kinds plus the
\ MATCH die are caught here alone (the long name and the defer metadata break the
\ engine's own build before this file even runs).
\
\ The cases deliberately overlap on ONE page - CW-EDGE measures from a fixed mark,
\ not from the moving CP - so the sweeps cost the region two pages instead of
\ hundreds. Only the last body on the page survives intact, which is why the
\ behaviour assertions use fresh definitions compiled after CW-RESTORE.

require lib/errors.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/process.f

package CODE-WINDOW-TEST

$800 constant CAP
20000 constant TIMEOUT-MS
\ The engine's crash handler prints the register dump and exits 134 itself
\ (src/habu/crash.f EMIT-CRASH-HANDLER), so a refused write is an EXIT on both
\ targets, not a signal.
134 constant CRASH-RC

create OUT CAP allot
create ERR CAP allot

variable CW-BASE

: CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE want T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

: TRAPS ( ptr u8 n -- )    CRASH-RC EXPECT ;

: ACCEPTS ( ptr u8 n -- )  0 EXPECT ;

: TEST-TAIL-AT-REST ( -- )
   s" a raw store above CP dies: the region tail is RX at rest" T-LABEL
   s" TRUSTED: CW-FAR ( -- ) 0 cp@ $40000 + ! ; CW-FAR" TRAPS
   s" TRUSTED: CW-PAGE ( -- ) 0 cp@ PROT-PAGE-MAX + ! ; CW-PAGE" TRAPS
   s" TRUSTED: CW-AT ( -- ) 0 cp@ ! ; CW-AT" TRAPS
   s" the same address reads, so the refusal is about writability" T-LABEL
   s" TRUSTED: CW-READ ( -- ) cp@ $40000 + @ drop ; CW-READ" ACCEPTS
   s" the live code and the dictionary below CP are RX at rest too" T-LABEL
   s" TRUSTED: CW-LIVE ( -- ) 0 dbase@ DICT-SIZE + ! ; CW-LIVE" TRAPS
   s" TRUSTED: CW-DICT ( -- ) 0 dbase@ ! ; CW-DICT" TRAPS ;

\ Park CP `pad` bytes below the first window page boundary at least one whole
\ page above the mark, so no case can reach the words compiled before the mark.
: CW-MARK ( -- )  cp@ CW-BASE ! ;

: CW-EDGE ( n -- ) {: pad:n :}
   CW-BASE @ PROT-PAGE-MAX + PROT-PAGE-MAX + PROT-PAGE-MAX 1- invert and pad - cp! ;

: CW-RESTORE ( -- )
   CW-BASE @ PROT-PAGE-MAX + PROT-PAGE-MAX + PROT-PAGE-MAX 1- invert and
   PROT-PAGE-MAX + cp! ;

CW-MARK

\ s" : the literal bytes are copied straight at CP (habu2.f C-SDQ).
4   CW-EDGE  : CW-S-004 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
8   CW-EDGE  : CW-S-008 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
12  CW-EDGE  : CW-S-012 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
16  CW-EDGE  : CW-S-016 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
20  CW-EDGE  : CW-S-020 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
24  CW-EDGE  : CW-S-024 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
28  CW-EDGE  : CW-S-028 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
32  CW-EDGE  : CW-S-032 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
36  CW-EDGE  : CW-S-036 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
40  CW-EDGE  : CW-S-040 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
44  CW-EDGE  : CW-S-044 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
48  CW-EDGE  : CW-S-048 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
52  CW-EDGE  : CW-S-052 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
56  CW-EDGE  : CW-S-056 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
60  CW-EDGE  : CW-S-060 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
64  CW-EDGE  : CW-S-064 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
68  CW-EDGE  : CW-S-068 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
72  CW-EDGE  : CW-S-072 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
76  CW-EDGE  : CW-S-076 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
80  CW-EDGE  : CW-S-080 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
84  CW-EDGE  : CW-S-084 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
88  CW-EDGE  : CW-S-088 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
92  CW-EDGE  : CW-S-092 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
96  CW-EDGE  : CW-S-096 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
100 CW-EDGE  : CW-S-100 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
104 CW-EDGE  : CW-S-104 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
108 CW-EDGE  : CW-S-108 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
112 CW-EDGE  : CW-S-112 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
116 CW-EDGE  : CW-S-116 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
120 CW-EDGE  : CW-S-120 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
124 CW-EDGE  : CW-S-124 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
128 CW-EDGE  : CW-S-128 ( -- ptr u8 n ) s" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;

\ s\" : the same copy, through the escape decoder (habu2.f C-ESDQ).
4   CW-EDGE  : CW-ES-004 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
8   CW-EDGE  : CW-ES-008 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
12  CW-EDGE  : CW-ES-012 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
16  CW-EDGE  : CW-ES-016 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
20  CW-EDGE  : CW-ES-020 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
24  CW-EDGE  : CW-ES-024 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
28  CW-EDGE  : CW-ES-028 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
32  CW-EDGE  : CW-ES-032 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
36  CW-EDGE  : CW-ES-036 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
40  CW-EDGE  : CW-ES-040 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
44  CW-EDGE  : CW-ES-044 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
48  CW-EDGE  : CW-ES-048 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
52  CW-EDGE  : CW-ES-052 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
56  CW-EDGE  : CW-ES-056 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
60  CW-EDGE  : CW-ES-060 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
64  CW-EDGE  : CW-ES-064 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
68  CW-EDGE  : CW-ES-068 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
72  CW-EDGE  : CW-ES-072 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
76  CW-EDGE  : CW-ES-076 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
80  CW-EDGE  : CW-ES-080 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
84  CW-EDGE  : CW-ES-084 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
88  CW-EDGE  : CW-ES-088 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
92  CW-EDGE  : CW-ES-092 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
96  CW-EDGE  : CW-ES-096 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
100 CW-EDGE  : CW-ES-100 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
104 CW-EDGE  : CW-ES-104 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
108 CW-EDGE  : CW-ES-108 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
112 CW-EDGE  : CW-ES-112 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
116 CW-EDGE  : CW-ES-116 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
120 CW-EDGE  : CW-ES-120 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
124 CW-EDGE  : CW-ES-124 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
128 CW-EDGE  : CW-ES-128 ( -- ptr u8 n ) s\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;

\ c" : a count byte and then the bytes, at CP (habu2.f C-CQ).
4   CW-EDGE  : CW-C-004 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
8   CW-EDGE  : CW-C-008 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
12  CW-EDGE  : CW-C-012 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
16  CW-EDGE  : CW-C-016 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
20  CW-EDGE  : CW-C-020 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
24  CW-EDGE  : CW-C-024 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
28  CW-EDGE  : CW-C-028 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
32  CW-EDGE  : CW-C-032 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
36  CW-EDGE  : CW-C-036 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
40  CW-EDGE  : CW-C-040 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
44  CW-EDGE  : CW-C-044 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
48  CW-EDGE  : CW-C-048 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
52  CW-EDGE  : CW-C-052 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
56  CW-EDGE  : CW-C-056 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
60  CW-EDGE  : CW-C-060 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
64  CW-EDGE  : CW-C-064 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
68  CW-EDGE  : CW-C-068 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
72  CW-EDGE  : CW-C-072 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
76  CW-EDGE  : CW-C-076 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
80  CW-EDGE  : CW-C-080 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
84  CW-EDGE  : CW-C-084 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
88  CW-EDGE  : CW-C-088 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
92  CW-EDGE  : CW-C-092 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
96  CW-EDGE  : CW-C-096 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
100 CW-EDGE  : CW-C-100 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
104 CW-EDGE  : CW-C-104 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
108 CW-EDGE  : CW-C-108 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
112 CW-EDGE  : CW-C-112 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
116 CW-EDGE  : CW-C-116 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
120 CW-EDGE  : CW-C-120 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
124 CW-EDGE  : CW-C-124 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
128 CW-EDGE  : CW-C-128 ( -- ptr u8 ) c" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;

\ c\" : the counted form through the escape decoder (habu2.f C-ECQ).
4   CW-EDGE  : CW-EC-004 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
8   CW-EDGE  : CW-EC-008 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
12  CW-EDGE  : CW-EC-012 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
16  CW-EDGE  : CW-EC-016 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
20  CW-EDGE  : CW-EC-020 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
24  CW-EDGE  : CW-EC-024 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
28  CW-EDGE  : CW-EC-028 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
32  CW-EDGE  : CW-EC-032 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
36  CW-EDGE  : CW-EC-036 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
40  CW-EDGE  : CW-EC-040 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
44  CW-EDGE  : CW-EC-044 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
48  CW-EDGE  : CW-EC-048 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
52  CW-EDGE  : CW-EC-052 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
56  CW-EDGE  : CW-EC-056 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
60  CW-EDGE  : CW-EC-060 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
64  CW-EDGE  : CW-EC-064 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
68  CW-EDGE  : CW-EC-068 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
72  CW-EDGE  : CW-EC-072 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
76  CW-EDGE  : CW-EC-076 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
80  CW-EDGE  : CW-EC-080 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
84  CW-EDGE  : CW-EC-084 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
88  CW-EDGE  : CW-EC-088 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
92  CW-EDGE  : CW-EC-092 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
96  CW-EDGE  : CW-EC-096 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
100 CW-EDGE  : CW-EC-100 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
104 CW-EDGE  : CW-EC-104 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
108 CW-EDGE  : CW-EC-108 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
112 CW-EDGE  : CW-EC-112 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
116 CW-EDGE  : CW-EC-116 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
120 CW-EDGE  : CW-EC-120 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
124 CW-EDGE  : CW-EC-124 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
128 CW-EDGE  : CW-EC-128 ( -- ptr u8 ) c\" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;

\ a definition name past DNAME-INL spills at CP (habu2.f C-STORE-NAME).
4   CW-EDGE  : CW-NAME-004-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
8   CW-EDGE  : CW-NAME-008-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
12  CW-EDGE  : CW-NAME-012-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
16  CW-EDGE  : CW-NAME-016-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
20  CW-EDGE  : CW-NAME-020-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
24  CW-EDGE  : CW-NAME-024-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
28  CW-EDGE  : CW-NAME-028-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
32  CW-EDGE  : CW-NAME-032-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
36  CW-EDGE  : CW-NAME-036-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
40  CW-EDGE  : CW-NAME-040-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
44  CW-EDGE  : CW-NAME-044-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
48  CW-EDGE  : CW-NAME-048-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
52  CW-EDGE  : CW-NAME-052-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
56  CW-EDGE  : CW-NAME-056-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
60  CW-EDGE  : CW-NAME-060-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
64  CW-EDGE  : CW-NAME-064-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
68  CW-EDGE  : CW-NAME-068-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
72  CW-EDGE  : CW-NAME-072-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
76  CW-EDGE  : CW-NAME-076-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
80  CW-EDGE  : CW-NAME-080-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
84  CW-EDGE  : CW-NAME-084-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
88  CW-EDGE  : CW-NAME-088-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
92  CW-EDGE  : CW-NAME-092-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
96  CW-EDGE  : CW-NAME-096-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
100 CW-EDGE  : CW-NAME-100-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
104 CW-EDGE  : CW-NAME-104-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
108 CW-EDGE  : CW-NAME-108-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
112 CW-EDGE  : CW-NAME-112-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
116 CW-EDGE  : CW-NAME-116-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
120 CW-EDGE  : CW-NAME-120-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
124 CW-EDGE  : CW-NAME-124-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;
128 CW-EDGE  : CW-NAME-128-nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ( -- ) ;

\ a defer writes its magic and dispatch cell at CP, 16 bytes past the body
\ it has just emitted (habu2.f C-DEFER-META-WRITE).
4   CW-EDGE  defer CW-D-004 ( -- )
8   CW-EDGE  defer CW-D-008 ( -- )
12  CW-EDGE  defer CW-D-012 ( -- )
16  CW-EDGE  defer CW-D-016 ( -- )
20  CW-EDGE  defer CW-D-020 ( -- )
24  CW-EDGE  defer CW-D-024 ( -- )
28  CW-EDGE  defer CW-D-028 ( -- )
32  CW-EDGE  defer CW-D-032 ( -- )
36  CW-EDGE  defer CW-D-036 ( -- )
40  CW-EDGE  defer CW-D-040 ( -- )
44  CW-EDGE  defer CW-D-044 ( -- )
48  CW-EDGE  defer CW-D-048 ( -- )
52  CW-EDGE  defer CW-D-052 ( -- )
56  CW-EDGE  defer CW-D-056 ( -- )
60  CW-EDGE  defer CW-D-060 ( -- )
64  CW-EDGE  defer CW-D-064 ( -- )
68  CW-EDGE  defer CW-D-068 ( -- )
72  CW-EDGE  defer CW-D-072 ( -- )
76  CW-EDGE  defer CW-D-076 ( -- )
80  CW-EDGE  defer CW-D-080 ( -- )
84  CW-EDGE  defer CW-D-084 ( -- )
88  CW-EDGE  defer CW-D-088 ( -- )
92  CW-EDGE  defer CW-D-092 ( -- )
96  CW-EDGE  defer CW-D-096 ( -- )
100 CW-EDGE  defer CW-D-100 ( -- )
104 CW-EDGE  defer CW-D-104 ( -- )
108 CW-EDGE  defer CW-D-108 ( -- )
112 CW-EDGE  defer CW-D-112 ( -- )
116 CW-EDGE  defer CW-D-116 ( -- )
120 CW-EDGE  defer CW-D-120 ( -- )
124 CW-EDGE  defer CW-D-124 ( -- )
128 CW-EDGE  defer CW-D-128 ( -- )


\ a MATCH compiles its invalid-tag die INLINE into the word, message bytes and
\ all, at CP (habu2.f C-DIE-BAD-TAG). The die sits at the END of the MATCH, about
\ 250 bytes into it, so this sweep runs further under the boundary than the
\ others - the distance a spill sits from the definition's first word is exactly
\ what the sweep is not allowed to assume.
4   CW-EDGE  : CW-BT-004 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
8   CW-EDGE  : CW-BT-008 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
12  CW-EDGE  : CW-BT-012 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
16  CW-EDGE  : CW-BT-016 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
20  CW-EDGE  : CW-BT-020 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
24  CW-EDGE  : CW-BT-024 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
28  CW-EDGE  : CW-BT-028 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
32  CW-EDGE  : CW-BT-032 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
36  CW-EDGE  : CW-BT-036 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
40  CW-EDGE  : CW-BT-040 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
44  CW-EDGE  : CW-BT-044 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
48  CW-EDGE  : CW-BT-048 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
52  CW-EDGE  : CW-BT-052 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
56  CW-EDGE  : CW-BT-056 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
60  CW-EDGE  : CW-BT-060 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
64  CW-EDGE  : CW-BT-064 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
68  CW-EDGE  : CW-BT-068 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
72  CW-EDGE  : CW-BT-072 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
76  CW-EDGE  : CW-BT-076 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
80  CW-EDGE  : CW-BT-080 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
84  CW-EDGE  : CW-BT-084 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
88  CW-EDGE  : CW-BT-088 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
92  CW-EDGE  : CW-BT-092 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
96  CW-EDGE  : CW-BT-096 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
100 CW-EDGE  : CW-BT-100 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
104 CW-EDGE  : CW-BT-104 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
108 CW-EDGE  : CW-BT-108 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
112 CW-EDGE  : CW-BT-112 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
116 CW-EDGE  : CW-BT-116 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
120 CW-EDGE  : CW-BT-120 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
124 CW-EDGE  : CW-BT-124 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
128 CW-EDGE  : CW-BT-128 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
132 CW-EDGE  : CW-BT-132 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
136 CW-EDGE  : CW-BT-136 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
140 CW-EDGE  : CW-BT-140 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
144 CW-EDGE  : CW-BT-144 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
148 CW-EDGE  : CW-BT-148 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
152 CW-EDGE  : CW-BT-152 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
156 CW-EDGE  : CW-BT-156 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
160 CW-EDGE  : CW-BT-160 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
164 CW-EDGE  : CW-BT-164 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
168 CW-EDGE  : CW-BT-168 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
172 CW-EDGE  : CW-BT-172 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
176 CW-EDGE  : CW-BT-176 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
180 CW-EDGE  : CW-BT-180 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
184 CW-EDGE  : CW-BT-184 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
188 CW-EDGE  : CW-BT-188 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
192 CW-EDGE  : CW-BT-192 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
196 CW-EDGE  : CW-BT-196 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
200 CW-EDGE  : CW-BT-200 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
204 CW-EDGE  : CW-BT-204 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
208 CW-EDGE  : CW-BT-208 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
212 CW-EDGE  : CW-BT-212 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
216 CW-EDGE  : CW-BT-216 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
220 CW-EDGE  : CW-BT-220 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
224 CW-EDGE  : CW-BT-224 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
228 CW-EDGE  : CW-BT-228 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
232 CW-EDGE  : CW-BT-232 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
236 CW-EDGE  : CW-BT-236 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
240 CW-EDGE  : CW-BT-240 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
244 CW-EDGE  : CW-BT-244 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
248 CW-EDGE  : CW-BT-248 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
252 CW-EDGE  : CW-BT-252 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
256 CW-EDGE  : CW-BT-256 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
260 CW-EDGE  : CW-BT-260 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
264 CW-EDGE  : CW-BT-264 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
268 CW-EDGE  : CW-BT-268 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
272 CW-EDGE  : CW-BT-272 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
276 CW-EDGE  : CW-BT-276 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
280 CW-EDGE  : CW-BT-280 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
284 CW-EDGE  : CW-BT-284 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
288 CW-EDGE  : CW-BT-288 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
292 CW-EDGE  : CW-BT-292 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
296 CW-EDGE  : CW-BT-296 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
300 CW-EDGE  : CW-BT-300 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
304 CW-EDGE  : CW-BT-304 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
308 CW-EDGE  : CW-BT-308 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
312 CW-EDGE  : CW-BT-312 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
316 CW-EDGE  : CW-BT-316 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
320 CW-EDGE  : CW-BT-320 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
324 CW-EDGE  : CW-BT-324 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
328 CW-EDGE  : CW-BT-328 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
332 CW-EDGE  : CW-BT-332 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
336 CW-EDGE  : CW-BT-336 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
340 CW-EDGE  : CW-BT-340 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
344 CW-EDGE  : CW-BT-344 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
348 CW-EDGE  : CW-BT-348 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
352 CW-EDGE  : CW-BT-352 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
356 CW-EDGE  : CW-BT-356 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
360 CW-EDGE  : CW-BT-360 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
364 CW-EDGE  : CW-BT-364 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
368 CW-EDGE  : CW-BT-368 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
372 CW-EDGE  : CW-BT-372 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
376 CW-EDGE  : CW-BT-376 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
380 CW-EDGE  : CW-BT-380 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
384 CW-EDGE  : CW-BT-384 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
388 CW-EDGE  : CW-BT-388 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
392 CW-EDGE  : CW-BT-392 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
396 CW-EDGE  : CW-BT-396 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;
400 CW-EDGE  : CW-BT-400 ( outcome -- n ) MATCH outcome exited OF drop 1 ENDOF signaled OF drop 2 ENDOF timeout OF 3 ENDOF ;MATCH ;

CW-RESTORE

\ The literal kinds compiled where the compiler would ordinarily put them, so the
\ sweep above is not the only evidence that they still produce right answers.
\ The defer's metadata has no readable value of its own; its sweep is its test.
: CW-OK-S ( -- ptr u8 n ) s" abcdefghijklmnopqrstuvwxyz" ;
: CW-OK-ES ( -- ptr u8 n ) s\" abc\x41def" ;
: CW-OK-C ( -- ptr u8 ) c" abcdefghijklmnopqrstuvwxyz" ;
: CW-OK-EC ( -- ptr u8 ) c\" abc\x41def" ;
: CW-OK-NAME-THAT-IS-LONGER-THAN-DNAME-INL ( -- n ) 7 ;

: TEST-SPILLED-WORDS ( -- )
   s" every spilled literal still reads back what it holds" T-LABEL
   CW-OK-S nip 26 T=
   CW-OK-ES nip 7 T=
   CW-OK-C c@ 26 T=
   CW-OK-EC c@ 7 T=
   CW-OK-NAME-THAT-IS-LONGER-THAN-DNAME-INL 7 T= ;

public

: RUN ( -- )
   T-RESET
   TEST-TAIL-AT-REST
   TEST-SPILLED-WORDS
   T-REPORT
   s" code-window: ok" type cr ;

;package

CODE-WINDOW-TEST:RUN
