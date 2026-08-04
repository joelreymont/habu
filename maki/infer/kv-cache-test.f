\ maki/infer/kv-cache-test.f - linear device KV cache contract.
\ Run: bin/hb --load maki/infer/kv-cache-test.f

require lib/test.f
require lib/process-fork.f
require lib/test/mmap-exhaust.f
require lib/test/outcome.f
require test/checker-assert.f
require maki/infer/kv-cache.f

package KV-OPEN-PROBE

: CALL ( GPU:session KV:config -- GPU:session result<KV:cache,n> )
   KV:OPEN ;

;package

package KV
private

4 constant KVT-H-CAP
create KVT-H-CID  KVT-H-CAP cells allot
create KVT-H-SLOT KVT-H-CAP cells allot
create KVT-H-GEN  KVT-H-CAP cells allot

: KVT-H! ( seq n -- ) {: idx:n :}
   SEQ-PARTS {: cid:n slot:n gen:n :}
   cid idx cells KVT-H-CID + !
   slot idx cells KVT-H-SLOT + !
   gen idx cells KVT-H-GEN + ! ;

: KVT-H@ ( n -- seq ) {: idx:n :}
   idx cells KVT-H-CID + @
   idx cells KVT-H-SLOT + @
   idx cells KVT-H-GEN + @ LOAD-SEQ ;

: KVT-MUST-SESSION ( -- GPU:session )
   GPU:OPEN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-MUST-SESSION-CLOSE ( GPU:session -- )
   GPU:CLOSE MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-MUST-OPEN
   ( GPU:session config -- GPU:session KV:cache )
   OPEN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-MUST-CLOSE ( GPU:session KV:cache -- GPU:session )
   CLOSE MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-OPEN-ERR ( GPU:session config n -- GPU:session ) {: want:n :}
   OPEN MATCH result
      ok OF KVT-MUST-CLOSE 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-CLOSE-ERR ( GPU:session KV:cache n -- GPU:session ) {: want:n :}
   CLOSE MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-ALLOC ( KV:cache n n -- KV:cache ) {: max:n idx:n :}
   max ALLOC-SEQ MATCH result
      ok OF idx KVT-H! ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-FORK ( KV:cache n n -- KV:cache ) {: parent:n child:n :}
   parent KVT-H@ FORK-SEQ MATCH result
      ok OF child KVT-H! ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-CANCEL ( KV:cache n -- KV:cache ) {: idx:n :}
   idx KVT-H@ CANCEL-SEQ MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-APPEND ( GPU:session KV:cache n -- GPU:session KV:cache ) {: idx:n :}
   idx KVT-H@ APPEND-TOKEN MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-APPEND-ERR
   ( GPU:session KV:cache n n -- GPU:session KV:cache )
   {: idx:n want:n :}
   idx KVT-H@ APPEND-TOKEN MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-N ( KV:cache result<n,n> -- KV:cache n )
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-ERR ( KV:cache result<n,n> n -- KV:cache ) {: want:n :}
   MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-ALLOC-ERR ( KV:cache result<seq,n> n -- KV:cache ) {: want:n :}
   MATCH result
      ok OF KV-SEQ:UNMAKE drop drop drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-SEQ-LEN ( KV:cache n -- KV:cache n )
   KVT-H@ SEQ-LEN KVT-N ;

: KVT-SEQ-RES ( KV:cache n -- KV:cache n )
   KVT-H@ SEQ-RESERVED KVT-N ;

: KVT-PAGE-ID ( KV:cache n n -- KV:cache n ) {: idx:n pageix:n :}
   idx KVT-H@ SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen LOAD-SEQ pageix SEQ-PAGE-INNER {: pid:n :}
   h KC-MINT pid ;

: KVT-HOST-ID ( KV:cache -- KV:cache ptr u8 )
   KC-TAKE {: h:ptr :}
   h KC-MINT h ;

: KVT-FIELD@ ( KV:cache n -- KV:cache n ) {: off:n :}
   KC-TAKE {: h:ptr :}
   h off H@ {: value:n :}
   h KC-MINT value ;

: KVT-CHECK ( KV:cache -- KV:cache )
   KC-TAKE {: h:ptr :}
   h KV-CHECK
   h KC-MINT ;

2048 constant KVT-SNAP-CAP
create KVT-SNAP KVT-SNAP-CAP allot
variable KVT-SNAP-N

: KVT-SNAPSHOT ( KV:cache -- KV:cache )
   KC-TAKE {: h:ptr :}
   h HOSTB-OFF H@ {: bytes:n :}
   bytes KVT-SNAP-CAP > if E-KV-BOUNDS throw then
   h 1 cells + KVT-SNAP bytes 1 cells - BYTE-COPY
   bytes 1 cells - KVT-SNAP-N !
   h KC-MINT ;

: KVT-SNAPSHOT= ( KV:cache -- KV:cache bool )
   KC-TAKE {: h:ptr :}
   true
   h HOSTB-OFF H@ 1 cells - KVT-SNAP-N @ = and
   KVT-SNAP-N @ 0 ?do h 1 cells + i + c@ KVT-SNAP i + c@ = and loop
   {: same:bool :}
   h KC-MINT same ;

: KVT-IO-CODE
   ( GPU:session GPU:buffer result<n,n> -- GPU:session GPU:buffer n )
   RES-CODE ;

: KVT-UPLOAD
   ( GPU:session KV:cache n ptr u8 n -- GPU:session KV:cache )
   {: off:n src:ptr bytes:n :}
   KC-TAKE {: h:ptr :}
   off CAD-OFF src bytes CAD-LEN GPU:UPLOAD KVT-IO-CODE
   {: code:n :}
   h KC-MINT
   code 0<> if code throw then ;

: KVT-DOWNLOAD
   ( GPU:session KV:cache n ptr u8 n -- GPU:session KV:cache )
   {: off:n dst:ptr bytes:n :}
   KC-TAKE {: h:ptr :}
   off CAD-OFF dst bytes CAD-LEN GPU:DOWNLOAD KVT-IO-CODE
   {: code:n :}
   h KC-MINT
   code 0<> if code throw then ;

: KVT-SPAN-N
   ( GPU:session KV:cache result<cuda-devptr,n> -- GPU:session KV:cache n )
   MATCH result
      ok OF CUDA-DEVPTR>N ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-SPAN-ERR
   ( GPU:session KV:cache result<cuda-devptr,n> n -- GPU:session KV:cache )
   {: want:n :}
   MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-KV-ERR
   ( GPU:session KV:cache n n n n n -- GPU:session KV:cache )
   {: idx:n layer:n tok:n head:n want:n :}
   idx KVT-H@ layer tok head K-SPAN want KVT-SPAN-ERR
   idx KVT-H@ layer tok head V-SPAN want KVT-SPAN-ERR ;

: KVT-K-SPAN
   ( GPU:session KV:cache n n n n -- GPU:session KV:cache n )
   {: idx:n layer:n tok:n head:n :}
   idx KVT-H@ layer tok head K-SPAN KVT-SPAN-N ;

: KVT-V-SPAN
   ( GPU:session KV:cache n n n n -- GPU:session KV:cache n )
   {: idx:n layer:n tok:n head:n :}
   idx KVT-H@ layer tok head V-SPAN KVT-SPAN-N ;

1024 constant KVT-PAGE-BYTES
create KVT-SRC KVT-PAGE-BYTES allot
create KVT-DST KVT-PAGE-BYTES allot

: KVT-FILL ( -- )
   KVT-PAGE-BYTES 0 ?do
      i 37 * 11 + 255 and KVT-SRC i + c!
      0 KVT-DST i + c!
   loop ;

: KVT-PAGE= ( -- bool )
   true KVT-PAGE-BYTES 0 ?do
      KVT-SRC i + c@ KVT-DST i + c@ = and
   loop ;

variable KVT-DTOD-RC
variable KVT-DTOD-N
variable KVT-GPU-ALLOC-CALLS

: KVT-FDTOD ( cuda-devptr cuda-devptr len -- rc )
   {: dst:cuda-devptr src:cuda-devptr bytes:len :}
   dst drop src drop
   bytes LEN>N KVT-DTOD-N !
   KVT-DTOD-RC @ >RC ;

: KVT-FALLOC ( ptr a len -- rc )
   1 KVT-GPU-ALLOC-CALLS +!
   2drop 701 >RC ;

: KVT-HOST-THROW ( -- )  E-MEM-MAP throw ;

: KVT-FHOST ( ptr u8 n -- ptr u8 n )
   KVT-HOST-THROW ;

variable KVT-HOST-CALLS
PTR-VARIABLE KVT-ALLOC-P
variable KVT-ALLOC-B
PTR-VARIABLE KVT-RELEASE-P
variable KVT-RELEASE-B
variable KVT-RELEASE-N
variable KVT-EVENT
variable KVT-FREE-EVENT
variable KVT-RELEASE-EVENT

: KVT-FCOUNT-HOST ( ptr u8 n -- ptr u8 n )
   1 KVT-HOST-CALLS +!
   KVT-HOST-THROW ;

: KVT-FCOUNT-REAL-HOST ( ptr u8 n -- ptr u8 n )
   1 KVT-HOST-CALLS +!
   ALLOC-HOST-REAL ;

: KVT-FTRACK-HOST ( ptr u8 n -- ptr u8 n )
   ALLOC-HOST-REAL {: h:ptr bytes:n :}
   h KVT-ALLOC-P !
   bytes KVT-ALLOC-B !
   h bytes ;

: KVT-RELEASE-RESET ( -- )
   NULL$ drop KVT-RELEASE-P !
   0 KVT-RELEASE-B !
   0 KVT-RELEASE-N !
   0 KVT-EVENT !
   0 KVT-FREE-EVENT !
   0 KVT-RELEASE-EVENT ! ;

: KVT-RELEASE-RECORD ( ptr u8 n -- )
   {: h:ptr bytes:n :}
   h KVT-RELEASE-P !
   bytes KVT-RELEASE-B !
   1 KVT-RELEASE-N +! ;

: KVT-FRELEASE ( ptr u8 n -- )
   1 KVT-EVENT +!
   KVT-EVENT @ KVT-RELEASE-EVENT !
   2dup KVT-RELEASE-RECORD
   RELEASE-HOST-REAL ;

: KVT-FFREE ( cuda-devptr -- rc )
   1 KVT-EVENT +!
   KVT-EVENT @ KVT-FREE-EVENT !
   CUDA:CU-MEM-FREE drop
   702 >RC ;

: KVT-CONFIG ( -- config )
   2 2 4 2 8 4 65 16 CONFIG/P ;

: KVT-FORGED-CONFIG ( -- config )
   1 1 1 0 1 1 1 16 KV-CONFIG:MAKE ;

: KVT-MUL-CONFIG ( -- config )
   1449 1449 1449 1449 1449 1 1 1449 KV-CONFIG:MAKE ;

: KVT-ADD-CONFIG ( -- config )
   1 1 1 1 1073741818 1073741821 1073741818 1 KV-CONFIG:MAKE ;

: KVT-OPEN-STANDARD ( GPU:session -- GPU:session KV:cache )
   KVT-CONFIG KVT-MUST-OPEN ;

: KVT-MUST-BUF
   ( GPU:session result<GPU:buffer,n> -- GPU:session GPU:buffer )
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-BUF-SPAN-OK
   ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer )
   GPU:SPAN MATCH result
      ok OF CUDA-DEVPTR>N drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-BUF-SPAN-ERR
   ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer )
   GPU:SPAN MATCH result
      ok OF CUDA-DEVPTR>N drop 0 1 T= ENDOF
      err OF E-BUF-BOUNDS T= ENDOF
   ;MATCH ;

: KVT-FOOT-ROLE
   ( GPU:session KV:cache CAD-NUM:alloc-byte-len n -- GPU:session KV:cache )
   {: expected:n :}
   swap >r
   GPU:ALLOC KVT-MUST-BUF
   expected 1- CAD-OFF 1 CAD-LEN KVT-BUF-SPAN-OK
   expected CAD-OFF 1 CAD-LEN KVT-BUF-SPAN-ERR
   GPU:FREE RES-CODE 0 T=
   r> ;

: KVT-GEOMETRY ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   FOOTPRINT >r
   688 KVT-FOOT-ROLE
   r> 8192 KVT-FOOT-ROLE
   HOSTB-OFF KVT-FIELD@ 688 T=
   DEVB-OFF KVT-FIELD@ 8192 T=
   PAGE-SIZE 16 T=
   PAGE-BYTES KVT-PAGE-BYTES T=
   TOK-BYTES 64 T=
   NUM-PAGES 8 T=
   BLOCK-CAPACITY 5 T=
   MAX-CONTEXT 65 T=
   FREE-PAGES 8 T=
   RESERVED-PAGES 0 T=
   WATERMARK 0 T=
   HIGH-WATER 0 T=
   65 PAGES-FOR KVT-N 5 T=
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-SPANS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   17 0 ?do 0 KVT-APPEND loop
   0 0 KVT-PAGE-ID {: p0:n :}
   0 1 KVT-PAGE-ID {: p1:n :}
   0 0 0 0 KVT-K-SPAN {: k000:n :}
   0 0 0 1 KVT-K-SPAN k000 8 + T=
   0 0 15 1 KVT-K-SPAN k000 488 + T=
   0 0 0 0 KVT-V-SPAN k000 16 + T=
   0 0 15 1 KVT-V-SPAN k000 504 + T=
   0 1 0 0 KVT-K-SPAN k000 512 + T=
   p0 7 T=
   0 1 15 1 KVT-V-SPAN 8 + k000 KVT-PAGE-BYTES + T=
   0 0 16 0 KVT-K-SPAN
   k000 p1 p0 - KVT-PAGE-BYTES * + T=
   0 -1 0 0 E-KV-BOUNDS KVT-KV-ERR
   0 2 0 0 E-KV-BOUNDS KVT-KV-ERR
   0 0 -1 0 E-KV-BOUNDS KVT-KV-ERR
   0 0 17 0 E-KV-BOUNDS KVT-KV-ERR
   0 0 0 -1 E-KV-BOUNDS KVT-KV-ERR
   0 0 0 2 E-KV-BOUNDS KVT-KV-ERR
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-STALE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   0 KVT-CANCEL
   16 1 KVT-ALLOC
   0 KVT-H@ SEQ-LEN E-KV-SEQ KVT-ERR
   0 0 0 0 E-KV-SEQ KVT-KV-ERR
   1 KVT-SEQ-LEN 0 T=
   1 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-CROSS-CACHE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   swap KVT-CONFIG KVT-MUST-OPEN
   0 KVT-H@ SEQ-LEN E-KV-SEQ KVT-ERR
   0 0 0 0 E-KV-SEQ KVT-KV-ERR
   KVT-MUST-CLOSE
   swap 0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-CAPACITY ( -- )
   KVT-MUST-SESSION
   1 1 1 1 4 2 64 16 CONFIG/P KVT-MUST-OPEN
   64 0 KVT-ALLOC
   RESERVED-PAGES 4 T=
   1 ALLOC-SEQ E-KV-ADMIT KVT-ALLOC-ERR
   RESERVED-PAGES 4 T=
   0 KVT-SEQ-RES 4 T=
   64 0 ?do 0 KVT-APPEND loop
   RESERVED-PAGES 0 T=
   WATERMARK 4 T=
   0 KVT-SEQ-LEN 64 T=
   0 E-KV-ADMIT KVT-APPEND-ERR
   0 KVT-CANCEL
   FREE-PAGES 4 T=
   RESERVED-PAGES 0 T=
   HIGH-WATER 4 T=
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-COW ( -- )
   KVT-FILL
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-HOST-ID {: host:ptr :}
   32 0 KVT-ALLOC
   4 0 ?do 0 KVT-APPEND loop
   0 0 KVT-PAGE-ID {: old:n :}
   old KVT-PAGE-BYTES * KVT-SRC KVT-PAGE-BYTES KVT-UPLOAD
   0 1 KVT-FORK
   0 2 KVT-FORK
   RESERVED-PAGES 5 T=
   FREE-PAGES 7 T=
   SHARED-PAGES 1 T=
   TAIL-WASTE 12 T=
   KVT-CHECK
   KVT-SNAPSHOT
   703 KVT-DTOD-RC ! 0 KVT-DTOD-N !
   [: KVT-FDTOD ;] MKD:DTOD!
   1 703 KVT-APPEND-ERR
   MKD:USE-REAL
   KVT-DTOD-N @ KVT-PAGE-BYTES T=
   KVT-SNAPSHOT= TTRUE
   0 KVT-SEQ-LEN 4 T=
   1 KVT-SEQ-LEN 4 T=
   2 KVT-SEQ-LEN 4 T=
   RESERVED-PAGES 5 T=
   FREE-PAGES 7 T=
   SHARED-PAGES 1 T=
   TAIL-WASTE 12 T=
   1 KVT-APPEND
   1 0 KVT-PAGE-ID {: new1:n :}
   new1 old <> TTRUE
   new1 KVT-PAGE-BYTES * KVT-DST KVT-PAGE-BYTES KVT-DOWNLOAD
   KVT-PAGE= TTRUE
   RESERVED-PAGES 4 T=
   FREE-PAGES 6 T=
   SHARED-PAGES 1 T=
   TAIL-WASTE 23 T=
   WATERMARK 2 T=
   2 KVT-APPEND
   2 0 KVT-PAGE-ID {: new2:n :}
   new2 old <> TTRUE
   new2 new1 <> TTRUE
   new2 KVT-PAGE-BYTES * KVT-DST KVT-PAGE-BYTES KVT-DOWNLOAD
   KVT-PAGE= TTRUE
   RESERVED-PAGES 3 T=
   FREE-PAGES 5 T=
   SHARED-PAGES 0 T=
   TAIL-WASTE 34 T=
   WATERMARK 3 T=
   KVT-HOST-ID host = TTRUE
   KVT-CHECK
   1 KVT-CANCEL
   RESERVED-PAGES 2 T=
   FREE-PAGES 6 T=
   SHARED-PAGES 0 T=
   TAIL-WASTE 23 T=
   KVT-CHECK
   2 KVT-CANCEL
   RESERVED-PAGES 1 T=
   FREE-PAGES 7 T=
   TAIL-WASTE 12 T=
   KVT-CHECK
   0 KVT-CANCEL
   RESERVED-PAGES 0 T=
   FREE-PAGES 8 T=
   TAIL-WASTE 0 T=
   SHARED-PAGES 0 T=
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-CHURN ( -- )
   0 KVT-HOST-CALLS !
   [: KVT-FCOUNT-REAL-HOST ;] is HOST-ALLOC
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-HOST-ID {: host:ptr :}
   100 0 ?do
      16 0 KVT-ALLOC
      0 KVT-APPEND
      0 KVT-CANCEL
      KVT-CHECK
   loop
   FREE-PAGES 8 T=
   RESERVED-PAGES 0 T=
   WATERMARK 0 T=
   HIGH-WATER 1 T=
   KVT-HOST-ID host = TTRUE
   KVT-MUST-CLOSE
   HOST-USE-REAL
   KVT-HOST-CALLS @ 1 T=
   KVT-MUST-SESSION-CLOSE ;

: KVT-NO-ALLOC-BODY ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   1 MMAP-TEST:EXHAUST-CHILD
   1 MMAP-TEST:EXHAUSTED? 0= if E-KV-INVARIANT throw then
   32 0 KVT-ALLOC
   WATERMARK drop
   FREE-PAGES drop
   RESERVED-PAGES drop
   SHARED-PAGES drop
   TAIL-WASTE drop
   HIGH-WATER drop
   NUM-PAGES drop
   PAGE-SIZE drop
   PAGE-BYTES drop
   TOK-BYTES drop
   MAX-CONTEXT drop
   BLOCK-CAPACITY drop
   0 KVT-H@ SEQ-LEN KVT-N drop
   0 KVT-H@ SEQ-RESERVED KVT-N drop
   1 PAGES-FOR KVT-N drop
   FOOTPRINT 2drop
   4 0 ?do 0 KVT-APPEND loop
   0 0 0 0 KVT-K-SPAN drop
   0 0 0 0 KVT-V-SPAN drop
   0 1 KVT-FORK
   1 KVT-APPEND
   KVT-CHECK
   1 KVT-CANCEL
   0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-NO-ALLOC-CHILD ( -- )
   [: KVT-NO-ALLOC-BODY ;] catch {: code:n :}
   s" " code die ;

: KVT-NO-ALLOC ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if KVT-NO-ALLOC-CHILD then
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

: KVT-DEVICE-ALLOC-FAIL ( -- )
   KVT-RELEASE-RESET
   0 KVT-GPU-ALLOC-CALLS !
   KVT-MUST-SESSION
   [: KVT-FTRACK-HOST ;] is HOST-ALLOC
   [: KVT-FRELEASE ;] is HOST-RELEASE
   [: KVT-FALLOC ;] MKD:CUMEMALLOC!
   KVT-CONFIG 701 KVT-OPEN-ERR
   MKD:USE-REAL
   HOST-USE-REAL
   KVT-RELEASE-N @ 1 T=
   KVT-RELEASE-P @ KVT-ALLOC-P @ = TTRUE
   KVT-RELEASE-B @ KVT-ALLOC-B @ T=
   KVT-RELEASE-B @ 688 T=
   KVT-GPU-ALLOC-CALLS @ 1 T=
   KVT-MUST-SESSION-CLOSE ;

: KVT-HOST-ALLOC-FAIL ( -- )
   KVT-RELEASE-RESET
   0 KVT-GPU-ALLOC-CALLS !
   KVT-MUST-SESSION
   [: KVT-FHOST ;] is HOST-ALLOC
   [: KVT-RELEASE-RECORD ;] is HOST-RELEASE
   [: KVT-FALLOC ;] MKD:CUMEMALLOC!
   KVT-CONFIG E-MEM-MAP KVT-OPEN-ERR
   MKD:USE-REAL
   HOST-USE-REAL
   KVT-RELEASE-N @ 0 T=
   KVT-GPU-ALLOC-CALLS @ 0 T=
   KVT-MUST-SESSION-CLOSE ;

: KVT-OPEN-CODE ( GPU:session config -- GPU:session n )
   OPEN MATCH result
      ok OF KVT-MUST-CLOSE 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: KVT-PLAN-RESET ( -- )
   0 KVT-HOST-CALLS !
   0 KVT-GPU-ALLOC-CALLS !
   KVT-RELEASE-RESET
   [: KVT-FCOUNT-HOST ;] is HOST-ALLOC
   [: KVT-RELEASE-RECORD ;] is HOST-RELEASE
   [: KVT-FALLOC ;] MKD:CUMEMALLOC! ;

: KVT-PLAN-ASSERT ( GPU:session n -- ) {: code:n :}
   MKD:USE-REAL
   HOST-USE-REAL
   code E-KV-CONFIG T=
   KVT-HOST-CALLS @ 0 T=
   KVT-RELEASE-N @ 0 T=
   KVT-GPU-ALLOC-CALLS @ 0 T=
   KVT-MUST-SESSION-CLOSE ;

: KVT-CONFIG-FAIL ( -- )
   KVT-PLAN-RESET
   KVT-MUST-SESSION KVT-FORGED-CONFIG KVT-OPEN-CODE
   KVT-PLAN-ASSERT ;

: KVT-MUL-FAIL ( -- )
   KVT-PLAN-RESET
   KVT-MUST-SESSION KVT-MUL-CONFIG KVT-OPEN-CODE
   KVT-PLAN-ASSERT ;

: KVT-ADD-FAIL ( -- )
   KVT-PLAN-RESET
   KVT-MUST-SESSION KVT-ADD-CONFIG KVT-OPEN-CODE
   KVT-PLAN-ASSERT ;

: KVT-CLOSE-FAIL ( -- )
   KVT-RELEASE-RESET
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-HOST-ID {: host:ptr :}
   HOSTB-OFF KVT-FIELD@ {: bytes:n :}
   [: KVT-FRELEASE ;] is HOST-RELEASE
   [: KVT-FFREE ;] MKD:CUMEMFREE!
   702 KVT-CLOSE-ERR
   MKD:USE-REAL
   HOST-USE-REAL
   KVT-RELEASE-N @ 1 T=
   KVT-RELEASE-P @ host = TTRUE
   KVT-RELEASE-B @ bytes T=
   KVT-RELEASE-B @ 688 T=
   KVT-FREE-EVENT @ 1 T=
   KVT-RELEASE-EVENT @ 2 T=
   KVT-MUST-SESSION-CLOSE ;

: KVT-OLD-API ( -- )
   s" KVT-NO-HDR ( -- n ) KV:HDR-BYTES" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-NO-INIT ( ptr a KV:config -- ) KV:INIT" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-NO-DISPOSE ( ptr a -- ) KV:DISPOSE" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-NO-TOKEN-PTR ( ptr a KV:seq n n -- ptr u8 ) KV:TOKEN-PTR" CHECK-QUIET-CANDIDATE! 1 T= ;

: KVT-PRIVATE-SPANS ( -- )
   s" KVT-NO-K-SPAN ( GPU:session KV:cache KV:seq n n n -- GPU:session KV:cache result<cuda-devptr,n> ) KV:K-SPAN" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-NO-V-SPAN ( GPU:session KV:cache KV:seq n n n -- GPU:session KV:cache result<cuda-devptr,n> ) KV:V-SPAN" CHECK-QUIET-CANDIDATE! 1 T= ;

: KVT-LINEAR-REJECTS ( -- )
   s" KVT-BAD-DUP ( KV:cache -- KV:cache KV:cache ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-DROP ( KV:cache -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-DOUBLE-CLOSE ( GPU:session KV:cache -- GPU:session result<n,n> result<n,n> ) KV:CLOSE KV:CLOSE" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-POST-CLOSE ( GPU:session KV:cache -- GPU:session result<n,n> n ) KV:CLOSE KV:WATERMARK" CHECK-QUIET-CANDIDATE! 0 T= ;

: KVT-RUN ( -- )
   T-RESET
   s" post-open operations require no host allocation" T-LABEL [: KVT-NO-ALLOC ;] 0 TTHROWSQ
   s" exact geometry and footprint" T-LABEL [: KVT-GEOMETRY ;] 0 TTHROWSQ
   s" bounded K and V device spans" T-LABEL [: KVT-SPANS ;] 0 TTHROWSQ
   s" stale generation" T-LABEL [: KVT-STALE ;] 0 TTHROWSQ
   s" cross-cache identity" T-LABEL [: KVT-CROSS-CACHE ;] 0 TTHROWSQ
   s" reservation and capacity" T-LABEL [: KVT-CAPACITY ;] 0 TTHROWSQ
   s" copy failure atomicity and real full-page COW" T-LABEL [: KVT-COW ;] 0 TTHROWSQ
   s" allocation-free churn" T-LABEL [: KVT-CHURN ;] 0 TTHROWSQ
   s" device allocation failure cleanup" T-LABEL [: KVT-DEVICE-ALLOC-FAIL ;] 0 TTHROWSQ
   s" host allocation failure cleanup" T-LABEL [: KVT-HOST-ALLOC-FAIL ;] 0 TTHROWSQ
   s" forged configuration rejected before allocation" T-LABEL [: KVT-CONFIG-FAIL ;] 0 TTHROWSQ
   s" multiplicative overflow rejected before allocation" T-LABEL [: KVT-MUL-FAIL ;] 0 TTHROWSQ
   s" additive overflow rejected before allocation" T-LABEL [: KVT-ADD-FAIL ;] 0 TTHROWSQ
   s" device close failure with host release" T-LABEL [: KVT-CLOSE-FAIL ;] 0 TTHROWSQ
   s" old pointer API absent" T-LABEL [: KVT-OLD-API ;] 0 TTHROWSQ
   s" device span borrows remain private" T-LABEL [: KVT-PRIVATE-SPANS ;] 0 TTHROWSQ
   s" linear cache misuse rejected" T-LABEL [: KVT-LINEAR-REJECTS ;] 0 TTHROWSQ
   MKD:USE-REAL
   HOST-USE-REAL
   T-REPORT ;

KVT-RUN

;package
