\ maki/infer/kv-cache-device-test.f - linear device KV cache contract.
\ Run: bin/hb --load maki/infer/kv-cache-device-test.f   (DEVICE-REQUIRED - see below)
\
\ DEVICE-REQUIRED SUITE. Every case here opens a real GPU session
\ (KVT-MUST-SESSION -> GPU:OPEN -> maki/gpu-session.f GS-ACQUIRE -> MKD -> the
\ CUDA driver), allocates real device memory, and moves real bytes through
\ GPU:UPLOAD / GPU:DOWNLOAD / GPU:COPY; one failure-injection fake even calls
\ CUDA:CU-MEM-FREE directly. lib/ptx/cuda-driver.f CUDA:OPEN? reaches the driver
\ by dlopen("libcuda.so.1"), so a host with no CUDA driver cannot run this suite
\ at all: GPU:OPEN returns E-CUDA (-5002) and every case that needs a session
\ fails. That is a missing precondition, not a KV defect, and REQUIRE-DEVICE
\ below says so once by name instead of leaving 37 anonymous assertion failures
\ behind.
\
\ It is a member of the *-device-test.f family (docs/ablation.md): kept out of
\ maki/test.f and its slices, run explicitly on a device host. The sibling
\ off-device suites reach the driver only through the MKD injection seam
\ (maki/cuda-run-fake.f); this suite deliberately does not, because it is
\ asserting real device bytes.

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

: KVT-MUST-BEGIN ( KV:cache -- KV:cache KV:batch )
   BEGIN-BATCH MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: KVT-MUST-CANCEL ( KV:cache KV:batch -- KV:cache )
   CANCEL-BATCH MATCH cancel-result
      cancelled OF ENDOF
      refused OF throw ENDOF
   ;MATCH ;

: KVT-BEGIN-ERR ( KV:cache n -- KV:cache ) {: want:n :}
   BEGIN-BATCH MATCH result
      ok OF E-KV-INVARIANT throw ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: KVT-DUP-BEGIN ( KV:cache KV:batch -- KV:cache KV:batch )
   swap BEGIN-BATCH MATCH result
      ok OF
         CANCEL-BATCH MATCH cancel-result
            cancelled OF 0 1 T= ENDOF
            refused OF throw ENDOF
         ;MATCH
         swap
      ENDOF
      err OF E-KV-BATCH T= swap ENDOF
   ;MATCH ;

: KVT-CANCEL-ERR ( KV:cache KV:batch -- KV:cache KV:batch )
   CANCEL-BATCH MATCH cancel-result
      cancelled OF E-KV-INVARIANT throw ENDOF
      refused OF E-KV-BATCH T= ENDOF
   ;MATCH ;

: KVT-CROSS-ONE
   ( KV:cache KV:batch KV:cache KV:batch -- KV:cache KV:batch KV:cache KV:batch )
   swap rot KVT-CANCEL-ERR swap rot ;

: KVT-BATCH-GEN ( KV:batch -- KV:batch n )
   KB-TAKE {: gen:n :}
   gen KB-MINT gen ;

: ACTIVE-ALLOC
   ( KV:cache KV:batch -- KV:cache KV:batch result<KV:seq,n> )
   >r 16 ALLOC-SEQ r> swap ;

: KVT-MUST-ADD ( KV:cache KV:batch n -- KV:cache KV:batch )
   KVT-H@ ADD MATCH add-result
      added OF ENDOF
      refused OF throw ENDOF
   ;MATCH ;

: KVT-ADD-OK ( KV:cache KV:batch n -- KV:cache KV:batch )
   KVT-H@ ADD MATCH add-result
      added OF ENDOF
      refused OF drop 0 1 T= ENDOF
   ;MATCH ;

: KVT-ADD-ERR ( KV:cache KV:batch n n -- KV:cache KV:batch )
   {: idx:n want:n :}
   idx KVT-H@ ADD MATCH add-result
      added OF 0 1 T= ENDOF
      refused OF want T= ENDOF
   ;MATCH ;

: KVT-STALE-ADD ( KV:cache KV:batch KV:batch n n -- KV:cache KV:batch )
   {: idx:n want:n :}
   rot swap idx E-KV-BATCH KVT-ADD-ERR
   KB-TAKE want T=
   swap ;

: KVT-CROSS-ADD
   ( KV:cache KV:batch KV:cache KV:batch -- KV:cache KV:batch KV:cache KV:batch )
   swap rot 1 E-KV-BATCH KVT-ADD-ERR swap rot ;

: KVT-STALE-ONE ( KV:cache KV:batch KV:batch n -- KV:cache KV:batch )
   {: want:n :}
   rot swap KVT-CANCEL-ERR
   KB-TAKE want T=
   swap ;

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

: KVT-ACTIVE-ALLOC-ERR ( KV:cache KV:batch n -- KV:cache KV:batch )
   {: want:n :}
   >r 16 ALLOC-SEQ MATCH result
      ok OF KV-SEQ:UNMAKE drop drop drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH
   r> ;

: KVT-ACTIVE-FORK-ERR ( KV:cache KV:batch n n -- KV:cache KV:batch )
   {: idx:n want:n :}
   >r idx KVT-H@ FORK-SEQ MATCH result
      ok OF KV-SEQ:UNMAKE drop drop drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH
   r> ;

: KVT-ACTIVE-CANCEL-ERR ( KV:cache KV:batch n n -- KV:cache KV:batch )
   {: idx:n want:n :}
   >r idx KVT-H@ CANCEL-SEQ MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH
   r> ;

: KVT-ACTIVE-APPEND-ERR
   ( GPU:session KV:cache KV:batch n n -- GPU:session KV:cache KV:batch )
   {: idx:n want:n :}
   >r idx KVT-H@ APPEND-TOKEN MATCH result
      ok OF drop 0 1 T= ENDOF
      err OF want T= ENDOF
   ;MATCH
   r> ;

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

: KVT-FIELD! ( KV:cache n n -- KV:cache ) {: value:n off:n :}
   KC-TAKE {: h:ptr :}
   value h off H!
   h KC-MINT ;

: KVT-SEQ-RES! ( KV:cache n n -- KV:cache ) {: idx:n value:n :}
   KC-TAKE {: h:ptr :}
   value h idx SEQRES!
   h KC-MINT ;

: KVT-BLK-LEN! ( KV:cache n n -- KV:cache ) {: idx:n value:n :}
   KC-TAKE {: h:ptr :}
   value h idx SEQBLEN!
   h KC-MINT ;

: KVT-D-MODE ( KV:cache n -- KV:cache n ) {: idx:n :}
   KC-TAKE {: h:ptr :}
   h idx D-MODE@ {: value:n :}
   h KC-MINT value ;

: KVT-D-OLD ( KV:cache n -- KV:cache n ) {: idx:n :}
   KC-TAKE {: h:ptr :}
   h idx D-OLD@ {: value:n :}
   h KC-MINT value ;

: KVT-D-NEW ( KV:cache n -- KV:cache n ) {: idx:n :}
   KC-TAKE {: h:ptr :}
   h idx D-NEW@ {: value:n :}
   h KC-MINT value ;

: KVT-CHECK ( KV:cache -- KV:cache )
   KC-TAKE {: h:ptr :}
   h KV-CHECK
   h KC-MINT ;

: KVT-CHECK-ERR ( KV:cache n -- KV:cache ) {: want:n :}
   KC-TAKE {: h:ptr :}
   h [: dup KV-CHECK ;] catch {: rh:ptr code:n :}
   rh drop
   code want T=
   h KC-MINT ;

: KVT-BATCH-OFF! ( KV:cache KV:batch n -- KV:cache KV:batch ) {: value:n :}
   KB-TAKE {: gen:n :}
   KC-TAKE {: h:ptr :}
   value h BATCH-OFF H!
   h KC-MINT gen KB-MINT ;

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

: KVT-HIGH-BYTE? ( n -- bool )
   dup HIWATER-OFF 1 cells - >=
   swap HIWATER-OFF < and ;

: KVT-SNAPSHOT/HIGH= ( KV:cache -- KV:cache bool )
   KC-TAKE {: h:ptr :}
   true
   h HOSTB-OFF H@ 1 cells - KVT-SNAP-N @ = and
   KVT-SNAP-N @ 0 ?do
      i KVT-HIGH-BYTE? 0= if
         h 1 cells + i + c@ KVT-SNAP i + c@ = and
      then
   loop
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
variable KVT-DEV-CALLS

: KVT-FDTOD ( cuda-devptr cuda-devptr len -- rc )
   {: dst:cuda-devptr src:cuda-devptr bytes:len :}
   dst drop src drop
   bytes LEN>N KVT-DTOD-N !
   KVT-DTOD-RC @ >RC ;

: KVT-FALLOC ( ptr a len -- rc )
   1 KVT-GPU-ALLOC-CALLS +!
   2drop 701 >RC ;

: KVT-FDEV ( -- rc )  1 KVT-DEV-CALLS +! 703 >RC ;
: KVT-FDEV-FREE ( cuda-devptr -- rc )  drop KVT-FDEV ;
: KVT-FDEV-SET ( cuda-devptr n count -- rc )  2drop drop KVT-FDEV ;
: KVT-FDEV-HTOD ( cuda-devptr ptr u8 len -- rc )  2drop drop KVT-FDEV ;
: KVT-FDEV-DTOH ( ptr u8 cuda-devptr len -- rc )  2drop drop KVT-FDEV ;
: KVT-FDEV-DTOD ( cuda-devptr cuda-devptr len -- rc )  2drop drop KVT-FDEV ;

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
   856 KVT-FOOT-ROLE
   r> 8192 KVT-FOOT-ROLE
   HOSTB-OFF KVT-FIELD@ 856 T=
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

: KVT-BATCH-LIFETIME ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-MUST-BEGIN KVT-BATCH-GEN {: first:n :}
   first 0 > TTRUE
   NEXT-BATCH-GEN @ first T=
   swap KVT-SNAPSHOT swap
   KVT-DUP-BEGIN
   NEXT-BATCH-GEN @ first T=
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   BATCH-OFF KVT-FIELD@ 0 T=
   KVT-MUST-BEGIN KVT-BATCH-GEN {: second:n :}
   second first > TTRUE
   KVT-MUST-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-STALE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-MUST-BEGIN KVT-BATCH-GEN {: stale:n :}
   KVT-MUST-CANCEL
   KVT-MUST-BEGIN KVT-BATCH-GEN {: active:n :}
   swap KVT-SNAPSHOT swap
   stale KB-MINT stale KVT-STALE-ONE
   NEXT-BATCH-GEN @ active T=
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-ZERO ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   NEXT-BATCH-GEN @ {: saved:n :}
   KVT-SNAPSHOT
   0 KB-MINT KVT-CANCEL-ERR
   KB-TAKE 0 T=
   NEXT-BATCH-GEN @ saved T=
   KVT-SNAPSHOT= TTRUE
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-CROSS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD KVT-MUST-BEGIN
   rot KVT-CONFIG KVT-MUST-OPEN
   2swap rot
   KVT-MUST-BEGIN
   KVT-CROSS-ONE
   KVT-MUST-CANCEL
   -rot 2swap
   KVT-MUST-CLOSE
   -rot KVT-MUST-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-EXHAUST ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   NEXT-BATCH-GEN @ {: saved:n :}
   KV-ID-MAX NEXT-BATCH-GEN !
   KVT-SNAPSHOT
   E-KV-ID KVT-BEGIN-ERR
   NEXT-BATCH-GEN @ KV-ID-MAX T=
   KVT-SNAPSHOT= TTRUE
   E-KV-ID KVT-BEGIN-ERR
   NEXT-BATCH-GEN @ KV-ID-MAX T=
   saved NEXT-BATCH-GEN !
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-PURE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   KVT-SNAPSHOT
   0 KVT-HOST-CALLS !  0 KVT-GPU-ALLOC-CALLS !  0 KVT-DEV-CALLS !
   [: KVT-FCOUNT-HOST ;] is HOST-ALLOC
   [: KVT-FALLOC ;] MKD:CUMEMALLOC!
   [: KVT-FDEV-FREE ;] MKD:CUMEMFREE!
   [: KVT-FDEV-SET ;] MKD:CUMEMSETD32!
   [: KVT-FDEV-HTOD ;] MKD:HTOD!
   [: KVT-FDEV-DTOH ;] MKD:DTOH!
   [: KVT-FDEV-DTOD ;] MKD:DTOD!
   KVT-MUST-BEGIN KVT-MUST-CANCEL
   MKD:USE-REAL  HOST-USE-REAL
   KVT-SNAPSHOT= TTRUE
   KVT-HOST-CALLS @ 0 T=
   KVT-GPU-ALLOC-CALLS @ 0 T=
   KVT-DEV-CALLS @ 0 T=
   FREE-PAGES 8 T=  RESERVED-PAGES 0 T=
   WATERMARK 0 T=  HIGH-WATER 0 T=
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-GROW ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-SNAPSHOT
   KVT-MUST-BEGIN 0 KVT-MUST-ADD
   swap
   0 KVT-D-MODE D-GROW T=
   0 KVT-SEQ-LEN 0 T=
   0 KVT-SEQ-RES 0 T=
   FREE-PAGES 7 T=  RESERVED-PAGES 0 T=
   WATERMARK 1 T=  HIGH-WATER 1 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT/HIGH= TTRUE
   HIGH-WATER 1 T=
   KVT-CHECK
   0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-SAME ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   0 KVT-APPEND
   0 0 KVT-PAGE-ID {: old:n :}
   KVT-SNAPSHOT
   KVT-MUST-BEGIN 0 KVT-MUST-ADD
   swap
   0 KVT-D-MODE D-SAME T=
   0 KVT-D-OLD old T=
   0 KVT-D-NEW old T=
   0 KVT-SEQ-LEN 1 T=
   FREE-PAGES 7 T=  RESERVED-PAGES 1 T=
   WATERMARK 1 T=  HIGH-WATER 1 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT= TTRUE
   KVT-CHECK
   0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-COW ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   4 0 ?do 0 KVT-APPEND loop
   0 1 KVT-FORK
   1 0 KVT-PAGE-ID {: old:n :}
   KVT-SNAPSHOT
   KVT-MUST-BEGIN 1 KVT-MUST-ADD
   swap
   1 KVT-D-MODE D-COW T=
   1 KVT-D-OLD old T=
   1 KVT-D-NEW old <> TTRUE
   1 KVT-SEQ-LEN 4 T=
   FREE-PAGES 6 T=  RESERVED-PAGES 2 T=
   WATERMARK 2 T=  HIGH-WATER 2 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT/HIGH= TTRUE
   HIGH-WATER 2 T=
   KVT-CHECK
   1 KVT-CANCEL  0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-SHARED ( n n -- ) {: first:n second:n :}
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   0 KVT-APPEND
   0 1 KVT-FORK
   KVT-SNAPSHOT
   KVT-MUST-BEGIN
   first KVT-ADD-OK  second KVT-ADD-OK
   swap
   first KVT-D-MODE D-COW T=
   second KVT-D-MODE D-SAME T=
   FREE-PAGES 6 T=  RESERVED-PAGES 2 T=
   HIGH-WATER 2 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT/HIGH= TTRUE
   KVT-CHECK
   1 KVT-CANCEL  0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-SHARED3 ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   0 KVT-APPEND
   0 1 KVT-FORK  0 2 KVT-FORK
   KVT-SNAPSHOT
   KVT-MUST-BEGIN
   2 KVT-ADD-OK  0 KVT-ADD-OK  1 KVT-ADD-OK
   swap
   2 KVT-D-MODE D-COW T=
   0 KVT-D-MODE D-COW T=
   1 KVT-D-MODE D-SAME T=
   FREE-PAGES 5 T=  RESERVED-PAGES 3 T=
   HIGH-WATER 3 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT/HIGH= TTRUE
   KVT-CHECK
   2 KVT-CANCEL  1 KVT-CANCEL  0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-N-ROLLBACK ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC  16 1 KVT-ALLOC  16 2 KVT-ALLOC
   KVT-SNAPSHOT
   KVT-MUST-BEGIN
   2 KVT-MUST-ADD  0 KVT-MUST-ADD  1 KVT-MUST-ADD
   swap
   FREE-PAGES 5 T=  RESERVED-PAGES 0 T=
   HIGH-WATER 3 T=
   KVT-CHECK
   swap
   KVT-MUST-CANCEL
   KVT-SNAPSHOT/HIGH= TTRUE
   HIGH-WATER 3 T=
   KVT-CHECK
   2 KVT-CANCEL  1 KVT-CANCEL  0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-DUP ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-MUST-BEGIN 0 KVT-MUST-ADD
   swap KVT-SNAPSHOT swap
   0 E-KV-BATCH KVT-ADD-ERR
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-STALE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC  0 KVT-CANCEL  16 1 KVT-ALLOC
   KVT-MUST-BEGIN
   swap KVT-SNAPSHOT swap
   0 E-KV-SEQ KVT-ADD-ERR
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   1 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-CROSS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   swap KVT-CONFIG KVT-MUST-OPEN
   16 1 KVT-ALLOC
   >r swap KVT-MUST-BEGIN r>
   >r swap KVT-SNAPSHOT swap r>
   >r 1 E-KV-SEQ KVT-ADD-ERR r>
   >r swap KVT-SNAPSHOT= TTRUE swap r>
   >r KVT-MUST-CANCEL r>
   swap >r
   1 KVT-CANCEL KVT-MUST-CLOSE
   r> 0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-LIMIT ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   1 0 KVT-ALLOC  0 KVT-APPEND
   KVT-MUST-BEGIN
   swap KVT-SNAPSHOT swap
   0 E-KV-ADMIT KVT-ADD-ERR
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-BATCH-STALE ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-MUST-BEGIN KVT-BATCH-GEN {: stale:n :}
   KVT-MUST-CANCEL
   KVT-MUST-BEGIN KVT-BATCH-GEN {: active:n :}
   swap KVT-SNAPSHOT swap
   stale KB-MINT 0 stale KVT-STALE-ADD
   NEXT-BATCH-GEN @ active T=
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-BATCH-ZERO ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-SNAPSHOT
   0 KB-MINT 0 E-KV-BATCH KVT-ADD-ERR
   KB-TAKE 0 T=
   KVT-SNAPSHOT= TTRUE
   KVT-CHECK
   0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-BATCH-CROSS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC KVT-MUST-BEGIN
   rot KVT-CONFIG KVT-MUST-OPEN
   16 1 KVT-ALLOC
   2swap rot KVT-MUST-BEGIN
   swap KVT-SNAPSHOT swap
   KVT-CROSS-ADD
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   1 KVT-CANCEL
   -rot 2swap
   KVT-MUST-CLOSE
   -rot KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-ADD-REFUSALS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-MUST-BEGIN
   swap 0 0 KVT-SEQ-RES! swap
   swap 0 RESERVED-OFF KVT-FIELD! swap
   0 E-KV-INVARIANT KVT-ADD-ERR
   swap 0 1 KVT-SEQ-RES! swap
   swap 1 RESERVED-OFF KVT-FIELD! swap
   swap 0 FREETOP-OFF KVT-FIELD! swap
   0 E-KV-INVARIANT KVT-ADD-ERR
   swap 8 FREETOP-OFF KVT-FIELD! swap
   swap 0 5 KVT-BLK-LEN! swap
   0 E-KV-ADMIT KVT-ADD-ERR
   swap 0 0 KVT-BLK-LEN! swap
   swap KV-MAX-N PAGEB-OFF KVT-FIELD! swap
   0 E-KV-CONFIG KVT-ADD-ERR
   swap KVT-PAGE-BYTES PAGEB-OFF KVT-FIELD! swap
   swap 3 NLAYER-OFF KVT-FIELD! swap
   0 E-KV-BOUNDS KVT-ADD-ERR
   swap 2 NLAYER-OFF KVT-FIELD! swap
   swap 0 KVT-D-MODE 0 T= swap
   KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-BATCH-MUTATORS ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   32 0 KVT-ALLOC
   KVT-MUST-BEGIN
   swap KVT-SNAPSHOT swap
   E-KV-BATCH KVT-ACTIVE-ALLOC-ERR
   0 E-KV-BATCH KVT-ACTIVE-FORK-ERR
   0 E-KV-BATCH KVT-ACTIVE-CANCEL-ERR
   0 E-KV-BATCH KVT-ACTIVE-APPEND-ERR
   swap KVT-SNAPSHOT= TTRUE swap
   KVT-MUST-CANCEL
   16 1 KVT-ALLOC
   0 2 KVT-FORK
   0 KVT-APPEND
   1 KVT-CANCEL  2 KVT-CANCEL  0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

: KVT-SKIP-ROLLBACK ( -- )
   KVT-MUST-SESSION KVT-OPEN-STANDARD
   16 0 KVT-ALLOC
   KVT-MUST-BEGIN KVT-BATCH-GEN {: gen:n :}
   0 KVT-MUST-ADD
   0 KVT-BATCH-OFF!
   swap E-KV-INVARIANT KVT-CHECK-ERR swap
   gen KVT-BATCH-OFF!
   KVT-MUST-CANCEL
   0 KVT-CANCEL
   KVT-CHECK
   KVT-MUST-CLOSE KVT-MUST-SESSION-CLOSE ;

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
   KVT-MUST-BEGIN 0 KVT-MUST-ADD KVT-MUST-CANCEL
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
   KVT-RELEASE-B @ 856 T=
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
   KVT-RELEASE-B @ 856 T=
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

: KVT-BATCH-REJECTS ( -- )
   s" KVT-BAD-BATCH-DUP ( KV:batch -- KV:batch KV:batch ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-BATCH-DROP ( KV:batch -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-BATCH-RAW ( n -- KV:batch ) drop 0" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-BATCH-MINT ( n -- KV:batch ) KV:KB-MINT" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-BAD-BATCH-TAKE ( KV:batch -- n ) KV:KB-TAKE" CHECK-QUIET-CANDIDATE! 1 T=
   s" KVT-BAD-CANCEL-DROP ( KV:cancel-result -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-ADD-DUP ( KV:add-result -- KV:add-result KV:add-result ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" KVT-BAD-ADD-DROP ( KV:add-result -- ) drop" CHECK-QUIET-CANDIDATE! 0 T= ;

: KVT-ADD-PRESENT ( -- )
   s" KVT-ADD-CALL ( KV:cache KV:batch KV:seq -- KV:add-result ) KV:ADD"
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ The suite's one precondition. CUDA:OPEN? answers whether the driver loaded
\ without throwing, so the diagnostic names the missing host capability at the
\ top of the run rather than letting each case report a downstream assertion.
\ This is a hard stop, not a skip: a device suite on a device-less host has not
\ passed, and the exit code plus message say exactly why.
: KVT-REQUIRE-DEVICE ( -- )
   CUDA:OPEN? if exit then
   s" kv-cache-device-test: no CUDA driver (dlopen libcuda.so.1 failed); this suite is device-required - run it on a host with a CUDA driver" 74 die ;

: KVT-RUN ( -- )
   KVT-REQUIRE-DEVICE
   T-RESET
   s" post-open operations require no host allocation" T-LABEL [: KVT-NO-ALLOC ;] 0 TTHROWSQ
   s" exact geometry and footprint" T-LABEL [: KVT-GEOMETRY ;] 0 TTHROWSQ
   s" bounded K and V device spans" T-LABEL [: KVT-SPANS ;] 0 TTHROWSQ
   s" stale generation" T-LABEL [: KVT-STALE ;] 0 TTHROWSQ
   s" cross-cache identity" T-LABEL [: KVT-CROSS-CACHE ;] 0 TTHROWSQ
   s" reservation and capacity" T-LABEL [: KVT-CAPACITY ;] 0 TTHROWSQ
   s" copy failure atomicity and real full-page COW" T-LABEL [: KVT-COW ;] 0 TTHROWSQ
   s" allocation-free churn" T-LABEL [: KVT-CHURN ;] 0 TTHROWSQ
   s" one active batch and monotone generation" T-LABEL [: KVT-BATCH-LIFETIME ;] 0 TTHROWSQ
   s" stale batch refusal preserves the active owner" T-LABEL [: KVT-BATCH-STALE ;] 0 TTHROWSQ
   s" zero batch generation cannot cancel an inactive cache" T-LABEL [: KVT-BATCH-ZERO ;] 0 TTHROWSQ
   s" cross-cache batch refusal preserves both owners" T-LABEL [: KVT-BATCH-CROSS ;] 0 TTHROWSQ
   s" batch generation exhaustion is mutation-free" T-LABEL [: KVT-BATCH-EXHAUST ;] 0 TTHROWSQ
   s" batch lifetime changes no allocator, device, or metric state" T-LABEL [: KVT-BATCH-PURE ;] 0 TTHROWSQ
   s" ADD surface certifies" T-LABEL [: KVT-ADD-PRESENT ;] 0 TTHROWSQ
   s" provisional grow and exact rollback" T-LABEL [: KVT-ADD-GROW ;] 0 TTHROWSQ
   s" unique partial tail stages without page mutation" T-LABEL [: KVT-ADD-SAME ;] 0 TTHROWSQ
   s" provisional copy-on-write and exact rollback" T-LABEL [: KVT-ADD-COW ;] 0 TTHROWSQ
   s" shared partial tail stages parent then child" T-LABEL [: 0 1 KVT-ADD-SHARED ;] 0 TTHROWSQ
   s" shared partial tail stages child then parent" T-LABEL [: 1 0 KVT-ADD-SHARED ;] 0 TTHROWSQ
   s" three shared tails stage two departures then unique" T-LABEL [: KVT-ADD-SHARED3 ;] 0 TTHROWSQ
   s" multi-row cancellation restores allocator state" T-LABEL [: KVT-ADD-N-ROLLBACK ;] 0 TTHROWSQ
   s" duplicate ADD refusal preserves staged state" T-LABEL [: KVT-ADD-DUP ;] 0 TTHROWSQ
   s" stale ADD refusal" T-LABEL [: KVT-ADD-STALE ;] 0 TTHROWSQ
   s" cross-cache ADD refusal" T-LABEL [: KVT-ADD-CROSS ;] 0 TTHROWSQ
   s" maximum-token ADD refusal" T-LABEL [: KVT-ADD-LIMIT ;] 0 TTHROWSQ
   s" reservation capacity layer and arithmetic refusals" T-LABEL [: KVT-ADD-REFUSALS ;] 0 TTHROWSQ
   s" inactive zero batch ADD refusal preserves returned owners" T-LABEL [: KVT-ADD-BATCH-ZERO ;] 0 TTHROWSQ
   s" stale batch ADD refusal preserves returned owners" T-LABEL [: KVT-ADD-BATCH-STALE ;] 0 TTHROWSQ
   s" cross-cache batch ADD refusal preserves returned owners" T-LABEL [: KVT-ADD-BATCH-CROSS ;] 0 TTHROWSQ
   s" four batch mutators refuse and re-enable" T-LABEL [: KVT-BATCH-MUTATORS ;] 0 TTHROWSQ
   s" skipped rollback mutation fails KV-CHECK" T-LABEL [: KVT-SKIP-ROLLBACK ;] 0 TTHROWSQ
   s" device allocation failure cleanup" T-LABEL [: KVT-DEVICE-ALLOC-FAIL ;] 0 TTHROWSQ
   s" host allocation failure cleanup" T-LABEL [: KVT-HOST-ALLOC-FAIL ;] 0 TTHROWSQ
   s" forged configuration rejected before allocation" T-LABEL [: KVT-CONFIG-FAIL ;] 0 TTHROWSQ
   s" multiplicative overflow rejected before allocation" T-LABEL [: KVT-MUL-FAIL ;] 0 TTHROWSQ
   s" additive overflow rejected before allocation" T-LABEL [: KVT-ADD-FAIL ;] 0 TTHROWSQ
   s" device close failure with host release" T-LABEL [: KVT-CLOSE-FAIL ;] 0 TTHROWSQ
   s" old pointer API absent" T-LABEL [: KVT-OLD-API ;] 0 TTHROWSQ
   s" device span borrows remain private" T-LABEL [: KVT-PRIVATE-SPANS ;] 0 TTHROWSQ
   s" linear cache misuse rejected" T-LABEL [: KVT-LINEAR-REJECTS ;] 0 TTHROWSQ
   s" linear batch misuse and representation access rejected" T-LABEL [: KVT-BATCH-REJECTS ;] 0 TTHROWSQ
   MKD:USE-REAL
   HOST-USE-REAL
   T-REPORT ;

KVT-RUN

;package
