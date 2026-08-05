\ engine-test.f - sole GPT-2 session/model/KV owner.
\ Run: bin/hb --load maki/infer/engine-test.f -- gpt2-model

require lib/test.f
require lib/fs-path.f
require lib/process-fork.f
require lib/test/mmap-exhaust.f
require lib/test/outcome.f
require test/checker-assert.f
require maki/infer/engine.f

package KV
private

: ENGINE-HOST-FAIL ( ptr n n -- ptr n n ) E-MEM-MAP throw ;

: ENGINE-DEV-EXHAUST ( ptr n len -- rc )
   {: out:ptr bytes:len :}
   out bytes CUDA:CU-MEM-ALLOC RC>N {: code:n :}
   code 0= if 1 MMAP-TEST:EXHAUST-CHILD then
   code >RC ;

: ENGINE-HOST-EXHAUST ( ptr n n -- ptr n n )
   ALLOC-HOST-REAL
   [: ENGINE-DEV-EXHAUST ;] MKD:CUMEMALLOC! ;

public

: ENGINE-HOST-FAIL! ( -- ) [: ENGINE-HOST-FAIL ;] is HOST-ALLOC ;
: ENGINE-HOST-EXHAUST! ( -- ) [: ENGINE-HOST-EXHAUST ;] is HOST-ALLOC ;
: ENGINE-HOST-REAL! ( -- ) HOST-USE-REAL ;

;package

package INFER
private

-7698 constant E-FIX
$86 constant FAULT-RC
$7FFFFFFFFFFFFFFF constant MAX-N

variable OPEN-N
variable FREE-N
variable SYNC-N
variable FREE-AT-SYNC
PTR-VARIABLE SAVED-REC

: IC ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: MODEL-PATH ( -- FS:path ) 0 SCRIPT-ARGV$ FS-PATH:MAKE ;
: BAD-PATH ( -- FS:path ) s" /tmp/habu-no-engine-model" FS-PATH:MAKE ;

: MUST-STOP ( INFER:engine -- )
   STOP MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: START-ERR
   ( FS:path CAD-NUM:item-count CAD-NUM:item-count n -- )
   {: want:n :}
   START-GPT2 MATCH result
      err OF want T= ENDOF
      ok OF MUST-STOP false TTRUE ENDOF
   ;MATCH ;

: MUST-START ( -- INFER:engine )
   MODEL-PATH 2 IC 128 IC START-GPT2 MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: YES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 0 T= ;

: TEST-TYPES ( -- )
   s" engine exposes only its linear start/stop owner boundary" T-LABEL
   s" EI-CFG ( GPT2:model -- GPT2:model GPT2:config ) GPT2:CONFIG@" YES
   s" EI-START ( FS:path CAD-NUM:item-count CAD-NUM:item-count -- result<INFER:engine,n> ) INFER:START-GPT2" YES
   s" EI-STOP ( INFER:engine -- result<n,n> ) INFER:STOP" YES
   s" EI-FORGE ( n -- INFER:engine )" NO
   s" EI-DUP ( INFER:engine -- INFER:engine INFER:engine ) dup" NO
   s" EI-DROP ( INFER:engine -- ) drop" NO
   s" EI-RAW ( INFER:engine -- n )" NO
   s" INFER:I-MINT" XREF-FIND XREF-FOUND? TFALSE
   s" INFER:I-TAKE" XREF-FIND XREF-FOUND? TFALSE
   s" INFER:SESSION@" XREF-FIND XREF-FOUND? TFALSE
   s" INFER:MODEL@" XREF-FIND XREF-FOUND? TFALSE
   s" INFER:CACHE@" XREF-FIND XREF-FOUND? TFALSE ;

: TEST-KV-CONFIG ( -- )
   s" authenticated asymmetric GPT-2 config maps every KV field exactly" T-LABEL
   MAKI-DATATYPE:DF32
   32 64 5 24 3 true 63 63 0.00001 true GPT2:BUILD
   2 7 I-KV-CONFIG
   MATCH result
      err OF throw ENDOF
      ok OF
         KV-CONFIG:UNMAKE
         {: nl:n nh:n hd:n db:n pages:n seqs:n ctx:n ptok:n :}
         nl 5 T=
         nh 3 T=
         hd 8 T=
         db 4 T=
         pages 7 T=
         seqs 2 T=
         ctx 32 T=
         ptok 16 T=
      ENDOF
   ;MATCH ;

: F-OPEN ( -- )
   CUDA:OPEN
   1 OPEN-N +!
   601 throw ;

: TEST-EARLY-FAILURES ( -- )
   s" zero capacities precede GPU acquisition" T-LABEL
   0 OPEN-N !
   [: F-OPEN ;] MKD:OPEN!
   BAD-PATH 0 IC 128 IC E-KV-CONFIG START-ERR
   BAD-PATH 2 IC 0 IC E-KV-CONFIG START-ERR
   OPEN-N @ 0 T=
   MKD:USE-REAL ;

: TEST-SESSION-FAIL ( -- )
   s" driver-open refusal propagates" T-LABEL
   0 OPEN-N !
   [: F-OPEN ;] MKD:OPEN!
   BAD-PATH 2 IC 128 IC 601 START-ERR
   MKD:USE-REAL
   OPEN-N @ 1 T= ;

: F-SYNC-611 ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   stream CUDA:CU-STREAM-SYNCHRONIZE RC>N {: real:n :}
   1 SYNC-N +!
   real 0<> if real >RC else 611 >RC then ;

: TEST-MODEL-FAIL ( -- )
   s" model refusal outranks session cleanup failure" T-LABEL
   0 SYNC-N !
   [: F-SYNC-611 ;] MKD:STREAMSYNC!
   BAD-PATH 2 IC 128 IC E-FS-OPEN START-ERR
   MKD:USE-REAL
   SYNC-N @ 1 T= ;

: TEST-CONFIG-FAIL ( -- )
   s" authenticated model dimensions drive KV validation before cache allocation" T-LABEL
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   MODEL-PATH 2 IC MAX-N IC E-KV-CONFIG START-ERR
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T= ;

: F-FREE-612 ( cuda-devptr -- rc ) {: dev:cuda-devptr :}
   dev CUDA:CU-MEM-FREE RC>N {: real:n :}
   1 FREE-N +!
   real 0<> if real >RC else 612 >RC then ;

: F-SYNC-613 ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   stream CUDA:CU-STREAM-SYNCHRONIZE RC>N {: real:n :}
   1 SYNC-N +!
   real 0<> if real >RC else 613 >RC then ;

: REC-FAIL-CHILD ( -- )
   0 FREE-N ! 0 SYNC-N !
   KV:ENGINE-HOST-EXHAUST!
   [: F-FREE-612 ;] MKD:CUMEMFREE!
   [: F-SYNC-613 ;] MKD:STREAMSYNC!
   MODEL-PATH 2 IC 128 IC START-GPT2 MATCH result
      err OF E-MEM-MAP <> if E-FIX throw then ENDOF
      ok OF MUST-STOP E-FIX throw ENDOF
   ;MATCH
   FREE-N @ 2 <> if E-FIX throw then
   SYNC-N @ 1 <> if E-FIX throw then
   s" " 0 die ;

: TEST-REC-FAIL ( -- )
   s" late engine record refusal releases every acquired owner" T-LABEL
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if REC-FAIL-CHILD then
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

: TEST-CACHE-FAIL ( -- )
   s" cache refusal releases model and session but keeps the primary error" T-LABEL
   0 FREE-N ! 0 SYNC-N !
   KV:ENGINE-HOST-FAIL!
   [: F-FREE-612 ;] MKD:CUMEMFREE!
   [: F-SYNC-613 ;] MKD:STREAMSYNC!
   MODEL-PATH 2 IC 128 IC E-MEM-MAP START-ERR
   MKD:USE-REAL KV:ENGINE-HOST-REAL!
   FREE-N @ 1 T=
   SYNC-N @ 1 T= ;

: CONFIG= ( GPT2:config -- )
   GPT2:DATATYPE@ MAKI-DATATYPE:DF32 MAKI-DATATYPE:EQ TTRUE
   GPT2:NCTX@ 1024 T=
   GPT2:NVOCAB@ 50257 T=
   GPT2:NLAYER@ 12 T=
   GPT2:NEMBD@ 768 T=
   GPT2:NHEAD@ 12 T=
   GPT2:TIED? TTRUE
   GPT2:BOS@ 50256 T=
   GPT2:EOS@ 50256 T=
   GPT2:LN-EPS@ 0.00001 f= TTRUE
   GPT2:ATTN-SCALE? TTRUE
   drop ;

: INSPECT ( INFER:engine -- INFER:engine )
   I-TAKE
   swap GPT2:CONFIG@ CONFIG=
   swap
   KV:NUM-PAGES 128 T=
   KV:PAGE-SIZE 16 T=
   KV:TOK-BYTES 73728 T=
   KV:PAGE-BYTES 1179648 T=
   KV:MAX-CONTEXT 1024 T=
   KV:BLOCK-CAPACITY 64 T=
   KV:FREE-PAGES 128 T=
   KV:WATERMARK 0 T=
   I-MINT ;

: ALLOC-SEQ ( KV:cache -- KV:cache )
   16 KV:ALLOC-SEQ MATCH result
      err OF throw ENDOF
      ok OF KV-SEQ:UNMAKE drop drop drop ENDOF
   ;MATCH ;

: ALLOC-SEQ-ERR ( KV:cache -- KV:cache )
   16 KV:ALLOC-SEQ MATCH result
      err OF E-KV-SEQS T= ENDOF
      ok OF KV-SEQ:UNMAKE drop drop drop false TTRUE ENDOF
   ;MATCH ;

: SEQ-CAP ( INFER:engine -- INFER:engine )
   s" engine admits exactly its configured live-sequence capacity" T-LABEL
   I-TAKE
   ALLOC-SEQ ALLOC-SEQ
   KV:RESERVED-PAGES 2 T=
   KV:FREE-PAGES 128 T=
   ALLOC-SEQ-ERR
   KV:RESERVED-PAGES 2 T=
   I-MINT ;

: SAVE-REC ( INFER:engine -- INFER:engine )
   I-TAKE
   >r >r >r
   MEM:BORROW {: rec:ptr recu:CAD-NUM:byte-len :}
   recu drop
   rec SAVED-REC !
   r> r> r> I-MINT ;

: READ-REC ( -- )
   SAVED-REC @ c@ drop
   s" " 0 die ;

: REC-UNMAPPED ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if 2 close-rc drop READ-REC then
   pid PROC-WAIT-OUTCOME FAULT-RC T-OUTCOME-EXITED= ;

: TEST-REAL ( -- )
   s" two real engines coexist and stop in either order" T-LABEL
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   MUST-START INSPECT SEQ-CAP MUST-START
   swap SAVE-REC MUST-STOP MUST-STOP REC-UNMAPPED
   MUST-START MUST-START
   MUST-STOP MUST-STOP
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T= ;

: F-FREE-FIRST ( cuda-devptr -- rc ) {: dev:cuda-devptr :}
   dev CUDA:CU-MEM-FREE RC>N {: real:n :}
   1 FREE-N +!
   real 0<> if real >RC exit then
   FREE-N @ 1 = if 701 else 702 then >RC ;

: F-SYNC-703 ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   stream CUDA:CU-STREAM-SYNCHRONIZE RC>N {: real:n :}
   FREE-N @ FREE-AT-SYNC !
   1 SYNC-N +!
   real 0<> if real >RC else 703 >RC then ;

: TEST-STOP-FAIL ( -- )
   s" STOP attempts cache, model, and session release with first-error precedence" T-LABEL
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   MUST-START
   0 FREE-N ! 0 SYNC-N ! 0 FREE-AT-SYNC !
   [: F-FREE-FIRST ;] MKD:CUMEMFREE!
   [: F-SYNC-703 ;] MKD:STREAMSYNC!
   STOP MATCH result
      ok OF drop false TTRUE ENDOF
      err OF 701 T= ENDOF
   ;MATCH
   MKD:USE-REAL
   FREE-N @ 2 T=
   FREE-AT-SYNC @ 2 T=
   SYNC-N @ 1 T=
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T= ;

: RUN ( -- )
   T-RESET
   TEST-TYPES
   TEST-KV-CONFIG
   TEST-EARLY-FAILURES
   TEST-SESSION-FAIL
   SCRIPT-ARGC 0= if
      s" infer-engine: no model root argument -> device leg SKIPPED" type cr
   else
      TEST-REC-FAIL
      TEST-MODEL-FAIL
      TEST-CONFIG-FAIL
      TEST-CACHE-FAIL
      TEST-REAL
      TEST-STOP-FAIL
   then
   MKD:USE-REAL KV:ENGINE-HOST-REAL!
   T-REPORT ;

RUN

;package
