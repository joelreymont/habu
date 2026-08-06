\ gpt2-model-test.f - packed GPT-2 layout and real GPU ownership.

require lib/test.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-path.f
require lib/process-fork.f
require test/checker-assert.f
require lib/test/mmap-exhaust.f
require lib/test/outcome.f
require maki/infer/gpt2-model.f

package GPT2
private

-7697 constant E-FIX
$86 constant FAULT-RC

create BASE-BYTE 1 allot
create ROOT FS-PATH-CAP allot
create SRC FS-PATH-CAP allot
create DST FS-PATH-CAP allot
create JOIN-ROOT FS-PATH-CAP allot
create JOIN-DST FS-PATH-CAP allot
create EMPTY-MODEL 2 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 123 c, 125 c,
create MUT-BYTE $5B c,

variable ROOT-U
variable SRC-U
variable DST-U
variable UPLOAD-N
variable UPLOAD-BASE
variable UPLOAD-NEXT
variable UPLOAD-BAD
variable STREAM-CREATE-N
variable STREAM-DESTROY-N
TYPED-VARIABLE TOK-LEN-OBS CAD-NUM:alloc-byte-len

: BASE ( -- ptr u8 ) BASE-BYTE ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SRC$ ( -- ptr u8 n ) SRC SRC-U @ ;
: DST$ ( -- ptr u8 n ) DST DST-U @ ;

: PREPARE-EMPTY ( ptr u8 n -- ) {: model:ptr modelu:n :}
   CLEANUP-RESET
   s" habu-gpt2-empty" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   model modelu GPT2PIN:CONFIG-NAME$ SRC JOIN-PATH SRC-U !
   ROOT$ GPT2PIN:CONFIG-NAME$ DST JOIN-PATH DST-U !
   SRC$ DST$ COPY-FILE-STREAM
   model modelu GPT2PIN:VOCAB-NAME$ SRC JOIN-PATH SRC-U !
   ROOT$ GPT2PIN:VOCAB-NAME$ DST JOIN-PATH DST-U !
   SRC$ DST$ COPY-FILE-STREAM
   model modelu GPT2PIN:MERGES-NAME$ SRC JOIN-PATH SRC-U !
   ROOT$ GPT2PIN:MERGES-NAME$ DST JOIN-PATH DST-U !
   SRC$ DST$ COPY-FILE-STREAM
   ROOT$ GPT2PIN:MODEL-NAME$ DST JOIN-PATH DST-U !
   DST$ EMPTY-MODEL 10 WRITE-ALL ;

: CFG ( -- GPT2:config )
   MAKI-DATATYPE:DF32
   8 5 2 4 2 true 4 4 0.00001 true GPT2:BUILD ;

: CFG-DEEP ( -- GPT2:config )
   MAKI-DATATYPE:DF32
   1 1 MAX-N 1 1 true 0 0 0.00001 true GPT2:BUILD ;

: IDX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: BOFF ( n -- CAD-NUM:byte-off )
   CAD-NUM:BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: END-OFF ( CAD-NUM:byte-off CAD-NUM:byte-len -- CAD-NUM:byte-off )
   CAD-NUM:ADVANCE-BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: SPAN= ( GPT2:config n n n -- GPT2:config )
   {: slot:n want-off:n want-len:n :}
   slot IDX GPT2:TENSOR-ID-FOR-SLOT GPT2:SPAN
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   BASE off CAD-NUM:BYTE+
   BASE want-off BOFF CAD-NUM:BYTE+ = TTRUE
   BASE off len END-OFF CAD-NUM:BYTE+
   BASE want-off want-len + BOFF CAD-NUM:BYTE+ = TTRUE ;

: ALL-SPANS
   ( GPT2:config CAD-NUM:byte-off n n -- GPT2:config CAD-NUM:byte-off )
   {: want:CAD-NUM:byte-off slot:n total:n :}
   slot total = if want exit then
   slot IDX GPT2:TENSOR-ID-FOR-SLOT GPT2:SPAN
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   BASE off CAD-NUM:BYTE+ BASE want CAD-NUM:BYTE+ = TTRUE
   BASE off len END-OFF CAD-NUM:BYTE+ BASE off CAD-NUM:BYTE+ > TTRUE
   want len END-OFF slot 1+ total RECURSE ;

: TEST-SPAN ( -- )
   s" SPAN packs every GPT-2 tensor contiguously through the final byte" T-LABEL
   CFG COUNT {: total:n :}
   0 BOFF 0 total ALL-SPANS
   {: final:CAD-NUM:byte-off :}
   M-TOTAL-LEN {: bytes:CAD-NUM:byte-len :}
   BASE final CAD-NUM:BYTE+
   BASE 0 BOFF bytes END-OFF CAD-NUM:BYTE+ = TTRUE
   drop
   s" SPAN retains exact global, layer, mask, and final pins" T-LABEL
   CFG
   0 0 80 SPAN=
   4 240 16 SPAN=
   6 272 256 SPAN=
   29 2688 16 SPAN=
   drop ;

: FILL ( ptr u8 n n -- ) {: dst:ptr u:n ch:n :}
   u 0 ?do ch dst i + c! loop ;

: TEST-PATH-LIMIT ( -- )
   s" joined GPT-2 paths accept the exact capacity with either root form" T-LABEL
   GPT2PIN:MODEL-NAME$ nip {: nameu:n :}
   FS-PATH-CAP nameu - 1- {: plainu:n :}
   JOIN-ROOT plainu 120 FILL
   JOIN-ROOT plainu nameu M-JOIN-LEN FS-PATH-CAP T=
   JOIN-ROOT plainu GPT2PIN:MODEL-NAME$ JOIN-DST JOIN-PATH FS-PATH-CAP T=
   FS-PATH-CAP nameu - {: slashu:n :}
   JOIN-ROOT slashu 120 FILL
   FS-SLASH JOIN-ROOT slashu 1- + c!
   JOIN-ROOT slashu nameu M-JOIN-LEN FS-PATH-CAP T=
   JOIN-ROOT slashu GPT2PIN:MODEL-NAME$ JOIN-DST JOIN-PATH FS-PATH-CAP T= ;

: YES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 0 T= ;
: UNK ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 1 T= ;

: TEST-OPAQUE ( -- )
   s" GPT2:model has no public forge or raw-cell conversion" T-LABEL
   s" GM-OPEN ( GPU:session FS:path -- GPU:session result<GPT2:model,n> ) GPT2:OPEN" YES
   s" GM-OLD-OPEN ( FS:path -- result<GPT2:model,n> ) GPT2:OPEN" NO
   s" GM-LOGITS ( GPU:session GPT2:model n ptr u8 CAD-NUM:byte-len -- GPU:session GPT2:model result<n,n> ) GPT2:LOGITS" YES
   s" GM-OLD-LOGITS ( GPT2:model n ptr u8 CAD-NUM:byte-len -- GPT2:model result<n,n> ) GPT2:LOGITS" NO
   s" GM-CLOSE ( GPU:session GPT2:model -- GPU:session result<n,n> ) GPT2:CLOSE" YES
   s" GM-OLD-CLOSE ( GPT2:model -- result<n,n> ) GPT2:CLOSE" NO
   s" GM-MAKE ( ptr u8 -- GPT2:model ) GPT2-MODEL:MAKE" UNK
   s" GM-UNMAKE ( GPT2:model -- ptr u8 ) GPT2-MODEL:UNMAKE" UNK
   s" GM-DUP ( GPT2:model -- GPT2:model GPT2:model ) dup" NO
   s" GM-DROP ( GPT2:model -- ) drop" NO
   s" GM-RAW-IN ( n -- GPT2:model )" NO
   s" GM-RAW-OUT ( GPT2:model -- n )" NO ;

: CLOSE-OK ( result<n,n> -- )
   MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: SESSION-OPEN ( -- GPU:session )
   GPU:OPEN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: SESSION-CLOSE ( GPU:session -- )
   GPU:CLOSE CLOSE-OK ;

: TRACK-STREAM-CREATE ( ptr n n -- rc )
   1 STREAM-CREATE-N +!
   CUDA:CU-STREAM-CREATE ;

: TRACK-STREAM-DESTROY ( CUDA:stream -- rc )
   1 STREAM-DESTROY-N +!
   CUDA:CU-STREAM-DESTROY ;

: STREAM-TRACK ( -- )
   0 STREAM-CREATE-N !
   0 STREAM-DESTROY-N !
   [: TRACK-STREAM-CREATE ;] MKD:STREAMCREATE!
   [: TRACK-STREAM-DESTROY ;] MKD:STREAMDESTROY! ;

: DROP-RC0 ( a -- rc ) drop 0 >RC ;

: SYNTH-OUT ( ptr n a -- rc )
   drop 1 swap ! 0 >RC ;

: SYNTH-CLOSE ( GPU:session -- )
   [: DROP-RC0 ;] MKD:CTXSET!
   [: DROP-RC0 ;] MKD:STREAMSYNC!
   [: DROP-RC0 ;] MKD:STREAMDESTROY!
   [: DROP-RC0 ;] MKD:CTXRELEASE!
   SESSION-CLOSE
   MKD:USE-REAL ;

: MODEL-CLOSE ( GPU:session GPT2:model -- GPU:session )
   GPT2:CLOSE CLOSE-OK ;

: LAYOUT-FIRST-CHILD ( -- )
   [: ;] MKD:OPEN!
   [: DROP-RC0 ;] MKD:CUINIT!
   [: SYNTH-OUT ;] MKD:CUDEVICEGET!
   [: SYNTH-OUT ;] MKD:CTXRETAIN!
   [: DROP-RC0 ;] MKD:CTXSET!
   [: SYNTH-OUT ;] MKD:STREAMCREATE!
   SESSION-OPEN
   FS-PATH-CAP MMAP-TEST:EXHAUST-CHILD
   FS-PATH-CAP MMAP-TEST:EXHAUSTED? 0= if E-FIX throw then
   s" /tmp" CFG-DEEP M-FROM-CFG
   MATCH result
      err OF E-SIZE <> if E-FIX throw then ENDOF
      ok OF MODEL-CLOSE E-FIX throw ENDOF
   ;MATCH
   FS-PATH-CAP MMAP-TEST:EXHAUSTED? 0= if E-FIX throw then
   SYNTH-CLOSE
   s" " 0 die ;

: TEST-LAYOUT-FIRST ( -- )
   s" layout overflow precedes scratch allocation and preserves the session" T-LABEL
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if LAYOUT-FIRST-CHILD then
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

: TEST-REFUSAL ( GPU:session -- GPU:session )
   s" OPEN refusal preserves the caller session and leaves no source owner" T-LABEL
   SAFET:LIVE-OWNERS {: before:n :}
   SAFET-MAP:LIVE {: maps:n :}
   s" /tmp/habu-no-gpt2-model" FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF E-FS-OPEN T= ENDOF
      ok OF MODEL-CLOSE false TTRUE ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS before T=
   SAFET-MAP:LIVE maps T= ;

: OPEN-REFUSAL ( GPU:session n -- GPU:session ) {: want:n :}
   ROOT$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF want <> if E-FIX throw then ENDOF
      ok OF MODEL-CLOSE E-FIX throw ENDOF
   ;MATCH ;

: REFUSAL-PROOF ( n -- ) {: want:n :}
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   SESSION-OPEN
   want OPEN-REFUSAL
   SAFET:LIVE-OWNERS owners <> if E-FIX throw then
   SAFET-MAP:LIVE maps <> if E-FIX throw then
   SESSION-CLOSE ;

: TEST-EMPTY ( ptr u8 n -- )
   s" OPEN rejects an empty real Safetensors catalog and restores VM ownership" T-LABEL
   PREPARE-EMPTY
   E-CATALOG REFUSAL-PROOF
   CLEANUP-RUN ;

: TEST-TOKEN-MISSING ( ptr u8 n -- )
   s" OPEN rejects a missing pinned tokenizer asset before GPU ownership" T-LABEL
   PREPARE-EMPTY
   ROOT$ GPT2PIN:VOCAB-NAME$ DST JOIN-PATH DST-U !
   DST$ REMOVE-FILE
   SAFET:LIVE-OWNERS {: before:n :}
   SAFET-MAP:LIVE {: maps:n :}
   SESSION-OPEN
   ROOT$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF E-TOK-IO T= ENDOF
      ok OF MODEL-CLOSE false TTRUE ENDOF
   ;MATCH
   SESSION-CLOSE
   SAFET:LIVE-OWNERS before T=
   SAFET-MAP:LIVE maps T=
   CLEANUP-RUN ;

: MUTATE-VOCAB ( -- )
   ROOT$ GPT2PIN:VOCAB-NAME$ DST JOIN-PATH DST-U !
   DST$ MUT-BYTE 1 FS-O-WRONLY FS-WRITE-BY-FLAGS ;

: MUTATE-MERGES ( -- )
   ROOT$ GPT2PIN:MERGES-NAME$ DST JOIN-PATH DST-U !
   DST$ MUT-BYTE 1 FS-O-WRONLY FS-WRITE-BY-FLAGS ;

: TEST-VOCAB-DIGEST ( ptr u8 n -- )
   s" OPEN rejects a mutated pinned vocab and restores VM ownership" T-LABEL
   PREPARE-EMPTY
   MUTATE-VOCAB
   E-TOK-DIGEST REFUSAL-PROOF
   CLEANUP-RUN ;

: TEST-MERGES-DIGEST ( ptr u8 n -- )
   s" OPEN rejects mutated pinned merges and restores VM ownership" T-LABEL
   PREPARE-EMPTY
   MUTATE-MERGES
   E-TOK-DIGEST REFUSAL-PROOF
   CLEANUP-RUN ;

: TOKEN-LIVE-CELLS ( -- n )
   8
   T-MERGE-N + T-MERGE-N +
   T-HCAP + T-HCAP + T-HCAP +
   T-ID-CAP +
   T-GID-N +
   T-VOCAB-N +
   T-DEC-CAP +
   T-ARENA-CAP +
   T-SCAP + T-SCAP + T-SCAP + T-SCAP +
   T-ID-CAP +
   T-LOGIT-CELLS + ;

: TOKEN-BYTES ( -- n )
   TOKEN-LIVE-CELLS >COUNT MEM-CELLS>BYTES ;

: TOKEN-ALLOC-LEN ( -- CAD-NUM:alloc-byte-len )
   TOKEN-BYTES MEM:BYTES-ALLOC-LEN ;

: ALLOC-REFUSAL-CHILD ( -- )
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   SESSION-OPEN
   TOKEN-BYTES MMAP-TEST:EXHAUST-CHILD
   TOKEN-BYTES MMAP-TEST:EXHAUSTED? 0= if E-FIX throw then
   0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF E-MEM-MAP <> if E-FIX throw then ENDOF
      ok OF MODEL-CLOSE E-FIX throw ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS owners <> if E-FIX throw then
   SAFET-MAP:LIVE maps <> if E-FIX throw then
   TOKEN-BYTES MMAP-TEST:EXHAUSTED? 0= if E-FIX throw then
   \ Keep synthetic VM exhaustion active through every OPEN assertion. Only
   \ teardown is injected: CUDA cleanup itself needs allocator headroom, and
   \ process exit releases the doomed child's real driver resources.
   SYNTH-CLOSE
   s" " 0 die ;

: TEST-ALLOC-REFUSAL ( -- )
   s" OPEN preserves its session and source owners on tokenizer allocation refusal" T-LABEL
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if ALLOC-REFUSAL-CHILD then
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

: TOKEN-OWNER ( GPT2:model -- GPT2:model ptr n CAD-NUM:alloc-byte-len )
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   tokstate toklen ;

: READ-TOKEN-TAIL ( ptr n -- )
   TOKEN-LIVE-CELLS 1- cells + @ drop
   s" " 0 die ;

: TOKEN-UNMAPPED ( ptr n -- ) {: tokstate:ptr :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if 2 close-rc drop tokstate READ-TOKEN-TAIL then
   pid PROC-WAIT-OUTCOME FAULT-RC T-OUTCOME-EXITED= ;

: TRACK-HTOD ( cuda-devptr ptr u8 len -- rc )
   {: dst:cuda-devptr src:ptr len:len :}
   dst CUDA-DEVPTR>N {: raw:n :}
   len LEN>N {: u:n :}
   UPLOAD-N @ 0= if raw UPLOAD-BASE ! then
   raw UPLOAD-BASE @ UPLOAD-NEXT @ + <> if 1 UPLOAD-BAD ! then
   UPLOAD-NEXT @ u + UPLOAD-NEXT !
   1 UPLOAD-N +!
   dst src len CUDA:CU-MEMCPY-HTOD ;

: UPLOAD-RESET ( -- )
   0 UPLOAD-N !
   0 UPLOAD-BASE !
   0 UPLOAD-NEXT !
   0 UPLOAD-BAD ! ;

: TEST-UPLOAD-TOTAL ( CAD-NUM:byte-len -- ) {: bytes:CAD-NUM:byte-len :}
   BASE 0 BOFF bytes END-OFF CAD-NUM:BYTE+
   BASE UPLOAD-NEXT @ BOFF CAD-NUM:BYTE+ = TTRUE ;

: REAL-TOTAL ( ptr u8 n -- CAD-NUM:byte-len )
   FS-PATH:MAKE HF:OPEN-GPT2
   MATCH result
      err OF throw ENDOF
      ok OF
         M-TOTAL-LEN {: bytes:CAD-NUM:byte-len :}
         drop bytes
      ENDOF
   ;MATCH ;

: TEST-DEVICE ( -- )
   TEST-LAYOUT-FIRST
   SCRIPT-ARGC 0= if
      s" gpt2-model: no model root argument -> device leg SKIPPED" type cr
      exit
   then
   \ CUDA driver state cannot cross fork; run every fork-isolated refusal
   \ before this process opens its caller-owned session.
   TEST-ALLOC-REFUSAL
   0 SCRIPT-ARGV$ TEST-VOCAB-DIGEST
   0 SCRIPT-ARGV$ TEST-MERGES-DIGEST
   0 SCRIPT-ARGV$ TEST-EMPTY
   SESSION-OPEN
   STREAM-TRACK
   TEST-REFUSAL
   STREAM-CREATE-N @ 0 T=
   STREAM-DESTROY-N @ 0 T=
   SESSION-CLOSE
   STREAM-CREATE-N @ 0 T=
   STREAM-DESTROY-N @ 1 T=
   MKD:USE-REAL
   0 SCRIPT-ARGV$ TEST-TOKEN-MISSING
   s" OPEN uploads the pinned GPT-2 model and CLOSE releases every owner" T-LABEL
   0 SCRIPT-ARGV$ REAL-TOTAL {: total:CAD-NUM:byte-len :}
   SAFET:LIVE-OWNERS {: before:n :}
   SAFET-MAP:LIVE {: maps:n :}
   UPLOAD-RESET
   SESSION-OPEN
   STREAM-TRACK
   [: TRACK-HTOD ;] MKD:HTOD!
   0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF
         {: code:n :}
         MKD:USE-REAL
         SESSION-CLOSE
         code throw
      ENDOF
      ok OF
         UPLOAD-N @ 160 T=
         UPLOAD-BAD @ 0 T=
         total TEST-UPLOAD-TOTAL
         TOKEN-OWNER
         {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
         TOKEN-LIVE-CELLS T-CELLS T=
         toklen TOK-LEN-OBS !
         [: TOK-LEN-OBS @ ;] [: TOKEN-ALLOC-LEN ;] SNAP=
         MODEL-CLOSE
         STREAM-CREATE-N @ 0 T=
         STREAM-DESTROY-N @ 0 T=
         MKD:USE-REAL
         SESSION-CLOSE
         tokstate TOKEN-UNMAPPED
      ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS before T=
   SAFET-MAP:LIVE maps T= ;

: RUN ( -- )
   T-RESET
   TEST-SPAN
   TEST-PATH-LIMIT
   TEST-OPAQUE
   TEST-DEVICE
   T-REPORT ;

RUN

;package
