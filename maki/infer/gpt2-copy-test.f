\ gpt2-copy-test.f - GPT2LOAD checkpoint-load acceptance for copied storage.
\ CHECK-COPY, LOAD-COPIED, the copy-buffer layout, cleanup after each
\ allocation failure, and the presence-gated real-checkpoint test.
\
\ The PREPARE phase it consumes is proved in maki/infer/gpt2-prepare-test.f, and the
\ mapped-storage path in maki/infer/gpt2-mapped-test.f. All three run on
\ maki/infer/gpt2-checkpoint-fixture.f, which owns the synthetic-checkpoint emitter, the
\ leak counters and the shared assertion helpers; that file's header explains how the
\ fixtures are generated and why these suites reopen package GPT2LOAD.
\
\ Copy correctness is checked against the copy buffer the load actually filled, not
\ against a re-derivation: RAW-COPY-BUFFER is where LOAD-COPIED put the weights and
\ COPY-OFFSET is where it decided each slot goes, so reading those bytes back and
\ comparing them with what the parsed tensor index itself copies out is a comparison between the
\ load's output and the file. It has to be done this way today because there is no
\ way to read a slot back through a loaded model - that needs model-owned scoped
\ access that returns the embedded store on every exit.

require maki/infer/gpt2-checkpoint-fixture.f

package GPT2LOAD
using SAFET

\ ---- the copied-storage path's gate: compare and retype, nothing moves -----------------
: T-CHECK-COPY ( -- )
   s" CHECK-COPY rejects a prepared-load for a different configuration and moves nothing" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         OTHER-IDENTITY-CONFIG CHECK-COPY
         MATCH GPT2LOAD:copy-check-result
            ready OF
               s" a different configuration must not match" T-LABEL
               DISCARD-COPY
               0 0= 0= TTRUE
            ENDOF
            rejected OF
               E-CONFIG-MISMATCH T=
               s" and the rejected prepared load is whole and still discardable" T-LABEL
               PREPARED-ROW-COUNT TENSOR-COUNT T=
               DISCARD-PREPARED
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" CHECK-COPY test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- the copied load -------------------------------------------------------------
$800 constant COPY-PROBE-CAPACITY
create COPY-FILE-BUFFER COPY-PROBE-CAPACITY allot                  \ what the parsed tensor index says the tensor is
create COPY-BUFFER-PROBE COPY-PROBE-CAPACITY allot                  \ what the load put in the copy buffer

: COPY-BUFFER-BYTES ( n n -- )                     \ copy-buffer offset and length
   {: aoff:n len:n :}
   RAW-COPY-BUFFER @ aoff BYTE+  COPY-BUFFER-PROBE  len BYTE-COPY ;

\ One slot, checked at both ends: the parsed tensor index's own copy of the tensor against the bytes the
\ load placed at that slot's copy-buffer offset. Parameterized by slot so the test can walk
\ tensors of different rank and file position rather than trusting one.
\ Compares a bounded PREFIX of the span, the way the mapped-storage path's real-artifact probe
\ does: a weight can be megabytes, and what is being proved is that the load put THIS
\ tensor's bytes at this copy-buffer offset - a wrong ID, offset, or shifted row all
\ show up in the first bytes. COPY-DATA? truncates to the capacity it is given, so the
\ prefix length is what it returns and what both sides are compared over.
\ Reads the LAST bytes of a tensor out of the parsed tensor index, which COPY-DATA? cannot do - it
\ copies from the start and truncates to the capacity it is given, so a head-only probe
\ passes for a span that was copied short. The tail is where a truncation shows.
variable TAIL-LEN

: COPY-TAIL ( SAFET:census ptr u8 n -- SAFET:census ) {: sa:ptr slen:n :}
   slen COPY-PROBE-CAPACITY > if COPY-PROBE-CAPACITY else slen then {: take:n :}
   sa slen take - BYTE+  COPY-FILE-BUFFER  take BYTE-COPY
   take TAIL-LEN ! ;

\ Head AND tail, because either alone is blind to a real defect: the head misses a copy
\ that was truncated, and a span long enough to exceed the compare buffer is exactly where
\ that happens. Both ends are compared against a fresh parsed tensor index of the same file.
: CHECK-COPIED-SPAN ( SAFET:census n n n -- SAFET:census )
   {: id:n aoff:n len:n :}
   len COPY-PROBE-CAPACITY > if COPY-PROBE-CAPACITY else len then {: take:n :}
   id COPY-FILE-BUFFER take COPY-DATA? OPTION-VALUE take T=
   aoff take COPY-BUFFER-BYTES
   COPY-FILE-BUFFER take COPY-BUFFER-PROBE take ASSERT-BYTES-EQUAL
   id [: COPY-TAIL ;] WITH-TENSOR OPTION-VALUE drop
   TAIL-LEN @ {: tn:n :}
   aoff len + tn -  tn COPY-BUFFER-BYTES
   COPY-FILE-BUFFER tn COPY-BUFFER-PROBE tn ASSERT-BYTES-EQUAL ;

\ ---- forced failure at each allocation step ---------------------------------
\ There is no allocation fault injector, so each test changes one field of a
\ copy-ready block from CHECK-COPY and runs the real LOAD-COPIED. The counters prove
\ ordinary releases return the acquired resources after each forced failure.
\ They do not prove the E-MEM-UNMAP path; habu-make-owned-release-79de2b5c owns
\ that total-release correction.

\ A copy-buffer size no allocator can satisfy: the allocation fails before any
\ copied-storage resource is acquired.
: CORRUPT-TOTAL-BYTES ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   MAX-CELL 16 - blk LOAD-TOTAL-BYTES-CELL cells + !
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

\ A row extent no copy buffer can lay out: the table step meets it while the copy
\ buffer and its owner are already live. It changes a row rather than
\ LOAD-ROW-COUNT-CELL because that count also determines the block's release size;
\ changing it would damage cleanup instead of testing the table failure.
: CORRUPT-ROW-LENGTH ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   MAX-CELL  blk 0 LOAD-ROW-LENGTH PREPARED-ROW-CELL !
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

\ A tensor ID absent from the parsed tensor index makes the copy step reject while
\ the copy buffer, its owner, and the copy-offset table are live.
: CORRUPT-TENSOR-ID ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   $7FFFFFF  blk 0 LOAD-ROW-ID PREPARED-ROW-CELL !
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

\ Each test builds a real load, changes the field that makes its step fail, and
\ runs the real load. Reaching RELEASE-MODEL means LOAD-COPIED did not throw;
\ TTHROWSQ detects that error.
: CHECK-FIXTURE-COPY ( -- GPT2LOAD:copy-check-result )
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF MATCHING-CONFIG CHECK-COPY ENDOF
      rejected OF
         s" forced-failure test could not prepare" T-LABEL
         . cr RELEASE
         E-BAD-FIXTURE throw
      ENDOF
   ;MATCH ;

: REPORT-CHECK-REJECTION ( n -- )
   s" forced-failure test was rejected by CHECK-COPY" T-LABEL
   . cr
   0 0= 0= TTRUE ;

: RUN-CORRUPT-TOTAL-BYTES ( -- )
   CHECK-FIXTURE-COPY
   MATCH GPT2LOAD:copy-check-result
      ready OF CORRUPT-TOTAL-BYTES LOAD-COPIED RELEASE-MODEL RESULT-VALUE drop ENDOF
      rejected OF REPORT-CHECK-REJECTION DISCARD-PREPARED ENDOF
   ;MATCH ;

: RUN-CORRUPT-ROW-LENGTH ( -- )
   CHECK-FIXTURE-COPY
   MATCH GPT2LOAD:copy-check-result
      ready OF CORRUPT-ROW-LENGTH LOAD-COPIED RELEASE-MODEL RESULT-VALUE drop ENDOF
      rejected OF REPORT-CHECK-REJECTION DISCARD-PREPARED ENDOF
   ;MATCH ;

: RUN-CORRUPT-TENSOR-ID ( -- )
   CHECK-FIXTURE-COPY
   MATCH GPT2LOAD:copy-check-result
      ready OF CORRUPT-TENSOR-ID LOAD-COPIED RELEASE-MODEL RESULT-VALUE drop ENDOF
      rejected OF REPORT-CHECK-REJECTION DISCARD-PREPARED ENDOF
   ;MATCH ;

\ The successful path and two span checks prove the copy buffer holds the file's bytes at the
\ offsets the load chose. The probes run against a SECOND parsed tensor index of the same file, so what
\ they compare the copy buffer with is the file rather than anything the load computed.
\ ONE slot pair, used by both halves of the probe. The recording word and the comparing
\ word must name the same slots: recording slot A's copy-buffer location and then comparing
\ slot B's bytes against it passes whenever the data is uniform and means nothing when it
\ is not, which is exactly the shape this pair replaced.
2 constant VECTOR-SLOT                         \ ln_f.weight: a rank-1 vector
variable REAL-VECTOR-OFF   variable REAL-VECTOR-LEN   variable REAL-VECTOR-ID
variable REAL-ATTENTION-WEIGHT-OFFSET   variable REAL-ATTENTION-WEIGHT-LENGTH   variable REAL-ATTENTION-WEIGHT-ID

variable COPY-VECTOR-OFFSET                          \ the vector slot's copy-buffer offset and extent
variable COPY-VECTOR-LENGTH
variable COPY-ATTENTION-WEIGHT-OFFSET                         \ and the Conv1D slot's
variable COPY-ATTENTION-WEIGHT-LENGTH

\ Resolve a slot's tensor name to its tensor ID in the parsed tensor index.
: ID-FOR-SLOT ( SAFET:census n -- SAFET:census n ) {: slot:n :}
   MATCHING-CONFIG slot ROLE-NAME-LENGTH {: name-len:n :}
   drop                                         \ the configuration copy
   ROLE-NAME-BUFFER name-len FIND OPTION-VALUE ;

: CHECK-COPY-SPANS ( -- )
   FIXTURE-PATH LOAD
   VECTOR-SLOT ID-FOR-SLOT {: vid:n :}
   ATTENTION-WEIGHT-SLOT ID-FOR-SLOT {: cid:n :}
   s" the rank-1 vector span is the file's own bytes at its copy-buffer offset" T-LABEL
   vid  COPY-VECTOR-OFFSET @  COPY-VECTOR-LENGTH @  CHECK-COPIED-SPAN
   s" and so is the Conv1D matrix the forward pass reads" T-LABEL
   cid  COPY-ATTENTION-WEIGHT-OFFSET @  COPY-ATTENTION-WEIGHT-LENGTH @  CHECK-COPIED-SPAN
   RELEASE ;

\ Find which slot carries a tensor ID. The real checkpoint test names a tensor,
\ resolves the name to its tensor ID, and finds
\ the slot from the block's own carried rows - the same rows the load copied by.
: SLOT-FOR-ID ( ptr n n -- n ) {: blk:ptr id:n :}
   -1
   blk LOAD-ROW-COUNT-CELL cells + @ 0 ?do
      blk i LOAD-ROW-ID PREPARED-ROW-CELL @ id = if drop i then
   loop ;

: SAVE-REAL-SPAN ( ptr n n ptr n ptr n -- ) {: blk:ptr id:n ovar:ptr lvar:ptr :}
   blk id SLOT-FOR-ID {: slot:n :}
   slot 0 < if E-BAD-FIXTURE throw then
   blk slot COPY-OFFSET ovar !
   blk slot LOAD-ROW-LENGTH PREPARED-ROW-CELL @ lvar ! ;

\ ---- the packed layout, asserted as a recurrence -------------------------------------
\ The span probes check two slots. This checks the whole copy-buffer layout, which is
\ what a two-slot check cannot: the copy buffer starts at zero, every row begins exactly where
\ the previous one ended - so there is no gap and no overlap anywhere - and the last row
\ ends exactly at the copy-buffer size. A layout walk that drifts by one
\ row, or a length that disagrees with the sum, shows up here rather than in whichever
\ two slots the probes happened to pick.
variable LAYOUT-ERROR

: CHECK-COPY-LAYOUT ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   blk LOAD-ROW-COUNT-CELL cells + @ {: count:n :}
   0 LAYOUT-ERROR !
   s" the copy buffer starts at zero" T-LABEL
   blk 0 COPY-OFFSET 0 T=
   count 0 ?do
      blk i COPY-OFFSET  blk i LOAD-ROW-LENGTH PREPARED-ROW-CELL @ +  {: end:n :}
      i 1 + count < if
         blk i 1 + COPY-OFFSET end <> if 1 LAYOUT-ERROR ! then
      else
         end blk LOAD-TOTAL-BYTES-CELL cells + @ <> if 1 LAYOUT-ERROR ! then
      then
   loop
   s" every row begins where the previous ended, and the last ends at the copy-buffer size" T-LABEL
   LAYOUT-ERROR @ 0 T=
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

: SAVE-COPY-SPANS ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   blk VECTOR-SLOT COPY-OFFSET COPY-VECTOR-OFFSET !
   blk VECTOR-SLOT LOAD-ROW-LENGTH PREPARED-ROW-CELL @ COPY-VECTOR-LENGTH !
   blk ATTENTION-WEIGHT-SLOT COPY-OFFSET COPY-ATTENTION-WEIGHT-OFFSET !
   blk ATTENTION-WEIGHT-SLOT LOAD-ROW-LENGTH PREPARED-ROW-CELL @ COPY-ATTENTION-WEIGHT-LENGTH !
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

\ The real checkpoint loaded to a copied model: 548 MB copied span by span into
\ one copy buffer. Both checks compare that buffer against a second parsed tensor index of the same
\ file, so what they agree with is the file on disk and not any number the load derived.
: SAVE-REAL-SPANS ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready )
   TAKE-COPY-BLOCK {: blk:ptr :}
   REAL-CHECKPOINT-PATH LOAD
   s" h.0.ln_1.weight" FIND OPTION-VALUE {: vid:n :}
   s" h.0.attn.c_attn.weight" FIND OPTION-VALUE {: cid:n :}
   RELEASE
   vid REAL-VECTOR-ID !  cid REAL-ATTENTION-WEIGHT-ID !
   blk vid REAL-VECTOR-OFF REAL-VECTOR-LEN SAVE-REAL-SPAN
   blk cid REAL-ATTENTION-WEIGHT-OFFSET REAL-ATTENTION-WEIGHT-LENGTH SAVE-REAL-SPAN
   blk BLOCK>BYTE-POINTER BLOCK>COPY-READY ;

: CHECK-REAL-SPANS ( -- )
   REAL-CHECKPOINT-PATH LOAD
   s" the real rank-1 vector is the file's own bytes at its copy-buffer offset" T-LABEL
   REAL-VECTOR-ID @ REAL-VECTOR-OFF @ REAL-VECTOR-LEN @ CHECK-COPIED-SPAN
   s" and so is the real Conv1D matrix the forward pass reads" T-LABEL
   REAL-ATTENTION-WEIGHT-ID @ REAL-ATTENTION-WEIGHT-OFFSET @ REAL-ATTENTION-WEIGHT-LENGTH @ CHECK-COPIED-SPAN
   RELEASE ;

: T-REAL-COPIED ( -- )
   REAL-CHECKPOINT-PATH PRESENT? 0= if
      s" gpt2-copy: real-artifact copied test SKIPPED" type cr exit
   then
   s" the real checkpoint loads to a copied model that owns its copy buffer" T-LABEL
   REAL-MODEL-CONFIG SAVE-CONFIG-KEY
   REAL-CHECKPOINT-PATH LOAD REAL-MODEL-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         PREPARED-TOTAL-BYTES {: want:n :}
         REAL-MODEL-CONFIG CHECK-COPY
         MATCH GPT2LOAD:copy-check-result
            ready OF
               CHECK-COPY-LAYOUT
               SAVE-REAL-SPANS
               LOAD-COPIED
               MODEL-LAYER-COUNT 12 T=
               SAVE-MODEL-CONFIG-KEY
               KEY-MATCHES-CONFIG
               CHECK-REAL-SPANS
               s" and its ordinary release returns the copy-buffer size" T-LABEL
               RELEASE-MODEL RESULT-VALUE want T=
               EXPECT-NO-LEAK
            ENDOF
            rejected OF REPORT-CHECK-REJECTION DISCARD-PREPARED ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" real copied-model test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-LOAD-COPIED ( -- )
   s" a copy-ready load becomes a copied model that owns its copy buffer" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   MATCHING-CONFIG SAVE-CONFIG-KEY
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         PREPARED-TOTAL-BYTES {: want:n :}
         MATCHING-CONFIG CHECK-COPY
         MATCH GPT2LOAD:copy-check-result
            ready OF
               CHECK-COPY-LAYOUT
               SAVE-COPY-SPANS
               LOAD-COPIED
               s" it reports the layer count PREPARE validated" T-LABEL
               MODEL-LAYER-COUNT LAYER-COUNT T=
               s" and the configuration's identity, cell for cell" T-LABEL
               SAVE-MODEL-CONFIG-KEY
               KEY-MATCHES-CONFIG
               CHECK-COPY-SPANS
               s" and its ordinary release returns the copy-buffer size" T-LABEL
               RELEASE-MODEL RESULT-VALUE want T=
               EXPECT-NO-LEAK
            ENDOF
            rejected OF REPORT-CHECK-REJECTION DISCARD-PREPARED ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" copied-load test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

;using

: T-COPY-LOAD-FAILURES ( -- )
   s" an impossible copy-buffer size releases ordinary resources after allocation fails" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   [: RUN-CORRUPT-TOTAL-BYTES ;] E-MEM-MAP TTHROWSQ
   EXPECT-NO-LEAK
   s" an invalid row extent releases the live copy buffer after table creation fails" T-LABEL
   [: RUN-CORRUPT-ROW-LENGTH ;] WSTORE:E-EXTENT TTHROWSQ
   EXPECT-NO-LEAK
   s" an absent tensor ID releases the copy buffer and table after copying fails" T-LABEL
   [: RUN-CORRUPT-TENSOR-ID ;] E-SHORT-COPY TTHROWSQ
   EXPECT-NO-LEAK ;

\ ---- checker rules for copied-storage ownership ---------------------------------------
\ The PREPARE phase's static rules live with gpt2-prepare-test.f and the mapped-storage path's with
\ gpt2-mapped-test.f; these are the rules added by copy-ready. They are split into
\ four short sections rather than one long word because each candidate is compiled and
\ checked in turn, and a single block of this many rows is more than one definition
\ should carry.
: T-CHECKER-COPY-READY ( -- )
   s" copy-ready is linear and unforgeable" T-LABEL
   s" LOAD-BAD-COPY-READY-DUP ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready GPT2LOAD:copy-ready ) dup" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-DROP ( GPT2LOAD:copy-ready -- ) drop" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-STORE ( GPT2LOAD:copy-ready ptr n -- ) !" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-RAW-CELL ( n -- GPT2LOAD:copy-ready ) " EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-RAW-POINTER ( ptr u8 -- GPT2LOAD:copy-ready ) " EXPECT-CHECKER-REJECTION
   \ The same shape the mapped-storage path relies on: the copied load's argument type is
   \ one only CHECK-COPY produces, so "the identity was compared" is a static
   \ precondition and no edit to a body can make an unchecked prepared load loadable.
   s" the copied load is unreachable without CHECK-COPY" T-LABEL
   s" LOAD-BAD-LOAD-COPIED-PREPARED ( GPT2LOAD:prepared-load -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-COPIED-MAPPED-READY ( GPT2LOAD:mapped-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-COPIED-TENSOR-INDEX ( SAFET:census -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-COPIED-WITHOUT-INPUT ( -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-REJECTION ;

\ mapped-ready and copy-ready share one runtime representation, so their types
\ prevent either state from using the other's operation. Each crossing is
\ rejected in both directions, and neither value can be consumed twice.
: T-CHECKER-EXITS ( -- )
   s" mapped-ready and copy-ready do not substitute at any operation" T-LABEL
   s" LOAD-BAD-COPY-READY-DISCARD-MAPPED ( GPT2LOAD:copy-ready -- ) GPT2LOAD:DISCARD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MAPPED-READY-DISCARD-COPY ( GPT2LOAD:mapped-ready -- ) GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-PREPARED-DISCARD-COPY ( GPT2LOAD:prepared-load -- ) GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-LOAD-MAPPED ( GPT2LOAD:copy-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-DISCARD-PREPARED ( GPT2LOAD:copy-ready -- ) GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   s" a ready state cannot be consumed twice or consumed and kept" T-LABEL
   s" LOAD-BAD-DISCARD-COPY-TWICE ( GPT2LOAD:copy-ready -- ) GPT2LOAD:DISCARD-COPY GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-COPIED-THEN-DISCARD-COPY ( GPT2LOAD:copy-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DISCARD-COPY-RETURNS-CONSUMED-VALUE ( GPT2LOAD:copy-ready -- GPT2LOAD:copy-ready ) GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-COPIED-DROPPED ( GPT2LOAD:copy-ready -- ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-COPY-RESULT-DROPPED ( GPT2LOAD:prepared-load MDLCFG:mcfg -- ) GPT2LOAD:CHECK-COPY" EXPECT-CHECKER-REJECTION ;

\ The gate consumes the prepared-load it is given, so it cannot run twice on one prepared-load and cannot
\ run after that prepared load has been discarded; and it answers about the prepared load it was handed,
\ never about ambient state.
: T-CHECK-COPY-TYPES ( -- )
   s" the gate cannot run twice, nor after the prepared-load is gone" T-LABEL
   s" LOAD-BAD-CHECK-COPY-TWICE ( GPT2LOAD:prepared-load MDLCFG:mcfg MDLCFG:mcfg -- GPT2LOAD:copy-check-result ) GPT2LOAD:CHECK-COPY GPT2LOAD:CHECK-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-COPY-AFTER-DISCARD-PREPARED ( GPT2LOAD:prepared-load MDLCFG:mcfg -- GPT2LOAD:copy-check-result ) >r GPT2LOAD:DISCARD-PREPARED r> GPT2LOAD:CHECK-COPY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-COPY-WITHOUT-INPUT ( MDLCFG:mcfg -- GPT2LOAD:copy-check-result ) GPT2LOAD:CHECK-COPY" EXPECT-CHECKER-REJECTION
   s" copy-check-result payloads cannot cross roles" T-LABEL
   s" LOAD-BAD-CHECK-COPY-READY-PREPARED ( GPT2LOAD:prepared-load -- GPT2LOAD:copy-check-result ) GPT2LOAD-COPY--CHECK--RESULT:READY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-COPY-READY-MAPPED-READY ( GPT2LOAD:mapped-ready -- GPT2LOAD:copy-check-result ) GPT2LOAD-COPY--CHECK--RESULT:READY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-COPY-REJECTED-COPY-READY ( GPT2LOAD:copy-ready n -- GPT2LOAD:copy-check-result ) GPT2LOAD-COPY--CHECK--RESULT:REJECTED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-COPY-READY-CROSS-RESULT ( GPT2LOAD:copy-ready -- GPT2LOAD:mapped-check-result ) GPT2LOAD-MAPPED--CHECK--RESULT:READY" EXPECT-CHECKER-REJECTION
   \ The controls: every rejection above has to be the type rejecting the candidate,
   \ rather than an unrelated compile failure, so the same surface is accepted here
   \ with its arguments in the right roles.
   s" the copied-storage path's public surface resolves (controls)" T-LABEL
   s" LOAD-OK-CHECK-COPY ( GPT2LOAD:prepared-load MDLCFG:mcfg -- GPT2LOAD:copy-check-result ) GPT2LOAD:CHECK-COPY" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-DISCARD-COPY ( GPT2LOAD:copy-ready -- ) GPT2LOAD:DISCARD-COPY" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-LOAD-COPIED ( GPT2LOAD:copy-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-COPIED" EXPECT-CHECKER-ACCEPTANCE ;

\ Verdict 1 is "the dictionary cannot resolve this token at all", which is how a
\ package-private word looks from outside; it is kept apart from verdict 0, a real type
\ error, so "private" is proved by non-resolution rather than by any accident. These are
\ the copied-storage path's own owner-to-cell conversions and row readers.
: T-CHECKER-PRIVATE ( -- )
   s" the copied-storage path's audited owner-to-cell conversions stay package-private" T-LABEL
   s" LOAD-BAD-BLOCK-TO-COPY-READY ( ptr u8 -- GPT2LOAD:copy-ready ) GPT2LOAD:BLOCK>COPY-READY" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-TAKE-COPY-BLOCK ( GPT2LOAD:copy-ready -- ptr n ) GPT2LOAD:TAKE-COPY-BLOCK" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-BUFFER-TO-CELL ( WSTORE:buffer -- n ) GPT2LOAD:BUFFER>CELL" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-CELL-TO-BUFFER ( n -- WSTORE:buffer ) GPT2LOAD:CELL>BUFFER" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-COPY-OFFSET ( ptr n n -- n ) GPT2LOAD:COPY-OFFSET" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-PREPARED-ROW ( GPT2LOAD:prepared-load n -- GPT2LOAD:prepared-load n n n ) GPT2LOAD:PREPARED-ROW" EXPECT-PRIVATE-NAME ;

: RUN-COPIED ( -- )
   T-RESET
   SAVE-BASELINE
   T-CHECKER-COPY-READY
   T-CHECKER-EXITS
   T-CHECK-COPY-TYPES
   T-CHECKER-PRIVATE
   T-CHECK-COPY        EXPECT-NO-LEAK
   T-LOAD-COPIED       EXPECT-NO-LEAK
   T-COPY-LOAD-FAILURES EXPECT-NO-LEAK
   T-REAL-COPIED         EXPECT-NO-LEAK
   DELETE-FIXTURES ;

RUN-COPIED
T-REPORT

;package
