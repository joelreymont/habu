\ gpt2-mapped-test.f - GPT2LOAD checkpoint-load acceptance for mapped storage:
\ CHECK-MAPPED, LOAD-MAPPED, the loaded model, and the exits for all three of
\ the load's owners, plus the presence-gated real-checkpoint test.
\
\ The PREPARE phase it consumes is proved in maki/infer/gpt2-prepare-test.f, and the
\ copied-storage path in maki/infer/gpt2-copy-test.f. All three run on
\ maki/infer/gpt2-checkpoint-fixture.f, which owns the synthetic-checkpoint emitter, the
\ leak counters and the shared assertion helpers; that file's header explains how
\ the fixtures are generated and why these suites reopen package GPT2LOAD.
\
\ These tests walk the load one state at a time. SAFET reserves the mapping
\ record before publishing the parsed tensor index, so CHECK-MAPPED does not
\ allocate. A prepared-load may be discarded, returned as a parsed tensor index,
\ or checked; mapped-ready may be discarded or loaded; a model may be released.
\ The tests cover ordinary successful release; habu-make-owned-release-79de2b5c
\ owns the E-MEM-UNMAP total-release gap.

require maki/infer/gpt2-checkpoint-fixture.f

package GPT2LOAD

using SAFET

: T-CHECK-DIFFERENT-CONFIG ( -- )
   s" a different configuration is rejected, and nothing has moved" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         OTHER-IDENTITY-CONFIG CHECK-MAPPED                          \ same parsed tensor index, other identity
         MATCH GPT2LOAD:mapped-check-result
            ready OF
               s" the alternate configuration was accepted as this model" T-LABEL
               0 0= 0= TTRUE
               DISCARD-MAPPED
            ENDOF
            rejected OF
               {: code:n :}
               code E-CONFIG-MISMATCH T=
               s" and the rejected prepared load is whole and still discardable" T-LABEL
               EXPECT-PREPARED-RESOURCES
               DISCARD-PREPARED
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" different-configuration test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

: T-CHECK-READY ( -- )
   s" the configuration that built the prepared-load matches it" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         MATCHING-CONFIG CHECK-MAPPED
         MATCH GPT2LOAD:mapped-check-result
            ready OF
               \ The parsed tensor index gave up its image and was released; the mapping it gave up
               \ is now the mapped-ready value's. One owner out, one owner in, so the totals
               \ are exactly where PREPARE left them - and no kernel mapping moved.
               s" the mapping moved out of the parsed tensor index without changing what is owned" T-LABEL
               EXPECT-PREPARED-RESOURCES
               s" and mapped-ready can be discarded" T-LABEL
               DISCARD-MAPPED
            ENDOF
            rejected OF
               {: code:n :}
               s" a prepared-load was rejected by the configuration it was prepared for, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               DISCARD-PREPARED
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" CHECK-MAPPED test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

: T-LOAD-MAPPED ( -- )
   s" mapped-ready loads to a mapped model" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   MATCHING-CONFIG SAVE-CONFIG-KEY                          \ the configuration's own key, to compare
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         MATCHING-CONFIG CHECK-MAPPED
         MATCH GPT2LOAD:mapped-check-result
            ready OF
               LOAD-MAPPED
               s" the prepared-load block is gone and the model owns the store" T-LABEL
               EXPECT-MAPPED-MODEL-RESOURCES
               s" the model carries the layer count the prepared load validated" T-LABEL
               MODEL-LAYER-COUNT LAYER-COUNT T=
               s" and the identity the prepared-load captured, cell for cell" T-LABEL
               SAVE-MODEL-CONFIG-KEY
               KEY-MATCHES-CONFIG
               s" and the model's exit gives back the whole mapped image" T-LABEL
               RELEASE-MODEL RESULT-VALUE IMAGE-LENGTH @ T=
            ENDOF
            rejected OF
               {: code:n :}
               s" mapped load was rejected, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               DISCARD-PREPARED
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" mapped-load test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- a rejection leaves the prepared-load genuinely usable, not merely alive ---------------
\ The counters in T-CHECK-DIFFERENT-CONFIG show a rejected prepared load still
\ belongs to the caller; they cannot show
\ it is still usable. This test passes the same rejected prepared load to
\ CHECK-MAPPED again under the configuration that built it, and it loads all the
\ way to a model. That is the property a loader actually relies on when it tries a
\ second configuration on a prepared-load, and nothing weaker demonstrates it - a prepared-load whose
\ block had been partly consumed on the rejection path would pass every counter assertion
\ and fail right here.
: T-REJECT-THEN-LOAD ( -- )
   s" a rejected prepared-load still loads under its own configuration" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   MATCHING-CONFIG SAVE-CONFIG-KEY
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         OTHER-IDENTITY-CONFIG CHECK-MAPPED                          \ rejected: alternate identity
         MATCH GPT2LOAD:mapped-check-result
            ready OF
               s" the alternate configuration was accepted, so the rejection never happened" T-LABEL
               0 0= 0= TTRUE
               DISCARD-MAPPED
            ENDOF
            rejected OF
               {: code:n :}
               code E-CONFIG-MISMATCH T=
               MATCHING-CONFIG CHECK-MAPPED                    \ the very same prepared-load, its own identity
               MATCH GPT2LOAD:mapped-check-result
                  ready OF
                     LOAD-MAPPED
                     s" and the model it yields is complete" T-LABEL
                     MODEL-LAYER-COUNT LAYER-COUNT T=
                     SAVE-MODEL-CONFIG-KEY
                     KEY-MATCHES-CONFIG
                     EXPECT-MAPPED-MODEL-RESOURCES
                     RELEASE-MODEL RESULT-VALUE IMAGE-LENGTH @ T=
                  ENDOF
                  rejected OF
                     {: code:n :}
                     s" the rejection damaged the prepared load: its own configuration was rejected, code" T-LABEL
                     code . cr
                     0 0= 0= TTRUE
                     DISCARD-PREPARED
                  ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" reject-then-load test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- presence-gated real artifact ----------------------------------------------
768 4 * constant MAPPED-PROBE-LENGTH                      \ h.0.ln_1.weight: nembd F32 values
create FILE-PROBE-BUFFER MAPPED-PROBE-LENGTH allot                   \ the bytes the parsed tensor index copied out
create MAPPED-PROBE-BUFFER MAPPED-PROBE-LENGTH allot                   \ the bytes found at the computed offset
variable MAPPED-OFFSET

: READ-MAPPED-BYTES ( SAFET:mapping ptr u8 n -- SAFET:mapping ) {: ba:ptr blen:n :}
   ba MAPPED-OFFSET @ BYTE+  MAPPED-PROBE-BUFFER  MAPPED-PROBE-LENGTH  BYTE-COPY ;

\ WHAT THIS PROVES, AND WHAT IT DOES NOT. The mapped store serves a slot as
\ mapping-base + the row's offset, and the row's offset is the parsed tensor index's MAP-OFFSET?.
\ This test checks that arithmetic against the real checkpoint from both ends: the bytes
\ the parsed tensor index copies out of one real tensor, and the bytes sitting at that tensor's
\ computed offset inside the detached mapping, are the same bytes. Since the mapping IS
\ the file mapped read-only, those are the file's bytes at that offset.
\
\ It stops one link short of reading through the loaded model. The remaining
\ WSTORE link is value-only and refusal-total: U32-LE@? returns the unchanged
\ store with option<n>. Its mapped base+row+relative-offset arithmetic is pinned
\ in weight-store-test.f against the allocated arm and fixed expected values.
\ One tensor, named by its real checkpoint tensor name: copy its first bytes out of the parsed tensor index, note
\ the mapping-offset base offset the table will carry, then read the mapping at exactly that
\ offset and require the same bytes. Parameterized by key so the test can walk several
\ tensors of different shapes and file positions rather than trusting one.
: CHECK-MAPPED-TENSOR ( ptr u8 n -- ) {: ka:ptr ku:n :}
   REAL-CHECKPOINT-PATH LOAD
   ka ku FIND OPTION-VALUE {: id:n :}
   id FILE-PROBE-BUFFER MAPPED-PROBE-LENGTH COPY-DATA? OPTION-VALUE MAPPED-PROBE-LENGTH T=
   id MAP-OFFSET? OPTION-VALUE MAPPED-OFFSET !
   DETACH-MAPPING EXPECT-DETACHED-MAPPING        \ ( parsed tensor index mapping )
   swap RELEASE                           \ ( mapping )
   [: READ-MAPPED-BYTES ;] WITH-MAPPING drop
   UNMAP-MAPPING RESULT-CODE 0 T=
   FILE-PROBE-BUFFER MAPPED-PROBE-LENGTH MAPPED-PROBE-BUFFER MAPPED-PROBE-LENGTH ASSERT-BYTES-EQUAL ;

\ Three tensors, chosen so a single lucky offset cannot pass the test: a rank-1 vector
\ near the start of the data section, the rank-2 Conv1D matrix whose [in,out]
\ orientation the forward pass depends on, and a tensor in the LAST block, whose data
\ sits hundreds of megabytes into the file where a 32-bit offset truncation or a
\ header-length slip would show up and a small-offset probe would not.
: CHECK-REAL-BYTES ( -- )
   s" a rank-1 vector is byte-identical at its computed mapping offset" T-LABEL
   s" h.0.ln_1.weight" CHECK-MAPPED-TENSOR
   s" so is the Conv1D matrix the forward pass reads untransposed" T-LABEL
   s" h.0.attn.c_attn.weight" CHECK-MAPPED-TENSOR
   s" and so is a tensor far into the file, where a truncated offset would show" T-LABEL
   s" h.11.mlp.c_proj.weight" CHECK-MAPPED-TENSOR ;

548105171 constant REAL-FILE-BYTES               \ the pinned checkpoint's exact file size

: LOAD-REAL-MAPPED ( -- )
   REAL-MODEL-CONFIG SAVE-CONFIG-KEY                      \ the real configuration's own identity
   REAL-CHECKPOINT-PATH LOAD REAL-MODEL-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         PREPARED-ROW-COUNT 160 T=
         REAL-MODEL-CONFIG CHECK-MAPPED
         MATCH GPT2LOAD:mapped-check-result
            ready OF
               LOAD-MAPPED
               s" the real checkpoint loads to a mapped model of 12 layers" T-LABEL
               MODEL-LAYER-COUNT 12 T=
               EXPECT-MAPPED-MODEL-RESOURCES
               s" loaded with the real configuration's identity, cell for cell" T-LABEL
               SAVE-MODEL-CONFIG-KEY
               KEY-MATCHES-CONFIG
               \ ok is not a token success flag: it carries the byte count WSTORE gave
               \ back, which for a mapped model is the whole checkpoint mapping. Pinning
               \ the exact file size is what proves the model was serving the entire
               \ file rather than some truncated span of it.
               s" and its exit gives back the whole checkpoint mapping, to the byte" T-LABEL
               RELEASE-MODEL RESULT-VALUE REAL-FILE-BYTES T=
            ENDOF
            rejected OF
               {: code:n :}
               s" the real checkpoint was rejected by its matching configuration, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               DISCARD-PREPARED
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" the real checkpoint did not prepare, code" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-REAL-MAPPED ( -- )
   REAL-CHECKPOINT-PATH PRESENT? 0= if
      s" gpt2-mapped: gpt2-model/model.safetensors absent -> real-artifact test SKIPPED" type cr
      0 0= TTRUE exit
   then
   s" the real GPT-2 checkpoint loads through mapped storage" T-LABEL
   CHECK-REAL-BYTES
   EXPECT-NO-LEAK
   LOAD-REAL-MAPPED
   EXPECT-NO-LEAK ;

;using

\ ---- checker rules for mapped-storage ownership ----------------------------------------
\ The PREPARE phase's own static rules live with gpt2-prepare-test.f; these are the ones
\ the mapped-storage path adds.
: T-CHECKER-MAPPED ( -- )
   s" the mapped-storage public surface resolves" T-LABEL
   s" LOAD-OK-CHECK-MAPPED ( GPT2LOAD:prepared-load MDLCFG:mcfg -- GPT2LOAD:mapped-check-result ) GPT2LOAD:CHECK-MAPPED" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-DISCARD-MAPPED ( GPT2LOAD:mapped-ready -- ) GPT2LOAD:DISCARD-MAPPED" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-LOAD-MAPPED ( GPT2LOAD:mapped-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-MAPPED" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-RELEASE-MODEL ( GPT2LOAD:gpt2-model -- result<n,n> ) GPT2LOAD:RELEASE-MODEL" EXPECT-CHECKER-ACCEPTANCE
   \ LOAD-MAPPED requires mapped-ready, which only CHECK-MAPPED produces. The
   \ configuration-identity comparison is therefore a static precondition rather
   \ than a rule LOAD-MAPPED must remember. A prepared-load cannot substitute.
   s" the load is unreachable without the compare" T-LABEL
   s" LOAD-BAD-LOAD-MAPPED-PREPARED ( GPT2LOAD:prepared-load -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-MAPPED-TENSOR-INDEX ( SAFET:census -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-MAPPED-ON-MAPPED-READY ( GPT2LOAD:mapped-ready MDLCFG:mcfg -- GPT2LOAD:mapped-check-result ) GPT2LOAD:CHECK-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-MAPPED-WITHOUT-INPUT ( MDLCFG:mcfg -- GPT2LOAD:mapped-check-result ) GPT2LOAD:CHECK-MAPPED" EXPECT-CHECKER-REJECTION
   s" mapped-ready is linear: no copy, discard, raw store, or raw construction" T-LABEL
   s" LOAD-BAD-MAPPED-READY-DUP ( GPT2LOAD:mapped-ready -- GPT2LOAD:mapped-ready GPT2LOAD:mapped-ready ) dup" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MAPPED-READY-DROP ( GPT2LOAD:mapped-ready -- ) drop" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MAPPED-READY-STORE ( GPT2LOAD:mapped-ready ptr n -- ) !" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MAPPED-READY-RAW-CELL ( n -- GPT2LOAD:mapped-ready ) " EXPECT-CHECKER-REJECTION
   s" prepared-load and mapped-ready do not substitute for each other at any exit" T-LABEL
   s" LOAD-BAD-PREPARED-DISCARD-MAPPED ( GPT2LOAD:prepared-load -- ) GPT2LOAD:DISCARD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MAPPED-READY-DISCARD-PREPARED ( GPT2LOAD:mapped-ready -- ) GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DISCARD-MAPPED-TWICE ( GPT2LOAD:mapped-ready -- ) GPT2LOAD:DISCARD-MAPPED GPT2LOAD:DISCARD-MAPPED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-LOAD-MAPPED-THEN-DISCARD-MAPPED ( GPT2LOAD:mapped-ready -- GPT2LOAD:gpt2-model ) GPT2LOAD:LOAD-MAPPED GPT2LOAD:DISCARD-MAPPED" EXPECT-CHECKER-REJECTION
   s" mapped-check-result payloads cannot cross roles" T-LABEL
   s" LOAD-BAD-READY-PREPARED ( GPT2LOAD:prepared-load -- GPT2LOAD:mapped-check-result ) GPT2LOAD-MAPPED--CHECK--RESULT:READY" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-REJECTED-MAPPED-READY ( GPT2LOAD:mapped-ready n -- GPT2LOAD:mapped-check-result ) GPT2LOAD-MAPPED--CHECK--RESULT:REJECTED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-CHECK-MAPPED-RESULT-DROPPED ( GPT2LOAD:prepared-load MDLCFG:mcfg -- ) GPT2LOAD:CHECK-MAPPED" EXPECT-CHECKER-REJECTION
   \ A model owns a linear store, so the RECORD is linear by containment: the
   \ checker rejects copying or discarding it, which makes the checkpoint mapping
   \ impossible to leak or to free twice.
   s" a loaded model is linear by containment and cannot be built from a raw cell" T-LABEL
   s" LOAD-BAD-MODEL-DUP ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model GPT2LOAD:gpt2-model ) dup" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MODEL-DROP ( GPT2LOAD:gpt2-model -- ) drop" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MODEL-STORE ( GPT2LOAD:gpt2-model ptr n -- ) !" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MODEL-RAW-CELL ( n -- GPT2LOAD:gpt2-model ) " EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-MODEL-PROOF-RAW ( WSTORE:store n MDLCFG:cfgkey n -- GPT2LOAD:gpt2-model ) GPT2LOAD-GPT2--MODEL:MAKE" EXPECT-CHECKER-REJECTION
   s" the model's exit consumes it exactly once" T-LABEL
   s" LOAD-BAD-RELEASE-MODEL-TWICE ( GPT2LOAD:gpt2-model -- result<n,n> result<n,n> ) GPT2LOAD:RELEASE-MODEL GPT2LOAD:RELEASE-MODEL" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RELEASE-MODEL-RETURNS-CONSUMED-VALUE ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model result<n,n> ) GPT2LOAD:RELEASE-MODEL" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RELEASE-MODEL-DROPPED ( GPT2LOAD:gpt2-model -- ) GPT2LOAD:RELEASE-MODEL" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RELEASE-MODEL-STORE ( WSTORE:store -- result<n,n> ) GPT2LOAD:RELEASE-MODEL" EXPECT-CHECKER-REJECTION
   s" and a model cannot substitute for the store its generated UNMAKE returns" T-LABEL
   s" LOAD-BAD-MODEL-DISPOSE ( GPT2LOAD:gpt2-model -- result<n,n> ) WSTORE:DISPOSE" EXPECT-CHECKER-REJECTION
   \ The owner-to-cell conversions are the whole soundness cost of this module, so their confinement
   \ is asserted, not assumed. A candidate names them qualified, the way a different configuration
   \ file would have to; each is unresolvable, which is what package-private means
   \ here. This tests the private representation claim in gpt2-load.f's header;
   \ refine-lint separately confines the inverse conversions to that file, because
   \ these probes cannot speak for a file that REOPENS the package.
   s" the mapped-storage path's audited owner-to-cell conversions stay package-private" T-LABEL
   s" LOAD-BAD-BLOCK-TO-MAPPED-READY ( ptr u8 -- GPT2LOAD:mapped-ready ) GPT2LOAD:BLOCK>MAPPED-READY" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-TAKE-MAPPED-BLOCK ( GPT2LOAD:mapped-ready -- ptr n ) GPT2LOAD:TAKE-MAPPED-BLOCK" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-MAPPING-TO-CELL ( SAFET:mapping -- n ) GPT2LOAD:MAPPING>CELL" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-CELL-TO-MAPPING ( n -- SAFET:mapping ) GPT2LOAD:CELL>MAPPING" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-MAKE-MODEL-PROOF ( -- GPT2LOAD:model-proof ) GPT2LOAD:MAKE-MODEL-PROOF" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-MODEL-LAYER-COUNT ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model n ) GPT2LOAD:MODEL-LAYER-COUNT" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-MODEL-CONFIG-KEY ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model MDLCFG:cfgkey ) GPT2LOAD:MODEL-CONFIG-KEY" EXPECT-PRIVATE-NAME ;

: RUN-MAPPED ( -- )
   T-RESET
   SAVE-BASELINE
   T-CHECKER-MAPPED
   T-CHECK-DIFFERENT-CONFIG    EXPECT-NO-LEAK
   T-CHECK-READY      EXPECT-NO-LEAK
   T-LOAD-MAPPED           EXPECT-NO-LEAK
   T-REJECT-THEN-LOAD EXPECT-NO-LEAK
   T-REAL-MAPPED
   s" the whole suite released every owner it took" T-LABEL
   EXPECT-NO-LEAK
   DELETE-FIXTURES ;

RUN-MAPPED

;package

\ ---------------------------------------------------------------------------------
\ package GPT2LOAD-OUTSIDE-TEST - the destruction-review pins, from OUTSIDE the package.
\
\ WHY THIS SECTION DOES NOT REOPEN GPT2LOAD. Everything above runs inside the package,
\ because the tests above inspect validated rows that no public
\ word exposes. That is also why those tests cannot show what an outside file
\ can do: inside the package the private words resolve, so an "is this reachable?"
\ question asked there tests the wrong boundary. This section uses a package that
\ has never opened GPT2LOAD and pins the
\ gap the model's private construction proof does NOT close, the MODELPROV-TEST T-KNOWN-GAP
\ convention.
\
\ WHAT THE PROOF DOES AND DOES NOT BUY. `model-proof` makes a model unforgeable from
\ nothing: no outside file can conjure the proof, so no outside file can MAKE a model
\ out of thin air. It does not make a REAL model tamper-proof, because the generated
\ UNMAKE is public: a holder of a genuine model can take the store straight out of
\ it, release it behind the model's back, or rebuild the record with an invalid layer count and
\ the original proof. The three pins below are written as ACCEPT deliberately - they
\ record what compiles TODAY. When the sealed-destructure capability
\ (habu-checker-sealed-destructure-d967fc03) lands, these three tests fail, and that
\ failure is the signal to delete them and retire the caveat in gpt2-load.f's header.
\ The fourth pin is the control that keeps the other three honest: the package's own
\ readers stay unreachable from here, so these are gaps in the GENERATED surface, not
\ an accident of package scope.
\ ---------------------------------------------------------------------------------
package GPT2LOAD-OUTSIDE-TEST

: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: EXPECT-PRIVATE-NAME ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: T-KNOWN-GAP ( -- )
   s" KNOWN GAP: the generated UNMAKE extracts a real model's store TODAY" T-LABEL
   s" LOAD-GAP-UNMAKE ( GPT2LOAD:gpt2-model -- WSTORE:store n MDLCFG:cfgkey GPT2LOAD:model-proof ) GPT2LOAD-GPT2--MODEL:UNMAKE"
      ACCEPTED
   s" KNOWN GAP: so the store can be released behind the model's back" T-LABEL
   s" LOAD-GAP-RELEASE-INNER-STORE ( GPT2LOAD:gpt2-model -- result<n,n> ) GPT2LOAD-GPT2--MODEL:UNMAKE drop drop drop WSTORE:DISPOSE"
      ACCEPTED
   s" KNOWN GAP: a real model can be rebuilt with an invalid layer count" T-LABEL
   s" LOAD-GAP-INVALID-LAYER-COUNT ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model ) GPT2LOAD-GPT2--MODEL:UNMAKE >r >r drop 99 r> r> GPT2LOAD-GPT2--MODEL:MAKE"
      ACCEPTED
   s" the proof still rejects a model built from nothing, which is what it is for" T-LABEL
   s" LOAD-GAP-RAW-MODEL ( WSTORE:store n MDLCFG:cfgkey n -- GPT2LOAD:gpt2-model ) GPT2LOAD-GPT2--MODEL:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CONTROL: the package's own readers are unreachable from outside it, so the" T-LABEL
   s" three gaps above are in the generated surface, not package scope" T-LABEL
   s" LOAD-OUTSIDE-MODEL-LAYER-COUNT ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model n ) GPT2LOAD:MODEL-LAYER-COUNT" EXPECT-PRIVATE-NAME
   s" LOAD-OUTSIDE-MAKE-MODEL-PROOF ( -- GPT2LOAD:model-proof ) GPT2LOAD:MAKE-MODEL-PROOF" EXPECT-PRIVATE-NAME ;

T-KNOWN-GAP
T-REPORT

;package
