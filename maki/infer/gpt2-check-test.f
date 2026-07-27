\ gpt2-check-test.f - GPT2TX bind-transaction acceptance: the MAPPED arm (S6b3,
\ redesign 2). CHECK, COMMIT-MAPPED, the bound model, and the exits for all three of
\ the transaction's owners, plus the presence-gated real-checkpoint leg.
\
\ The PREPARE half it consumes is proved in maki/infer/gpt2-bind-test.f, and the
\ allocated arm in maki/infer/gpt2-alloc-test.f. All three run on
\ maki/infer/gpt2-bind-fixture.f, which owns the synthetic-checkpoint emitter, the
\ leak counters and the shared assertion helpers; that file's header explains how
\ the fixtures are generated and why these suites reopen package GPT2TX.
\
\ THESE LEGS WALK THE TRANSACTION ONE STATE AT A TIME and prove the same thing about
\ each: every state has an owner and a total exit, so no step in the sequence can
\ leave a resource with nobody to give it back.
\
\ SAFET reserves the mapping record before publishing the census, so CHECK has no
\ allocation or catch. These legs prove each state has a total exit: prep -> ABORT,
\ checked prep -> ABORT-CHECKED, model -> MODEL-DISPOSE. The checked-prep type keeps
\ COMMIT-MAPPED free of fallible preparation.

require maki/infer/gpt2-bind-fixture.f

package GPT2TX

: CHECK-TAKE-MOVED ( SAFET:map-take -- SAFET:mapping )
   MATCH SAFET:map-take
      moved OF ENDOF
      empty OF E-GX-IMAGE throw ENDOF
   ;MATCH ;

: T-CHECK-FOREIGN ( -- )
   s" a foreign configuration is refused, and nothing has moved" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK                          \ the twin: same census, other identity
         MATCH GPT2TX:check-result
            matched OF
               s" the identity twin was accepted as this model" T-LABEL
               0 0= 0= TTRUE
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               code E-GX-FOREIGN T=
               s" and the refused prep is whole: still held, still ABORTable" T-LABEL
               TX-HELD
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" foreign leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-CHECK-MATCH ( -- )
   s" the configuration that built the prep matches it" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-A CHECK
         MATCH GPT2TX:check-result
            matched OF
               \ The census gave up its image and was released; the mapping it gave up
               \ is now the compared prep's. One owner out, one owner in, so the totals
               \ are exactly where PREPARE left them - and no kernel mapping moved.
               s" the mapping moved out of the census without changing what is owned" T-LABEL
               TX-HELD
               s" and a compared prep that declines to commit disposes totally" T-LABEL
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               s" a prep was called foreign to its own configuration, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" match leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-COMMIT ( -- )
   s" a compared prep commits to a mapped model" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!                          \ the configuration's own key, to compare
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-A CHECK
         MATCH GPT2TX:check-result
            matched OF
               COMMIT-MAPPED
               s" the prep block is gone and the residency is held" T-LABEL
               TX-MODEL-HELD
               s" the model carries the depth the prep validated" T-LABEL
               MODEL-NL TX-NL T=
               s" and the identity the prep captured, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               s" and the model's exit gives every owner back" T-LABEL
               MODEL-DISPOSE RES-CODE 0 T=
            ENDOF
            refused OF
               {: code:n :}
               s" commit leg was refused, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" commit leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- a refusal leaves the prep genuinely usable, not merely alive ----------------
\ The counters in T-CHECK-FOREIGN show a refused prep is still HELD; they cannot show
\ it is still WORKABLE. This leg spends it: the same prep that was just refused is
\ handed to CHECK again under the configuration that built it, and it commits all the
\ way to a model. That is the property a binder actually relies on when it tries a
\ second configuration on a prep, and nothing weaker demonstrates it - a prep whose
\ block had been half-consumed on the refusal path would pass every counter assertion
\ and fail right here.
: T-REFUSE-THEN-BIND ( -- )
   s" a refused prep still binds under its own configuration" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK                          \ refused: the twin's identity
         MATCH GPT2TX:check-result
            matched OF
               s" the twin was accepted, so the refusal never happened" T-LABEL
               0 0= 0= TTRUE
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               code E-GX-FOREIGN T=
               TX-CFG-A CHECK                    \ the very same prep, its own identity
               MATCH GPT2TX:check-result
                  matched OF
                     COMMIT-MAPPED
                     s" and the model it yields is complete" T-LABEL
                     MODEL-NL TX-NL T=
                     TX-STASH-MKEY
                     TX-KEY-IS-CFG
                     TX-MODEL-HELD
                     MODEL-DISPOSE RES-CODE 0 T=
                  ENDOF
                  refused OF
                     {: c2:n :}
                     s" the refusal damaged the prep: its own configuration was refused, code" T-LABEL
                     c2 . cr
                     0 0= 0= TTRUE
                     ABORT
                  ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" refuse-then-bind leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- presence-gated real artifact ----------------------------------------------
768 4 * constant TX-PB-LEN                      \ h.0.ln_1.weight: nembd F32 values
create TX-PBA TX-PB-LEN allot                   \ the bytes the census copied out
create TX-PBB TX-PB-LEN allot                   \ the bytes found at the computed offset
variable TX-MOFF

: TX-MAP-BODY ( SAFET:mapping ptr u8 n -- SAFET:mapping ) {: ba:ptr blen:n :}
   ba TX-MOFF @ BYTE+  TX-PBB  TX-PB-LEN  BYTE-COPY ;

\ WHAT THIS PROVES, AND WHAT IT DOES NOT. The mapped store serves a slot as
\ mapping-base + the row's offset, and the row's offset is the census's MAP-OFFSET?.
\ This leg checks that arithmetic against the real checkpoint from both ends: the bytes
\ the census copies out of one real tensor, and the bytes sitting at that tensor's
\ computed offset inside the detached mapping, are the same bytes. Since the mapping IS
\ the file mapped read-only, those are the file's bytes at that offset.
\
\ It stops one link short of reading through the committed model, and the reason is
\ structural rather than an omission: reading a slot through a held resident needs a
\ scoped access that disposes its owner on the throw path (WSTORE:WITH-SLOT throws
\ E-SLOT/E-EXTENT, and a resident cannot be rebuilt around a throw), which is the
\ linear-scope capability and belongs to the forward-pass leaf. The remaining link -
\ that WITH-SLOT over a mapped store returns the bytes at base+offset - is already
\ pinned by the byte-equality legs in weight-store-test.f over both arms.
\ One tensor, named by its real HF key: copy its first bytes out of the census, note
\ the mapping-frame offset the table will carry, then read the mapping at exactly that
\ offset and require the same bytes. Parameterised by key so the leg can walk several
\ tensors of different shapes and file positions rather than trusting one.
: TX-PROBE-ONE ( ptr u8 n -- ) {: ka:ptr ku:n :}
   TX-REAL-PATH SAFET:LOAD
   ka ku SAFET:FIND TX-OPT-VAL {: id:n :}
   id TX-PBA TX-PB-LEN SAFET:COPY-DATA? TX-OPT-VAL TX-PB-LEN T=
   id SAFET:MAP-OFFSET? TX-OPT-VAL TX-MOFF !
   SAFET:DETACH-MAPPING CHECK-TAKE-MOVED        \ ( census mapping )
   swap SAFET:RELEASE                           \ ( mapping )
   [: TX-MAP-BODY ;] SAFET:WITH-MAPPING drop
   SAFET:UNMAP-MAPPING RES-CODE 0 T=
   TX-PBA TX-PB-LEN TX-PBB TX-PB-LEN TX-BYTES= ;

\ Three tensors, chosen so a single lucky offset cannot pass the leg: a rank-1 vector
\ near the start of the data section, the rank-2 Conv1D matrix whose [in,out]
\ orientation the forward pass depends on, and a tensor in the LAST block, whose data
\ sits hundreds of megabytes into the file where a 32-bit offset truncation or a
\ header-length slip would show up and a small-offset probe would not.
: TX-REAL-BYTES ( -- )
   s" a rank-1 vector is byte-identical at its computed mapping offset" T-LABEL
   s" h.0.ln_1.weight" TX-PROBE-ONE
   s" so is the Conv1D matrix the forward pass reads untransposed" T-LABEL
   s" h.0.attn.c_attn.weight" TX-PROBE-ONE
   s" and so is a tensor far into the file, where a truncated offset would show" T-LABEL
   s" h.11.mlp.c_proj.weight" TX-PROBE-ONE ;

548105171 constant TX-REAL-BYTES-N               \ the pinned checkpoint's exact file size

: TX-REAL-COMMIT ( -- )
   TX-CFG-124M TX-CFG-KEY!                      \ the real configuration's own identity
   TX-REAL-PATH SAFET:LOAD TX-CFG-124M PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         s" real gpt2 prepared rows=" type PREP-COUNT dup . cr
         160 T=
         TX-CFG-124M CHECK
         MATCH GPT2TX:check-result
            matched OF
               COMMIT-MAPPED
               s" the real checkpoint commits to a mapped model of 12 layers" T-LABEL
               MODEL-NL 12 T=
               TX-MODEL-HELD
               s" bound to the real configuration's identity, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               \ ok is not a token success flag: it carries the byte count WSTORE gave
               \ back, which for a mapped model is the whole checkpoint mapping. Pinning
               \ the exact file size is what proves the model was serving the entire
               \ file rather than some truncated span of it.
               s" and its exit gives back the whole checkpoint mapping, to the byte" T-LABEL
               MODEL-DISPOSE TX-RES-VAL TX-REAL-BYTES-N T=
            ENDOF
            refused OF
               {: code:n :}
               s" the real checkpoint was refused as foreign, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" the real checkpoint did not prepare, code" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-REAL ( -- )
   TX-REAL-PATH SAFET:PRESENT? 0= if
      s" gpt2-check: gpt2-model/model.safetensors absent -> real-artifact leg SKIPPED" type cr
      0 0= TTRUE exit
   then
   s" the real gpt2 checkpoint binds end to end" T-LABEL
   TX-REAL-BYTES
   TX-NO-LEAK
   TX-REAL-COMMIT
   TX-NO-LEAK ;

\ ---- static half: the checker enforces the mapped arm's ownership rules --------
\ The PREPARE phase's own static rules live with gpt2-bind-test.f; these are the ones
\ the compare-and-commit half adds.
: T-CHECKER-COMMIT ( -- )
   s" the commit half's public surface resolves" T-LABEL
   s" GX-OK-CHECK ( GPT2TX:prep MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-ACCEPTED
   s" GX-OK-ABORT-CHECKED ( GPT2TX:checked-prep -- ) GPT2TX:ABORT-CHECKED" TX-ACCEPTED
   s" GX-OK-COMMIT ( GPT2TX:checked-prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-ACCEPTED
   s" GX-OK-MODEL-DISPOSE ( GPT2TX:gpt2-model -- result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-ACCEPTED
   \ THE POINT OF THE WHOLE SHAPE. The commit cannot be reached with an uncompared prep:
   \ its argument type is one only CHECK produces, so "the identity was compared" is a
   \ static precondition rather than a rule the commit's body has to remember. Deleting
   \ the compare from CHECK cannot restore this candidate either - it is the TYPE that
   \ refuses, so no edit to a body makes an unchecked prep committable.
   s" the commit is unreachable without the compare" T-LABEL
   s" GX-BAD-COMMIT-PREP ( GPT2TX:prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-REJECTED
   s" GX-BAD-COMMIT-CENSUS ( SAFET:census -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-REJECTED
   s" GX-BAD-CHECK-CHECKED ( GPT2TX:checked-prep MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-REJECTED
   s" GX-BAD-CHECK-AMBIENT ( MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-REJECTED
   s" a checked prep is linear: no copy, no discard, no store, no forging" T-LABEL
   s" GX-BAD-CHK-DUP ( GPT2TX:checked-prep -- GPT2TX:checked-prep GPT2TX:checked-prep ) dup" TX-REJECTED
   s" GX-BAD-CHK-DROP ( GPT2TX:checked-prep -- ) drop" TX-REJECTED
   s" GX-BAD-CHK-STORE ( GPT2TX:checked-prep ptr n -- ) !" TX-REJECTED
   s" GX-BAD-CHK-FORGE ( n -- GPT2TX:checked-prep ) " TX-REJECTED
   s" the two prep kinds do not substitute for each other at any exit" T-LABEL
   s" GX-BAD-ABORT-CHECKED-PREP ( GPT2TX:prep -- ) GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" GX-BAD-ABORT-CHK ( GPT2TX:checked-prep -- ) GPT2TX:ABORT" TX-REJECTED
   s" GX-BAD-CHK-TWICE ( GPT2TX:checked-prep -- ) GPT2TX:ABORT-CHECKED GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" GX-BAD-COMMIT-THEN-ABORT ( GPT2TX:checked-prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" check-result payloads cannot cross roles" T-LABEL
   s" GX-BAD-MATCHED-PREP ( GPT2TX:prep -- GPT2TX:check-result ) GPT2TX-CHECK--RESULT:MATCHED" TX-REJECTED
   s" GX-BAD-REFUSED-CHK ( GPT2TX:checked-prep n -- GPT2TX:check-result ) GPT2TX-CHECK--RESULT:REFUSED" TX-REJECTED
   s" GX-BAD-CHECK-DROPPED ( GPT2TX:prep MDLCFG:mcfg -- ) GPT2TX:CHECK" TX-REJECTED
   \ A model owns a linear residency, so the RECORD is linear by containment: the
   \ checker refuses to copy or discard it, which is what makes the checkpoint mapping
   \ impossible to leak or to free twice.
   s" a bound model is linear by containment, and cannot be forged" T-LABEL
   s" GX-BAD-MODEL-DUP ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model GPT2TX:gpt2-model ) dup" TX-REJECTED
   s" GX-BAD-MODEL-DROP ( GPT2TX:gpt2-model -- ) drop" TX-REJECTED
   s" GX-BAD-MODEL-STORE ( GPT2TX:gpt2-model ptr n -- ) !" TX-REJECTED
   s" GX-BAD-MODEL-FORGE ( n -- GPT2TX:gpt2-model ) " TX-REJECTED
   s" GX-BAD-MODEL-PROOF-RAW ( WSTORE:resident n MDLCFG:cfgkey n -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:MAKE" TX-REJECTED
   s" the model's exit consumes it exactly once" T-LABEL
   s" GX-BAD-MD-TWICE ( GPT2TX:gpt2-model -- result<n,n> result<n,n> ) GPT2TX:MODEL-DISPOSE GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-KEEPS ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-DROPPED ( GPT2TX:gpt2-model -- ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-RESIDENT ( WSTORE:resident -- result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" and the residency inside a model cannot be reached around it" T-LABEL
   s" GX-BAD-MODEL-RD ( GPT2TX:gpt2-model -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" TX-REJECTED
   \ The erasures are the whole soundness cost of this module, so their confinement
   \ is asserted, not assumed. A candidate names them qualified, the way a foreign
   \ file would have to; each is unresolvable, which is what package-private means
   \ here. This is the probe half of the opacity claim in gpt2-bind.f's header - the
   \ other half is refine-lint confining the two inverse mints to that file, because
   \ these probes cannot speak for a file that REOPENS the package.
   s" the mapped arm's audited erasures stay package-private" T-LABEL
   s" GX-BAD-MINT-CHECKED ( ptr u8 -- GPT2TX:checked-prep ) GPT2TX:MINT-CHECKED" UNRESOLVED
   s" GX-BAD-TAKE-CHECKED ( GPT2TX:checked-prep -- ptr n ) GPT2TX:TAKE-CHECKED" UNRESOLVED
   s" GX-BAD-MAPPING-N ( SAFET:mapping -- n ) GPT2TX:MAPPING>N" UNRESOLVED
   s" GX-BAD-N-MAPPING ( n -- SAFET:mapping ) GPT2TX:N>MAPPING" UNRESOLVED
   s" GX-BAD-MINT-PROOF ( -- GPT2TX:mdl-proof ) GPT2TX:MINT-MDL-PROOF" UNRESOLVED
   s" GX-BAD-MODEL-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model n ) GPT2TX:MODEL-NL" UNRESOLVED
   s" GX-BAD-MODEL-KEY ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model MDLCFG:cfgkey ) GPT2TX:MODEL-KEY" UNRESOLVED ;

: RUN-MAPPED ( -- )
   T-RESET
   TX-BASELINE!
   T-CHECKER-COMMIT
   T-CHECK-FOREIGN    TX-NO-LEAK
   T-CHECK-MATCH      TX-NO-LEAK
   T-COMMIT           TX-NO-LEAK
   T-REFUSE-THEN-BIND TX-NO-LEAK
   T-REAL
   s" the whole suite released every owner it took" T-LABEL
   TX-NO-LEAK
   TX-CLEANUP ;

RUN-MAPPED

;package

\ ---------------------------------------------------------------------------------
\ package GPT2TX-DR - the destruction-review pins, from OUTSIDE the package.
\
\ WHY THIS SECTION DOES NOT REOPEN GPT2TX. Everything above runs inside the package,
\ because the leaf contract asks the fixtures to read validated rows that no public
\ word hands out. That is also why the legs above cannot speak for what a FOREIGN file
\ can do: inside the package the private words resolve, so an "is this reachable?"
\ question asked from in there answers about the wrong vantage point. This section is
\ the outside vantage point - a package that has never opened GPT2TX - and it pins the
\ gap the model's private-mint proof does NOT close, the MODELPROV-TEST T-KNOWN-GAP
\ convention.
\
\ WHAT THE PROOF DOES AND DOES NOT BUY. `mdl-proof` makes a model unforgeable from
\ nothing: no outside file can conjure the proof, so no outside file can MAKE a model
\ out of thin air. It does not make a REAL model tamper-proof, because the generated
\ UNMAKE is public: a holder of a genuine model can take the residency straight out of
\ it, dispose it behind the model's back, or rebuild the record with a forged depth and
\ the original proof. The three pins below are written as ACCEPT deliberately - they
\ record what compiles TODAY. When the sealed-destructure capability
\ (habu-checker-sealed-destructure-d967fc03) lands, these three legs FAIL, and that
\ failure is the signal to delete them and retire the caveat in gpt2-bind.f's header.
\ The fourth pin is the control that keeps the other three honest: the package's own
\ readers stay unreachable from here, so these are gaps in the GENERATED surface, not
\ an accident of the suite's vantage point.
\ ---------------------------------------------------------------------------------
package GPT2TX-DR

: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: T-KNOWN-GAP ( -- )
   s" KNOWN GAP: the generated UNMAKE extracts a real model's residency TODAY" T-LABEL
   s" DRX-UNMAKE ( GPT2TX:gpt2-model -- WSTORE:resident n MDLCFG:cfgkey GPT2TX:mdl-proof ) GPT2TX-GPT2--MODEL:UNMAKE"
      ACCEPTED
   s" KNOWN GAP: so the residency can be disposed behind the model's back" T-LABEL
   s" DRX-STEAL ( GPT2TX:gpt2-model -- result<n,n> ) GPT2TX-GPT2--MODEL:UNMAKE drop drop drop WSTORE:RESIDENT-DISPOSE"
      ACCEPTED
   s" KNOWN GAP: and a real model can be rebuilt with a forged depth" T-LABEL
   s" DRX-FORGE-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:UNMAKE >r >r drop 99 r> r> GPT2TX-GPT2--MODEL:MAKE"
      ACCEPTED
   s" the proof still refuses a model built from nothing, which is what it is for" T-LABEL
   s" DRX-FORGE-WHOLE ( WSTORE:resident n MDLCFG:cfgkey n -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CONTROL: the package's own readers are unreachable from outside it, so the" T-LABEL
   s" three gaps above are in the GENERATED surface, not in this suite's vantage" T-LABEL
   s" DRX-MODEL-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model n ) GPT2TX:MODEL-NL" UNRESOLVED
   s" DRX-MINT-PROOF ( -- GPT2TX:mdl-proof ) GPT2TX:MINT-MDL-PROOF" UNRESOLVED ;

T-KNOWN-GAP
T-REPORT

;package
