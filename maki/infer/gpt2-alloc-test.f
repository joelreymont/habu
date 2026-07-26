\ gpt2-alloc-test.f - GPT2TX bind-transaction acceptance: the ALLOCATED arm (S6b2).
\ CHECK-ALLOC, COMMIT-ALLOCATED, the packed arena the commit lays out, the unwind at
\ each fallible step, and the presence-gated real-checkpoint leg.
\
\ The PREPARE half it consumes is proved in maki/infer/gpt2-bind-test.f, and the
\ mapped arm in maki/infer/gpt2-check-test.f. All three run on
\ maki/infer/gpt2-bind-fixture.f, which owns the synthetic-checkpoint emitter, the
\ leak counters and the shared assertion helpers; that file's header explains how the
\ fixtures are generated and why these suites reopen package GPT2TX.
\
\ COPY CORRECTNESS IS CHECKED AGAINST THE ARENA THE COMMIT ACTUALLY FILLED, not
\ against a re-derivation: CA-ARENA is where COMMIT-ALLOCATED put the weights and
\ ARENA-OFF is where it decided each slot goes, so reading those bytes back and
\ comparing them with what the census itself copies out is a comparison between the
\ commit's output and the file. It has to be done this way today because there is no
\ way to read a slot back through a bound model - that needs the scoped access over a
\ held resident which the linear-scope capability provides, exactly as the mapped
\ arm's real-artifact leg records.

require maki/infer/gpt2-bind-fixture.f

package GPT2TX

\ ---- the allocated arm's gate: compare and retype, nothing moves -----------------
: T-CHECK-ALLOC ( -- )
   s" CHECK-ALLOC refuses a foreign prep and moves nothing" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               s" a foreign configuration must not match" T-LABEL
               ABORT-CHECKED-ALLOC
               0 0= 0= TTRUE
            ENDOF
            refused OF
               E-GX-FOREIGN T=
               s" and the refused prep is still whole and still ABORTable" T-LABEL
               PREP-COUNT TX-CNT T=
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" check-alloc leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the allocated commit -------------------------------------------------------------
$800 constant TX-CB-CAP
create TX-CB-A TX-CB-CAP allot                  \ what the census says the tensor is
create TX-CB-B TX-CB-CAP allot                  \ what the commit put in the arena

: TX-ARENA-BYTES ( n n -- )                     \ arena offset, length -> TX-CB-B
   {: aoff:n len:n :}
   CA-ARENA @ aoff BYTE+  TX-CB-B  len BYTE-COPY ;

\ One slot, compared end to end: the census's own copy of the tensor against the bytes the
\ commit placed at that slot's arena offset. Parameterised by slot so the leg can walk
\ tensors of different rank and file position rather than trusting one.
\ Compares a bounded PREFIX of the span, the way the mapped arm's real-artifact probe
\ does: a weight can be megabytes, and what is being proved is that the commit put THIS
\ tensor's bytes at THIS arena offset - a wrong id, a wrong offset or a shifted row all
\ show up in the first bytes. COPY-DATA? truncates to the capacity it is given, so the
\ prefix length is what it returns and what both sides are compared over.
\ Reads the LAST bytes of a tensor out of the census, which COPY-DATA? cannot do - it
\ copies from the start and truncates to the capacity it is given, so a head-only probe
\ passes for a span that was copied short. The tail is where a truncation shows.
variable TX-TAIL-N

: TX-TAIL-BODY ( SAFET:census ptr u8 n -- SAFET:census ) {: sa:ptr slen:n :}
   slen TX-CB-CAP > if TX-CB-CAP else slen then {: take:n :}
   sa slen take - BYTE+  TX-CB-A  take BYTE-COPY
   take TX-TAIL-N ! ;

\ Head AND tail, because either alone is blind to a real defect: the head misses a copy
\ that was truncated, and a span long enough to exceed the compare buffer is exactly where
\ that happens. Both ends are compared against a fresh census of the same file.
: TX-SPAN-AGREES ( SAFET:census n n n -- SAFET:census )
   {: id:n aoff:n len:n :}
   len TX-CB-CAP > if TX-CB-CAP else len then {: take:n :}
   id TX-CB-A take SAFET:COPY-DATA? TX-OPT-VAL take T=
   aoff take TX-ARENA-BYTES
   TX-CB-A take TX-CB-B take TX-BYTES=
   id [: TX-TAIL-BODY ;] SAFET:WITH-TENSOR TX-OPT-VAL drop
   TX-TAIL-N @ {: tn:n :}
   aoff len + tn -  tn TX-ARENA-BYTES
   TX-CB-A tn TX-CB-B tn TX-BYTES= ;

\ ---- forced failure at each fallible step ----------------------------------------------
\ There is no allocation fault injector in the tree, so each leg forces its step to fail by
\ corrupting ONE field of a real witness block produced by a real PREPARE, and then runs the
\ real COMMIT-ALLOCATED. What is under test is the unwind, and the unwind does not care
\ which code came out of the step - it cares that everything acquired so far, plus the two
\ owners the prep still held, are given back before the code surfaces. Every leg therefore
\ ends at the same place: TX-NO-LEAK, every counter back where the suite found it. That
\ helper compares against the captured baseline rather than against zero, which is what
\ makes these legs correct under maki/test.f - the suites share one process, and the
\ weight-store suite ahead of this one leaves its own documented throw strands behind.

\ An arena size no allocator can satisfy: the step that asks for it fails, and nothing has
\ been acquired yet beyond the prep's own owners.
: TX-WRECK-SUM ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   MAX-N 16 - blk P-SUM cells + !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ A row extent no arena can lay out: the table step meets it while the arena and its buffer
\ are already live, so this leg is the one that proves the buffer is given back. It wrecks a
\ ROW rather than P-CNT deliberately - P-CNT is how the block describes its own size to the
\ release, so corrupting that would break the unwind itself instead of testing it.
: TX-WRECK-LEN ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   MAX-N  blk 0 PR-LEN ROW-CELL !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ A census id no census has: the copy walk refuses with the arena, its buffer AND the
\ arena-frame table all live, which is the widest unwind this word has.
: TX-WRECK-ID ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   $7FFFFFF  blk 0 PR-ID ROW-CELL !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ Each leg builds a real transaction, wrecks the one field that makes its step fail, and
\ runs the real commit. A leg that reaches MODEL-DISPOSE has NOT thrown, and TTHROWSQ says
\ so; the baseline assertion after it is what proves the unwind was complete.
: TX-CA-WITNESS ( -- GPT2TX:check-alloc-result )
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF TX-CFG-A CHECK-ALLOC ENDOF
      rejected OF
         s" forced-failure leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         E-TX-FIX throw
      ENDOF
   ;MATCH ;

: TX-GATE-REFUSED ( n -- )
   s" forced-failure leg was refused by the gate" T-LABEL
   . cr
   0 0= 0= TTRUE ;

: TX-WRECK-SUM-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-SUM COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

: TX-WRECK-LEN-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-LEN COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

: TX-WRECK-ID-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-ID COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

\ The happy path, and the two span probes that prove the arena holds the file's bytes at the
\ offsets the commit chose. The probes run against a SECOND census of the same file, so what
\ they compare the arena with is the file rather than anything the commit computed.
\ ONE slot pair, used by both halves of the probe. The recording word and the comparing
\ word must name the SAME slots: recording slot A's arena location and then comparing
\ slot B's bytes against it passes whenever the data is uniform and means nothing when it
\ is not, which is exactly the shape this pair replaced.
2 constant TX-PROBE-VEC                         \ ln_f.weight: a rank-1 vector
variable TX-RA-OFF   variable TX-RA-LEN   variable TX-RA-ID
variable TX-RB-OFF   variable TX-RB-LEN   variable TX-RB-ID

variable TX-CA-VEC-OFF                          \ the vector slot's arena offset and extent
variable TX-CA-VEC-LEN
variable TX-CA-CONV-OFF                         \ and the Conv1D slot's
variable TX-CA-CONV-LEN

\ The census id a slot's HF key resolves to, asked of the census on the stack.
: TX-ID-OF-SLOT ( SAFET:census n -- SAFET:census n ) {: slot:n :}
   TX-CFG-A slot TX-KEY-LEN {: klen:n :}
   drop                                         \ the mcfg copy
   TX-KBUF klen SAFET:FIND TX-OPT-VAL ;

: TX-PROBE-SPANS ( -- )
   TX-PATH SAFET:LOAD
   TX-PROBE-VEC TX-ID-OF-SLOT {: vid:n :}
   TX-PROBE-CONV TX-ID-OF-SLOT {: cid:n :}
   s" the rank-1 vector span is the file's own bytes, at its arena offset" T-LABEL
   vid  TX-CA-VEC-OFF @  TX-CA-VEC-LEN @  TX-SPAN-AGREES
   s" and so is the Conv1D matrix the forward pass reads" T-LABEL
   cid  TX-CA-CONV-OFF @  TX-CA-CONV-LEN @  TX-SPAN-AGREES
   SAFET:RELEASE ;

\ Which slot carries a given census id. The real checkpoint's slots are not knowable by
\ eye, so a probe names its tensor by HF key, resolves the key to a census id, and finds
\ the slot from the block's own carried rows - the same rows the commit copied by.
: TX-SLOT-OF-ID ( ptr n n -- n ) {: blk:ptr id:n :}
   -1
   blk P-CNT cells + @ 0 ?do
      blk i PR-ID ROW-CELL @ id = if drop i then
   loop ;

: TX-NOTE-ID ( ptr n n ptr n ptr n -- ) {: blk:ptr id:n ovar:ptr lvar:ptr :}
   blk id TX-SLOT-OF-ID {: slot:n :}
   slot 0 < if E-TX-FIX throw then
   blk slot ARENA-OFF ovar !
   blk slot PR-LEN ROW-CELL @ lvar ! ;

\ ---- the packed layout, asserted as a recurrence -------------------------------------
\ The span probes check two slots. This checks the SHAPE of the whole layout, which is
\ what a two-slot probe cannot: the arena starts at zero, every row begins exactly where
\ the previous one ended - so there is no gap and no overlap anywhere - and the last row
\ ends exactly at the arena size the commit allocated. An arena walk that drifts by one
\ row, or a length that disagrees with the sum, shows up here rather than in whichever
\ two slots the probes happened to pick.
variable TX-WALK-BAD

: TX-ARENA-WALK ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   blk P-CNT cells + @ {: count:n :}
   0 TX-WALK-BAD !
   s" the packed arena starts at zero" T-LABEL
   blk 0 ARENA-OFF 0 T=
   count 0 ?do
      blk i ARENA-OFF  blk i PR-LEN ROW-CELL @ +  {: end:n :}
      i 1 + count < if
         blk i 1 + ARENA-OFF end <> if 1 TX-WALK-BAD ! then
      else
         end blk P-SUM cells + @ <> if 1 TX-WALK-BAD ! then
      then
   loop
   s" every row begins where the previous ended, and the last ends at the arena size" T-LABEL
   TX-WALK-BAD @ 0 T=
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

: TX-NOTE-ARENA ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   blk TX-PROBE-VEC ARENA-OFF TX-CA-VEC-OFF !
   blk TX-PROBE-VEC PR-LEN ROW-CELL @ TX-CA-VEC-LEN !
   blk TX-PROBE-CONV ARENA-OFF TX-CA-CONV-OFF !
   blk TX-PROBE-CONV PR-LEN ROW-CELL @ TX-CA-CONV-LEN !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ The real checkpoint, committed to an ALLOCATED model: 548 MB copied span by span into
\ one packed arena. Both probes compare the arena against a second census of the same
\ file, so what they agree with is the file on disk and not any number the commit derived.
: TX-NOTE-REAL ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   TX-REAL-PATH SAFET:LOAD
   s" h.0.ln_1.weight" SAFET:FIND TX-OPT-VAL {: vid:n :}
   s" h.0.attn.c_attn.weight" SAFET:FIND TX-OPT-VAL {: cid:n :}
   SAFET:RELEASE
   vid TX-RA-ID !  cid TX-RB-ID !
   blk vid TX-RA-OFF TX-RA-LEN TX-NOTE-ID
   blk cid TX-RB-OFF TX-RB-LEN TX-NOTE-ID
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

: TX-REAL-SPANS ( -- )
   TX-REAL-PATH SAFET:LOAD
   s" the real rank-1 vector is the file's own bytes at its arena offset" T-LABEL
   TX-RA-ID @ TX-RA-OFF @ TX-RA-LEN @ TX-SPAN-AGREES
   s" and so is the real Conv1D matrix the forward pass reads" T-LABEL
   TX-RB-ID @ TX-RB-OFF @ TX-RB-LEN @ TX-SPAN-AGREES
   SAFET:RELEASE ;

: T-REAL-ALLOC ( -- )
   TX-REAL-PATH SAFET:PRESENT? 0= if
      s" gpt2-alloc: real-artifact allocated leg SKIPPED" type cr exit
   then
   s" the real checkpoint commits to an ALLOCATED model that owns its arena" T-LABEL
   TX-CFG-124M TX-CFG-KEY!
   TX-REAL-PATH SAFET:LOAD TX-CFG-124M PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         PREP-SUM {: want:n :}
         TX-CFG-124M CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               TX-ARENA-WALK
               TX-NOTE-REAL
               COMMIT-ALLOCATED
               MODEL-NL 12 T=
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               TX-REAL-SPANS
               s" and its exit gives back exactly the packed arena" T-LABEL
               MODEL-DISPOSE TX-RES-VAL want T=
               TX-NO-LEAK
            ENDOF
            refused OF TX-GATE-REFUSED ABORT ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" real allocated leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-COMMIT-ALLOC ( -- )
   s" a matched prep commits to an allocated model that owns its arena" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         PREP-SUM {: want:n :}
         TX-CFG-A CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               TX-ARENA-WALK
               TX-NOTE-ARENA
               COMMIT-ALLOCATED
               s" it reports the depth PREPARE validated" T-LABEL
               MODEL-NL TX-NL T=
               s" and the configuration's identity, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               TX-PROBE-SPANS
               s" and its exit gives back exactly the packed arena" T-LABEL
               MODEL-DISPOSE TX-RES-VAL want T=
               TX-NO-LEAK
            ENDOF
            refused OF TX-GATE-REFUSED ABORT ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" allocated-commit leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-COMMIT-ALLOC-FAILS ( -- )
   s" an arena the allocator cannot satisfy unwinds completely" T-LABEL
   TX-CLEAN!  TX-LAY
   [: TX-WRECK-SUM-RUN ;] E-MEM-MAP TTHROWSQ
   TX-NO-LEAK
   s" a row extent no arena can lay out unwinds the live buffer too" T-LABEL
   [: TX-WRECK-LEN-RUN ;] WSTORE:E-EXTENT TTHROWSQ
   TX-NO-LEAK
   s" a census id the census does not have unwinds buffer and table together" T-LABEL
   [: TX-WRECK-ID-RUN ;] E-GX-COPY TTHROWSQ
   TX-NO-LEAK ;

\ ---- static half: the checker enforces the allocated arm's ownership rules -------
\ The PREPARE phase's static rules live with gpt2-bind-test.f and the mapped arm's with
\ gpt2-check-test.f; these are the ones the allocated witness adds. They are split into
\ four short sections rather than one long word because each candidate is compiled and
\ checked in turn, and a single block of this many rows is more than one definition
\ should carry.
: T-CHECKER-WITNESS ( -- )
   s" the allocated witness is linear and unforgeable" T-LABEL
   s" GX-BAD-CA-DUP ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc GPT2TX:checked-prep-alloc ) dup" TX-REJECTED
   s" GX-BAD-CA-DROP ( GPT2TX:checked-prep-alloc -- ) drop" TX-REJECTED
   s" GX-BAD-CA-STORE ( GPT2TX:checked-prep-alloc ptr n -- ) !" TX-REJECTED
   s" GX-BAD-CA-FORGE ( n -- GPT2TX:checked-prep-alloc ) " TX-REJECTED
   s" GX-BAD-CA-FORGE-P ( ptr u8 -- GPT2TX:checked-prep-alloc ) " TX-REJECTED
   \ The same shape the mapped arm relies on: the allocated commit's argument type is
   \ one only CHECK-ALLOC produces, so "the identity was compared" is a static
   \ precondition and no edit to a body can make an ungated prep committable.
   s" the allocated commit is unreachable without CHECK-ALLOC" T-LABEL
   s" GX-BAD-CAC-PREP ( GPT2TX:prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED" TX-REJECTED
   s" GX-BAD-CAC-CHECKED ( GPT2TX:checked-prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED" TX-REJECTED
   s" GX-BAD-CAC-CENSUS ( SAFET:census -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED" TX-REJECTED
   s" GX-BAD-CAC-AMBIENT ( -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED" TX-REJECTED ;

\ The two witnesses carry the same runtime representation, so nothing but their types
\ keeps a mapped exit off an allocated witness. Each crossing is refused in both
\ directions, and a witness cannot be spent twice or spent and kept.
: T-CHECKER-EXITS ( -- )
   s" the two witnesses do not substitute at any exit" T-LABEL
   s" GX-BAD-CA-ABORT-MAPPED ( GPT2TX:checked-prep-alloc -- ) GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" GX-BAD-CHK-ABORT-ALLOC ( GPT2TX:checked-prep -- ) GPT2TX:ABORT-CHECKED-ALLOC" TX-REJECTED
   s" GX-BAD-PREP-ABORT-ALLOC ( GPT2TX:prep -- ) GPT2TX:ABORT-CHECKED-ALLOC" TX-REJECTED
   s" GX-BAD-CA-COMMIT-MAPPED ( GPT2TX:checked-prep-alloc -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-REJECTED
   s" GX-BAD-CA-ABORT-PREP ( GPT2TX:checked-prep-alloc -- ) GPT2TX:ABORT" TX-REJECTED
   s" a witness cannot be consumed twice, nor consumed and kept" T-LABEL
   s" GX-BAD-CA-TWICE ( GPT2TX:checked-prep-alloc -- ) GPT2TX:ABORT-CHECKED-ALLOC GPT2TX:ABORT-CHECKED-ALLOC" TX-REJECTED
   s" GX-BAD-CAC-THEN-ABORT ( GPT2TX:checked-prep-alloc -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED GPT2TX:ABORT-CHECKED-ALLOC" TX-REJECTED
   s" GX-BAD-CA-KEEPS ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc ) GPT2TX:ABORT-CHECKED-ALLOC" TX-REJECTED
   s" GX-BAD-CAC-DROPPED ( GPT2TX:checked-prep-alloc -- ) GPT2TX:COMMIT-ALLOCATED" TX-REJECTED
   s" GX-BAD-CA-RESULT-DROPPED ( GPT2TX:prep MDLCFG:mcfg -- ) GPT2TX:CHECK-ALLOC" TX-REJECTED ;

\ The gate consumes the prep it is given, so it cannot run twice on one prep and cannot
\ run after that prep has been aborted; and it answers about the prep it was handed,
\ never about ambient state.
: T-CHECKER-GATE ( -- )
   s" the gate cannot run twice, nor after the prep is gone" T-LABEL
   s" GX-BAD-GATE-TWICE ( GPT2TX:prep MDLCFG:mcfg MDLCFG:mcfg -- GPT2TX:check-alloc-result ) GPT2TX:CHECK-ALLOC GPT2TX:CHECK-ALLOC" TX-REJECTED
   s" GX-BAD-GATE-AFTER-ABORT ( GPT2TX:prep MDLCFG:mcfg -- GPT2TX:check-alloc-result ) >r GPT2TX:ABORT r> GPT2TX:CHECK-ALLOC" TX-REJECTED
   s" GX-BAD-CA-AMBIENT ( MDLCFG:mcfg -- GPT2TX:check-alloc-result ) GPT2TX:CHECK-ALLOC" TX-REJECTED
   s" check-alloc-result payloads cannot cross roles" T-LABEL
   s" GX-BAD-CA-MATCHED-PREP ( GPT2TX:prep -- GPT2TX:check-alloc-result ) GPT2TX-CHECK--ALLOC--RESULT:MATCHED" TX-REJECTED
   s" GX-BAD-CA-MATCHED-CHK ( GPT2TX:checked-prep -- GPT2TX:check-alloc-result ) GPT2TX-CHECK--ALLOC--RESULT:MATCHED" TX-REJECTED
   s" GX-BAD-CA-REFUSED-W ( GPT2TX:checked-prep-alloc n -- GPT2TX:check-alloc-result ) GPT2TX-CHECK--ALLOC--RESULT:REFUSED" TX-REJECTED
   s" GX-BAD-CA-CROSS-RESULT ( GPT2TX:checked-prep-alloc -- GPT2TX:check-result ) GPT2TX-CHECK--RESULT:MATCHED" TX-REJECTED
   \ The controls: every refusal above has to be the TYPE refusing, not the candidate
   \ failing to compile for some unrelated reason, so the same surface is accepted here
   \ with its arguments in the right roles.
   s" the allocated arm's public surface resolves (controls)" T-LABEL
   s" GX-OK-CA ( GPT2TX:prep MDLCFG:mcfg -- GPT2TX:check-alloc-result ) GPT2TX:CHECK-ALLOC" TX-ACCEPTED
   s" GX-OK-CA-ABORT ( GPT2TX:checked-prep-alloc -- ) GPT2TX:ABORT-CHECKED-ALLOC" TX-ACCEPTED
   s" GX-OK-CAC ( GPT2TX:checked-prep-alloc -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-ALLOCATED" TX-ACCEPTED ;

\ Verdict 1 is "the dictionary cannot resolve this token at all", which is how a
\ package-private word looks from outside; it is kept apart from verdict 0, a real type
\ error, so "private" is proved by non-resolution rather than by any accident. These are
\ the allocated arm's own erasures and row readers.
: T-CHECKER-PRIVATE ( -- )
   s" the allocated arm's audited erasures stay package-private" T-LABEL
   s" GX-BAD-MINT-CA ( ptr u8 -- GPT2TX:checked-prep-alloc ) GPT2TX:MINT-CHECKED-ALLOC" UNRESOLVED
   s" GX-BAD-TAKE-CA ( GPT2TX:checked-prep-alloc -- ptr n ) GPT2TX:TAKE-CHECKED-ALLOC" UNRESOLVED
   s" GX-BAD-BUFFER-N ( WSTORE:buffer -- n ) GPT2TX:BUFFER>N" UNRESOLVED
   s" GX-BAD-N-BUFFER ( n -- WSTORE:buffer ) GPT2TX:N>BUFFER" UNRESOLVED
   s" GX-BAD-ARENA-OFF ( ptr n n -- n ) GPT2TX:ARENA-OFF" UNRESOLVED
   s" GX-BAD-PREP-ROW ( GPT2TX:prep n -- GPT2TX:prep n n n ) GPT2TX:PREP-ROW" UNRESOLVED ;

: RUN-ALLOCATED ( -- )
   T-RESET
   TX-BASELINE!
   T-CHECKER-WITNESS
   T-CHECKER-EXITS
   T-CHECKER-GATE
   T-CHECKER-PRIVATE
   T-CHECK-ALLOC        TX-NO-LEAK
   T-COMMIT-ALLOC       TX-NO-LEAK
   T-COMMIT-ALLOC-FAILS TX-NO-LEAK
   T-REAL-ALLOC         TX-NO-LEAK
   TX-CLEANUP ;

RUN-ALLOCATED
T-REPORT

;package
