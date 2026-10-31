\ checker-model-schema.f - Accepted and rejected checker programs.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f

package CHECKER-MODEL-PROOF
private

$10000 constant POOL-CAP
$400 constant STR-MAX
$80 constant ROW-MAX

create POOL POOL-CAP allot
create SOFF STR-MAX cells allot
create SLEN STR-MAX cells allot

variable POOL-U
variable STR-N

\ Every text below is interned once and referred to by handle, so a table row is
\ a run of cells and the data blocks stay readable as data.

: STR+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   STR-N @ STR-MAX >= if E-CMP-POOL throw then
   POOL-U @ u + POOL-CAP > if E-CMP-POOL throw then
   a POOL POOL-U @ + u BYTE-COPY
   POOL-U @ STR-N @ cells SOFF + !
   u STR-N @ cells SLEN + !
   STR-N @ POOL-U @ u + POOL-U !
   STR-N @ 1+ STR-N ! ;

public

: STR$ ( n -- ptr u8 n ) {: h:n :}
   h 0 < h STR-N @ >= or if E-CMP-ROW throw then
   POOL h cells SOFF + @ + h cells SLEN + @ ;

private

create VEC-NAME ROW-MAX cells allot
create VEC-SRC ROW-MAX cells allot
create VEC-VERD ROW-MAX cells allot

variable VEC-N

: COL! ( n ptr n n -- ) {: v:n col:ptr i:n :}
   i ROW-MAX >= if E-CMP-POOL throw then
   v col i cells + ! ;

: COL@ ( ptr n n n -- n ) {: col:ptr i:n cnt:n :}
   i 0 < i cnt >= or if E-CMP-ROW throw then
   col i cells + @ ;

\ One verdict per row, written once. `V-CERT`, `V-UNCK` and `V-REJECT` are the
\ model's three outcomes; the cases file maps the checker's -1 / 1 / 0 into
\ them, so an unresolvable is never read as a refusal.

public

0 constant V-CERT
1 constant V-UNCK
2 constant V-REJECT

: VERDICT-NAME$ ( n -- ptr u8 n )
   case
      V-CERT of s" VCert" endof
      V-UNCK of s" VUncheckable" endof
      V-REJECT of s" VReject" endof
      E-CMP-ROW throw
   endcase ;

private

: VEC-ROW ( ptr u8 n ptr u8 n n -- )
   {: na:ptr nu:n sa:ptr su:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su STR+ VEC-SRC VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

\ Two rows about the widening lattice, which nothing else here reaches. The
\ definition boundary joins at `UK-COERCE`, so `INT-WIDENS?` decides both: a
\ narrow unsigned integer reaches a wider generic one and certifies, while two
\ distinct nominal roles never meet however they are declared. A checker that
\ let same-class types stand in for each other would certify the second row.
: BUILD-WIDENING-VECTORS ( -- )
   s" int_widens_into_the_declared_output"
      s" CMV11 ( u8 -- cell )"
      V-CERT VEC-ROW
   s" a_role_never_reaches_a_sibling_role"
      s" CMV12 ( idx -- len )"
      V-REJECT VEC-ROW ;

\ The control-frame ceiling. `CF-PUSH` (src/core/checker.f) turns the frame
\ after the last one into an unresolvable instead of pushing it, so the sharpest
\ pair of programs is two runs of `begin` differing only in length: at the
\ ceiling the definition is merely unbalanced and is refused, one deeper it
\ stops being checkable and the verdict changes class. Both texts are built
\ from the ceiling below, so a row cannot drift from the number it is about.

32 constant FRAME-CEIL

: +OPENERS ( n -- ) {: opens:n :}
   opens 0 ?do s"  begin" SB-APPEND loop ;

: OPENERS$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n opens:n :}
   SB-RESET a u SB-APPEND opens +OPENERS SB$ ;

: FRAME-CAP-ROW ( ptr u8 n ptr u8 n n n -- )
   {: na:ptr nu:n sa:ptr su:n opens:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su opens OPENERS$ STR+ VEC-SRC VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

: BUILD-FRAME-CAP-VECTORS ( -- )
   s" frames_at_the_ceiling_are_still_checked"
      s" CMV13 ( i64 -- i64 )" FRAME-CEIL V-REJECT FRAME-CAP-ROW
   s" one_frame_past_the_ceiling_is_unresolvable"
      s" CMV14 ( i64 -- i64 )" FRAME-CEIL 1+ V-UNCK FRAME-CAP-ROW ;

\ `MATCH`'s own depth guard, a different rule from the ceiling above. A match
\ form opens two frames (the form's and a branch's), and `MATCH-FAM-TOK`
\ (src/core/checker.f) hard-rejects once the frame stack is deeper than the
\ number below, so a match never installs one frame and overflows on the other.
\ At that depth the form takes the ceiling's last two slots and the next opener
\ makes the definition uncheckable; one deeper the guard refuses first, and a
\ hard reject outranks an uncheckable. Lowering the guard turns the first row
\ into a refusal; raising or removing it turns the second into an uncheckable.
\ Both texts are built from the number, and the number is derived from the
\ ceiling, the relation the checker's two literals stand in: a ceiling moved on
\ one side only breaks a row.

2 constant MATCH-FRAMES               \ the form's frame, and one branch's
FRAME-CEIL MATCH-FRAMES - constant MATCH-DEPTH-MAX

: MATCH-SRC$ ( ptr u8 n n ptr u8 n -- ptr u8 n ) {: ha:ptr hu:n opens:n ta:ptr tu:n :}
   SB-RESET ha hu SB-APPEND opens +OPENERS s"  " SB-APPEND ta tu SB-APPEND SB$ ;

: MATCH-DEPTH-ROW ( ptr u8 n ptr u8 n n ptr u8 n n -- )
   {: na:ptr nu:n sa:ptr su:n opens:n ha:ptr hu:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su opens ha hu MATCH-SRC$ STR+ VEC-SRC VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

: BUILD-MATCH-DEPTH-VECTORS ( -- )
   s" a_match_at_the_deepest_frame_that_fits_takes_both_frames"
      s" CMV15 ( cmres -- n )" MATCH-DEPTH-MAX
      s" MATCH cmres cmok OF begin"
      V-UNCK MATCH-DEPTH-ROW
   s" a_match_one_frame_deeper_is_refused_before_the_overflow"
      s" CMV16 ( cmres -- n )" MATCH-DEPTH-MAX 1+
      s" MATCH cmres cmok OF"
      V-REJECT MATCH-DEPTH-ROW ;

\ Eight rows about `construct`, a three-token form and a small state machine:
\ the rules are the capture, the truncation test at the definition boundary, and
\ the inline step that turns a variant payload into the family's bundle.
\ The first pair is the step: certifies with its payload on the row, refused
\ without it. The second pair is the capture, and its verdicts differ in class:
\ `construct` takes its two operands whatever they spell (`CONSTRUCT-TOK`,
\ src/core/checker.f), so an operand naming no variant is a refusal, while the
\ same token outside the form is an unknown word, merely uncheckable; a trailing
\ operand that fell through to word lookup would answer uncheckable for both.
\ The third pair is the payload's owner: two variants of one family differing
\ only in what they carry. The last row round-trips through `MATCH`, the
\ inverse of `construct`.
\
\ Measured by mutating the checker, rebuilding the fixpoint and rerunning this
\ gate: dropping the `CONM` clause from `CHECK`'s open-form test turns only the
\ unterminated row red; deleting the inline step from `CONSTRUCT-TOK` turns the
\ first, captured, payload-owner and round-trip rows red; making the family
\ operand close the form turns every row but the uncheckable control red.
\
\ `TFAM-CONSTRUCT-FAM` (src/core/type-family.f) resolves a construct family in
\ the active package only: checker-model-manifest.f supplies that scope, and
\ checker-model-cases.f asserts CHECKER-AUTH-PACKAGE$ before asking the checker,
\ so a missing family cannot make a refusal case pass.
: BUILD-CONSTRUCT-VECTORS ( -- )
   s" construct_builds_the_bundle_from_the_variant_payload"
      s" CMV19 ( n -- cmres ) construct cmres cmok"
      V-CERT VEC-ROW
   s" construct_without_its_payload_underflows"
      s" CMV20 ( -- cmres ) construct cmres cmok"
      V-REJECT VEC-ROW
   s" an_unterminated_construct_is_refused_at_the_boundary"
      s" CMV21 ( cmres -- cmres ) construct cmres"
      V-REJECT VEC-ROW
   s" a_construct_operand_is_captured_whatever_it_spells"
      s" CMV22 ( cmres -- cmres ) construct cmres CMNOVAR"
      V-REJECT VEC-ROW
   s" the_same_operand_outside_the_form_is_only_uncheckable"
      s" CMV23 ( cmres -- cmres ) CMNOVAR"
      V-UNCK VEC-ROW
   s" the_payload_is_the_variants_and_not_the_familys"
      s" CMV24 ( n -- cmbres ) construct cmbres cmbn"
      V-CERT VEC-ROW
   s" a_sibling_variant_of_the_same_family_wants_its_own_payload"
      s" CMV25 ( n -- cmbres ) construct cmbres cmbf"
      V-REJECT VEC-ROW
   s" construct_then_match_returns_the_payload_it_was_given"
      s" CMV26 ( n -- n ) construct cmres cmok MATCH cmres cmok OF ENDOF cmerr OF ENDOF ;MATCH"
      V-CERT VEC-ROW ;

\ Two rows about `MATCH`'s scrutinee pop, the walk that takes the scrutinee's
\ whole width-expanded bundle off the row before any branch (`MATCH-SCRUT?`,
\ src/core/checker.f). Every other family here is two cells wide, and both
\ decisions show only above that. `cmwide` variants carry two cells (a
\ three-cell bundle); `cmtwin` has the same width, variant count and payloads
\ and a different identity. The two texts differ only in the family token and
\ variant names:
\   - the first pops a three-cell bundle whole and certifies; a walk that
\     stopped short would leave the rest of the bundle on the row and fail it;
\   - the second names the same-width family and is refused; a pop that
\     compared cell counts, not the family id, would certify it.
\ Both families are in the signature because a `MATCH` family token resolves as
\ a signature type name does (`TFAM-MATCH-FAM`, src/core/type-family.f).
\
\ Measured by mutating the checker, rebuilding the fixpoint and rerunning this
\ gate: dropping the family test from `MATCH-SCRUT-CELL?` moves only the second
\ row (to certified); walking a three-cell bundle as two moves only the first
\ (to refused); walking every bundle as two fails the fixpoint self-check,
\ because `FIND-EXECUTABLE-IN-PATH` (src/lib/process-env.f) matches an `option`
\ whose payload is a multi-cell layout, so the second mutation names a width
\ nothing in `src/lib/` uses.
: BUILD-SCRUTINEE-VECTORS ( -- )
   s" a_multi_cell_bundle_is_popped_whole"
      s" CMV27 ( cmtwin cmwide -- cmtwin n ) MATCH cmwide cmwa OF CHECKER-MODEL-CASES:DROP-N ENDOF cmwb OF CHECKER-MODEL-CASES:DROP-N ENDOF ;MATCH"
      V-CERT VEC-ROW
   s" a_same_width_bundle_of_another_family_is_refused"
      s" CMV28 ( cmtwin cmwide -- cmtwin n ) MATCH cmtwin cmta OF CHECKER-MODEL-CASES:DROP-N ENDOF cmtb OF CHECKER-MODEL-CASES:DROP-N ENDOF ;MATCH"
      V-REJECT VEC-ROW ;

\ Six rows about rigid host-allocation identities. An identity is minted at a
\ call site (`E-I-AK`, src/core/checker.f), once per template slot per
\ instantiation, from the counter its domain owns. Three pairs:
\   - one call against two: `MK-REGION-PAIR` names one slot twice, so its two
\     outputs carry one identity and the consumer certifies; two `MK-REGION`
\     calls are two allocations and the same consumer is refused. A counter
\     that restarted or did not advance would certify the second row;
\   - two domains at the same number: `MK-REGION` and `MK-GEN` each mint first
\     in their own domain, so both ids are 1 and the row is still refused; a
\     comparison of the number alone would certify it;
\   - a declared input identity against an atom token: a `fresh-*` name the
\     candidate's inputs declare is one identity its body holds
\     (`SIG-INPUT-FRESH`), so naming it on both sides certifies; a `mask-*`
\     token is an atom whose identity is its spelling, so the same shape
\     certifies and differing spellings are refused.
\ Measured by mutating the checker, rebuilding the fixpoint and rerunning this
\ gate: dropping `ATOM-OK?`'s name comparison moves only the two-domain and
\ differing-spelling rows (to certified); a per-domain counter that does not
\ advance moves only the two-allocation row (to certified).
: BUILD-ATOM-VECTORS ( -- )
   s" one_call_hands_both_its_outputs_one_identity"
      s" CMV29 ( -- ) CHECKER-MODEL-CASES:MK-REGION-PAIR CHECKER-MODEL-CASES:SAME-ID"
      V-CERT VEC-ROW
   s" two_calls_are_two_allocations_and_never_one"
      s" CMV30 ( -- ) CHECKER-MODEL-CASES:MK-REGION CHECKER-MODEL-CASES:MK-REGION CHECKER-MODEL-CASES:SAME-ID"
      V-REJECT VEC-ROW
   s" two_domains_at_the_same_number_still_reject"
      s" CMV31 ( -- ) CHECKER-MODEL-CASES:MK-REGION CHECKER-MODEL-CASES:MK-GEN CHECKER-MODEL-CASES:SAME-ID"
      V-REJECT VEC-ROW
   s" a_declared_input_identity_is_one_identity"
      s" CMV32 ( fresh-region-a -- fresh-region-a )"
      V-CERT VEC-ROW
   s" an_atom_tokens_identity_is_its_spelling"
      s" CMV33 ( mask-a -- mask-a )"
      V-CERT VEC-ROW
   s" a_different_spelling_is_a_different_atom"
      s" CMV34 ( mask-a -- mask-b )"
      V-REJECT VEC-ROW ;

\ Loop resources are independent of the typed data/return rows. These rows
\ bind each discharge, scope, live join and reachable loop-exit decision to
\ both the source checker and Control.v. Dead paths contribute no latch or
\ branch obligation; live paths must agree on the exact enclosing frames.
: BUILD-DISCHARGE-VECTORS ( -- )
   s" exit_requires_the_loop_resource_discharged"
      s" CMV35 ( -- ) 3 0 do exit loop"
      V-REJECT VEC-ROW
   s" unloop_without_a_loop_is_refused"
      s" CMV36 ( -- ) unloop"
      V-REJECT VEC-ROW
   s" unloop_discharges_one_loop_for_exit"
      s" CMV37 ( -- ) 3 0 do unloop exit loop"
      V-CERT VEC-ROW
   s" unloop_cannot_discharge_the_same_frame_twice"
      s" CMV38 ( -- ) 3 0 do unloop unloop exit loop"
      V-REJECT VEC-ROW
   s" nested_exit_requires_both_resources"
      s" CMV39 ( -- ) 3 0 do 3 0 do unloop exit loop loop"
      V-REJECT VEC-ROW
   s" nested_exit_after_two_unloops_certifies"
      s" CMV40 ( -- ) 3 0 do 3 0 do unloop unloop exit loop loop"
      V-CERT VEC-ROW
   s" i_skips_the_discharged_inner_frame"
      s" CMV41 ( -- ) 3 0 do 3 0 do unloop i drop unloop exit loop loop"
      V-CERT VEC-ROW
   s" j_skips_the_discharged_inner_frame"
      s" CMV42 ( -- ) 3 0 do 3 0 do 3 0 do unloop j drop unloop unloop exit loop loop loop"
      V-CERT VEC-ROW
   s" i_cannot_use_a_discharged_frame"
      s" CMV43 ( -- ) 3 0 do unloop i drop exit loop"
      V-REJECT VEC-ROW
   s" j_needs_two_remaining_resources"
      s" CMV44 ( -- ) 3 0 do 3 0 do unloop j drop unloop exit loop loop"
      V-REJECT VEC-ROW
   s" a_live_latch_requires_its_own_resource"
      s" CMV45 ( -- ) 3 0 do unloop loop"
      V-REJECT VEC-ROW
   s" leave_requires_its_lexical_loop_resource"
      s" CMV46 ( -- ) 3 0 do unloop leave loop"
      V-REJECT VEC-ROW ;

: BUILD-LOOP-BRANCH-VECTORS ( -- )
   s" live_then_joins_loop_obligations"
      s" CMV47 ( -- ) 3 0 do true if unloop then exit loop"
      V-REJECT VEC-ROW
   s" live_else_joins_loop_obligations"
      s" CMV48 ( -- ) 3 0 do true if else unloop then exit loop"
      V-REJECT VEC-ROW
   s" both_arms_can_discharge_before_exit"
      s" CMV49 ( -- ) 3 0 do true if unloop else unloop then exit loop"
      V-CERT VEC-ROW
   s" dead_then_restores_the_live_entry_obligations"
      s" CMV50 ( -- ) 3 0 do true if unloop exit then i drop loop"
      V-CERT VEC-ROW
   s" dead_else_restores_the_live_if_obligations"
      s" CMV51 ( -- ) 3 0 do true if else unloop exit then i drop loop"
      V-CERT VEC-ROW
   s" dead_if_keeps_the_live_else_obligations"
      s" CMV52 ( -- ) 3 0 do true if unloop exit else then i drop loop"
      V-CERT VEC-ROW
   s" both_dead_arms_leave_no_latch"
      s" CMV53 ( -- ) 3 0 do true if unloop exit else unloop exit then loop"
      V-CERT VEC-ROW ;

: BUILD-LOOP-QUOTATION-VECTORS ( -- )
   s" quotation_i_cannot_reach_an_outer_loop"
      s" CMV54 ( -- ) 3 0 do [: i drop ;] drop loop"
      V-REJECT VEC-ROW
   s" quotation_j_cannot_reach_outer_loops"
      s" CMV55 ( -- ) 3 0 do 3 0 do [: j drop ;] drop loop loop"
      V-REJECT VEC-ROW
   s" quotation_unloop_cannot_discharge_an_outer_loop"
      s" CMV56 ( -- ) 3 0 do [: unloop ;] drop loop"
      V-REJECT VEC-ROW
   s" quotation_exit_owns_a_separate_resource_scope"
      s" CMV57 ( -- ) 3 0 do [: exit ;] execute i drop loop"
      V-CERT VEC-ROW
   s" quotation_close_restores_outer_obligations"
      s" CMV58 ( -- ) 3 0 do [: ;] execute exit loop"
      V-REJECT VEC-ROW
   s" quotation_loops_own_their_indices"
      s" CMV59 ( -- ) 3 0 do [: 3 0 do i drop loop ;] execute i drop loop"
      V-CERT VEC-ROW ;

: BUILD-LOOP-REACHABILITY-VECTORS ( -- )
   s" do_with_no_live_exit_has_no_continuation"
      s" CMV60 ( -- ) 3 0 do unloop exit loop 0 drop"
      V-REJECT VEC-ROW
   s" qdo_keeps_its_zero_trip_continuation"
      s" CMV61 ( -- ) 0 0 ?do unloop exit loop 0 drop"
      V-CERT VEC-ROW
   s" leave_keeps_a_normal_continuation"
      s" CMV62 ( -- ) 3 0 do leave loop 0 drop"
      V-CERT VEC-ROW
   s" leave_survives_a_dead_sibling_latch"
      s" CMV63 ( -- ) 3 0 do true if leave else unloop exit then loop 0 drop"
      V-CERT VEC-ROW
   s" do_with_a_throwing_body_stays_dead"
      s" CMV64 ( -- ) 3 0 do -99 throw loop 0 drop"
      V-REJECT VEC-ROW
   s" qdo_with_a_throwing_body_keeps_zero_trip"
      s" CMV65 ( -- ) 0 0 ?do -99 throw loop 0 drop"
      V-CERT VEC-ROW
   s" dead_plus_loop_does_not_consume_an_increment"
      s" CMV66 ( ptr u8 -- ptr u8 ) >r 3 0 do r> unloop exit +loop"
      V-CERT VEC-ROW ;

: BUILD-LOOP-BACKEDGE-VECTORS ( -- )
   s" dead_again_does_not_check_a_backedge"
      s" CMV67 ( -- ) 3 0 do begin unloop exit again loop"
      V-CERT VEC-ROW
   s" dead_repeat_restores_the_while_exit"
      s" CMV68 ( -- ) 3 0 do begin true while unloop exit repeat i drop loop"
      V-CERT VEC-ROW
   s" live_until_requires_entry_obligations"
      s" CMV69 ( -- ) 3 0 do begin unloop true until exit loop"
      V-REJECT VEC-ROW
   s" live_again_requires_entry_obligations"
      s" CMV70 ( -- ) 3 0 do begin unloop again loop"
      V-REJECT VEC-ROW
   s" live_repeat_requires_entry_obligations"
      s" CMV71 ( -- ) 3 0 do begin true while unloop repeat loop"
      V-REJECT VEC-ROW ;

: BUILD-LOOP-CASE-VECTORS ( -- )
   s" case_live_arms_join_obligations"
      s" CMV72 ( -- ) 3 0 do 1 case 1 of unloop endof endcase exit loop"
      V-REJECT VEC-ROW
   s" case_dead_arm_restores_entry_obligations"
      s" CMV73 ( -- ) 3 0 do 1 case 1 of unloop exit endof endcase i drop loop"
      V-CERT VEC-ROW
   s" case_preserves_discharge_before_of"
      s" CMV74 ( -- ) 3 0 do 3 0 do 1 case unloop 1 of endof endcase i drop unloop exit loop loop"
      V-CERT VEC-ROW ;

: BUILD-LOOP-MATCH-VECTORS ( -- )
   s" match_live_arms_join_obligations"
      s" CMV75 ( -- ) 3 0 do 0 construct cmres cmok MATCH cmres cmok OF drop unloop ENDOF cmerr OF drop  ENDOF ;MATCH exit loop"
      V-REJECT VEC-ROW
   s" match_dead_arm_restores_entry_obligations"
      s" CMV76 ( -- ) 3 0 do 0 construct cmres cmok MATCH cmres cmok OF drop unloop exit ENDOF cmerr OF drop  ENDOF ;MATCH i drop loop"
      V-CERT VEC-ROW
   s" match_both_arms_can_discharge"
      s" CMV77 ( -- ) 3 0 do 0 construct cmres cmok MATCH cmres cmok OF drop unloop ENDOF cmerr OF drop unloop ENDOF ;MATCH exit loop"
      V-CERT VEC-ROW
   s" match_both_dead_arms_have_no_latch"
      s" CMV78 ( -- ) 3 0 do 0 construct cmres cmok MATCH cmres cmok OF drop unloop exit ENDOF cmerr OF drop unloop exit ENDOF ;MATCH loop"
      V-CERT VEC-ROW ;

: BUILD-LOOP-RETURN-VECTORS ( -- )
   s" unloop_exit_after_taking_back_a_parked_cell"
      s" CMV79 ( n -- n ) >r 3 0 do r> unloop exit loop"
      V-CERT VEC-ROW
   s" unloop_exit_cannot_leave_a_parked_cell"
      s" CMV80 ( n -- ) >r 3 0 do unloop exit loop"
      V-REJECT VEC-ROW ;

: BUILD-VECTORS ( -- )
   s" straight_line"
      s" CMV1 ( i64 -- i64 ) CHECKER-MODEL-CASES:STEP1"
      V-CERT VEC-ROW
   s" arity_mismatch"
      s" CMV2 ( i64 -- i64 ) CHECKER-MODEL-CASES:DUP1"
      V-REJECT VEC-ROW
   s" branch_arms_join"
      s" CMV3 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1 else CHECKER-MODEL-CASES:STEP1 then"
      V-CERT VEC-ROW
   s" branch_arms_disagree"
      s" CMV4 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1 else CHECKER-MODEL-CASES:DROP1 CHECKER-MODEL-CASES:MK-CELL then"
      V-REJECT VEC-ROW
   s" loop_body_neutral"
      s" CMV5 ( i64 -- i64 ) begin CHECKER-MODEL-CASES:MK-BOOL until"
      V-CERT VEC-ROW
   s" loop_body_not_neutral"
      s" CMV6 ( i64 -- i64 ) begin CHECKER-MODEL-CASES:DUP1 CHECKER-MODEL-CASES:MK-BOOL until"
      V-REJECT VEC-ROW
   s" linear_kept_once"
      s" CMV7 ( cmltok -- cmltok ) CHECKER-MODEL-CASES:KEEP-POLY"
      V-CERT VEC-ROW
   s" linear_copied_after_use"
      s" CMV8 ( cmltok -- cmltok cmltok ) CHECKER-MODEL-CASES:DUP-POLY"
      V-REJECT VEC-ROW
   s" linear_dropped_after_use"
      s" CMV9 ( cmltok -- ) CHECKER-MODEL-CASES:DROP-POLY"
      V-REJECT VEC-ROW
   s" unclosed_frame"
      s" CMV10 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1"
      V-REJECT VEC-ROW
   BUILD-WIDENING-VECTORS
   BUILD-FRAME-CAP-VECTORS
   BUILD-MATCH-DEPTH-VECTORS
   BUILD-CONSTRUCT-VECTORS
   BUILD-SCRUTINEE-VECTORS
   BUILD-ATOM-VECTORS
   BUILD-DISCHARGE-VECTORS
   BUILD-LOOP-BRANCH-VECTORS
   BUILD-LOOP-QUOTATION-VECTORS
   BUILD-LOOP-REACHABILITY-VECTORS
   BUILD-LOOP-BACKEDGE-VECTORS
   BUILD-LOOP-CASE-VECTORS
   BUILD-LOOP-MATCH-VECTORS
   BUILD-LOOP-RETURN-VECTORS ;

0 POOL-U !  0 STR-N !  0 VEC-N !
BUILD-VECTORS

public

: VECTORS ( -- n )      VEC-N @ ;
: VEC-NAME$ ( n -- ptr u8 n )    VEC-NAME swap VEC-N @ COL@ STR$ ;
: VEC-SRC$ ( n -- ptr u8 n )     VEC-SRC swap VEC-N @ COL@ STR$ ;
: VEC-VERD@ ( n -- n )           VEC-VERD swap VEC-N @ COL@ ;

;package
