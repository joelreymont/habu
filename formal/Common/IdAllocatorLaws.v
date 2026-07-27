From Stdlib Require Import Bool ZArith List Lia Sorting.Sorted.
From Habu.Common Require Import Ids IdAllocator.

Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.

Definition event_serial (e : event) : option Z :=
  match e with
  | Tried _ (Issued serial) => Some serial
  | _ => None
  end.

Fixpoint issued_serials (es : list event) : list Z :=
  match es with
  | [] => []
  | e :: rest =>
      match event_serial e with
      | Some serial => serial :: issued_serials rest
      | None => issued_serials rest
      end
  end.

Lemma serial_ok_spec :
  forall serial,
    serial_ok serial = true <->
    Ids.serial_min <= serial <= Ids.serial_max.
Proof.
  intros serial.
  unfold serial_ok, Ids.serial_validb, Ids.betweenb.
  rewrite andb_true_iff, !Z.leb_le.
  reflexivity.
Qed.

Lemma next_serial_some :
  forall seen fresh,
    next_serial seen = Some fresh ->
    fresh = seen + 1 /\
    Ids.serial_min <= fresh <= Ids.serial_max.
Proof.
  intros seen fresh Hnext.
  unfold next_serial in Hnext.
  destruct ((0 <=? seen) && (seen <? Ids.serial_max)) eqn:Hguard;
    try discriminate.
  inversion Hnext; subst fresh.
  apply andb_true_iff in Hguard as [Hnonneg Hbelow].
  apply Z.leb_le in Hnonneg.
  apply Z.ltb_lt in Hbelow.
  unfold Ids.serial_min.
  lia.
Qed.

Lemma pure_attempt_transition :
  forall c seen c' out,
    pure_attempt c seen = (c', out) ->
    match out with
    | Issued serial =>
        cur c < serial /\
        cur c' = serial /\
        serial_ok serial = true
    | Retry want actual =>
        c' = c /\
        want = seen /\
        actual = cur c /\
        seen <> cur c
    | Exhausted stopped =>
        c' = c /\
        stopped = seen /\
        next_serial seen = None
    end.
Proof.
  intros c seen c' out Hattempt.
  unfold pure_attempt, attempt in Hattempt.
  destruct (next_serial seen) as [fresh |] eqn:Hnext.
  - unfold cas in Hattempt.
    destruct (Z.eqb (cur c) seen) eqn:Hsame.
    + inversion Hattempt; subst c' out.
      apply Z.eqb_eq in Hsame.
      pose proof (next_serial_some seen fresh Hnext)
        as [Hfresh Hbounds].
      simpl.
      split; [lia |].
      split; [reflexivity |].
      apply (proj2 (serial_ok_spec fresh)).
      exact Hbounds.
    + inversion Hattempt; subst c' out.
      apply Z.eqb_neq in Hsame.
      repeat split; auto.
  - inversion Hattempt; subst c' out.
    repeat split; auto.
Qed.

Theorem pure_attempt_success_strict :
  forall c seen c' serial,
    pure_attempt c seen = (c', Issued serial) ->
    cur c < serial /\ cur c' = serial.
Proof.
  intros c seen c' serial Hattempt.
  pose proof
    (pure_attempt_transition c seen c' (Issued serial) Hattempt)
    as Htransition.
  simpl in Htransition.
  tauto.
Qed.

Theorem pure_attempt_issued_in_domain :
  forall c seen c' serial,
    pure_attempt c seen = (c', Issued serial) ->
    serial <> 0 /\ serial_ok serial = true.
Proof.
  intros c seen c' serial Hattempt.
  pose proof
    (pure_attempt_transition c seen c' (Issued serial) Hattempt)
    as Htransition.
  simpl in Htransition.
  destruct Htransition as [_ [_ Hvalid]].
  apply serial_ok_spec in Hvalid.
  unfold Ids.serial_min in Hvalid.
  split; [lia |].
  apply (proj2 (serial_ok_spec serial)).
  exact Hvalid.
Qed.

Theorem pure_attempt_retry_preserved :
  forall c seen c' want actual,
    pure_attempt c seen = (c', Retry want actual) ->
    c' = c /\
    want = seen /\
    actual = cur c /\
    seen <> cur c.
Proof.
  intros c seen c' want actual Hattempt.
  simpl.
  exact
    (pure_attempt_transition c seen c' (Retry want actual) Hattempt).
Qed.

Theorem pure_attempt_exhausted_unchanged :
  forall c seen,
    next_serial seen = None ->
    pure_attempt c seen = (c, Exhausted seen).
Proof.
  intros c seen Hnext.
  unfold pure_attempt, attempt.
  rewrite Hnext.
  reflexivity.
Qed.

Theorem exhaustion_before_addition :
  next_serial Ids.serial_max = None.
Proof. cbn; reflexivity. Qed.

Theorem exhaustion_preserves_max_state :
  forall slot_id,
    pure_attempt
      (Cell slot_id Ids.serial_max)
      Ids.serial_max =
    (Cell slot_id Ids.serial_max, Exhausted Ids.serial_max).
Proof.
  intros slot_id.
  apply pure_attempt_exhausted_unchanged.
  exact exhaustion_before_addition.
Qed.

Lemma pure_step_transition :
  forall w a w' e,
    pure_step w a = (w', e) ->
    (event_serial e = None /\
     cur (shared w') = cur (shared w)) \/
    (exists serial,
      event_serial e = Some serial /\
      cur (shared w) < serial /\
      cur (shared w') = serial /\
      serial_ok serial = true).
Proof.
  intros w a w' e Hstep.
  unfold pure_step, step in Hstep.
  destruct a as [t | t].
  - inversion Hstep; subst w' e.
    left.
    split; reflexivity.
  - destruct (view_get t (observed w)) as [seen |] eqn:Hview.
    + fold pure_attempt in Hstep.
      destruct (pure_attempt (shared w) seen) as [c' out]
        eqn:Hattempt.
      inversion Hstep; subst.
      pose proof
        (pure_attempt_transition (shared w) seen c' out Hattempt)
        as Htransition.
      destruct out as [serial | want actual | stopped].
      * right.
        exists serial.
        simpl in Htransition |- *.
        split; [reflexivity |].
        exact Htransition.
      * left.
        simpl in Htransition |- *.
        destruct Htransition as [Hcell _].
        subst c'.
        split; reflexivity.
      * left.
        simpl in Htransition |- *.
        destruct Htransition as [Hcell _].
        subst c'.
        split; reflexivity.
    + inversion Hstep; subst w' e.
      left.
      split; reflexivity.
Qed.

Theorem pure_step_success_strict :
  forall w a w' e serial,
    pure_step w a = (w', e) ->
    event_serial e = Some serial ->
    cur (shared w) < serial /\
    cur (shared w') = serial /\
    serial_ok serial = true.
Proof.
  intros w a w' e serial Hstep Hserial.
  pose proof (pure_step_transition w a w' e Hstep) as Htransition.
  destruct Htransition as [[Hnone _] | [issued [Hsome Hfacts]]].
  - rewrite Hserial in Hnone.
    discriminate.
  - rewrite Hserial in Hsome.
    inversion Hsome; subst issued.
    exact Hfacts.
Qed.

Definition trace_laws (start : Z) (es : list event) : Prop :=
  StronglySorted Z.lt (issued_serials es) /\
  Forall (fun serial => start < serial) (issued_serials es) /\
  Forall (fun serial => serial_ok serial = true) (issued_serials es).

Theorem pure_run_trace_laws :
  forall w actions w' es,
    pure_run w actions = (w', es) ->
    trace_laws (cur (shared w)) es.
Proof.
  intros w actions.
  induction actions as [|a rest IH] in w |- *.
  - intros w' es Hrun.
    inversion Hrun; subst w' es.
    split.
    + constructor.
    + split; constructor.
  - intros w' es Hrun.
    unfold pure_run in Hrun.
    cbn [run] in Hrun.
    fold pure_step in Hrun.
    destruct (pure_step w a) as [w1 e] eqn:Hstep.
    destruct (run cas w1 rest) as [w2 tail] eqn:Htail.
    inversion Hrun; subst.
    unfold pure_run in IH.
    pose proof (IH w1 _ _ Htail) as IHtail.
    unfold trace_laws in IHtail |- *.
    destruct IHtail as [Hsorted [Hafter Hvalid]].
    pose proof (pure_step_transition w a w1 e Hstep)
      as Htransition.
    destruct Htransition as [[Hnone Hsame] |
                             [serial [Hsome [Hlt [Hcur Hserial]]]]].
    + cbn [issued_serials].
      rewrite Hnone.
      split.
      * exact Hsorted.
      * split.
        -- eapply Forall_impl; [| exact Hafter].
           intros n Hn.
           rewrite Hsame in Hn.
           exact Hn.
        -- exact Hvalid.
    + cbn [issued_serials].
      rewrite Hsome.
      split.
      * constructor.
        -- exact Hsorted.
        -- eapply Forall_impl; [| exact Hafter].
           intros n Hn.
           rewrite Hcur in Hn.
           exact Hn.
      * split.
        -- constructor.
           ++ exact Hlt.
           ++ eapply Forall_impl; [| exact Hafter].
              intros n Hn.
              rewrite Hcur in Hn.
              lia.
        -- constructor; assumption.
Qed.

Lemma strictly_sorted_no_duplicates :
  forall serials,
    StronglySorted Z.lt serials ->
    NoDup serials.
Proof.
  intros serials Hsorted.
  induction Hsorted as [|serial rest Hsorted IH Hafter].
  - constructor.
  - constructor.
    + intros Hin.
      apply Forall_forall with (x := serial) in Hafter.
      * lia.
      * exact Hin.
    + exact IH.
Qed.

Theorem pure_run_issues_strictly_increase :
  forall w actions w' es,
    pure_run w actions = (w', es) ->
    StronglySorted Z.lt (issued_serials es).
Proof.
  intros w actions w' es Hrun.
  pose proof (pure_run_trace_laws w actions w' es Hrun)
    as [Hsorted _].
  exact Hsorted.
Qed.

Theorem pure_run_every_issue_in_domain :
  forall w actions w' es,
    pure_run w actions = (w', es) ->
    Forall
      (fun serial => serial <> 0 /\ serial_ok serial = true)
      (issued_serials es).
Proof.
  intros w actions w' es Hrun.
  pose proof (pure_run_trace_laws w actions w' es Hrun)
    as [_ [_ Hvalid]].
  eapply Forall_impl; [| exact Hvalid].
  intros serial Hok.
  apply serial_ok_spec in Hok.
  unfold Ids.serial_min in Hok.
  split; [lia |].
  apply (proj2 (serial_ok_spec serial)).
  exact Hok.
Qed.

Theorem pure_run_unique :
  forall w actions w' es,
    pure_run w actions = (w', es) ->
    NoDup (issued_serials es).
Proof.
  intros w actions w' es Hrun.
  apply strictly_sorted_no_duplicates.
  eapply pure_run_issues_strictly_increase.
  exact Hrun.
Qed.

(* Host refinements use only the named atomic-CAS boundary law. *)

Theorem host_attempt_refines :
  forall c seen,
    host_attempt c seen = pure_attempt c seen.
Proof.
  intros c seen.
  unfold host_attempt, pure_attempt, attempt.
  destruct (next_serial seen) as [fresh |].
  - rewrite atomic_cas_linearizable.
    reflexivity.
  - reflexivity.
Qed.

Theorem host_step_refines :
  forall w a,
    host_step w a = pure_step w a.
Proof.
  intros w a.
  unfold host_step, pure_step, step.
  destruct a as [t | t].
  - reflexivity.
  - destruct (view_get t (observed w)) as [seen |].
    + rewrite host_attempt_refines.
      reflexivity.
    + reflexivity.
Qed.

Theorem host_run_refines :
  forall w actions,
    host_run w actions = pure_run w actions.
Proof.
  intros w actions.
  induction actions as [|a rest IH] in w |- *.
  - reflexivity.
  - change
      ((let '(w1, e) := host_step w a in
       let '(w2, es) := host_run w1 rest in
       (w2, e :: es)) =
      (let '(w1, e) := pure_step w a in
       let '(w2, es) := pure_run w1 rest in
       (w2, e :: es))).
    rewrite host_step_refines.
    destruct (pure_step w a) as [w1 e].
    rewrite IH.
    reflexivity.
Qed.

Theorem host_attempt_success_strict :
  forall c seen c' serial,
    host_attempt c seen = (c', Issued serial) ->
    cur c < serial /\
    cur c' = serial /\
    serial <> 0 /\
    serial_ok serial = true.
Proof.
  intros c seen c' serial Hattempt.
  rewrite host_attempt_refines in Hattempt.
  pose proof
    (pure_attempt_transition c seen c' (Issued serial) Hattempt)
    as [Hlt [Hcur Hvalid]].
  repeat split; try assumption.
  apply serial_ok_spec in Hvalid.
  unfold Ids.serial_min in Hvalid.
  lia.
Qed.

Theorem host_attempt_retry_preserved :
  forall c seen c' want actual,
    host_attempt c seen = (c', Retry want actual) ->
    c' = c /\
    want = seen /\
    actual = cur c /\
    seen <> cur c.
Proof.
  intros c seen c' want actual Hattempt.
  rewrite host_attempt_refines in Hattempt.
  exact
    (pure_attempt_retry_preserved c seen c' want actual Hattempt).
Qed.

Theorem host_step_success_strict :
  forall w a w' e serial,
    host_step w a = (w', e) ->
    event_serial e = Some serial ->
    cur (shared w) < serial /\
    cur (shared w') = serial /\
    serial_ok serial = true.
Proof.
  intros w a w' e serial Hstep Hserial.
  rewrite host_step_refines in Hstep.
  exact
    (pure_step_success_strict w a w' e serial Hstep Hserial).
Qed.

Theorem host_run_unique :
  forall w actions w' es,
    host_run w actions = (w', es) ->
    NoDup (issued_serials es).
Proof.
  intros w actions w' es Hrun.
  rewrite host_run_refines in Hrun.
  exact (pure_run_unique w actions w' es Hrun).
Qed.

Theorem host_run_every_issue_in_domain :
  forall w actions w' es,
    host_run w actions = (w', es) ->
    Forall
      (fun serial => serial <> 0 /\ serial_ok serial = true)
      (issued_serials es).
Proof.
  intros w actions w' es Hrun.
  rewrite host_run_refines in Hrun.
  exact (pure_run_every_issue_in_domain w actions w' es Hrun).
Qed.

Theorem host_exhaustion_preserves_max_state :
  forall slot_id,
    host_attempt
      (Cell slot_id Ids.serial_max)
      Ids.serial_max =
    (Cell slot_id Ids.serial_max, Exhausted Ids.serial_max).
Proof.
  intros slot_id.
  rewrite host_attempt_refines.
  apply exhaustion_preserves_max_state.
Qed.

(* Executable counterexamples for weakened allocator laws. *)

Definition next_serial_weak_domain (seen : Z) : option Z :=
  if (-1 <=? seen) && (seen <? Ids.serial_max) then
    Some (seen + 1)
  else
    None.

Example weakened_domain_mutation_counterexample :
  next_serial_weak_domain (-1) = Some 0 /\
  serial_ok 0 = false.
Proof. vm_compute. split; reflexivity. Qed.

Definition cas_stuck_success
    (c : cell) (want fresh : Z) : cell * cas_out :=
  if Z.eqb (cur c) want then
    (c, CasOk want fresh)
  else
    (c, CasMiss want (cur c)).

Definition stuck_success_attempt : cell -> Z -> cell * try_out :=
  attempt cas_stuck_success.

Example success_transition_mutation_counterexample :
  stuck_success_attempt (Cell 3 0) 0 =
    (Cell 3 0, Issued 1) /\
  cur (fst (stuck_success_attempt (Cell 3 0) 0)) = 0.
Proof. vm_compute. split; reflexivity. Qed.

Definition cas_loses_retry
    (c : cell) (want fresh : Z) : cell * cas_out :=
  if Z.eqb (cur c) want then
    (set_cur c fresh, CasOk want fresh)
  else
    (c, CasMiss want want).

Definition lost_retry_attempt : cell -> Z -> cell * try_out :=
  attempt cas_loses_retry.

Example retry_preservation_mutation_counterexample :
  pure_attempt (Cell 3 1) 0 =
    (Cell 3 1, Retry 0 1) /\
  lost_retry_attempt (Cell 3 1) 0 =
    (Cell 3 1, Retry 0 0).
Proof. vm_compute. split; reflexivity. Qed.

Definition next_serial_wrap (seen : Z) : option Z :=
  Some (Z.modulo (seen + 1) (Ids.serial_max + 1)).

Definition wrap_attempt (c : cell) (seen : Z) : cell * try_out :=
  match next_serial_wrap seen with
  | None => (c, Exhausted seen)
  | Some fresh =>
      let '(c', out) := cas c seen fresh in
      match out with
      | CasOk _ _ => (c', Issued fresh)
      | CasMiss want actual => (c', Retry want actual)
      end
  end.

Example exhaustion_guard_mutation_counterexample :
  wrap_attempt
    (Cell 3 Ids.serial_max)
    Ids.serial_max =
  (Cell 3 0, Issued 0).
Proof. vm_compute; reflexivity. Qed.

Print Assumptions pure_run_unique.
Print Assumptions pure_run_every_issue_in_domain.
Print Assumptions exhaustion_preserves_max_state.
Print Assumptions host_attempt_refines.
Print Assumptions host_step_refines.
Print Assumptions host_run_refines.
Print Assumptions host_attempt_success_strict.
Print Assumptions host_step_success_strict.
Print Assumptions host_run_unique.
Print Assumptions host_exhaustion_preserves_max_state.
