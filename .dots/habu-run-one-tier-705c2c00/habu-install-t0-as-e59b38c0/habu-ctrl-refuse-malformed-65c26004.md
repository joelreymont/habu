---
title: "ctrl: refuse malformed control flow at the native tiers as the JIT does"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T19:22:21.333659+02:00"
---

Gap A: unchecked/TRUSTED malformed control flow ends uncaught (-9601/-8502/-8575 rc 67); SETTLE-UNIT ends a control fault as a refusal (ncomp line with token, rc 70), as for an undefined word; JIT texts not reproduced (first worker found they need a new bootstrap file and hook-report suppression). Brief /tmp/jerry-big/t0-ctrl2-brief.md (first: t0-ctrl-brief.md). Tier-1 -8575 split to habu-report-a-tier-aeefcaf4. Base mktzwnmu. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ctrl

Review fold-ins (brief /tmp/jerry-big/ctrlf-brief.md) done in wqssnonk "Fold the ctrl review into the native refusals" on yuxsxwrk, workspace jerry-ctrlf: MALFORMED? keys on the scan verdict (TRUSTED? or M-UNJUDGED not -1); three tier-1 sessions in native-hookless-reject.f (fail on the parent: labels 38-43); comment cuts; GE-KIND-TOP asserts stderr before rc. Rows ok on the candidate: native-hookless-reject, load-reject-diag-test, runtime-regression-test, prim-owner-scope; error-code-lint 0 findings. For the batched review: UNCERTIFIED? now means "no hook enforces the scan" while MALFORMED? uses the scan verdict (name mismatch); the inline `M-UNJUDGED @ -1 <>` wants a named word. Squash into yuxsxwrk at integration.
