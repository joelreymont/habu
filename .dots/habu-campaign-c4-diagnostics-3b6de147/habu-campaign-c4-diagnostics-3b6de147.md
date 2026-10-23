---
title: "Campaign C4: diagnostics that name the fix"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.636339+03:00"
---

Problem: several checker and engine paths die or crash on input a user can reach, where a located diagnostic belongs. The tools/diag-code.f header gives checker diagnostics a stable JSON contract; this campaign holds the observed failures that still escape it.

Acceptance: every child below is closed, each with the fixture that reproduces its failure.

Children (open): habu-make-the-arm64-fa89e081 habu-throw-on-generated-38b50740 habu-derive-eq-dies-36f33fa0 habu-escape-control-bytes-bf1bb9c6 habu-reject-direct-defer-c517b62d habu-the-engine-crashes-fdfe5e28 habu-var-i-shadows-f4435867. The EOF-inside-definition child closed in 4abaf315.

Removed from the campaign: the JSON contract for runtime failures (habu-extend-the-repair-68f9b2cf), the sweep that moves every code to one owner in src/lib/errors.f, a provoking test per code, and a gate that refuses a new code without span, class and fixture.

Absorbed on 2026-09-16: 60 dots closed with the reason 'superseded by habu-campaign-c4-diagnostics-3b6de147'; find their text with dot find.

Files: src/core/render.f, src/lib/errors.f, src/habu/habu2.f diagnostic emission, tools/diag-code.f (header). Verify: test/gate-diagnostics.f, test/run.f green. Depends: none. Ownership: checker and engine lanes. Claim: unassigned.
