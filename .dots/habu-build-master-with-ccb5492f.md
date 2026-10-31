---
title: Build master with the public hb
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T22:24:58.858914+02:00"
---

Problem: master b7d767f5 does not build with the public hb (~/.local/lib/habu/bin/hb, installed 19:53): tools/native-build.f stops with 'native-build: uncaught throw code -8597' (E-NUNIT-PROFILE), rc 74, after 'support image built / files reused 0 / compiled 1 / definitions installed 0 / compiled 0'. Same with an empty HABU_OUT. Host /tmp/carl-ckr/eng/hb (20:52) builds master 39d57d87 rc 0. Suspect (unproven): facda3aa 'Admit a file's own mapped store when reusing it' (tools/native-unit-dependency.f H-POINTER), master's only native-unit change since 005393d9. Reported by jerry. Acceptance: root cause named with evidence (which BAD throws and why); if tree code is wrong, fixed at its layer and master builds with the public hb; if an older host cannot build by design, the evidence and the route in docs/bootstrap.md that covers it. Files: tools/native-unit-dependency.f, tools/native-definition-cache.f, tools/native-file-inputs.f (read). Verify: HOST=$HOME/.local/lib/habu/bin/hb; HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST TMPDIR=<tmp> $HOST --load tools/native-build.f -- .out/cand/hb </dev/null rc 0. Depends: none. Ownership: tools/native-*.f. Claim: agent=carl workspace=.jj-ws.noindex/carl-pubhb
