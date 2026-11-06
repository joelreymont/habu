---
title: Write each FSM table call last in its arm
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T19:57:00.631257+02:00"
---

Problem: src/lib/fsm.f writes tokens after the calls a table names: an IN line as `dup <ACTION> drop construct <states> <next>` (ACTION+ :103, NEXT+ :110, LINE :118) and the run word's IN arm as `dup <NAME>/<state> false` (IN-ROW :130). After a call whose body never returns the checker refuses any further token (docs/forth.md 'A word that never returns ends its branch', E-DEAD-CODE), so a total table cannot load: an IN line whose ACTION always throws fails at 'drop' (/tmp/stuart-fsmgap/throw-row.f), and a state whose every line refuses makes its row word diverge and the run word fail at 'false' (/tmp/stuart-fsmgap/all-illegal.f). Asked by stuart for Tender's typestate rule builder (2026-11-06). Acceptance: every call the table names (ACTION with a >state NEXT, a NEXT word, a row word, a FINAL ACTION) is the last token of its generated arm; row words keep ( <ctx> -- <states> ); grammar unchanged; fsm-test.f loads a refused pair and an all-refused state and asserts each throws through the run word and the row word; docs/forth.md 'State machines' listing matches. Files: src/lib/fsm.f, src/lib/fsm-test.f, docs/forth.md. Verify: bin/hb --load test/run.f -- fsm model; both /tmp/stuart-fsmgap files rc 0. Depends: none. Ownership: src/lib/fsm.f, src/lib/fsm-test.f, docs/forth.md State machines. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fsm.
