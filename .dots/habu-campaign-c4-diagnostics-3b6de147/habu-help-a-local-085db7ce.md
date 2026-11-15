---
title: "Help a local's annotation by the token it rejects"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:58.101675+01:00"
closed-at: "2026-11-16T04:07:35.922095+01:00"
---

Problem: re-review 7a of wluynmuy. (1) src/core/checker.f:11313-11318 SGBAD-UNKNOWN-FIX prefixes `l:` to a ptr's example whenever lu>0, so {: p:option<ptr> :} and {: p:^ptr :} get 'as in `p:^u8`', which replaces the whole annotation; its comment is false. (2) {: p: :} gives 'unknown type ``', placed as an empty span: LOCAL-TYPE (checker.f:7772) gives u=0 the unknown verdict with SGBAD-U 0. (3) render.f:1569 says 'bad signature at `--` in the annotation of local `p`', and SGBAD-FIX's binders arm (checker.f:11334) says 'this signature' for a local. (4) Tests: SN-NU and SN-AR (test/signature-nesting.f:408,415) pin the caret's width, not its column; the local-depth case (:224) pins only a prefix. Acceptance: each case's help fixes that case, an empty annotation gets SN-EN's record, no local's record says signature, and the tests pin column and whole text. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
