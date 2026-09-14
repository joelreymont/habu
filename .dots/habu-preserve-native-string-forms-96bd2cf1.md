---
title: Preserve counted and printed string forms in the native tape
status: closed
priority: 1
issue-type: bug
created-at: "2026-09-14T16:40:00+03:00"
---

The positive AOT bundle fails compiling `MAIN` with `E-NELAB-ARITY` (-8303,
exit 67) on engine SHA256 `8c1b07555a940b6ecf0ea1a42566631ef68aaef1a1200ae23e2d747a8d1fd39c`.
Checked reproducer through `bin/hb --load`:

```forth
1 set-tier
package STRING-FORM-REPRO
: PRINTED ( -- ) ." hi" cr ;
;package
```

`CHECKER-TAPE:STRING` reports every consumed string body as `K-STRING`, so
`NFEED` records every form as `NTAPE:string-literal`. `NELAB:EMIT-STRING`
consequently pushes address and length even for `c"` and `."`; the checker
correctly expects one output or none. Preserve the form chosen by the checker
reader through the tape and lower its proper behavior. Keep decoded bytes and
source spans authoritative, and retain literal ownership for stripped images.
Acceptance: checked plain/escaped counted and printed forms, empty bodies,
counted-length boundary/refusal, and the real AOT bundle build/run on a rebuilt
native engine. No fixture relaxation or added TRUST.

Fixed by retaining separate reader/tape kinds, lowering counted bytes through
the existing owned interner, and treating printed forms as calls in memory,
local-liveness and tail-call classification. Counted length is checked before
copying. Native rebuild SHA256
`c3273f26a756a90b245dc5bcdbdd6b943af9feb6301361065470cb14f9a8d298`
passes `native-string-forms.f` (13 cases), `native-feed.f`, `native-tape.f`, and
`native-string.f`. The original AOT bundle builds and runs with its exact
25-byte expected output; its next failure is the unchanged stripped-text size
ceiling, a separate claim from literal compilation and execution.
