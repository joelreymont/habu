---
title: Place records of generated declarations
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:42.247549+01:00"
closed-at: "2026-11-16T04:07:34.952843+01:00"
---

Problem: a record whose token no source holds (a runtime string or built text: define-word's E-NOT-EFFECT for NOT-BUILT, WIDE-24, RV-BAD, interpreter.f:2125; E-INPUT-UNDERFLOW for BROKEN, render.f:1370; the ED-REPLAY and SD-REPLAY test strings) prints ' --> FILE' and '= at `tok`:' with no line (src/core/rejection.f PLACE :520-537, PLACE-PROSE :571). Acceptance: such a record is placed at the token being run with an '= word:' note, generated names included (checker.f USIG-CERT-ADMIT :14944 and CHECKER-DUP-DEFINITION :14862 reject at a generated name such as CHECKER-DEFSUFFIX-NAME's NAME-RESERVE); test/postpone-define.f and test/engine-suite.f :466-476, :2229 pin code and place. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
