---
title: Answer whether the key light sees each box
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:18:27.308628+01:00"
---

Problem: the browser host's event 19 (src/lib/browser/turn.js, viewport.js occlude) says only whether a box is behind the last draw's depth, so Maki's view loads bodies whose shadow cannot show. Asked by tim for Maki; request in /tmp/tim-habu-key-sight.md. Acceptance: turn protocol 6; each byte of event 19 is bits, 1 as today and 2 when the last draw drew a shadow and some of the box is not behind its shadow map; test/browser/display.f and its peer show a box behind the caster from the key without bit 2 and one beside it with it; tim is told the commit before the install. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-keysight.
