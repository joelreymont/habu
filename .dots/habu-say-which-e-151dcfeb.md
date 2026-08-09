---
title: Say which E-A64SEL-CALL cause can actually happen
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T23:10:26.144621+02:00"
---

lib/errors.f:1007's text lists five causes for -8551; measured 2026-08-09: only 'convention names no data-stack place' is reachable from production (NABI's FRAME-FOR is >=1 slot for any calling routine so select.f:838 cannot fire; CALLED-CK and CALL-LIVE hold two elaborator-produced derivations of one fact against each other - mutation guards with existing refusal tests at native-chain.f:1352 and native-select.f:2487). Rewrite the error text to lead with the reachable cause and mark the other four as internal consistency guards, so the next reader is not sent four wrong ways. Files: lib/errors.f. Verify: error-code-lint. Depends: none.
