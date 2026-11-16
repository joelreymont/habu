---
title: "Reject a supplied canon's answer no file name holds"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T16:36:31.137706+01:00"
---

Problem: src/core/include.f TRY-CANON checks only the length of the path a supplied canon (SOURCE-INPUT:USE or WITH) answers as found, not that it is canonical. An answer holding a NUL byte reaches PATHZ (src/core/path.f) as a bare E-PATH-NUL that names no word; an empty or relative one is taken as canonical, though REQUIRE-STORE keeps program files apart from the engine's by "a canonical name begins with `/`": a canon answering `src/core/include.f` makes a require of another file load nothing. Acceptance: a found answer that holds a NUL byte or does not begin with `/` is rejected with a record naming SOURCE-INPUT:CANON and the path, as EXISTING-PARENT names the canon; cases in test/source-root-test.f fail before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w225.
