---
title: Keep earlier records when --all-errors meets a duplicate
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T11:11:45.300369+02:00"
---

Problem (review 285): under tools/check.f --all-errors a source with a refused definition and then a duplicate definition prints only the duplicate record; the earlier refusal's record is dropped ($HOME/.cache/tmp/kestrel-r4-rev285/dup/badthendup.f; parent identical). tools/check-all-errors-core.f CA-RUN-DEFS `rc DUP-RC = IF CA-HANDLE-DUP exit THEN` (~:502) exits before CA-EMIT-CAPTURED. Acceptance: --all-errors reports every refusal found before the duplicate, then the duplicate record, rc 78, in JSON and prose; a case through tools/check-test-lib.f seen to fail first. Files: tools/check-all-errors-core.f, tools/check-test-lib.f. Base: after 773eff2a (94ae4b9c) lands.
