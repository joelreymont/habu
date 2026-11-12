---
title: A capture whose spawn throws leaves its pipes open
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T15:18:07.765938+02:00"
---

Problem: the capture spawn paths (src/lib/process.f SPAWN-CAPTURE :1085; process-env.f PROC-SPAWN-ARGV-ENV-CAPTURE and -STDIN-CAPTURE :290-307; process-cwd.f PROC-SPAWN-ARGV-ENV-CWD-CAPTURE and -STDIN-CAPTURE :59-75; process-argv.f PROC-SPAWN-ARGV-CAPTURE and -STDIN-CAPTURE :116-128; test/gate-common-lib.f GE-EVAL-FORK-SPAWN :412) send only a negative pid through PROCESS:THROW-CAPTURE. PROC-RAW:ANSWER (process.f :173-176) rethrows every other code, so such a throw out of the spawn skips END-CAPTURE and the pipes CAPTURE-BEGIN opened stay open, and it skips the path's staging reset. process-command.f SPAWN :438-442 already ends the capture on any throw. Acceptance: every capture spawn ends the capture with the thrown code and resets its staging on any throw out of the spawn, through one word; a test makes a capture's spawn throw a code other than E-PROC-SPAWN and shows the descriptors closed and the next capture running. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-capture.
