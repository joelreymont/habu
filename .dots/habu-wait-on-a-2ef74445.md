---
title: Wait on a descriptor until a second descriptor or a deadline ends the wait
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:42:59.667553+02:00"
---

Problem (Maki, tim 2026-11-11, .out/tim-arena-arrays.md section 2): maki src/kernel.f MORE waits for the kernel worker's reply with PROCESS:POLL-IN, which takes one descriptor, so a Cancel during a 300 s exchange waits the reply out. PROCESS:PFD-AT!, PFD-REVENTS and POLL-RESTART (src/lib/process.f:290, :296, :329) could compose a two-slot poll in Maki; AIO:POLL/GROUP+/AWAIT-ANY need AIO's loop task; neither is a named word for this. Needed: wait until the first descriptor is readable, the second is readable or the time is up, and say which (tim's plan called it PROCESS:POLL-IN-OR-CANCEL ( fd fd ms -- count ); the name is Joel's). Gates Maki landing 4 (the service enters sessions). Acceptance: one public word in PROCESS, a test that ends a wait by the second descriptor and by the deadline. Files: src/lib/process.f, its test. Verify: the test rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.

## Design (lead, 2026-11-11, read src/lib/process.f:285-350, :655-682)

One public word in PROCESS beside POLL-IN-OR-TIMEOUT: `( fd fd ms -- count )`, the second descriptor the cancel. It arms slots 0 and 1 (PFD-AT!), waits with POLL-RESTART to the deadline as POLL-IN-OR-TIMEOUT does, and reports as a capture does: a negative rc is E-PROC-OUTPUT, the deadline E-PROC-TIMEOUT, the cancel descriptor readable or hung up E-PROC-CANCELLED (CANCEL-HEARD :660-663 is the rule), else the count. No new error code; "which one ended it" is the throw code. The capture's own cancel (CMD:CANCEL-FD!, slot 4) is unchanged: it belongs to a command, and a bare wait has none. Caller: maki src/kernel.f:469 MORE and :665 ENDED switch to it when the service holds the worker (Maki landing 4; the cancel descriptor is the context's, project.f:538). Open with Joel: the word's name (tim's plan: POLL-IN-OR-CANCEL).
