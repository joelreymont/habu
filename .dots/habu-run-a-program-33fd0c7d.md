---
title: Run a program file without reading stdin
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.710515+02:00"
---

Problem: src/habu/main.f:270-288 ROUTED reads all of a non-TTY stdin before a plain 'hb prog.f' route, and runs stdin's text instead of prog.f when it is non-empty. So 'hb tools/check.f x.f' with an open stdin pipe blocks at 0 CPU until EOF (measured: 'sleep 15 | timeout 10 hb tools/check.f ok.f' times out; '</dev/null' runs in 4 s; 'sleep 12 | hb bye.f' also blocks), and 'printf ".( from-stdin)" | hb ok.f' runs the piped text, not ok.f. Acceptance: a program file route never reads stdin before the program runs, so the program owns its stdin (tools/check.f's stdin mode keeps working); habu2.f's assembly route agrees; a regression row with an open pipe. Files: src/habu/main.f, src/habu/habu2.f, the cold-argv tests. Verify: the two probes. Depends: none. Ownership: engine command line. Claim: unassigned.
