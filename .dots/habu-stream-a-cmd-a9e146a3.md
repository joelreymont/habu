---
title: "Stream a command's stdout lines under an idle window"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:29:11.593651+02:00"
---

Maki design r2 (Joel approved 2026-10-31; /home/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/design-chatgpt-r2.md §4.5, §5 item 2, lane H3): CMD:RUN-STREAM ( CMD:command ptr u8 n ms [ ptr u8 n -- ] -- CMD:command outcome ), as RUN-OUTCOME (src/lib/process-command.f:500) but ms is an idle window restarted by each stdout byte, each complete stdout line goes to the quotation as it arrives, stderr captured as today, cancel as today, timeout means the window passed in silence and the tree is killed. Caller: Maki's job runner (STALL-MS 300000, Joel's number). Files: src/lib/process.f poll loop (~641-700), process-command.f, process-command-test.f (lines 1 s apart under a 3 s window complete; silence past the window is timeout with the lines already delivered; cancel mid-run). Joel chose the name CMD:RUN-STREAM (2026-10-31, DECISIONS.md). Ownership: carl. Claim: carl.

Claimed by carl 2026-11-01: workspace .jj-ws.noindex/carl-stream (brief: process.f stream loop with the idle window and line delivery; RUN-STREAM shares RUN-CAPTURE; stdout not kept, OUT$ holds only an unfinished line).

Redesign 2026-11-01: the callback lane (carl-stream wlymvwuu 83a00952) was rejected in review (a callback that starts another run on the same task corrupts the outer run: wrong exit status, lost lines, an unreaped child and leaked pipes; the idle window counts the callback's own time as silence). Joel chose "Pull: caller reads lines (Recommended)" (DECISIONS 2026-11-01): the caller starts the run, reads the next stdout line waiting at most the idle window, and ends the run; the linear CMD:command makes the checker require the end; each run keeps its own process state instead of the task's row, so a caller may run other commands between reads. New public names go to Joel before the lane. The rejected commit stays in carl-stream for its loop and tests.

Order 2026-11-02 (lead): the pull design passes the capture row to about 80 process.f words, which are public only because process.f has no package. Scoping process.f's capture words in a package first (habu-scope-process-f-bb981ba2) makes that change private, so Joel is asked once, with that package's public words and the stream words (CMD:child, CMD:read, START, READ-LINE, END) together, instead of approving 80 changed globals that the scope lane then hides. Claim: unassigned; carl-stream (rejected wlymvwuu) stays for its loop and tests until the pull lane reuses them.

2026-11-03: Maki r2 M2b's exact caller for H3 (tim): /home/joel/Work/maki/.jj-ws.noindex/tim/.out/r2/h3-caller.md. No total deadline; each read takes JOB:STALL-MS (300000 in service, 5000 in the stall test); time between reads (a PG UPDATE and pg_notify) is not silence; a read gives a line, end of stdout or a timeout; after a timeout, cancel or throw the end kills and reaps the whole tree; after end of stdout it answers PROC-OUTCOME>RC with CMD:ERR$; two runs live at once on two tasks. The file ends with H3's end-to-end rows and Maki's M2b row.

Workspace carl-stream retired 2026-11-04. The rejected callback commit stays in the repository: change wlymvwuu, commit 83a00952 "Stream a command's stdout lines" (`jj log -r wlymvwuu`).


Merged from habu-stream-child-output-29d2d657 (Stream child output to the caller), 2026-11-10:

After the 2026-10-27 demo (Joel 2026-10-23). Child-process capture picks sizes for its callers: each command context reserves IN-CAP 128 KiB, OUT-CAP 32 KiB and ERR-CAP 32 KiB (src/lib/process-command.f:77-84, no recorded basis) and refuses output past OUT-CAP. The only large-output path, CMD:RUN-STDIN-GROWING-OUTCOME (:419), takes a sink quotation [ n -- ptr u8 ] that carries no caller state, so tools/check-verify-core.f:1698-1710 hard-codes one quotation per lane. Adopted direction: a pull-based child stream (start, read stdout in caller-owned chunks in the caller's loop, stop early ending the child, outcome; deadline across the run; stderr drained; stdin from a caller span). 'Collect up to a caller limit' becomes a helper on the stream; the fixed spans and the growing sink retire, and every caller in habu, ../zen and ../tender migrates. First consumer: Zen's PDF paint scan (pdftocairo -svg to stdout, scanned in 64 KiB chunks, stopped at the first drawing; Zen lane A in zen .jj-ws.noindex/kevin-paint waits on this).
