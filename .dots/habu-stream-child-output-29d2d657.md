---
title: Stream child output to the caller
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:22:57.484167+03:00"
---

After the 2026-10-27 demo (Joel 2026-10-23). Child-process capture picks sizes for its callers: each command context reserves IN-CAP 128 KiB, OUT-CAP 32 KiB and ERR-CAP 32 KiB (src/lib/process-command.f:77-84, no recorded basis) and refuses output past OUT-CAP. The only large-output path, CMD:RUN-STDIN-GROWING-OUTCOME (:419), takes a sink quotation [ n -- ptr u8 ] that carries no caller state, so tools/check-verify-core.f:1698-1710 hard-codes one quotation per lane. Adopted direction: a pull-based child stream (start, read stdout in caller-owned chunks in the caller's loop, stop early ending the child, outcome; deadline across the run; stderr drained; stdin from a caller span). 'Collect up to a caller limit' becomes a helper on the stream; the fixed spans and the growing sink retire, and every caller in habu, ../zen and ../tender migrates. First consumer: Zen's PDF paint scan (pdftocairo -svg to stdout, scanned in 64 KiB chunks, stopped at the first drawing; Zen lane A in zen .jj-ws.noindex/kevin-paint waits on this). Draft design by Codex Astra: ~/.cache/tmp/kevin/habu-stream/design.md (brief prompt.md beside it); review it against the code before lanes.
