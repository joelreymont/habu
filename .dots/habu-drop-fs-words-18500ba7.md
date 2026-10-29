---
title: Drop FS words only tests reach
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:53:13.964701+02:00"
---

FS packaging review (rxrusvvk): private ENOTDIR, ELOOP, ENAMETOOLONG (fs.f:113-117), READ-LINK (fs.f:554), RENAME-NOREPLACE with LINK-NEW? (fs-mutate.f:246) and COPY-FILE (fs-mutate.f:412) have no production caller and are reached only by test forwarders; fs-anchored.f's public OPEN-DIR, OPEN-DIR-AT, READ-FILE-AT and CLOSE(-DIR) have only test callers in habu and the clients. Decide per word: production need, else delete with its tests. After the FS lane lands.
