---
title: Drop FS words only tests reach
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:53:13.964701+02:00"
---

FS packaging review (rxrusvvk): private ENOTDIR and ELOOP (fs.f:75-77) have no caller, and the public ENAMETOOLONG (fs.f:71-72) has only test callers; fs-anchored.f's public OPEN-DIR, OPEN-DIR-AT, READ-FILE-AT and CLOSE(-DIR) have only test callers in habu and the clients. Decide per word: production need, else delete with its tests. After the FS lane lands.
