---
title: "Make checker.f's public words private"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T03:12:08.987510+01:00"
---

Problem: once checker.f's top level is private to package CHECK, its public words are still names a program can call: 106 in its inner packages (CHECKER-TAPE 35, CHECKER-OVERLAY 24, CHECKER-RESOLVE 17, CHECKER-EFFECT-AUTHORITY 9 with PUBLISH, CHECKER-QUOT-FRESH 7, CHECKER-REG, CHECKER-ITEMS, SRCLOC, CHECKER-USE, CHECKER-RESYNC, CHECKER-LBUF, CHECKER-PREFLIGHT) and 34 in CHECK's public sections, among them the store writers FACTS!, LAY-FACTS, HOLD-RETIRED and UNDEFINE; nearly all are called only by engine packages (LINKER, TFAM, XREF, INTERPRETER, NPUB). src/habu/xref.f's undefine lines retire some by name. Acceptance: every such word is private to its package except MULTI-ERR-BEGIN, MULTI-ERR-END, ESC-DECODE and the words tools and tests call; an engine caller takes one by EXPORT; the xref.f undefine lines naming them are deleted.
Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.
