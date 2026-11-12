---
title: Constructor checks see every linked family
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:57:44.131915+02:00"
---

Problem: src/core/type-family.f TFAM-CTOR-PKG? (:1036), TFAM-CTOR-WORD? (:1311) and TFAM-CTOR-EXTEND? (:1327) answer from the family rows read so far; a family whose type entry a link left pending (src/habu/linker.f LINK-TYPE :4313 sets ENTRIES-PENDING, type-family.f :4704) is unread until READ-PENDING (:5118) or a missed lookup. So a warm build accepts `package SPAN-SPAN ;package` and `undefine SPAN-SPAN:MAKE` that a cold build refuses with 7111 E-CTOR-PROTECTED (/tmp/hunt-rewind/repro-uncaught.f, w4.f), and hb accepts them until something names the family (p1.f, p2.f). Acceptance: every constructor and generated-word check answers from every family, read or pending; a warm build refuses what the cold one does, tested. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/ctor-pending.
