---
title: Decode every encoding the Encoding Standard names
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T11:11:53.517961+01:00"
---

Problem: Habu has no decoder for the WHATWG Encoding Standard's legacy encodings, so Zen decodes them through iconv, whose code pages differ from the Standard's indexes and differ again between glibc and Apple's libiconv (Zen dot zen-decode-windows-1252-d8347ef2; design /tmp/claude-1000/-home-joel-Work-zen/c6ed13c7-2b6e-4b93-9990-dc14a03984ab/tasks/a3fede2352ded5f6b.output, copied to /tmp/phil-lanes/encoding-design.md). Acceptance: package ENCODING decodes all 40 encodings in fatal mode exactly as the Standard's pinned index files say, shown by SUITE encoding-exhaustive over every pointer and label. Depends: none. Claim: agent=phil workspace=.jj-ws.noindex/phil-enc-data
