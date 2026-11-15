---
title: "Pin the whole record in C4's weak pins"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.709989+01:00"
closed-at: "2026-11-16T04:07:35.141062+01:00"
---

Problem: using-test.f:82, 187, 191, 195 pin no help or packages note; tools/lsp-test-lib.f:1121, 2299, 2502, 6924, 6938 pin old texts (suite off); test/net/udp4.py:67, test/serial.py:81, test/serial-xmodem.py:427, test/xmodem.py:45 assert 'non-certified'; pins of bare code names by CONTAINS? (E-CURSOR-NESTED, E-SCHEMA-BAD, E-SEAL-VIOLATION, E-PATH-NUL). Acceptance: each pin checks the record C4 prints, so it fails with the defect back. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
