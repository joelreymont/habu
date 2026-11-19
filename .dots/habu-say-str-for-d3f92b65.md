---
title: Say str for every public byte input
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:01:17.720639+01:00"
blocks:
  - habu-buf-buf-a-48c31f14
---

Problem: Joel 2026-11-19: "no agent should be using "^u8 n" anymore! it's either str or vector". Public words still read bytes as ^u8 n or ^u8 NUM:byte-len, e.g. CMD:ARG+ and ENV+ (src/lib/process-command.f), XMODEM:ENCODE, DECODE and SEND (src/lib/xmodem.f), REJECTION-TEXT:FLAT and HAS? (src/lib/test/rejection-text.f), LOG:TEXT+ and LINE (src/lib/log.f). FUNCTION: inputs are dot habu-let-fn-take-40fcee0e. Acceptance: no public effect in src or tools reads bytes as ^u8 n or ^u8 NUM:byte-len; each consumer's callers move in the same install. Depends: habu-buf-buf-a-48c31f14. Claim: unassigned.
