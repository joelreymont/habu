---
title: "Say str in xmodem's byte-len rows"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:38:48.293838+01:00"
closed-at: "2026-11-23T10:11:21.557472+01:00"
---

Problem: XMODEM:DECODE (src/lib/xmodem.f:145) takes and answers ^u8 NUM:byte-len, fed by SERIAL-XMODEM READ-FRAME (serial-xmodem.f:265) and feeding ACCEPT-PAYLOAD (:279); SEND :347, RECEIVE :358 and the transfer result's completed field also say NUM:byte-len, so DECODE cannot say str alone. test/xmodem.py and test/serial-xmodem.py program text (no split-str) still pass ^u8 n to str words (xmodem.py :23 SPAN:$, :62 VEC:APPEND, :65 :73 FS:WRITE-ALL, :76 SAVE) and no suite runs them. Acceptance: those rows say str (or a vector where the word fills bytes), XMODEM:BYTES goes if nothing else needs it, both python tests run green. Depends: habu-say-str-in-4422bb6c. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2xmod
