---
title: Show control and format characters in found text
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T00:06:53.879869+01:00"
---

Problem: the JSON reader's found text (src/lib/json/read.f FOUND-STRING, FOUND-RUN) and the XML reader's (src/lib/xml/reject.f FOUND-RUN) copy a C1 control (U+0080-U+009F) or a format character such as U+202E RIGHT-TO-LEFT OVERRIDE into the rejection text raw; an agent or terminal that shows the rejection then shows a reordered or hidden message from untrusted input (JSON review of yvnwmwrs). Acceptance: a control or format character (general category Cc or Cf) in either reader's found text is written as its code point, U+XXXX, and counted as written; one test per reader with U+202E and U+0085 in a rejected token. Depends: none. Claim: unassigned.
