---
title: "Switchover wave B: option<tuple> + result<T,errno> process family"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.005375+02:00"
---

docs/census-switchover.md section 5 wave B. The 25 multi-value+flag words (SPLIT-NEXT string.f:180, NEXT-LINE object.f:274 r8, RX-FIND* regex.f:454/464, LOAD object-index.f:116, JSONLF-*/JSONL-PARSE-ROW, PROC-ENV-DEFAULT0, FL-STRIP-SIGN, tool row parsers) to option<tuple>; the 34-site process rc family (PROC-WAIT-RC r17, PROC-RUN-RC r11, RUN-CAPTURE/PROC-CAPTURE-RC@ len-len-rc + process-env/cwd/argv mirrors) to result<T,errno> over E-PROC-*. Raw habu1.f emitters (BRUNRC/BPIPE/BPOLL/...) stay rc-sentinel at the trusted boundary; only checked wrappers migrate. DEPENDS: wave A patterns proven, item 12 multi-cell.
