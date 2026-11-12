---
title: "A stripped program's write lays no facts texts"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:11:32.277116+02:00"
---

Problem: src/habu/link-data.f WRITE-PROGRAM (:530) and src/arch/wasm/link-module.f:238 run CHECK:PLACE-FACTS on every write, interning the facts texts the build body held into the build's data space (checker.f PLACE-FACT, intern-table.f LAY). Only an engine's link reads them (BUILD-REC, writes each record's facts offset, link-data.f:313); a stripped program carries no records, yet its first write allots them, its starting here lies past them, and when the data unit holding them is live its image copies them (/tmp/hunt-link344/repro-ld.f, out-ld/a 0x30f8-0x3157). Acceptance: a stripped program's write allots nothing for facts and its image holds none; an engine's link is unchanged. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/link-facts.
