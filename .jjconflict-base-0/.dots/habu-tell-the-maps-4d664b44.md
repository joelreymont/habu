---
title: Tell the maps the floor when forget reclaims
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T15:43:05.635744+02:00"
---

PRIORITY 2, the THIRD cursor rewind with the map-stale defect (found by the p2-map lane, measured: forgetting a definition rewound CP 68 bytes and left its address record standing over the reclaimed span): CODE-RECLAIM:TRUNCATE (src/habu/xref.f:592, reached by FORGET-DEFS-FROM, used by snap.f) lowers CP and leaves both relocation maps standing. The file ALREADY OWNS the right mechanism - its prose says everything holding an address-keyed fact registers once and is told the floor BEFORE the space is released, 3 of 4 watcher slots used - and neither map is registered. Design question: register a watcher (needs callmap-clear/addrmap-clear primitives that do not exist - the existing ones only SET; SNAP-RELOC:CLEAR-SPAN, from the p2-map landing is the emitter to expose) or clear inside TRUNCATE itself. The false claim the p2-map lane almost shipped is corrected in the CLEAR-CALLMAP-SPAN comment (publication-is-append is falsified by forget). Regression: forget a chain-carrying definition, assert both maps clean over the reclaimed span. Files: src/habu/xref.f, habu1.f (primitives). Depends: none.
