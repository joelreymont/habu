---
title: Bisect engine growth 90k to 132k
status: done
priority: 1
issue-type: task
created-at: "2026-07-02T04:00:00.000000+02:00"
---
# Problem
bin/hb was ~90 KB; it is 132,343 bytes now. Account for every chunk and
identify what grew and what can be reclaimed.

# Findings (2026-07-02, measured)
Composition of the 132,343-byte binary: 69,640 B compiler/interpreter
machinery (emitted from habu1/habu2 - tokenizer, FIND, compile/interpret
loops, keyword handlers); ~18,960 B baked source text of the REPL/debugger/
stepper bundle (compiled at tty startup); ~15,900 B small named routines
(checker hooks, snapshot writer, crash handler; 202 routines); 4,868 B
primitive bodies (84 words); ~22,975 B Mach-O overhead (16 KB DATA_CONST
page + header + linkedit + signature).

Growth 90k->132k tracks emitter-source growth 150 KB (06-22) -> 225 KB
(07-01): maki/FFI/spawn era (06-25..27), value records + checker symbol
table (06-29..30), object cache + escaped strings (07-01).

# Reclaim paths
1. Unbake the REPL bundle (~19 KB text; file -> ~116 KB): decision dot
   habu-decide-unbake-repl-735b1565.
2. Continue de-duplicating emitted patterns in the 69.6 KB machinery
   (the escape-decoder BL-subroutine recovered 3.3 KB; quote scanning and
   string-copy loops are the next candidates).
The size ratchet (test/gate-build-size.f, baseline 132343) pins every
future change.
