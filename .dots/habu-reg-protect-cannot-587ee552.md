---
title: "REG-PROTECT cannot reach the protector's own cells"
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T14:47:32.038563+02:00"
---

src/core/util.f REG-PROT-N, REG-PROT-IDX and REG-PROT-CAP, and
src/core/internal-mark.f IMK-I, are open and writable at top level on MASTER
(81d88a3a, measured: 'REG-PROT-N @ . cr' prints 61, '0 REG-PROT-N !' then
prints 0, rc 0 from an ordinary bin/hb --load program). They are the
write-protection mechanism's own state. REG-PROTECT (util.f:42) records
'ndict@ 1 -', the just-defined record, so it structurally cannot tag a record
defined before itself, and the three cells it needs are its own storage. IMK-I
is internal-mark.f's walk cursor, defined before IMK-SEAL-REGISTRY runs. Not
exploitable today: both sealing passes run during the cold prefix, before user
source loads, so a post-boot write is inert - it is an unclosed surface, not a
live hole. Fix shapes to weigh: give REG-PROTECT an index-taking sibling and
call it after the cells exist; or move the three cells into the IMPLEMENTATION
span shape once that mechanism covers data records; or hand-place them via
xref.f's XREF-RETIRE-INDEX at seal time. Found by route3-5 while closing route
3's own ten-cell residue (dot habu-route-3-the-64078d43 section 14), which
used REG-PROTECT at the definition site because those cells are all defined
after it.
