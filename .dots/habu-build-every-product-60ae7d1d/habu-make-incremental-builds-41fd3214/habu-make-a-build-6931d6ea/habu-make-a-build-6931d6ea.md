---
title: "Make a build's lookups of computed names dependencies"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T23:22:03.238688+01:00"
---

Problem: a piece's object depends only on the names its source spells and its does> clauses' names (src/habu/linker.f SPELL-TEXT, SPELL-CLAUSES, SPELL-HIT), so a lookup of a name the piece computes (search-wl, scope-find) is no dependency and a warm build can keep an object whose answer changed. Reproducer: h.f 'package QG public : FOUND ( -- n ) s" dup" get-current search-wl 0= if 0 else 1 then ; ;package', s.f 'package QS public variable Z ;package' then 'undefine dup', r.f 'QG:FOUND constant QK', main.f ': MAIN ( -- ) QK . cr ;': built cold it prints 0; with the undefine deleted it prints 1 cold and 0 warm. The same happens when the name probed is a record the build made. Fix: every typed lookup by name records the name and the wordlist it asked (src/core/compile-reads.f, a NAME-READ entry), and the linker adds those names to the object's NAMES as it adds spelled ones (linker.f PART-NAMES), so the object format is unchanged; neither kernel changes. The lookups are src/core/checker.f CHECKER-RESOLVE's WL-PROBE and three words beside it for scope-find, search-wl and tok-imm?; the global rows of those three are retired after xref.f and Habu's callers move. A lookup under WL:RETIRED makes the part never link. A part also depends on the names of the records it made (SPELL-CLAUSES widened). Reads by position are closed by privacy (habu-keep-private-words-2a4821ba). Acceptance: in both cases the warm build prints what the cold one does, and a test in the suite shows it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nr.
