---
title: "One resolver for the engine's bare-tail order"
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T14:53:54.227167+02:00"
---

Three walks of one lookup order now exist: the engine's own (src/habu/habu1.f EMIT-FIND, open package private then public then the global wordlist, then the used publics), the checker's CK-OPEN-CLAIMS? in src/core/checker.f, which asks it as a boolean, and NMIGRATE's SPELL-START in src/compiler/native/migrate.f, which asks it for an address. Two of the three are Habu-level copies of an order the engine owns, and a change to that order has to be made in three places or they disagree. The home for one Habu-level resolver is src/habu/xref.f, beside XREF-FIND and XREF-QUAL-INDEX, which already own the naming grammar - XREF-FIND deliberately sends a bare token to the global wordlist, so this is a sibling and not a change to it. xref.f is engine prefix, so the change is seed-affecting and needs an install --force refresh before the cold gate.
