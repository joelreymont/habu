---
title: Refuse at validation a does> slot and a second private wordlist the link refuses
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T05:05:50.625927+02:00"
---

Left out of F1 (habu-refuse-at-valid-34fa7eb5), found by the lead reading src/habu/linker.f: (1) SLOT-CK (:1330-1338) admits a does> slot whose target is not tag 5 (none), which NEMIT-FILL (:1753-1758) refuses with E-COMP-STATE until a patched slot links, and a slot site whose 4 bytes are not the zeroed slot (EMISSION-CODE+ :613), where the link writes a RET over a live instruction; (2) a KIND-PRIVATE entry naming a package that already has a private wordlist (own flag 1, an earlier KIND-PRIVATE, or an earlier file's record holding one) passes PKG-REF-CK, and LINK-PRIVATE's OUTER:PACKAGE-PRIVATE (src/habu/packages.f:179-185) throws E-COMP-STATE. Either ends the build mid-link, and the object is never rewritten. Tests first: crafted objects as test/fixtures/file-objects/build.f CRAFTS makes them (a definer with does> in a build file: defining one is untried in a build; creating through one ends the process today), then FRESH? refuses each (PRIVATE? from F1 covers (2)). Files: src/habu/linker.f, test/fixtures/file-objects/build.f, test/file-objects.f. Depends: F1 landed. Claim: unassigned.
