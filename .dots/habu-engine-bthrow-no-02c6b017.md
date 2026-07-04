---
title: "Engine: BTHROW no-handler exit masks throw code to 8 bits"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:00:25.900757+02:00"
---

src/habu/habu1.f BTHROW THROW-NOREC path ends '0 9 0 ADDI, NR-EXIT-GROUP SYS,' so an uncaught top-level throw exits with the RAW code masked by the kernel to 8 bits and prints NOTHING. Proven on current bin/hb: '-2802 throw' at top level exits 14 silently; '-2816 throw' (multiple of 256) exits 0 silently - a fail-open class for any tool relying on throw propagation for its exit status. Fix in the engine emitter: before NR-EXIT-GROUP, report the code on stderr like src/habu/driver-io.f DRV-FAIL ('driver: uncaught throw code N') and clamp the exit status to a deterministic nonzero rc when code & 0xFF is 0 (or always map to a fixed uncaught-throw rc). Needs fixpoint rebuild plus a regression that loads a file throwing -2816 and asserts nonzero exit + diagnostic. Context: found while fixing habu-install-force-exits-09c3c981; tools/build-fixpoint.f now has its own BF-CLI catch+die boundary, but every other --load tool is still exposed.
