---
title: Reprove the undrained pre-trust backstop
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:05:02.383638+02:00"
---

Static invariant: nothing statically guarantees that the SEAL-CAPTURE watermark token is the first fail-closed consumer of an undrained pre-trust pending table. Whether the engine dies at the check hook (exit 70) or at the SEAL-CAPTURE backstop (exit 73) is decided purely by the order in which prefix source happens to appear, so adding a checked 'is' on a pre-trust deferred word anywhere earlier in the prefix silently retires the only regression that covers the backstop, with no red anywhere.

Full context: test/pre-trust-defer.f UNDRAINED-CASE (line 211) blanks the whole DRAIN-PRETRUST region between the two PTD-REGRESSION-BLANK sentinels in src/core/checker.f (lines 8724-8726) and asserts that the child engine exits 73 with 'undrained pre-trust defer' naming TFAM-RESOLVE-XT. Measured 2026-07-29 on the proofs base, the child exits 70 instead and asserts 5, 6 and 7 go red. Its stderr is exactly two lines: 'habu: in install: at (is)' and 'hook: non-certified definition: install at (is)'.

Root cause, with file:line. With the drain blanked no pre-trust deferred word gets its checker rows, so the first CHECKED 'is' on a pre-trust deferred word fails the check hook (src/core/check-hook.f:34) and the engine exits 70 with the load/compile reject status. That first site is ': INSTALL ( -- ) [: LIVE ;] is PKG-LIVE-XT ;' at src/habu/xref.f lines 207-209. PKG-LIVE-XT is declared by 'defer PKG-LIVE-XT' at src/core/checker.f:465, which is before ': TRUST' at src/core/checker.f:8671, so it is a pre-trust deferred word. The baseline SEAL-CAPTURE token lives at src/habu/xref.f:492, 283 lines later in the same file, so the check hook always fires first. Both halves arrived together in commit e8c27f225303 'Harden package authority', which added the pre-trust 'defer PKG-LIVE-XT' and the checked 'is' in xref.f.

Evidence (falsification, not resemblance). Copy src/ to a private root and boot a child engine with that root as its working directory; the engine re-reads its prefix from source at boot, so patching the copy is enough. Unpatched: exit 0. Drain region blanked only: exit 70 with the two-line hook diagnostic above. Drain region blanked AND the 25 bytes '[: LIVE ;] is PKG-LIVE-XT' in src/habu/xref.f:209 overwritten with spaces: exit 73 with 'hb: undrained pre-trust defer: TFAM-RESOLVE-XT' followed by every other captured name. So the BSEALCAP backstop (src/habu/habu1.f:2410) still works exactly as designed; only the regression's assumption about which fail-closed boundary fires first is stale.

This is NOT the Gforth stage0 mirror replay defect owned by dot habu-fix-stage0-pre-88a4297e. Everything above reproduces under the native engine on the ordinary 'bin/hb --load' child-boot path, with no gforth and without tools/bootstrap.sh. The shared wording 'hook: non-certified definition ... at (is)' is the generic consequence of a pre-trust deferred word that has no checker rows, and appears in either engine; it is not evidence for the mirror.

Required result: test/pre-trust-defer.f keeps genuinely proving the exit-73 SEAL-CAPTURE backstop, and stops depending on implicit prefix ordering. Decide between: (a) assert the first fail-closed boundary explicitly (exit 70 plus the hook diagnostic) and add a second case that reaches the backstop by also neutralising the earlier checked 'is'; (b) move the baseline SEAL-CAPTURE token in src/habu/xref.f above the first checked 'is' on a pre-trust deferred word so the backstop really is the first consumer; (c) add a static gate that fails when any checked 'is' on a pre-trust deferred word precedes the baseline SEAL-CAPTURE token in prefix order. Option (c) is the one that closes the invariant rather than tracking it.

Forbidden: deleting the undrained case, weakening it to 'exits non-zero', or asserting 70 while leaving the exit-73 backstop with no coverage at all.

Acceptance: test/pre-trust-defer.f green through 'bin/hb --load test/pre-trust-defer.f' and through the pre-trust-defer suite registered at test/gate-stdlib-cases.f:735; a mutation that removes the BSEALCAP backstop reds the suite; a mutation that moves a checked 'is' on a pre-trust deferred word earlier in the prefix does not silently green it. Found by investigation dot habu-attr-three-unowned-3e144928.
