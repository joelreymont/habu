---
title: Trim Habu test audit rows g7
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.611626+03:00"
closed-at: "2026-10-23T18:00:32.649664+03:00"
close-reason: "Applied: x86-64-link-records runs strip/export/quot in one linked child (~17→10.6 s; INDEX-IMAGE kept as the x86-64 peer fixture); aot-shadow-capture readers load test/aot-shadow-reader.f instead of the producer (10.3→7.1 s). Rejected: engine-stack-debugger 0.27 s suite; compiler-native-wide-frame 0.39 s; aot-chain-capture: the repeated capture costs ~0.01 s per child, and removing the fixture load would change the path under test."
---

Problem: the Habu test audit (group g7) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
2. **x86-64-link-records — SLOW** · 18,119 / 15,217 ms  
   Fourteen child modes reload the suite; successful `strip`, `export`, and `quot` cases unnecessarily get separate processes. Each invocation recaptures its window, and those reaching layout rebuild the kernel. Run compatible successful cases through one loaded fixture/kernel; retain isolated fatal refusals and the deliberately shifted host. [child launcher](test/x86-64-link-records.f:606), [case dispatch](test/x86-64-link-records.f:797), [kernel emission](test/x86-64-boot-harness.f:57)  
   **Remove the unused index-image generation:** `INDEX-IMAGE` writes an executable containing lookup assertions but never runs it. `BOOT-CLOSE,` only writes the image; the explicit executable runner targets the different private image and is x64-only. Thus those generated assertions provide no runtime coverage in this ARM row. [INDEX-IMAGE](test/x86-64-link-records.f:542), [writer](test/x86-64-boot-harness.f:119), [runner](test/x86-64-link-records.f:616)

3. **aot-shadow-capture — SLOW** · 11,727 / 9,576 ms  
   Worth retaining for shadow-table persistence and rejection of malformed routine spans, call targets, code cells, and incompatible merges. [persistence assertions](test/aot-shadow-capture.f:416), [refusal cases](test/aot-shadow-capture.f:552)  
   Its three malformed-artifact readers and merge reader reload the entire producer suite. They open a shadow and compile the fixture window **before** reaching reader dispatch. Dispatch reader modes before producer setup. Keep their separate processes because refusal terminates the reader. [unconditional producer setup](test/aot-shadow-capture.f:115), [reader operations](test/aot-shadow-capture.f:579), [late dispatch](test/aot-shadow-capture.f:623)

4. **engine-stack-debugger — SLOW** · 320 / 0 ms  
   Four successful programs each launch another candidate process. Consolidate them into one child, preserving separate checks for empty/nonempty guarded stacks and leaf/calling breakpoint entries. These cases exercise trap handling and resumption, not fresh startup. [four calls and assertions](test/engine-stack-debugger.f:20), [process launcher](test/engine-stack-lifecycle-lib.f:89)  
   Coverage is weaker than its comment claims: the “persistent” cases only require one breakpoint marker. A breakpoint becoming one-shot could still pass both output checks. Do not credit these assertions with proving persistence. [persistent cases](test/engine-stack-debugger.f:46)

7. **compiler-native-wide-frame — MERGE** · 468 / 0 ms  
   Move its existing subject and two assertions into **compiler-native-frame-slot-e2e**. Despite the registry comment, the current test contains no ARM encoding or frame-size assertion: it checks 60 snapshots surviving 64 conditional calls and the resulting call count. [subject](test/compiler/native-wide-frame.f:12), [assertions](test/compiler/native-wide-frame.f:66)  
   The destination already builds and executes the production frame-slot image. Preserve the larger live-value case; smaller examples do not prove it redundant. [existing build and execution](test/compiler/native-frame-slot-e2e.f:21)

8. **aot-chain-capture — SLOW** · 9,009 / 7,706 ms  
   Retain its address-coordinate rebasing, sparse-data placement, malformed artifact rejection, and owned-buffer lifetime checks. [merge assertions](test/aot-artifact-rows.f:261), [relocation checks](test/aot-artifact-rows.f:319), [lifetime/refusal cases](test/aot-chain-capture-suite.f:220)  
   Every row-mode child imports the live capture fixture; every non-fresh mode then captures again before mutation. Produce the valid capture once and prepare mutations from it. Retain fatal-reader isolation and the explicitly fresh reader. [launcher](test/aot-chain-capture-suite.f:79), [fixture import](test/aot-artifact-rows.f:6), [repeated capture](test/aot-artifact-rows.f:412)
