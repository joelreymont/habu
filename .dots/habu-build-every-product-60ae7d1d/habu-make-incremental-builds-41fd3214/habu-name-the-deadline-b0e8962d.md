---
title: "Name the deadline that expired inside a build's child"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T16:22:18.902939+02:00"
closed-at: "2026-11-10T21:50:20.704717+02:00"
close-reason: "obsolete: src/lib/keyed-image.f and HBB-RUN-MAKER-CMD are gone; DECISIONS.md:1203-1210 removes the maker child and HB_BUILD_TIMEOUT_MS"
---

Workspace: none yet (finding by jerry, 2026-11-06).

What happens: lane A2a's engine build (installed hb, tools/native-build.f in .jj-ws.noindex/jerry-a2a) failed twice with `hb-build: code=E-PROC-TIMEOUT maker=engine source="src/habu/habu2.f" timeout_ms=N` long before N: run 1 with N=1800000 after about 5.5 minutes (15:58:31-16:04:00), run 2 with N=600000 after 183 s wall, 32 s CPU (16:04:15-16:07:18), load about 48 on 12 cores. The maker image (native-build-support) was never published (img/ empty), so the failure came from the maker image's build, not the maker run. A rerun at load about 30 passed (rc 0, image child 94 s, cold engine step 313 s wall, 124 s CPU).

Why the message is wrong: keyed-image.f BUILD-RUN (:170-190) maps a child that exits 124 (PROC-TIMEOUT-RC) to E-PROC-TIMEOUT, and hb-build-lib.f HBB-RUN-MAKER-CMD (:730-733) reports that as the maker timing out with hb-build's own limit (HBB-MAKER-TIMED-OUT :687). process.f PROC-OUTCOME>DEADLINE-RC (:268-276) states this is by design: "reports this deadline and one that expired inside the child alike". The child printed nothing, so which deadline expired is unknown.

Candidate inner deadline: the image save signs the image (src/habu/snap-lib.f:576 CODESIGN:SIGN-AS), and src/lib/codesign.f gives /usr/bin/security and /usr/bin/codesign 10 s each (CODESIGN-TIMEOUT-MS :31, CODESIGN-CAPTURE :80-87 throws E-PROC-TIMEOUT through PROC-OUTCOME>RC). No other deadline was found in the save path (snap*.f, aot-builder.f, executable-build.f, app-image*.f). The unified log shows a lone `security` run at 16:07:08.8 and the build ended about 10 s later; the log does not name its parent, so this is not proven. Measured at load 31: security find-identity 0.04-0.06 s, codesign of a 6.8 MB hb 0.04-0.08 s.

Acceptance: a deadline that expires inside a child is reported by the process that owns it (which command, its limit), and the parent reports a child exit, not its own limit, so the next occurrence names its cause. Then decide the codesign limit from that evidence.
