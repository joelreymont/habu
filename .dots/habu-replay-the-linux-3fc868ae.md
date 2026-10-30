---
title: Replay the Linux layout for an arm64 Linux product built on macOS
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T11:20:57.606409+02:00"
---

carl 2026-10-30 (lane habu-build-a-same-e8d102a9, carl-xbuild xtkuxvpk): with the launch refusal lifted, 'hb --load tools/native-build.f -- out/hb --target linux-aarch64' on the Mac builds an aarch64 ELF that starts on spark (echo bye: E-UNDEFINED rc 70; catch-frame.f ok; the REPL runs ': X 42 ; X .') but test/engine-error-package.f and a native engine build on spark crash SIGSEGV rc 139 on 'ldr x0,[x0]' with x0 = $44000003670 or $44000003680, macOS DATA-VA (src/os/macos/layout.f:6, $44000000000) plus an offset, while x20 = $340000000, Linux DATA-VA (src/os/linux/layout-constants.f:31); the pc ($1afe330) lies past the 6 MB image and matches no map row, so it is code the product's own compiler built at load time carrying the host's DATA-VA (candidates src/compiler/native/emit.f:1627, select.f:1204). src/os/linux-x86-64/target-layout.f:1-20 names this hazard for x86-64 and replays the Linux layout for it; nothing does for an arm64 Linux target built on macOS. Fix at that layer so the product's compiler binds the target's layout; proof: the cross-built engine runs engine-error-package.f and builds an engine natively on spark. Lands with or before e8d102a9: lifting the refusal alone would ship a product that crashes.
