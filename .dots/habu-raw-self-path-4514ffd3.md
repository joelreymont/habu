---
title: Raw self-path and startup-image read boundary
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T18:46:14.479782+02:00"
---

Capability owner for the raw process-self-introspection trust sites: lib/engine-id.f:44 ENGINE-SELF-MACOS (walks the apple[] startup vector past envp, ZLEN + prefix-match on executable_path=, raw pointer reads from the startup image) and lib/engine-id.f:59 ENGINE-SELF-LINUX (/proc/self/exe readlink into EID-PATH). Same boundary class as src/os/env-base.f's startup-image reads (envp/apple walking), which remain on the audit placeholder under builder-emit until that increment. No existing capability dot covers raw startup-image/self-path reads (habu-checker-capability-ptr-113a95e9 covers pointer-arithmetic byte-views, not these reads). Discharge path: a typed startup-image accessor surface (checked envp/apple iterator + typed self-path word) or a documented permanent boundary verdict; either way these sites need a focused test (engine-id already gated by content-key flows). Minted by the trusted-inventory audit (habu-audit-trusted-inventory-3a950436) owner-reassignment increment.
