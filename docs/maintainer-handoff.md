# Habu maintenance

Alder is the current Habu integrator; Hazel was the Claude Habu maintainer.
Birch belongs to Tender. The former lane ownership and integration roots in
this document are historical and remain available in version history.

Work from the default checkout and integrate completed changes into `master`.
Use [AGENTS.md](../AGENTS.md) for delegation, independent review, workspace
isolation and cleanup. Worker workspaces belong under `.jj-ws/`; the integrator
retires them after preserving their results, verification and pushing master.

## macOS

The native Apple Silicon product builds through
[the bootstrap workflow](bootstrap.md). The full native registry is
`bin/hb --load test/run.f`; [gate.md](gate.md) explains the frozen-tree check.
Keep development `bin/hb` separate from the versioned stable installation
described in [bootstrap.md](bootstrap.md#stable-local-command).

Maki is the dependent application being qualified. Its native image builds
with `tools/hb-build.f -- --repl /absolute/path/to/maki/maki.f -o /output/maki`.
Run that tool through a matching Habu engine and library tree. Maki's tests
require an output-directory argument. A native PCB construction, routing,
geometry and export smoke check exercises application behavior; it does not
qualify KiCad's external DRC or live IPC integration.

The Rocq subsystem and toolchain requirement have been removed. Habu's
executable checks carry the verification claims in [proofs.md](proofs.md).
The work here is the language and Maki integration; application policy stays
in the application repository.

## Work preserved for later

[Unmerged branch work](branch-work.md) records the remaining independent drafts
and why they have not been merged or discarded. Completed Alder and Hazel
changes are integrated, including terminal-call lowering, capture lifecycle,
filesystem mutation, generated declarations and PTY synchronization. Their
historical worker names are not continuing ownership claims.

The generated-definer design decision is preserved in
[its existing task](../.dots/habu-let-tools-check-6ac068c5.md).
The source-only recovery engine's stronger optimized-definition smoke remains
an uncompleted draft; [bootstrap.md](bootstrap.md) states that boundary.
Linux and Intel execution, physical serial devices and Maki's live KiCad
integration require their own qualification.
