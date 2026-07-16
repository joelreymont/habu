\ kernel-export.f - CLI: export a checked kernel's versioned artifact pair.
\
\ Usage: bin/hb --load tools/ptx/kernel-export.f -- KERNEL-NAME OUT-DIR
\ Writes <OUT-DIR>/<NAME>.ptx and <OUT-DIR>/<NAME>.manifest.json
\ (habu-kernel-manifest v1; contract in docs/ptx-sketch.md "Kernel ABI
\ contract"). Deterministic: the same source tree writes byte-identical
\ artifacts, so the export can run as an external build step. Host-only.
\ Named errors: E-KEXPORT-KERNEL, E-KEXPORT-OUTDIR, E-KEXPORT-EMPTY.

require tools/ptx/kernel-export-lib.f

KEXPORT:MAIN
