---
title: Move lib/ under src/ (src/lib)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:10:11.398145+02:00"
---

User decision 2026-07-04: use one source root. Move `lib/` to `src/lib/`
and update every live `require lib/...`, `--load lib/...`, bootstrap source list,
fixpoint prelude, build cache key, test source list, and documentation path.
Preserve the load-time boundary after the preceding rename: `src/core` and
`src/hb` are the baked engine prefix; `src/lib` remains runtime-loaded checked
standard library and must not be added to the baked prefix.

Run the whole-tree path sweep in one isolated change after the `src/habu` to
`src/hb` rename. Acceptance: no tracked source, test, tool, bootstrap file, or
documentation names the old `lib/` path; all moved modules keep their current
package ownership and standalone load behavior; the bootstrap check, Maki,
package and typed-local gates, and the full native gate pass.
