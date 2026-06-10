\ caf-lib.fs — the checker engine (everything except the `:` override).
\ Load this to get CHECK-DEF / PARSE-SIG / the DB etc. with NATIVE `:` intact.
\ caf.fs adds the override on top; test infrastructure loads the lib alone.

require src/config.fs
require src/forward.fs
require src/arena.fs
require src/types.fs
require src/rows.fs
require src/effects-repr.fs
require src/diag-state.fs
require src/unify.fs
require src/render.fs
require src/sigparse.fs
require src/db.fs
require src/prims.fs
require src/runtime.fs
require src/checker.fs
require src/diag.fs
require src/control.fs
require src/locals.fs
require src/quots.fs
require src/pickroll.fs
require src/parsing.fs
require src/defining.fs
require src/capture.fs
