\ all.fs — run the whole habu test suite; exit nonzero on any failure.
\ Works with either:   gforth test/all.fs     or     gforth src/habu.fs test/all.fs
\
\ The internal-API tests call the checker's arena/PARSE-SIG directly, so they
\ must compile with the NATIVE colon — we toggle CHECKING-ON? off while loading
\ them, then on for the tests that exercise checked *definitions*.

require ../src/habu.fs           \ engine + `:` override

CHECKING-ON? off                \ infra + internal-API tests compile natively
require tester.fs
require t-config.fs
require t-forward.fs
require t-arena.fs
require t-types.fs
require t-rows.fs
require t-effects.fs
require t-diag-state.fs
require t-unify.fs
require t-render.fs
require t-sigparse.fs
require t-db.fs
require t-prims.fs
require t-checker.fs
require t-diag.fs
require t-control.fs
require t-locals.fs
require t-quots.fs
require t-pickroll.fs
require t-dogfood.fs
require t-defining.fs
require t-parsing.fs
require t-cg-asm.fs
require t-cg-opt.fs

CHECKING-ON? on                 \ now actually check defined-with-effect code
require t-colon.fs
require t-selfhost.fs
require t-asm-checked.fs
require t-disasm-core.fs
require t-stepper.fs

cr ." habu test suite: " #ERRORS @ . ." failure(s)" cr
#ERRORS @ 0<> negate (bye)      \ exit 1 if any failures, else 0 (no IF: interpret mode)
