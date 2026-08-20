\ cell-effects.f - checker effects for target cell-width words.
\ CELL is the immutable target-width fact defined before the checker and
\ republished here for checked users.
\ CELL-WIDTH-CHECK is the pre-checker target-width assertion; republishing it
\ lets the focused checked bootstrap execute the identical body.
\ Retirement: habu-primitive-effect-axiom-1119f176.

s" CELL" s" -- n" TRUST
s" CELL-WIDTH-CHECK" s" --" TRUST
\ CORE-LAYOUT-RC is the exit code every internal record-layout assertion dies
\ with; the type registry loads after the hook and asserts its own layouts.
s" CORE-LAYOUT-RC" s" -- n" TRUST
