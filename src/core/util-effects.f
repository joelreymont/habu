\ util-effects.f - checker row for the shared case fold.
\ CORE-FOLD-C is the one fold the engine, the checker and the type registry all
\ use for A-Z, defined before the checker publishes anything. The registry loads
\ after the hook and folds interned package and family names with it.
\ Retirement: habu-primitive-effect-axiom-1119f176.

s" CORE-FOLD-C" s" n -- n" TRUST
