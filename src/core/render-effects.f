\ render-effects.f - checker row for the type-declaration diagnostic.
\ render.f installs RECXT, the only inferred-effect row producer, so it loads
\ before the hook and records nothing of its own. TDECL-DIAG is its declaration
\ reporter: sumtype.f and generated-declaration.f both load after the hook and
\ report through it.
\ Retirement: habu-primitive-effect-axiom-1119f176.

s" TDECL-DIAG" s" ptr u8 n ptr u8 n ptr u8 n ptr u8 n --" TRUST
