\ t-forward.fs — the deferred seam words exist (their bodies land later).
T{ s" OCCURS-TYPE"   find-name 0<> -> true }T
T{ s" OCCURS-ROW"    find-name 0<> -> true }T
T{ s" CHECK-CONTROL" find-name 0<> -> true }T
T{ s" CHECK-LOCAL"   find-name 0<> -> true }T
T{ s" CHECK-QUOT"    find-name 0<> -> true }T
\ a defer can be filled and called — use a THROWAWAY defer, never a real seam
\ (IS-ing OCCURS-TYPE/etc here would clobber unify for every later test).
defer T-DUMMY-SEAM ( x -- x )
:noname ( x -- x ) 1+ ; is T-DUMMY-SEAM
T{ 41 T-DUMMY-SEAM -> 42 }T
