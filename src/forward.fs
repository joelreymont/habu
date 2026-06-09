\ forward.fs — DEFER seams: the pinned cross-file interface for the
\ mutually-recursive parts, so files can reference them before they exist.

\ Mutually-recursive occurs (shallow halves in types.fs/rows.fs call these
\ deep halves; filled by unify.fs).
defer OCCURS-TYPE    ( id t -- f )   \ type-var id occurs in type term t?
defer OCCURS-ROW     ( id s -- f )   \ row-var id occurs in stack term s?

\ Checker structural hooks (filled by control.fs / locals.fs / quots.fs from
\ their own files; the checker body-walk dispatches through them).
defer CHECK-CONTROL  ( c-addr u -- f )   \ handled a control word? (IF/loops/…)
defer CHECK-LOCAL    ( c-addr u -- f )   \ handled a locals clause?
defer CHECK-QUOT     ( c-addr u -- f )   \ handled a quotation/combinator token?
defer CHECK-PARSE    ( c-addr u -- f )   \ handled a parsing word? (S" CHAR …)
defer CHECK-RESET    ( -- )              \ reset per-definition feature state

\ Default each hook to "not handled" so the checker runs before the feature
\ files load; control.fs / locals.fs / quots.fs re-IS them with real handlers.
:noname ( c-addr u -- f ) 2drop false ;  dup is CHECK-CONTROL
                                          dup is CHECK-LOCAL
                                          dup is CHECK-QUOT
                                              is CHECK-PARSE
:noname ( -- ) ;  is CHECK-RESET
