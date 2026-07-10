\ result.f - the result<ok,err> sum family: a success value (ok) or an error
\ value (err). The checked replacement for value+flag / rc-plus-value sentinel
\ returns whose flag distinguishes DIFFERENT errors — a success carrying a value
\ OR a failure carrying a REASON (an errno, an error code, a diagnostic). This is
\ the wave-B partner of option<T>: option<T> is pure present/absent (wave A);
\ result<T,E> is success/error-with-a-reason (PHASE 2 switchover, wave B; dot
\ habu-switchover-wave-a-54edcee6, epic habu-epic-adopt-adts-64833911). Returning
\ result<T,E> instead of a `value negative-rc` or `value 0=` sentinel makes the
\ checker FORCE every caller to MATCH both the ok and the err arm — dropping the
\ error or reading the value on the error path no longer type-checks.
\
\ PLACEMENT: lib/adt/ groups ADT TYPE modules. A type family (a type plus its
\ generated RESULT:OK / RESULT:ERR constructors) is not a set of hand-written
\ published words, so it is not modelled by the published-word stdlib manifest
\ (lib/std.manifest keys on word/effect rows). The lib/adt/ subdir keeps it off
\ that coverage walk exactly as lib/ptx/ research libs and lib/layout/ runtime
\ are, WITHOUT making result private: RESULT:OK / RESULT:ERR are public
\ dictionary words resolvable from any consumer, and public-sig renders their
\ signatures. Listing ADT type modules in the manifest would need a manifest
\ schema extension (a separate dot) — not done here, matching lib/adt/option.f.
\
\ LOAD ORDER: a consumer that returns result<T,E> or MATCHes one must
\ `require lib/adt/result.f` FIRST, so the family and its constructors are
\ declared before the consumer is compiled. result is ONE shared public family
\ (arity 2), declared once per session; every result<n,n> / result<len,errno> /
\ result<ptr u8 n,n> is an instantiation of that one family, so it stays well
\ under the protected-WID public-family cap, exactly like the arity-1 option.
\
\ PARAM ORDER (checker rule): a SUMTYPE's payload params bind in first-use
\ declaration order — param a (0) must be introduced before param b (1). The ok
\ variant carries a (the success type), err carries b (the error type). Writing
\ err before ok (b before a) throws E-SUMTYPE-DECL (7107), so the ok arm is
\ declared first.

SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
