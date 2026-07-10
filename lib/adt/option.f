\ option.f - the option<T> sum family: a present value (some) or its absence
\ (none). The checked replacement for -1-index / id-or-false / value+flag
\ sentinel returns across the stdlib and tools (PHASE 2 switchover, wave A; dot
\ habu-switchover-wave-a-54edcee6, epic habu-epic-adopt-adts-64833911). Returning
\ option<T> instead of a -1 sentinel makes the checker FORCE every caller to
\ handle the absent case via MATCH — a missing branch or a raw -1 comparison no
\ longer type-checks.
\
\ PLACEMENT: lib/adt/ groups ADT TYPE modules. A type family (a type plus its
\ generated OPTION:SOME / OPTION:NONE constructors) is not a set of hand-written
\ published words, so it is not modelled by the published-word stdlib manifest
\ (lib/std.manifest keys on word/effect rows). The lib/adt/ subdir keeps it off
\ that coverage walk exactly as lib/ptx/ research libs and lib/layout/ runtime
\ are, WITHOUT making option private: OPTION:SOME / OPTION:NONE are public
\ dictionary words resolvable from any consumer, and public-sig renders their
\ signatures. Listing ADT type modules in the manifest would need a manifest
\ schema extension (a separate dot) — not done here.
\
\ LOAD ORDER: a consumer that returns option<T> or MATCHes one must
\ `require lib/adt/option.f` FIRST, so the family and its constructors are
\ declared before the consumer is compiled. option is ONE shared public family
\ (arity 1), declared once per session; every option<n> / option<idx> /
\ option<ptr a> is an instantiation of that one family, so it stays well under
\ the protected-WID public-family cap.

SUMTYPE option 1
  VARIANT none    ;VARIANT
  VARIANT some a  ;VARIANT
;SUMTYPE
