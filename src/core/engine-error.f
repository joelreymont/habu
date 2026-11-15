\ engine-error.f - authoritative engine failure ABI.

package ENGINE-ERROR
public
82 constant DISPATCH-UNSET
83 constant SEAL-VIOLATION
84 constant SEAL-PACKAGE
85 constant BAD-TAG
88 constant CODE-CERT
\ 96, 98 and 99 belong to SNAP-RELOC relocation failures (layout.f).
102 constant STACK-BOUNDS       \ a physical VM stack access exceeds its active allocation
\ 103 belongs to SNAP-RELOC:SITE-RC, the x86-64 site-row band (layout.f).
\ 106: a callback thunk runs on C's stack with no catch frame above it, so it
\ cannot throw. Every rejection of a callback and of an outbound FFI call on a
\ context another thread owns writes a record with code E-CALLBACK to fd 2 and
\ exit_group 106 without touching the region; x86-64's kernel still writes
\ `hb: callback: <reason>`. A call on a busy context is rejected, never made to
\ wait.
106 constant CALLBACK           \ a C callback or FFI call found its region or slot unusable
107 constant POLICY             \ a sealed design named a word outside its admitted vocabulary (src/lib/vocabulary.f)
108 constant OVERLAY-OPEN       \ a definition, package row, wordlist or undefine while a checker overlay is open (src/core/checker.f CHECKER-OVERLAY)
110 constant MEMORY-FAULT       \ a load or store by Habu code faulted (src/habu/crash.f)
111 constant OUT-GROW           \ a Wasm run's output could not grow: the memory refused the pages
;package

\ Dictionary occurrence failures belong to DEF-OCC.
package DEF-OCC
public
-7231 constant E-STALE
-7232 constant E-SELECT
-7234 constant E-EXHAUSTED
;package
