\ launch-neg-test.f - fail-closed regressions for the PTX launch/emit contracts.
\
\ The collective fix (habu-fix-ptx-collective) requires that WHERE constraints
\ are VALIDATED, not skipped, and that block size is derived/rejected from
\ PTX-BLOCK@. These negatives prove each bad launch/emit contract throws the
\ named code rather than silently emitting or launching a wrong-shaped kernel:
\   - malformed WHERE (bad lhs / op / block literal)  -> E-PTX-SYNTAX
\   - block mismatch (WHERE block != declared %BLOCK)  -> E-PTX-BLOCK
\   - k > block (row width exceeds threads-per-row)    -> E-PTX-BLOCK
\ header.f/launch.f are the same on host and Orin, so the contract proven here is
\ the one the device goldens launch under. Run:
\ bin/hb --load lib/errors.f lib/string.f lib/test.f lib/ptx/header.f lib/ptx/launch.f tools/ptx/launch-neg-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/ptx/header.f
require lib/ptx/launch.f

256 %BLOCK   \ declared threads-per-row; WHERE block literals must match this

: NEG-WHERE ( -- )
   [: s" extent-c" s" <=" s" block-x"   PTX-WHERE-CHECK ;] E-PTX-SYNTAX TTHROWSQ
   [: s" cols"     s" <=" s" block-256" PTX-WHERE-CHECK ;] E-PTX-SYNTAX TTHROWSQ
   [: s" extent-c" s" <"  s" block-256" PTX-WHERE-CHECK ;] E-PTX-SYNTAX TTHROWSQ ;

: NEG-BLOCK ( -- )
   [: s" extent-c" s" <=" s" block-1024" PTX-WHERE-CHECK ;] E-PTX-BLOCK TTHROWSQ ;

: NEG-KBLOCK ( -- )
   [: 1 512 256 PTX-ROW-LAUNCH-CHECK ;] E-PTX-BLOCK TTHROWSQ ;

: POS-CONTROL ( -- )
   [: s" extent-c" s" <=" s" block-256" PTX-WHERE-CHECK ;] 0 TTHROWSQ
   [: 1 256 256 PTX-ROW-LAUNCH-CHECK ;] 0 TTHROWSQ ;

: RUN-ALL ( -- )
   NEG-WHERE
   NEG-BLOCK
   NEG-KBLOCK
   POS-CONTROL ;

T-RESET
RUN-ALL
T-REPORT
