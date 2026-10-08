\ window-rewind.f - discard the build host above its core prefix for a fresh window.
\
\ PAYLOAD TEXT, NO DEFINITIONS, as src/habu/prefix-rewind.f is: a build driver
\ `include`s this file from its reset step (tools/native-build-core.f
\ LOGICAL-RESET, test/native-window-owner-child.f) because the three words are
\ top-level boundaries no checked body names. `0 set-check` and `0 set-top-check`
\ leave the window without the host's hooks until its own src/core/check-hook.f
\ and src/core/top-row.f install theirs; `seed-ndict!` lowers the dictionary to
\ the first source-prefix record and clears the seal floor, the engine's one seam
\ for that. The driver resets the retained checker before including this file
\ and discards the host's address rows after it. The two hook flips run with the
\ host's top-level tracker still installed: it models `0 set-check` itself
\ (src/core/top-row.f TR-SUSP) and asks the checker nothing for these tokens.
0 set-check
0 set-top-check
CORE-PREFIX:FIRST-RECORD seed-ndict!
