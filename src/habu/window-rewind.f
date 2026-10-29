\ window-rewind.f - discard the build host above its core prefix for a fresh window.
\ Payload text with no definitions, as prefix-rewind.f: a build driver `include`s
\ it from its reset step (tools/native-build-core.f LOGICAL-RESET,
\ test/native-window-owner-child.f); no checked body names these top-level words.
\ The driver resets the retained checker before it and discards the host's
\ address rows after it.

\ The window runs without hooks until its own check-hook.f and top-row.f install
\ theirs. The host's tracker still runs: it models `0 set-check` (top-row.f
\ TR-HOOK) and steps nothing for these tokens. seed-ndict! lowers the dictionary
\ to the first source-prefix record and clears the seal floor, the engine's one
\ seam for that.
0 set-check
0 set-top-check
CORE-PREFIX:FIRST-RECORD seed-ndict!
