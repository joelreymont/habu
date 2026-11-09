\ window-rewind.f - discard the build host above its core prefix for a fresh window.
\ Payload text with no definitions, as prefix-rewind.f: a build driver `include`s
\ it from its reset step (tools/native-build-core.f LOGICAL-RESET,
\ test/native-window-owner-child.f); no checked body names these top-level words.
\ The driver resets the retained checker before it and discards the host's
\ address rows after it.

\ The machine-code interpreter's check cells stay empty until the window's own
\ check-hook.f and top-row.f fill them. seed-ndict! lowers the dictionary to the
\ first source-prefix record and clears the seal floor, the engine's one seam
\ for that.
0 set-check
0 set-top-check
CORE-PREFIX:FIRST-RECORD seed-ndict!
