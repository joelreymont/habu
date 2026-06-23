\ habu.fs — load the complete checked Forth (engine + `:` override).
\ After this, `: NAME ( typed-effect ) body ;` is checked at definition time;
\ `: NAME body ;` (no typed effect) is the ordinary Forth colon. Run from root.

require habu-lib.fs
require src/colon.fs
