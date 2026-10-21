\ reloc-window-prepare.f - the relocation owner's window, loaded last in
\ test/native-window-owner-child.f's list.
\
\ A test that marks the engine's relocation maps calls rows private to their
\ owner: callmap-set and addrmap-set (package NPUB). Outside its owner a checked
\ caller of either is refused. This file reopens the owner before
\ src/core/internal-mark.f seals the window and publishes one checked word per
\ row, typed by that row. The sealed product refuses such a reopen, so a child
\ that loads this file runs on the unsealed engine test/whitebox-child.f names.
\
\ A child loads src/lib/test.f and src/lib/memory.f after the seal, so before it the
\ window loads what the product loads before its own seal
\ (src/habu/native-runtime.f) and they need: the two declaration participants,
\ the last of which installs the declaration runner (until it does, an ENUM dies
\ on the unset TYPE-DECL:TDECL-TXN-XT), dynamic storage and the prelude.

package NPUB
public
: ADDRMAP-MARK ( n -- ) addrmap-set ;
: CALLMAP-MARK ( n -- ) callmap-set ;
;package

require src/core/prefix-boundary.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f
require src/core/dynamic-storage.f
require src/lib/prelude.f
include src/core/internal-mark.f
