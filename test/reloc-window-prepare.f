\ reloc-window-prepare.f - the relocation owners' window, loaded last in
\ test/native-window-owner-child.f's list.
\
\ A test that marks or clears the engine's relocation maps calls rows private to
\ their owners: callmap-set and addrmap-set (package NPUB) and reloc-maps-clear
\ (package CODE-RECLAIM, src/habu/prims.f). Outside its owner a checked caller
\ of any of them is refused. This file reopens both owners before
\ src/core/internal-mark.f seals the window and publishes one checked word per
\ row, typed by that row. The sealed product refuses such a reopen, so a child
\ that loads this file runs on the unsealed engine test/whitebox-child.f names.
\
\ A child loads lib/test.f and lib/memory.f after the seal, so before it the
\ window loads what the product loads before its own seal
\ (src/habu/native-runtime.f) and they need: the two declaration participants,
\ the last of which seals registration (an ENUM is refused with
\ E-REGISTRATION-SEALED until it does), dynamic storage and the prelude.

package NPUB
public
: ADDRMAP-MARK ( n -- ) addrmap-set ;
: CALLMAP-MARK ( n -- ) callmap-set ;
;package

package CODE-RECLAIM
public
: MAPS-CLEAR ( n n -- ) reloc-maps-clear ;
;package

require src/core/prefix-boundary.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f
require src/core/dynamic-storage.f
require lib/prelude.f
include src/core/internal-mark.f
