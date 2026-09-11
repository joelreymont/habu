\ native-window-cast-host-bad.f - a cast naming a host-only family. CAD-NUM is
\ loaded in the host that opened the window and in no window file, so the
\ window must not resolve it: the retained checker never answers for window
\ source. E-CAST-FAM.
package NW-HOST
private
CAST: MINT-C ( n -- CAD-NUM:alloc-byte-len )
;package
