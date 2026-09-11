\ native-window-cast-ok.f - window source that declares a family and casts to
\ it in the same window. The window's own checker must resolve its own
\ declaration; nothing else can.
package NW-OK
public
NEWTYPE alpha 0
private
CAST: MINT-A ( n -- NW-OK:alpha )
;package
