\ package-literal.f - a design body names its package with `[package]`, a
\ keyword inside the design span.
package PDESIGN-NAME
public
: NAME ( -- ^u8 n ) [package] ;
;package
PDESIGN-NAME:NAME PDEP:SHOW-NAME
