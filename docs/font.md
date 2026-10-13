# Native font outlines

`FONT` in `src/lib/font.f` shapes a short UTF-8 label with HarfBuzz and returns
an SVG glyph run. It is a checked Habu library, not a text layout engine. The
caller selects one face from its own font bytes; `FONT` does not search for a
font or replace missing glyphs. The implementation needs HarfBuzz 14.2 or newer
with both `libharfbuzz` and `libharfbuzz-vector` installed. It was verified
against 14.5.0. On macOS, the native test runs with Homebrew HarfBuzz and the
system Arial, Baskerville, STIX, and Apple Color Emoji fonts.

```
FONT:OPEN    ( ptr u8 n n -- FONT:font )   \ font bytes, byte count, face index
FONT:RENDER  ( FONT:font ptr u8 n n -- FONT:image )  \ UTF-8 bytes, em size
FONT:SVG$    ( FONT:image -- ptr u8 n )
FONT:BOUNDS  ( FONT:image -- r r r r )    \ SVG x, y, width, height
FONT:FREE    ( FONT:image -- )
FONT:CLOSE   ( FONT:font -- )
```

`OPEN` retains its own copy of the font bytes, so the caller may release the
input after it returns. `SVG$` borrows bytes owned by the image; use them before
`FREE`. The bounds use SVG's downward Y axis. The em size is a positive integer
at most `0x1ffffff`; the font scale uses 26.6 coordinates. A whitespace-only
run yields a valid zero-size SVG.

This boundary accepts one left-to-right script per run, including its common
punctuation and combining marks. It rejects malformed UTF-8, controls and
format characters, mixed or right-to-left scripts, missing glyphs, and glyphs
represented by COLR, SVG, or sbix/CBDT color data. Those cases raise `E-TEXT`
or `E-GLYPH`; a malformed font raises `E-OPEN`. A caller may split a label into
runs or choose another face, but this library never does either implicitly.

The glyph outlines come from HarfBuzz's vector API. Each shaped glyph uses its
26.6 advance and offset. HarfBuzz's ink extents can fall inside its emitted
decimal path: Arial `o` at em 17 reaches SVG Y -9.0146 although the ink box
rounds outward only to -9. `FONT` reads the absolute `M/L/Q/C` points from the
first SVG render, unions their outward whole-unit bounds with the ink extents,
then renders the same shaped run with that viewBox. An unexpected path syntax or
a box outside exact binary32 integer coordinates is rejected. The native test
checks the clipping regression in TrueType and CFF fonts, a combining-mark
stack, a true `fi` ligature, missing and color-only glyph rejection, and image
capture/reopen.
The font file and HarfBuzz version are material inputs to SVG bytes: pin both
when replay must produce identical output.

Run the focused checks from the repository root on macOS:

```sh
bin/hb --load test/font-native.f
bin/hb --load tools/hb-build.f -- --repl test/font-image-subject.f -o build/font-image
build/font-image
```

HarfBuzz references: [shaping](https://harfbuzz.github.io/harfbuzz-hb-shape.html),
[buffer and glyph positions](https://harfbuzz.github.io/harfbuzz-hb-buffer.html),
[vector drawing and extents](https://harfbuzz.github.io/harfbuzz-hb-vector.html),
and [color glyph queries](https://harfbuzz.github.io/harfbuzz-hb-ot-color.html).
The [14.5.0 SVG draw backend](https://github.com/harfbuzz/harfbuzz/blob/14.5.0/src/hb-vector-draw.cc)
defines the serialized path commands and render behavior used at this boundary.
