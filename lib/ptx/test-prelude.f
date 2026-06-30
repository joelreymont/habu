\ test-prelude.f - shared setup for PTX stdlib tests.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-vec.f
require lib/ptx/header.f
require lib/ptx/launch.f
require lib/ptx/cg-collective.f
require lib/ptx/tile.f
require lib/ptx/tile-loop.f
require lib/ptx/tile-smem.f
require lib/ptx/tile-acc.f
require lib/ptx/tile-v4.f
require lib/ptx/collective.f
require lib/ptx/ir.f
require lib/ptx/ad.f
require lib/ptx/ad-dag.f
require lib/ptx/ad-saved.f
