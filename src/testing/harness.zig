const std = @import("std");

const runtime = @import("../runtime/runtime.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const interp = @import("../interp/interp.zig");
const io = @import("../runtime/primitives/io.zig");

const compile_chunk = @import("compile_chunk.zig");

pub const Heap = runtime.Heap;
pub const Value = runtime.Value;
pub const Chunk = runtime.Chunk;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;

pub const Runner = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: Vm,
    comp: Compiler,
    chunk_pool: std.ArrayList(*Chunk),

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) !Runner {
        var vm = try Vm.init(allocator, heap);
        errdefer vm.deinit();

        var comp = try Compiler.initWithHeap(allocator, &vm);
        errdefer comp.deinit();

        vm.setGlobalEnv(&comp.globals);

        var chunk_pool = std.ArrayList(*Chunk){};
        errdefer chunk_pool.deinit(allocator);
        vm.setChunkPool(chunk_pool.items);

        return .{
            .allocator = allocator,
            .heap = heap,
            .vm = vm,
            .comp = comp,
            .chunk_pool = chunk_pool,
        };
    }

    pub fn deinit(self: *Runner) void {
        self.chunk_pool.deinit(self.allocator);
        self.comp.deinit();
        self.vm.deinit();
    }

    pub fn compile(self: *Runner, source: []const u8) !*Chunk {
        return try compile_chunk.compileChunk(self.allocator, self.heap, &self.vm, &self.comp, &self.chunk_pool, source);
    }

    pub fn run(self: *Runner, chunk: *const Chunk) !Value {
        return try self.vm.run(chunk);
    }

    pub fn enableJit(self: *Runner, code_buf_size: usize, hot: u32) !void {
        return try self.vm.enableJit(code_buf_size, hot);
    }
};

pub fn eval(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var r = try Runner.init(allocator, heap);
    defer r.deinit();

    const chunk = try r.compile(source);
    return try r.run(chunk);
}

pub fn valueToString(allocator: std.mem.Allocator, val: Value) ![]u8 {
    var buf = std.ArrayList(u8){};
    errdefer buf.deinit(allocator);

    var w = buf.writer(allocator);
    try io.writeValueToBuffer(val, w.any());
    return try buf.toOwnedSlice(allocator);
}

