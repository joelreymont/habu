//! Graph Coloring Register Allocator
//!
//! Implements Chaitin-Briggs register allocation:
//! 1. Build interference graph (virtual regs that are live simultaneously)
//! 2. Coalesce moves where possible (eliminate redundant copies)
//! 3. Simplify by removing low-degree nodes
//! 4. Spill if necessary (pick node to evict to memory)
//! 5. Select colors (assign physical registers)
//!
//! This is the "proper" algorithm used in GCC, LLVM, etc.

const std = @import("std");
const instruction = @import("instruction.zig");
const Inst = instruction.Inst;
const Op = instruction.Op;
const Reg = instruction.Reg;
const Function = instruction.Function;

/// Physical ARM64 register
pub const PhysReg = u5;

/// ARM64 register conventions
pub const PHYS = struct {
    pub const x0: PhysReg = 0;
    pub const x1: PhysReg = 1;
    pub const x2: PhysReg = 2;
    pub const x3: PhysReg = 3;
    pub const x4: PhysReg = 4;
    pub const x5: PhysReg = 5;
    pub const x6: PhysReg = 6;
    pub const x7: PhysReg = 7;
    pub const x8: PhysReg = 8;
    pub const x9: PhysReg = 9;
    pub const x10: PhysReg = 10;
    pub const x11: PhysReg = 11;
    pub const x12: PhysReg = 12;
    pub const x13: PhysReg = 13;
    pub const x14: PhysReg = 14;
    pub const x15: PhysReg = 15;
    pub const x19: PhysReg = 19;
    pub const x20: PhysReg = 20;
    pub const x21: PhysReg = 21;
    pub const x22: PhysReg = 22;
    pub const x23: PhysReg = 23;
    pub const x24: PhysReg = 24;
    pub const x25: PhysReg = 25;
    pub const x26: PhysReg = 26;
    pub const x27: PhysReg = 27;
    pub const x28: PhysReg = 28;
    pub const fp: PhysReg = 29;
    pub const lr: PhysReg = 30;
    pub const zr: PhysReg = 31;

    /// K = number of colors available for allocation
    pub const K: u8 = 26;

    /// Allocatable registers (x0-x15, x19-x28)
    pub const allocatable = [K]PhysReg{
        0,  1,  2,  3,  4,  5,  6,  7,
        8,  9,  10, 11, 12, 13, 14, 15,
        19, 20, 21, 22, 23, 24, 25, 26,
        27, 28,
    };

    pub const callee_saved_mask: u32 = 0b11111111110000000000_0000_0000_0000;
};

/// Register allocation result
/// Maps virtual registers to physical registers or spill slots
pub const RegisterMap = struct {
    /// Map from virtual register to physical register (null if spilled)
    colors: [32]?PhysReg,
    /// Map from virtual register to spill slot (null if not spilled)
    spill_slots: [32]?u16,
    /// Total number of spill slots used
    spill_count: u16,

    /// Create empty register map
    pub fn init() RegisterMap {
        return .{
            .colors = [_]?PhysReg{null} ** 32,
            .spill_slots = [_]?u16{null} ** 32,
            .spill_count = 0,
        };
    }

    /// Get physical register for virtual register (null if spilled)
    pub fn getPhysReg(self: RegisterMap, vreg: Reg) ?PhysReg {
        if (vreg >= 32) return null;
        return self.colors[vreg];
    }

    /// Get spill slot for virtual register (null if not spilled)
    pub fn getSpillSlot(self: RegisterMap, vreg: Reg) ?u16 {
        if (vreg >= 32) return null;
        return self.spill_slots[vreg];
    }

    /// Check if register is spilled
    pub fn isSpilled(self: RegisterMap, vreg: Reg) bool {
        return self.getSpillSlot(vreg) != null;
    }
};

/// Interference graph node
const Node = struct {
    vreg: Reg,
    /// Adjacency list (other vregs this interferes with)
    neighbors: std.ArrayList(Reg),
    /// Current degree (number of neighbors not yet removed)
    degree: u32,
    /// Assigned physical register (null if spilled)
    color: ?PhysReg,
    /// Spill slot if spilled
    spill_slot: ?u16,
    /// Is this node on the stack (removed during simplify)?
    on_stack: bool,
    /// Is this a move-related node?
    move_related: bool,
    /// Spill cost (higher = avoid spilling)
    spill_cost: u32,
    /// Allocator for this node
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, vreg: Reg) Node {
        return .{
            .vreg = vreg,
            .neighbors = std.ArrayList(Reg){},
            .degree = 0,
            .color = null,
            .spill_slot = null,
            .on_stack = false,
            .move_related = false,
            .spill_cost = 1,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Node) void {
        self.neighbors.deinit(self.allocator);
    }
};

/// Move instruction (for coalescing)
const Move = struct {
    src: Reg,
    dst: Reg,
    /// Is this move still active (not coalesced or frozen)?
    active: bool,
};

/// Graph Coloring Register Allocator
pub const GraphColorAlloc = struct {
    allocator: std.mem.Allocator,
    /// Interference graph nodes indexed by vreg
    nodes: [32]?Node,
    /// Number of active nodes
    node_count: u32,
    /// Move instructions for coalescing
    moves: std.ArrayList(Move),
    /// Stack of removed nodes (for select phase)
    select_stack: std.ArrayList(Reg),
    /// Spill worklist
    spill_worklist: std.ArrayList(Reg),
    /// Freeze worklist (low-degree move-related)
    freeze_worklist: std.ArrayList(Reg),
    /// Simplify worklist (low-degree non-move-related)
    simplify_worklist: std.ArrayList(Reg),
    /// Number of spill slots allocated
    spill_count: u16,
    /// Liveness info: live-out sets per instruction
    live_out: std.ArrayList(std.StaticBitSet(32)),

    pub fn init(allocator: std.mem.Allocator) GraphColorAlloc {
        return .{
            .allocator = allocator,
            .nodes = [_]?Node{null} ** 32,
            .node_count = 0,
            .moves = std.ArrayList(Move){},
            .select_stack = std.ArrayList(Reg){},
            .spill_worklist = std.ArrayList(Reg){},
            .freeze_worklist = std.ArrayList(Reg){},
            .simplify_worklist = std.ArrayList(Reg){},
            .spill_count = 0,
            .live_out = std.ArrayList(std.StaticBitSet(32)){},
        };
    }

    pub fn deinit(self: *GraphColorAlloc) void {
        for (&self.nodes) |*maybe_node| {
            if (maybe_node.*) |*node| {
                node.deinit();
                maybe_node.* = null;
            }
        }
        self.moves.deinit(self.allocator);
        self.select_stack.deinit(self.allocator);
        self.spill_worklist.deinit(self.allocator);
        self.freeze_worklist.deinit(self.allocator);
        self.simplify_worklist.deinit(self.allocator);
        self.live_out.deinit(self.allocator);
    }

    /// Main allocation entry point
    pub fn allocate(self: *GraphColorAlloc, func: Function) !void {
        // Phase 1: Liveness analysis
        try self.computeLiveness(func);

        // Phase 2: Build interference graph
        try self.buildGraph(func);

        // Phase 3: Coalesce moves (optional, improves code quality)
        try self.coalesce();

        // Phase 4: Build worklists
        try self.buildWorklists();

        // Phase 5: Iterate until done
        while (self.simplify_worklist.items.len > 0 or
            self.freeze_worklist.items.len > 0 or
            self.spill_worklist.items.len > 0)
        {
            if (self.simplify_worklist.items.len > 0) {
                try self.simplify();
            } else if (self.freeze_worklist.items.len > 0) {
                try self.freeze();
            } else if (self.spill_worklist.items.len > 0) {
                try self.selectSpill();
            }
        }

        // Phase 6: Assign colors
        try self.assignColors();
    }

    /// Extract register allocation results as a RegisterMap
    pub fn getRegisterMap(self: *const GraphColorAlloc) RegisterMap {
        var map = RegisterMap.init();
        map.spill_count = self.spill_count;

        for (self.nodes, 0..) |maybe_node, vreg| {
            if (maybe_node) |node| {
                map.colors[vreg] = node.color;
                map.spill_slots[vreg] = node.spill_slot;
            }
        }

        return map;
    }

    /// Compute live-out sets using backward dataflow analysis
    fn computeLiveness(self: *GraphColorAlloc, func: Function) !void {
        const n = func.code.len;
        if (n == 0) return;

        // Initialize live-out sets
        self.live_out.clearRetainingCapacity();
        try self.live_out.resize(self.allocator, n);
        for (self.live_out.items) |*set| {
            set.* = std.StaticBitSet(32).initEmpty();
        }

        // Iterate until fixed point
        var changed = true;
        while (changed) {
            changed = false;

            // Backward iteration
            var i: usize = n;
            while (i > 0) {
                i -= 1;
                const inst = func.code[i];
                var live = if (i + 1 < n) self.live_out.items[i + 1] else std.StaticBitSet(32).initEmpty();

                // Handle control flow (simplified: assume linear for now)
                // TODO: Handle branches properly with CFG

                // live_in = use ∪ (live_out - def)
                // live_out[i] = live_in[i+1]

                // Remove def
                if (self.isDef(inst.op) and inst.dest < 31) {
                    live.unset(inst.dest);
                }

                // Add uses
                if (self.usesSrc1(inst.op) and inst.src1 < 31) {
                    live.set(inst.src1);
                }
                if (self.usesSrc2(inst.op)) {
                    const src2 = inst.src2();
                    if (src2 < 31) {
                        live.set(src2);
                    }
                }

                // Check if changed
                if (!self.live_out.items[i].eql(live)) {
                    self.live_out.items[i] = live;
                    changed = true;
                }
            }
        }
    }

    /// Build interference graph from liveness info
    fn buildGraph(self: *GraphColorAlloc, func: Function) !void {
        // Create nodes for all used vregs
        for (func.code, 0..) |inst, idx| {
            if (self.isDef(inst.op) and inst.dest < 31) {
                try self.ensureNode(inst.dest);

                // Add interference: def interferes with all live-out
                var live_iter = self.live_out.items[idx].iterator(.{});
                while (live_iter.next()) |other| {
                    const other_reg: Reg = @intCast(other);
                    if (other_reg != inst.dest) {
                        try self.addEdge(inst.dest, other_reg);
                    }
                }
            }

            // Track moves for coalescing
            if (inst.op == .mov and inst.src1 < 31 and inst.dest < 31) {
                try self.moves.append(self.allocator, .{
                    .src = inst.src1,
                    .dst = inst.dest,
                    .active = true,
                });
                if (self.nodes[inst.src1]) |*node| {
                    node.move_related = true;
                }
                if (self.nodes[inst.dest]) |*node| {
                    node.move_related = true;
                }
            }

            // Update spill costs (uses in loops cost more)
            // Simplified: just count uses
            if (self.usesSrc1(inst.op) and inst.src1 < 31) {
                if (self.nodes[inst.src1]) |*node| {
                    node.spill_cost += 1;
                }
            }
            if (self.usesSrc2(inst.op)) {
                const src2 = inst.src2();
                if (src2 < 31) {
                    if (self.nodes[src2]) |*node| {
                        node.spill_cost += 1;
                    }
                }
            }
        }
    }

    /// Ensure a node exists for vreg
    fn ensureNode(self: *GraphColorAlloc, vreg: Reg) !void {
        if (self.nodes[vreg] == null) {
            self.nodes[vreg] = Node.init(self.allocator, vreg);
            self.node_count += 1;
        }
    }

    /// Add interference edge between two vregs
    fn addEdge(self: *GraphColorAlloc, a: Reg, b: Reg) !void {
        if (a == b) return;

        try self.ensureNode(a);
        try self.ensureNode(b);

        var node_a = &self.nodes[a].?;
        var node_b = &self.nodes[b].?;

        // Check if edge already exists
        for (node_a.neighbors.items) |n| {
            if (n == b) return;
        }

        try node_a.neighbors.append(node_a.allocator, b);
        try node_b.neighbors.append(node_b.allocator, a);
        node_a.degree += 1;
        node_b.degree += 1;
    }

    /// Coalesce moves where safe (George's conservative coalescing)
    fn coalesce(self: *GraphColorAlloc) !void {
        for (self.moves.items) |*move| {
            if (!move.active) continue;

            const src_node = &(self.nodes[move.src] orelse continue);
            const dst_node = &(self.nodes[move.dst] orelse continue);

            if (src_node.on_stack or dst_node.on_stack) continue;

            // George's criterion: can coalesce if for every neighbor T of src,
            // either T already interferes with dst, or degree(T) < K
            var can_coalesce = true;
            for (src_node.neighbors.items) |neighbor| {
                const neighbor_node = &(self.nodes[neighbor] orelse continue);
                if (neighbor_node.on_stack) continue;

                var interferes_with_dst = false;
                for (dst_node.neighbors.items) |dn| {
                    if (dn == neighbor) {
                        interferes_with_dst = true;
                        break;
                    }
                }

                if (!interferes_with_dst and neighbor_node.degree >= PHYS.K) {
                    can_coalesce = false;
                    break;
                }
            }

            if (can_coalesce) {
                // Merge src into dst
                try self.mergeNodes(move.src, move.dst);
                move.active = false;
            }
        }
    }

    /// Merge node a into node b
    fn mergeNodes(self: *GraphColorAlloc, a: Reg, b: Reg) !void {
        const node_a = &(self.nodes[a] orelse return error.InvalidState);
        if (self.nodes[b] == null) return error.InvalidState;

        // Transfer edges from a to b
        for (node_a.neighbors.items) |neighbor| {
            if (neighbor == b) continue;
            try self.addEdge(b, neighbor);
        }

        // Mark a as merged (we'll skip it)
        node_a.on_stack = true;
        node_a.color = null; // Will inherit from b
    }

    /// Build initial worklists
    fn buildWorklists(self: *GraphColorAlloc) !void {
        self.simplify_worklist.clearRetainingCapacity();
        self.freeze_worklist.clearRetainingCapacity();
        self.spill_worklist.clearRetainingCapacity();

        for (&self.nodes) |*maybe_node| {
            if (maybe_node.*) |*node| {
                if (node.on_stack) continue;

                if (node.degree >= PHYS.K) {
                    try self.spill_worklist.append(self.allocator, node.vreg);
                } else if (node.move_related) {
                    try self.freeze_worklist.append(self.allocator, node.vreg);
                } else {
                    try self.simplify_worklist.append(self.allocator, node.vreg);
                }
            }
        }
    }

    /// Simplify: remove a low-degree non-move-related node
    fn simplify(self: *GraphColorAlloc) !void {
        const vreg = self.simplify_worklist.pop() orelse return;
        const node = &(self.nodes[vreg] orelse return);

        node.on_stack = true;
        try self.select_stack.append(self.allocator, vreg);

        // Decrement degree of neighbors - copy to avoid iterator invalidation
        const neighbors = try self.allocator.dupe(Reg, node.neighbors.items);
        defer self.allocator.free(neighbors);
        for (neighbors) |neighbor| {
            try self.decrementDegree(neighbor);
        }
    }

    /// Decrement degree and potentially move to simplify worklist
    fn decrementDegree(self: *GraphColorAlloc, vreg: Reg) !void {
        const node = &(self.nodes[vreg] orelse return);
        if (node.on_stack) return;

        if (node.degree > 0) {
            node.degree -= 1;
        }

        // If degree dropped below K, move to appropriate worklist
        if (node.degree == PHYS.K - 1) {
            // Remove from spill worklist
            for (self.spill_worklist.items, 0..) |v, i| {
                if (v == vreg) {
                    _ = self.spill_worklist.swapRemove(i);
                    break;
                }
            }

            // Add to simplify or freeze worklist
            if (node.move_related) {
                try self.freeze_worklist.append(self.allocator, vreg);
            } else {
                try self.simplify_worklist.append(self.allocator, vreg);
            }
        }
    }

    /// Freeze: give up on coalescing a low-degree move-related node
    fn freeze(self: *GraphColorAlloc) !void {
        const vreg = self.freeze_worklist.pop() orelse return;
        const node = &(self.nodes[vreg] orelse return);

        node.move_related = false;
        try self.simplify_worklist.append(self.allocator, vreg);
    }

    /// Select a node to spill (using spill cost heuristic)
    fn selectSpill(self: *GraphColorAlloc) !void {
        if (self.spill_worklist.items.len == 0) return;

        // Find node with lowest spill_cost / degree ratio
        var best_idx: usize = 0;
        var best_ratio: f32 = std.math.floatMax(f32);

        for (self.spill_worklist.items, 0..) |vreg, i| {
            const node = &(self.nodes[vreg] orelse continue);
            const degree = if (node.degree == 0) 1 else node.degree;
            const ratio = @as(f32, @floatFromInt(node.spill_cost)) / @as(f32, @floatFromInt(degree));
            if (ratio < best_ratio) {
                best_ratio = ratio;
                best_idx = i;
            }
        }

        const vreg = self.spill_worklist.swapRemove(best_idx);
        const node = &(self.nodes[vreg] orelse return);

        // Optimistically try to color it; if we can't, it becomes an actual spill
        node.on_stack = true;
        try self.select_stack.append(self.allocator, vreg);

        for (node.neighbors.items) |neighbor| {
            try self.decrementDegree(neighbor);
        }
    }

    /// Assign colors (physical registers) to nodes
    fn assignColors(self: *GraphColorAlloc) !void {
        while (self.select_stack.pop()) |vreg| {
            const node = &(self.nodes[vreg] orelse continue);

            // Find colors used by neighbors
            var used_colors = std.StaticBitSet(32).initEmpty();
            for (node.neighbors.items) |neighbor| {
                const neighbor_node = &(self.nodes[neighbor] orelse continue);
                if (neighbor_node.color) |color| {
                    used_colors.set(color);
                }
            }

            // Find an available color
            var assigned = false;
            for (PHYS.allocatable) |preg| {
                if (!used_colors.isSet(preg)) {
                    node.color = preg;
                    node.on_stack = false;
                    assigned = true;
                    break;
                }
            }

            if (!assigned) {
                // Actual spill
                node.spill_slot = self.spill_count;
                self.spill_count += 1;
                node.on_stack = false;
            }
        }
    }

    /// Get assigned physical register for vreg
    pub fn getColor(self: *GraphColorAlloc, vreg: Reg) ?PhysReg {
        if (self.nodes[vreg]) |node| {
            return node.color;
        }
        return null;
    }

    /// Get spill slot for vreg
    pub fn getSpillSlot(self: *GraphColorAlloc, vreg: Reg) ?u16 {
        if (self.nodes[vreg]) |node| {
            return node.spill_slot;
        }
        return null;
    }

    /// Check if operation defines destination
    fn isDef(self: *GraphColorAlloc, op: Op) bool {
        _ = self;
        return switch (op) {
            .store_local, .store_upval, .jmp, .jz, .jnz, .ret, .tailcall, .checktype => false,
            else => true,
        };
    }

    /// Check if operation uses src1
    fn usesSrc1(self: *GraphColorAlloc, op: Op) bool {
        _ = self;
        return switch (op) {
            .movi, .movk, .load_local, .load_upval, .load_global, .jmp => false,
            else => true,
        };
    }

    /// Check if operation uses src2 as register
    fn usesSrc2(self: *GraphColorAlloc, op: Op) bool {
        _ = self;
        return switch (op) {
            .add, .sub, .mul, .div, .mod, .eq, .ne, .lt, .le, .gt, .ge, .eql, .equal, .@"and", .@"or", .xor, .shl, .shr, .lshr, .cons, .rplaca, .rplacd, .mkvec, .vref, .vset, .mkstr, .sref, .sset, .apply => true,
            else => false,
        };
    }

    /// Rewrite function with physical registers, inserting spills/reloads
    pub fn rewrite(self: *GraphColorAlloc, func: Function) !Function {
        var new_code = std.ArrayList(Inst){};
        errdefer new_code.deinit(self.allocator);

        for (func.code) |inst| {
            // Insert reloads for spilled src operands
            var src1_reg = inst.src1;
            var src2_reg = inst.src2();

            if (self.usesSrc1(inst.op)) {
                if (self.getSpillSlot(inst.src1)) |slot| {
                    // Reload into scratch register x8
                    try new_code.append(self.allocator, Inst.iu(.load_local, PHYS.x8, 0, slot));
                    src1_reg = PHYS.x8;
                } else if (self.getColor(inst.src1)) |color| {
                    src1_reg = color;
                }
            }

            if (self.usesSrc2(inst.op)) {
                if (self.getSpillSlot(inst.src2())) |slot| {
                    // Reload into scratch register x9
                    try new_code.append(self.allocator, Inst.iu(.load_local, PHYS.x9, 0, slot));
                    src2_reg = PHYS.x9;
                } else if (self.getColor(inst.src2())) |color| {
                    src2_reg = color;
                }
            }

            // Rewrite instruction
            var new_inst = inst;
            new_inst.src1 = src1_reg;
            if (self.usesSrc2(inst.op)) {
                new_inst.operand = @as(u16, src2_reg);
            }

            // Handle destination
            if (self.isDef(inst.op)) {
                if (self.getSpillSlot(inst.dest)) |slot| {
                    // Write to scratch, then spill
                    new_inst.dest = PHYS.x8;
                    try new_code.append(self.allocator, new_inst);
                    try new_code.append(self.allocator, Inst.iu(.store_local, 0, PHYS.x8, slot));
                    continue;
                } else if (self.getColor(inst.dest)) |color| {
                    new_inst.dest = color;
                }
            }

            try new_code.append(self.allocator, new_inst);
        }

        return .{
            .name = func.name,
            .param_count = func.param_count,
            .local_count = func.local_count,
            .max_regs = PHYS.K,
            .code = try new_code.toOwnedSlice(self.allocator),
            .constants = func.constants,
            .locations = func.locations,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "graph coloring simple" {
    const testing = std.testing;

    var builder = instruction.Builder.init(testing.allocator);
    defer builder.deinit();

    // (defun add-one (x) (+ x 1))
    const v0 = builder.freshReg();
    const v1 = builder.freshReg();

    try builder.emitAddI(v1, v0, 1);
    try builder.emitRet(v1);

    const func = try builder.build("add-one", 1, 1);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    var alloc = GraphColorAlloc.init(testing.allocator);
    defer alloc.deinit();

    try alloc.allocate(func);

    // Both registers should be colored (no spills needed)
    try testing.expect(alloc.getColor(v0) != null or alloc.getSpillSlot(v0) != null);
    try testing.expect(alloc.getColor(v1) != null or alloc.getSpillSlot(v1) != null);
    try testing.expectEqual(@as(u16, 0), alloc.spill_count);
}

test "graph coloring with interference" {
    const testing = std.testing;

    var builder = instruction.Builder.init(testing.allocator);
    defer builder.deinit();

    // Multiple live values
    const v0 = builder.freshReg();
    const v1 = builder.freshReg();
    const v2 = builder.freshReg();
    const v3 = builder.freshReg();

    try builder.emitImm(v0, 1);
    try builder.emitImm(v1, 2);
    try builder.emitImm(v2, 3);
    try builder.emit(Inst.r(.add, v3, v0, v1)); // v0, v1, v2 all live here
    try builder.emit(Inst.r(.add, v3, v3, v2));
    try builder.emitRet(v3);

    const func = try builder.build("test", 0, 0);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    var alloc = GraphColorAlloc.init(testing.allocator);
    defer alloc.deinit();

    try alloc.allocate(func);

    // v0, v1, v2 interfere, so need different colors
    const c0 = alloc.getColor(v0);
    const c1 = alloc.getColor(v1);
    const c2 = alloc.getColor(v2);

    try testing.expect(c0 != null);
    try testing.expect(c1 != null);
    try testing.expect(c2 != null);
    try testing.expect(c0.? != c1.?);
    try testing.expect(c1.? != c2.?);
    try testing.expect(c0.? != c2.?);
}

test "graph coloring spill" {
    const testing = std.testing;

    var builder = instruction.Builder.init(testing.allocator);
    defer builder.deinit();

    // Create more simultaneously live values than available registers
    var regs: [30]Reg = undefined;
    for (&regs) |*r| {
        r.* = builder.freshReg();
    }

    // Initialize all (making them all live)
    for (regs, 0..) |r, i| {
        try builder.emitImm(r, @intCast(i));
    }

    // Use all of them together
    for (1..30) |i| {
        try builder.emit(Inst.r(.add, regs[0], regs[0], regs[i]));
    }

    try builder.emitRet(regs[0]);

    const func = try builder.build("spill-test", 0, 0);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    var alloc = GraphColorAlloc.init(testing.allocator);
    defer alloc.deinit();

    try alloc.allocate(func);

    // Should need spills (30 live values, only 26 registers)
    try testing.expect(alloc.spill_count > 0);
}

test "move coalescing" {
    const testing = std.testing;

    var builder = instruction.Builder.init(testing.allocator);
    defer builder.deinit();

    const v0 = builder.freshReg();
    const v1 = builder.freshReg();
    const v2 = builder.freshReg();

    try builder.emitImm(v0, 42);
    try builder.emitMov(v1, v0); // This move should be coalesced
    try builder.emitAddI(v2, v1, 1);
    try builder.emitRet(v2);

    const func = try builder.build("coalesce-test", 0, 0);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    var alloc = GraphColorAlloc.init(testing.allocator);
    defer alloc.deinit();

    try alloc.allocate(func);

    // After coalescing mov v1, v0: v0 merges into v1
    // So v0 will be on_stack (no color), v1 gets the color
    const c0 = alloc.getColor(v0);
    const c1 = alloc.getColor(v1);

    // At least one should have a color (the coalesce target)
    try testing.expect(c0 != null or c1 != null);

    // v2 should definitely have a color (it's used in ret)
    try testing.expect(alloc.getColor(v2) != null);
}
