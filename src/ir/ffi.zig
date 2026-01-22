//! Zig FFI for Habu
//!
//! Seamless interop between Habu and Zig:
//! - Call Zig functions from Habu with automatic type marshaling
//! - Wrap Habu closures for Zig callbacks
//! - Type-safe conversions between Value and Zig types

const std = @import("std");
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = @import("../runtime/heap.zig").Heap;
const objects = @import("../runtime/objects.zig");

/// FFI error types
pub const FfiError = error{
    TypeMismatch,
    NullPointer,
    OutOfMemory,
    InvalidArgCount,
};

/// Convert Habu Value to Zig type
pub fn toZig(comptime T: type, val: Value, heap: *Heap) FfiError!T {
    return switch (@typeInfo(T)) {
        .int => |info| {
            if (!val.isFixnum()) return FfiError.TypeMismatch;
            const n = val.toFixnum();
            if (info.signedness == .signed) {
                return @intCast(n);
            } else {
                if (n < 0) return FfiError.TypeMismatch;
                return @intCast(n);
            }
        },
        .float => {
            if (val.isFixnum()) {
                return @floatFromInt(val.toFixnum());
            } else if (val.isFloat()) {
                return @floatCast(val.toFloat());
            }
            return FfiError.TypeMismatch;
        },
        .bool => {
            return !val.isNil();
        },
        .pointer => |ptr| {
            if (ptr.size == .slice and ptr.child == u8) {
                // []const u8 - string
                if (val.isString()) {
                    const str = val.toPtr(objects.String);
                    return str.bytes();
                } else if (val.isSymbol()) {
                    const sym = val.toPtr(objects.Symbol);
                    return sym.getName();
                }
                return FfiError.TypeMismatch;
            } else if (ptr.child == Value) {
                // *Value - pass through
                return @ptrCast(@alignCast(val.toPtr(anyopaque)));
            }
            return FfiError.TypeMismatch;
        },
        .optional => |opt| {
            if (val.isNil()) return null;
            return try toZig(opt.child, val, heap);
        },
        .void => return {},
        else => @compileError("Unsupported type for FFI: " ++ @typeName(T)),
    };
}

/// Convert Zig type to Habu Value
pub fn fromZig(comptime T: type, val: T, heap: *Heap) FfiError!Value {
    return switch (@typeInfo(T)) {
        .int => Value.makeFixnum(@intCast(val)),
        .float => Value.makeFloat(@floatCast(val)),
        .bool => if (val) Value.t else Value.nil,
        .pointer => |ptr| {
            if (ptr.size == .slice and ptr.child == u8) {
                // []const u8 - create string
                return heap.allocBaseString(val) catch return FfiError.OutOfMemory;
            } else if (ptr.child == Value) {
                return val.*;
            }
            return FfiError.TypeMismatch;
        },
        .optional => |opt| {
            if (val) |v| {
                return try fromZig(opt.child, v, heap);
            }
            return Value.nil;
        },
        .void => Value.nil,
        .@"struct" => |s| {
            var result = Value.nil;
            inline for (s.fields) |field| {
                const field_name = try heap.allocBaseString(field.name) catch return FfiError.OutOfMemory;
                const field_val = try fromZig(field.type, @field(val, field.name), heap);
                const pair = try heap.allocCons(field_name, field_val) catch return FfiError.OutOfMemory;
                result = try heap.allocCons(pair, result) catch return FfiError.OutOfMemory;
            }
            return result;
        },
        else => @compileError("Unsupported type for FFI: " ++ @typeName(T)),
    };
}

/// FFI function wrapper - wraps a Zig function for calling from Habu
pub fn FfiFunc(comptime func: anytype) type {
    const FnInfo = @typeInfo(@TypeOf(func)).@"fn";
    const ReturnType = FnInfo.return_type.?;
    const ParamTypes = FnInfo.params;

    return struct {
        /// Call the wrapped function with Habu arguments
        pub fn call(args: []const Value, heap: *Heap) FfiError!Value {
            if (args.len != ParamTypes.len) {
                return FfiError.InvalidArgCount;
            }

            // Convert arguments
            var zig_args: std.meta.ArgsTuple(@TypeOf(func)) = undefined;
            inline for (ParamTypes, 0..) |param, i| {
                zig_args[i] = try toZig(param.type.?, args[i], heap);
            }

            // Call function
            const result = @call(.auto, func, zig_args);

            // Handle error unions
            const actual_result = if (@typeInfo(ReturnType) == .error_union)
                result catch return FfiError.TypeMismatch
            else
                result;

            // Convert result
            const ResultType = if (@typeInfo(ReturnType) == .error_union)
                @typeInfo(ReturnType).error_union.payload
            else
                ReturnType;

            return try fromZig(ResultType, actual_result, heap);
        }
    };
}

/// Native function table entry
pub const NativeFunc = struct {
    name: []const u8,
    func: *const fn ([]const Value, *Heap) FfiError!Value,
    arity: u8,
};

/// Registry of native functions
pub const NativeRegistry = struct {
    funcs: std.StringHashMap(NativeFunc),

    pub fn init(allocator: std.mem.Allocator) NativeRegistry {
        return .{
            .funcs = std.StringHashMap(NativeFunc).init(allocator),
        };
    }

    pub fn deinit(self: *NativeRegistry) void {
        self.funcs.deinit();
    }

    /// Register a native function
    pub fn register(self: *NativeRegistry, name: []const u8, comptime func: anytype) !void {
        const Wrapper = FfiFunc(func);
        const FnInfo = @typeInfo(@TypeOf(func)).@"fn";

        try self.funcs.put(name, .{
            .name = name,
            .func = &Wrapper.call,
            .arity = @intCast(FnInfo.params.len),
        });
    }

    /// Look up a native function
    pub fn get(self: *NativeRegistry, name: []const u8) ?NativeFunc {
        return self.funcs.get(name);
    }

    /// Call a native function by name
    pub fn call(self: *NativeRegistry, name: []const u8, args: []const Value, heap: *Heap) FfiError!Value {
        const func = self.get(name) orelse return FfiError.TypeMismatch;
        if (args.len != func.arity) return FfiError.InvalidArgCount;
        return func.func(args, heap);
    }
};

/// Callback wrapper - wraps a Habu closure for Zig to call
pub fn Callback(comptime ReturnType: type, comptime ArgTypes: anytype) type {
    return struct {
        closure: Value,
        heap: *Heap,
        vm: *@import("../interp/vm.zig").Vm,

        const Self = @This();

        pub fn init(closure: Value, heap: *Heap, vm: *@import("../interp/vm.zig").Vm) Self {
            return .{ .closure = closure, .heap = heap, .vm = vm };
        }

        pub fn call(self: Self, args: ArgTypes) !ReturnType {
            if (!self.closure.isClosure()) return FfiError.TypeMismatch;
            const closure_obj = self.closure.toPtr(objects.Closure);

            // Convert args to Values and push on stack
            inline for (@typeInfo(ArgTypes).@"struct".fields, 0..) |field, i| {
                const arg_val = try fromZig(field.type, @field(args, field.name), self.heap);
                self.vm.stack[i] = arg_val;
            }

            const argc: u8 = @intCast(@typeInfo(ArgTypes).@"struct".fields.len);
            const result = try self.vm.callClosure(closure_obj, argc);
            return try toZig(ReturnType, result, self.heap);
        }
    };
}

// ============================================================================
// Tests
// ============================================================================

fn testAdd(a: i64, b: i64) i64 {
    return a + b;
}

fn testConcat(s: []const u8) []const u8 {
    return s;
}

fn testNullable(x: ?i64) i64 {
    return x orelse 0;
}

test "toZig fixnum" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const val = Value.makeFixnum(42);
    const result = try toZig(i64, val, &heap);
    try testing.expectEqual(@as(i64, 42), result);
}

test "toZig bool" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    try testing.expect(try toZig(bool, Value.t, &heap));
    try testing.expect(!try toZig(bool, Value.nil, &heap));
    try testing.expect(try toZig(bool, Value.makeFixnum(0), &heap)); // 0 is truthy in Lisp
}

test "toZig string" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const str = try heap.allocBaseString("hello");
    const result = try toZig([]const u8, str, &heap);
    try testing.expectEqualStrings("hello", result);
}

test "fromZig fixnum" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const val = try fromZig(i64, 42, &heap);
    try testing.expect(val.isFixnum());
    try testing.expectEqual(@as(i64, 42), val.toFixnum());
}

test "fromZig string" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const val = try fromZig([]const u8, "world", &heap);
    try testing.expect(val.isString());
    try testing.expectEqualStrings("world", val.toPtr(objects.String).bytes());
}

test "FfiFunc wrapper" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Wrapper = FfiFunc(testAdd);
    const args = [_]Value{ Value.makeFixnum(10), Value.makeFixnum(32) };
    const result = try Wrapper.call(&args, &heap);

    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "NativeRegistry" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var registry = NativeRegistry.init(testing.allocator);
    defer registry.deinit();

    try registry.register("add", testAdd);

    const func = registry.get("add");
    try testing.expect(func != null);
    try testing.expectEqual(@as(u8, 2), func.?.arity);

    const args = [_]Value{ Value.makeFixnum(100), Value.makeFixnum(23) };
    const result = try registry.call("add", &args, &heap);
    try testing.expectEqual(@as(i64, 123), result.toFixnum());
}
