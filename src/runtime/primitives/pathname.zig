const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const objects = @import("../objects.zig");
const Pathname = objects.Pathname;

/// Create a pathname from components
pub fn makePathname(
    allocator: std.mem.Allocator,
    heap: *Heap,
    host: Value,
    device: Value,
    directory: Value,
    name: Value,
    ty: Value,
    version: Value,
) !Value {
    _ = allocator;
    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Get pathname host component
pub fn pathnameHost(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.host;
}

/// Get pathname device component
pub fn pathnameDevice(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.device;
}

/// Get pathname directory component
pub fn pathnameDirectory(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.directory;
}

/// Get pathname name component
pub fn pathnameName(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.name;
}

/// Get pathname type component
pub fn pathnameType(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.type;
}

/// Get pathname version component
pub fn pathnameVersion(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.version;
}
