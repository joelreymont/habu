const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main executable
    const exe = b.addExecutable(.{
        .name = "habu",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Link Z3 for SMT solving (refinement types)
    exe.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    exe.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    exe.root_module.linkSystemLibrary("z3", .{});
    exe.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(exe);

    // Debug flet executable
    const debug_flet = b.addExecutable(.{
        .name = "debug_flet",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/debug_flet.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    debug_flet.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    debug_flet.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    debug_flet.root_module.linkSystemLibrary("z3", .{});
    debug_flet.root_module.linkSystemLibrary("c", .{});
    b.installArtifact(debug_flet);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the Habu REPL");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const lib_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lib.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Add ohsnap for snapshot testing
    if (b.lazyDependency("ohsnap", .{
        .target = target,
        .optimize = optimize,
    })) |ohsnap_dep| {
        lib_tests.root_module.addImport("ohsnap", ohsnap_dep.module("ohsnap"));
    }

    // Link Z3 for SMT solving (refinement types)
    // Z3 is optional - tests that don't use it will still work
    lib_tests.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    lib_tests.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    lib_tests.root_module.linkSystemLibrary("z3", .{});
    lib_tests.root_module.linkSystemLibrary("c", .{});

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(lib_tests).step);

    // Error masking check
    const check_errors = b.addSystemCommand(&.{
        "sh",
        "-c",
        "if grep -rn 'catch return null\\|catch return;\\|orelse unreachable\\|catch |_|' src --exclude-dir=tests --exclude='*.md' | grep -v test.zig | grep -v lineedit.zig | grep -v vector.zig; then echo 'ERROR: Found error masking patterns in src/' && exit 1; fi",
    });
    const check_step = b.step("check-errors", "Check for error masking patterns");
    check_step.dependOn(&check_errors.step);
    test_step.dependOn(&check_errors.step);

    // GC Benchmark
    const gc_bench = b.addExecutable(.{
        .name = "gc_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/gc.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(gc_bench);

    const bench_run_cmd = b.addRunArtifact(gc_bench);
    bench_run_cmd.step.dependOn(b.getInstallStep());
    const bench_step = b.step("bench", "Run GC benchmark");
    bench_step.dependOn(&bench_run_cmd.step);
}
