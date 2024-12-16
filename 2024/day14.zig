const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 4) {
        std.debug.print("Usage: {s} <filename> <max_x> <max_y>\n", .{args[0]});
        return error.InvalidArgCount;
    }

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const robots = try readCoordinates(allocator, args[1]);
    defer allocator.free(robots);

    const gridSize: Point = Point{
        .x = try std.fmt.parseInt(i32, args[2], 10),
        .y = try std.fmt.parseInt(i32, args[3], 10)
    };
    const steps = 100;

    const part1 = safety_factor(robots, gridSize, steps);
    std.debug.print("Safety factor: {d}\n", .{part1});

    var best_safety: i32 = 2<<29;
    var best_step: i32 = 0;

    var step: i32 = 0;
    while (step < gridSize.x) : (step += 1) {
        const safety = safety_factor(robots, gridSize, step);
        if (safety < best_safety) {
            best_safety = safety;
            best_step = step;
        }
    }

    std.debug.print("Best vertical safety factor: {d} at step {d}", .{best_safety, best_step});

    step = best_step;

    while (step < gridSize.y * gridSize.x) : (step += gridSize.x) {
        std.debug.print("Step {d}:\n", .{step});
        const safety = safety_factor(robots, gridSize, step);
        if (safety < best_safety) {
            best_safety = safety;
            best_step = step;
        }
    }

    std.debug.print("\nMaximal configuration at step {d}:\n", .{best_step});
    try printGrid(allocator, robots, gridSize, best_step);
    std.debug.print("Christmas Trees are the NOT SAFE!: [{d}] {d}\n", .{best_step, best_safety});
}

pub fn safety_factor(robots: []Coordinates, gridSize: Point, step: i32) i32 {
    const mid_x = @divTrunc(gridSize.x, 2);
    const mid_y = @divTrunc(gridSize.y, 2);

    var quadrants = [4]i32{ 0, 0, 0, 0 };

    for (robots) |robot| {
        const final = positionAtStep(robot, step, gridSize);

        if (final.x < mid_x) {
            if (final.y < mid_y) {
                quadrants[0] += 1;
            } else if (final.y > mid_y) {
                quadrants[1] += 1;
            }
        } else if (final.x > mid_x) {
            if (final.y < mid_y) {
                quadrants[2] += 1;
            } else if (final.y > mid_y) {
                quadrants[3] += 1;
            }
        }
    }

    return quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3];
}

pub fn printGrid(allocator: std.mem.Allocator, robots: []const Coordinates, gridSize: Point, step: i32) !void {
    var grid = try allocator.alloc(u32, @intCast(gridSize.x * gridSize.y));
    defer allocator.free(grid);
    @memset(grid, 0);

    for (robots) |robot| {
        const p = positionAtStep(robot, step, gridSize);
        grid[@intCast(p.y * gridSize.x + p.x)] += 1;
    }

    var y: usize = 0;
    while (y < gridSize.y) : (y += 1) {
        var x: usize = 0;
        while (x < gridSize.x) : (x += 1) {
            const val = grid[y * @as(usize, @intCast(gridSize.x)) + x];
            if (val == 0) {
                std.debug.print(".", .{});
            } else {
                std.debug.print("{d}", .{val});
            }
        }
        std.debug.print("\n", .{});
    }
}

pub fn positionAtStep(robot: Coordinates, step: i32, gridSize: Point) Point {
    return Point{
        .x = @mod(robot.p.x + step * robot.v.x, gridSize.x),
        .y = @mod(robot.p.y + step * robot.v.y, gridSize.y),
    };
}

pub fn readCoordinates(allocator: std.mem.Allocator, filename: []const u8) ![]Coordinates {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();

    var coords_list = std.ArrayList(Coordinates).init(allocator);
    defer coords_list.deinit();

    var line_buf: [1024]u8 = undefined;

    while (try reader.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        const coords = try parseLine(line);
        try coords_list.append(coords);
    }

    return try coords_list.toOwnedSlice();
}

const Point = struct {
    x: i32,
    y: i32,
};

const Coordinates = struct {
    p: Point,
    v: Point,
};

fn parseLine(line: []const u8) !Coordinates {
    const p_pos = std.mem.indexOf(u8, line, "p=") orelse return error.InvalidFormat;
    const v_pos = std.mem.indexOf(u8, line, "v=") orelse return error.InvalidFormat;
    const comma1 = std.mem.indexOfPos(u8, line, p_pos, ",") orelse return error.InvalidFormat;
    const comma2 = std.mem.indexOfPos(u8, line, v_pos, ",") orelse return error.InvalidFormat;

    const px_str = line[p_pos + 2 .. comma1];
    const py_str = line[comma1 + 1 .. v_pos - 1];
    const vx_str = line[v_pos + 2 .. comma2];
    const vy_str = line[comma2 + 1 ..];

    const px = try std.fmt.parseInt(i32, px_str, 10);
    const py = try std.fmt.parseInt(i32, py_str, 10);
    const vx = try std.fmt.parseInt(i32, vx_str, 10);
    const vy = try std.fmt.parseInt(i32, vy_str, 10);

    return Coordinates{
        .p = Point{.x = px, .y = py},
        .v = Point{.x = vx, .y = vy},
    };
}
