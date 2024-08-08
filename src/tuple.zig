const std = @import("std");
const compfmt = std.fmt.comptimePrint;

pub fn Init(comptime any: anytype) type {
    return struct {
        pub const value = blk: {
            if (isEmpty(any)) break :blk .{};
            if (isRaw(any)) break :blk any;
            if (isStruct(any)) {
                if (@hasDecl(any, "value")) {
                    if (isRaw(any.value)) break :blk any.value;
                } else {
                    break :blk .{any};
                }
            } else {
                break :blk .{any};
            }
        };
    };
}

pub fn Append(comptime tuple: anytype, comptime val: anytype) type {
    return struct {
        pub const value = Init(tuple).value ++ Init(val).value;
    };
}

pub fn AppendUnique(comptime any: anytype, comptime any2: anytype) type {
    comptime var ret = Init(any);
    inline for (Init(any2).value) |v| {
        if (!existed(ret, v)) {
            ret = Append(ret, v);
        }
    }
    return ret;
}

pub fn Remove(comptime any: anytype, comptime any_remove: anytype) type {
    comptime var ret = Init(.{});
    const remove = Init(any_remove);
    inline for (Init(any).value) |v| {
        if (!existed(remove, v)) {
            ret = Append(ret, v);
        }
    }
    return ret;
}

pub fn Unique(comptime any: anytype) type {
    return AppendUnique(.{}, any);
}

pub fn Intersection(comptime any: anytype, comptime any2: anytype) type {
    return Remove(Init(any), Remove(Init(any), Init(any2)));
}

pub inline fn len(any: anytype) comptime_int {
    comptime {
        var count: comptime_int = 0;
        if (isEmpty(any) == false) {
            for (Init(any).value) |_| {
                count += 1;
            }
        }
        return count;
    }
}

pub inline fn existed(any: anytype, val: anytype) bool {
    comptime {
        for (Init(any).value) |v| {
            if (@TypeOf(val) == @TypeOf(v) and val == v) return true;
        }
        return false;
    }
}

pub inline fn isRaw(any: anytype) bool {
    return comptime switch (@typeInfo(@TypeOf(any))) {
        .Struct => |s| s.is_tuple,
        else => false,
    };
}

pub inline fn isEmpty(any: anytype) bool {
    return blk: {
        switch (@TypeOf(any)) {
            type => {
                if (any == void) break :blk true;
                if (isStruct(any) and @hasDecl(any, "value")) {
                    break :blk isEmpty(any.value);
                }
                break :blk false;
            },
            else => |t| break :blk t == @TypeOf(null) or t == @TypeOf(.{}),
        }
    };
}

inline fn isStruct(any: anytype) bool {
    return comptime @TypeOf(any) == type and @typeInfo(any) == .Struct;
}

test "tuple" {
    const t = std.testing;

    try t.expect(!isEmpty(u32));
    try t.expect(isEmpty(null));
    try t.expect(isEmpty(void));
    try t.expect(isEmpty(.{}));
    try t.expect(isEmpty(Init(.{})));
    try t.expect(!isEmpty(Init(1)));
    try t.expect((.{}).len == 0);
    try t.expect(len(Init(.{ 1, 4, .xx })) == 3);
    try t.expect(existed(.{1}, 1) == true);
    try t.expect(@TypeOf(Init(u32).value) == @TypeOf(.{u32}));
    try t.expect(@TypeOf(Init(.{u32}).value) == @TypeOf(.{u32}));
    try t.expect(@TypeOf(Init(void).value) == @TypeOf(.{}));
    const funcs = .{
        struct {
            pub fn func1() void {}
        },
        struct {
            pub fn func2() void {}
        },
    };
    const inited = Init(funcs);
    try t.expect(!isRaw(inited));
    try t.expect(isStruct(inited));
    try t.expect(isRaw(funcs));
    try t.expect(isRaw(inited.value));
    try t.expect(isRaw(.{}) == true);
    try t.expect(len(8) == 1);
    const rmval = Remove(.{ 1, 2, 3 }, 2).value;
    try t.expect(rmval[0] == 1 and rmval[1] == 3 and len(rmval) == 2);
    const unique = Unique(.{ 1, 2, 1, 3, 1 }).value;
    try t.expect(unique[0] == 1 and unique[1] == 2 and unique[2] == 3 and len(unique) == 3);
    const intersection = Intersection(.{ 1, 2, 3, 4 }, .{ 4, 5, 6 });
    try t.expect(len(intersection) == 1 and intersection.value[0] == 4);
}
