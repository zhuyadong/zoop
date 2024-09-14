const std = @import("std");
const zoop = @import("zoop.zig");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = gpa.allocator();
var p: ?*Sub = null;
var i: ?IHuman = null;
fn assert(b: bool) void {
    if (!b) @panic("assert failed.");
}
pub fn main() !void {
    if (false) {
        const s = try zoop.new(allocator, Sub, null);
        p = s;
        i = zoop.cast(s, IHuman);
        const all = .{
            zoopicall(),
            zoopasclass(),
            zoopasiface(),
            zoopcastclass(),
            zoopcastiface(),
        };
        var l = all.len;
        if (l > 0) {
            l = 0;
        }
    }

    var custom = zoop.make(Custom, null);
    custom.class.init("sub");
    var intbase = @intFromPtr(&custom);
    var intss = @intFromPtr(klassPtr(&custom.class.super));
    var ints = @intFromPtr(klassPtr(&custom.class.super.super));
    var inth = @intFromPtr(klassPtr(&custom.class.super.super.super));
    assert(intbase == intss);
    assert(intbase == ints);
    assert(intbase == inth);
    intbase = @intFromPtr(&custom.class);
    intss = @intFromPtr(&custom.class.super);
    ints = @intFromPtr(&custom.class.super.super);
    inth = @intFromPtr(&custom.class.super.super.super);
    assert(intbase == intss);
    assert(intbase == ints);
    assert(intbase == inth);
    assert(zoop.isRootPtr(&custom));
    assert(zoop.isRootPtr(&custom.class));
    assert(!zoop.isRootPtr(&custom.class.super));
    assert(!zoop.isRootPtr(zoop.Klass(Sub).from(@ptrCast(@alignCast(&custom.class.super.super)))));
    assert(custom.class.super.super.offset() == 0);
}

fn zoopicall() @TypeOf(zoop.icall(i.?, "getName", .{})) {
    return zoop.icall(i.?, "getName", .{});
}
fn zoopasclass() @TypeOf(zoop.as(p.?, Human)) {
    return zoop.as(p.?, Human);
}
fn zoopasiface() @TypeOf(zoop.as(p.?, IHuman)) {
    return zoop.as(p.?, IHuman);
}
fn zoopcastiface() @TypeOf(zoop.cast(p.?, IHuman)) {
    return zoop.cast(p.?, IHuman);
}
fn zoopcastclass() @TypeOf(zoop.cast(p.?, Human)) {
    return zoop.cast(p.?, Human);
}

pub const IHuman = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getName(self: IHuman) []const u8 {
        return zoop.icall(self, "getName", .{});
        // return zoop.vptr(self).getName(zoop.cptr(self));
    }

    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.icall(self, "setName", .{name});
        // zoop.vptr(self).setName(zoop.cptr(self), name);
    }
};

pub const Human = struct {
    const HumanPtr = *align(zoop.alignment) Human;
    age: u8 align(zoop.alignment) = 99,
    name: []const u8 = "default",

    pub fn init(self: HumanPtr, name: []const u8) void {
        self.name = name;
    }

    pub fn deinit(self: HumanPtr) void {
        self.name = "";
    }

    pub fn getName(self: HumanPtr) []const u8 {
        return self.name;
    }

    pub fn setName(self: HumanPtr, name: []const u8) void {
        self.name = name;
    }
};

pub const Sub = struct {
    pub const extends = .{IHuman};
    // 'super' can be other name, but align must be zoop.alignment
    super: Human align(zoop.alignment),
    age: u65 = 99,

    pub fn init(self: *align(zoop.alignment) Sub, name: []const u8) void {
        self.super.init(name);
        const n = self.super.getName();
        if (n.len < 0) @panic("message: []const u8");
    }

    pub fn offset(self: *Sub) usize {
        const n = self.super.getName();
        if (n.len < 0) @panic("message: []const u8");
        return @intFromPtr(&self.super) - @intFromPtr(self);
    }
};

pub const SubSub = struct {
    super: Sub align(zoop.alignment),

    pub fn init(self: *align(zoop.alignment) SubSub, name: []const u8) void {
        self.super.init(name);
    }

    pub fn offset(self: *SubSub) usize {
        return @intFromPtr(&self.super) - @intFromPtr(self);
    }
};

pub const Custom = struct {
    super: SubSub align(zoop.alignment),
    age: u16 = 99,
    pub fn init(self: *Custom, name: []const u8) void {
        self.super.init(name);
    }

    // override
    pub fn getName(_: *Custom) []const u8 {
        return "custom";
    }

    pub fn offset(self: *Custom) usize {
        return @intFromPtr(&self.super) - @intFromPtr(self);
    }
};

fn klassPtr(any: anytype) *zoop.Klass(std.meta.Child(@TypeOf(any))) {
    return zoop.Klass(std.meta.Child(@TypeOf(any))).from(@ptrCast(@alignCast(any)));
}

test "zoop" {
    const t = std.testing;

    if (true) {
        // test interface excludes
        const Itest = struct {
            pub const excludes = .{"exfunc"};

            ptr: *anyopaque,
            vptr: *anyopaque,

            pub fn exfunc(self: @This()) void {
                zoop.icall(self, "exfunc", .{});
            }

            pub fn func(self: @This()) void {
                zoop.icall(self, "func", .{});
            }
        };

        const VTest = zoop.Vtable(Itest);
        try t.expect(@hasField(VTest, "func"));
        try t.expect(!@hasField(VTest, "exfunc"));

        // test inherit
        var human = zoop.make(Human, null);
        try t.expect(zoop.as(&human.class, IHuman) == null);

        var sub = zoop.make(Sub, .{ .super = .{ .name = "sub" } });
        const psub = &sub.class;
        const phuman: *Human = &sub.class.super;
        const ksub = zoop.Klass(Sub).from(psub);
        const khuman = zoop.Klass(Human).from(phuman);
        try t.expect(@intFromPtr(ksub) == @intFromPtr(khuman));
        try t.expectEqualStrings(phuman.getName(), "sub");

        const phuman2 = &sub.class.super;
        try t.expect(@intFromPtr(psub) == @intFromPtr(phuman2));
        var ihuman = zoop.cast(&sub.class, IHuman);
        try t.expect(@intFromPtr(&sub.class) == @intFromPtr(&sub.class.super));
        try t.expectEqualStrings(ihuman.getName(), "sub");
        ihuman = zoop.cast(&sub, IHuman);
        try t.expectEqualStrings(ihuman.getName(), "sub");
        ihuman.setName("sub2");
        try t.expectEqualStrings(ihuman.getName(), "sub2");
        try t.expectEqualStrings(sub.class.super.getName(), "sub2");

        // test classInfo
        try t.expect(zoop.classInfo(ihuman) == zoop.classInfo(&sub));
        try t.expect(zoop.classInfo(ihuman) == zoop.classInfo(&sub.class));
        try t.expect(zoop.classInfo(&sub) == zoop.classInfo(&sub.class));

        // test typeinfo
        try t.expect(zoop.typeInfo(ihuman) != zoop.typeInfo(sub));
        try t.expect(zoop.typeInfo(ihuman) == zoop.typeInfo(IHuman));

        // test deep inherit
        var subsub = zoop.make(SubSub, null);
        subsub.class.init("subsub");
        ihuman = zoop.cast(&subsub.class, IHuman);
        try t.expectEqualStrings(ihuman.getName(), "subsub");
        ihuman = zoop.cast(&subsub, IHuman);
        try t.expectEqualStrings(ihuman.getName(), "subsub");

        // test stack address
        var custom = zoop.make(Custom, null);
        custom.class.init("sub");
        var intbase = @intFromPtr(&custom);
        var intss = @intFromPtr(klassPtr(&custom.class.super));
        var ints = @intFromPtr(klassPtr(&custom.class.super.super));
        var inth = @intFromPtr(klassPtr(&custom.class.super.super.super));
        try t.expect(intbase == intss);
        try t.expect(intbase == ints);
        try t.expect(intbase == inth);
        intbase = @intFromPtr(&custom.class);
        intss = @intFromPtr(&custom.class.super);
        ints = @intFromPtr(&custom.class.super.super);
        inth = @intFromPtr(&custom.class.super.super.super);
        try t.expect(intbase == intss);
        try t.expect(intbase == ints);
        try t.expect(intbase == inth);

        try t.expect(zoop.isRootPtr(&custom));
        try t.expect(zoop.isRootPtr(&custom.class));
        try t.expect(!zoop.isRootPtr(&custom.class.super));
        try t.expect(!zoop.isRootPtr(zoop.Klass(Sub).from(&custom.class.super.super)));
        try t.expectEqualStrings(custom.class.super.super.super.getName(), "sub");
        try t.expectEqualStrings(zoop.cast(&custom, Human).name, "sub");
        try t.expectEqualStrings(zoop.cast(&custom.class, Human).name, "sub");
        try t.expectEqualStrings(zoop.as(&custom, Human).?.name, "sub");
        try t.expectEqualStrings(zoop.as(&custom.class, Human).?.name, "sub");
        try t.expectEqualStrings(zoop.getField(&custom, "name", []const u8).*, "sub");
        ihuman = zoop.as(zoop.as(&custom, zoop.IObject).?, IHuman).?;
        try t.expectEqualStrings(ihuman.getName(), "custom");

        // test deinit()
        zoop.destroy(&human);
        try t.expect(human.class.getName().len == 0);
        zoop.destroy(&sub);
        try t.expect(sub.class.super.getName().len == 0);
        zoop.destroy(&custom.class);
        try t.expect(custom.class.super.super.super.name.len == 0);

        // test default field value
        custom = zoop.make(Custom, null);
        try t.expect(custom.class.age == 99);
        try t.expectEqualStrings(custom.class.super.super.super.name, "default");
        // try t.expect(zoop.getAllocator(&custom) == null);

        // test heap address
        var psubsub = try zoop.new(t.allocator, SubSub, null);
        ihuman = zoop.cast(psubsub, IHuman);
        const ksubsub = zoop.Klass(SubSub).from(psubsub);
        try t.expect(@intFromPtr(ihuman.ptr) == @intFromPtr(ksubsub));
        try t.expectEqualStrings(ihuman.getName(), "default");
        const ph: *Human = @ptrCast(psubsub);
        try t.expect(@intFromPtr(ph) == @intFromPtr(psubsub));
        const pklass: *zoop.Klass(struct { x: u128 align(zoop.alignment) }) = @ptrFromInt(@intFromPtr(ihuman.ptr));
        const pksubsub: *zoop.Klass(SubSub) = @ptrFromInt(@intFromPtr(pklass));
        try t.expect(@intFromPtr(pklass) == @intFromPtr(pksubsub));
        try t.expect(@intFromPtr(pklass) == @intFromPtr(zoop.Klass(SubSub).from(psubsub)));
        try t.expect(pklass.allocator != null);
        try t.expect(@intFromPtr(ihuman.ptr) == @intFromPtr(zoop.Klass(SubSub).from(psubsub)));
        try t.expect(@intFromPtr(zoop.Klass(Human).from(&psubsub.super.super)) == @intFromPtr(zoop.Klass(SubSub).from(psubsub)));
        try t.expect(@intFromPtr(zoop.Klass(Sub).from(&psubsub.super)) == @intFromPtr(zoop.Klass(SubSub).from(psubsub)));

        //TODO: fix zoop.getAllocator() bug to pass tests below
        try t.expect(zoop.getAllocator(ihuman).?.ptr == t.allocator.ptr);
        try t.expect(zoop.getAllocator(ihuman).?.ptr == zoop.getAllocator(psubsub).?.ptr);
        try t.expect(zoop.getAllocator(psubsub).?.ptr == t.allocator.ptr);
        try t.expect(zoop.getAllocator(&psubsub.super).?.ptr == t.allocator.ptr);
        try t.expect(zoop.getAllocator(psubsub).?.ptr == zoop.getAllocator(&psubsub.super).?.ptr);
        try t.expect(zoop.getAllocator(&psubsub.super.super).?.ptr == t.allocator.ptr);
        try t.expect(zoop.getAllocator(zoop.cast(psubsub, zoop.IObject)).?.ptr == t.allocator.ptr);
        try t.expect(zoop.getAllocator(psubsub).?.ptr == zoop.getAllocator(&psubsub.super.super).?.ptr);
        zoop.destroy(ihuman);

        // test destroy
        psubsub = try zoop.new(t.allocator, SubSub, null);
        zoop.destroy(psubsub);
        psubsub = try zoop.new(t.allocator, SubSub, null);
        zoop.destroy(zoop.cast(psubsub, IHuman));
        psubsub = try zoop.new(t.allocator, SubSub, null);
        zoop.destroy(zoop.cast(psubsub, Human));
        var pcustom = try zoop.new(t.allocator, Custom, null);
        zoop.destroy(zoop.cast(pcustom, Human));
        pcustom = try zoop.new(t.allocator, Custom, null);
        zoop.destroy(zoop.cast(pcustom, IHuman));
        pcustom = try zoop.new(t.allocator, Custom, null);
        zoop.destroy(zoop.cast(pcustom, Sub));
    }
}
