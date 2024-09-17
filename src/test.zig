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

    const psubsub = zoop.cast(&custom, SubSub);
    const psub = zoop.cast(&custom, Sub);
    const phuman = zoop.cast(&custom, Human);
    const ihuman = zoop.cast(&custom, IHuman);
    assert(zoop.classInfo(ihuman) == zoop.classInfo(psub));
    assert(zoop.classInfo(psub) == zoop.classInfo(phuman));
    const hinfo = zoop.classInfo(phuman);
    const sinfo = zoop.classInfo(psub);
    const ssinfo = zoop.classInfo(psubsub);
    const cinfo = zoop.classInfo(&custom);
    assert(hinfo == cinfo);
    assert(sinfo == cinfo);
    assert(ssinfo == cinfo);
    assert(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(Human, zoop.IFormat));
    assert(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(Sub, zoop.IFormat));
    assert(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(SubSub, zoop.IFormat));
    assert(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(Human, IAge));
    assert(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(Sub, IAge));
    assert(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(SubSub, IAge));
    assert(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(Human, zoop.IObject));
    assert(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(Sub, zoop.IObject));
    assert(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(SubSub, zoop.IObject));
    assert(hinfo.getVtableOf(Custom, IHuman) == sinfo.getVtableOf(Sub, IHuman));
    assert(hinfo.getVtableOf(Custom, IHuman) == sinfo.getVtableOf(SubSub, IHuman));
    assert(hinfo.getVtableOf(Custom, ISetName) == sinfo.getVtableOf(Sub, ISetName));
    assert(hinfo.getVtableOf(Custom, ISetName) == sinfo.getVtableOf(SubSub, ISetName));
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

pub const IAge = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getAge(self: IAge) u8 {
        return zoop.icall(self, .getAge, .{});
    }

    pub fn cast(self: IAge, comptime T: type) @TypeOf(zoop.cast(self, T)) {
        return zoop.cast(self, T);
    }

    pub fn as(self: IAge, comptime T: type) @TypeOf(zoop.as(self, T)) {
        return zoop.as(self, T);
    }
};

pub const ISetName = struct {
    pub const extends = .{IAge};
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn setName(self: ISetName, name: []const u8) void {
        zoop.icall(self, .setName, .{name});
    }

    pub fn cast(self: ISetName, comptime T: type) @TypeOf(zoop.cast(self, T)) {
        return zoop.cast(self, T);
    }

    pub fn as(self: ISetName, comptime T: type) @TypeOf(zoop.as(self, T)) {
        return zoop.as(self, T);
    }
};

pub const IHuman = struct {
    pub const extends = .{ ISetName, IAge };
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getName(self: IHuman) []const u8 {
        return zoop.icall(self, .getName, .{});
    }

    pub fn cast(self: IHuman, comptime T: type) @TypeOf(zoop.cast(self, T)) {
        return zoop.cast(self, T);
    }

    pub fn as(self: IHuman, comptime T: type) @TypeOf(zoop.as(self, T)) {
        return zoop.as(self, T);
    }
};

pub const Human = struct {
    pub const extends = .{ zoop.IFormat, IAge };
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

    pub fn getAge(self: HumanPtr) u8 {
        return self.age;
    }

    pub fn formatAny(self: HumanPtr, writer: std.io.AnyWriter) anyerror!void {
        const classinfo = zoop.classInfo(self);
        try writer.print("{s}{{.age = {}, .name='{s}'}}\n", .{ classinfo.typeinfo.typename, self.age, self.name });
        const ilist = &.{ 1, 2, 3, 4 };
        try zoop.format(ilist, writer);
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
    pub const extends = .{ IAge, IHuman };
    super: Sub align(zoop.alignment),

    pub fn init(self: *align(zoop.alignment) SubSub, name: []const u8) void {
        self.super.init(name);
    }

    pub fn offset(self: *SubSub) usize {
        return @intFromPtr(&self.super) - @intFromPtr(self);
    }
};

pub const Custom = struct {
    pub const extends = .{ zoop.IFormat, ISetName };
    const Self = *align(zoop.alignment) @This();

    super: SubSub align(zoop.alignment),
    age: u16 = 99,
    pub fn init(self: Self, name: []const u8) void {
        self.super.init(name);
    }

    // override
    pub fn getName(_: Self) []const u8 {
        return "custom";
    }

    // override
    pub fn getAge(_: Self) u8 {
        return 88;
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
        var psub = &sub.class;
        var phuman: *Human = &sub.class.super;
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
        zoop.vcall(ihuman, ISetName.setName, .{"sub2"});
        try t.expectEqualStrings(ihuman.getName(), "sub2");
        try t.expectEqualStrings(sub.class.super.getName(), "sub2");

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

        // test classInfo
        var psubsub = zoop.cast(&custom, SubSub);
        psub = zoop.cast(&custom, Sub);
        phuman = zoop.cast(&custom, Human);
        try t.expect(zoop.classInfo(ihuman) == zoop.classInfo(psub));
        try t.expect(zoop.classInfo(psub) == zoop.classInfo(phuman));
        const hinfo = zoop.classInfo(phuman);
        const sinfo = zoop.classInfo(psub);
        const ssinfo = zoop.classInfo(psubsub);
        const cinfo = zoop.classInfo(&custom);
        try t.expect(hinfo == cinfo);
        try t.expect(sinfo == cinfo);
        try t.expect(ssinfo == cinfo);
        try t.expect(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(Human, zoop.IFormat));
        try t.expect(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(Sub, zoop.IFormat));
        try t.expect(hinfo.getVtableOf(Custom, zoop.IFormat) == sinfo.getVtableOf(SubSub, zoop.IFormat));
        try t.expect(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(Human, IAge));
        try t.expect(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(Sub, IAge));
        try t.expect(hinfo.getVtableOf(Custom, IAge) == sinfo.getVtableOf(SubSub, IAge));
        try t.expect(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(Human, zoop.IObject));
        try t.expect(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(Sub, zoop.IObject));
        try t.expect(hinfo.getVtableOf(Custom, zoop.IObject) == sinfo.getVtableOf(SubSub, zoop.IObject));
        try t.expect(hinfo.getVtableOf(Custom, IHuman) == sinfo.getVtableOf(Sub, IHuman));
        try t.expect(hinfo.getVtableOf(Custom, IHuman) == sinfo.getVtableOf(SubSub, IHuman));
        try t.expect(hinfo.getVtableOf(Custom, ISetName) == sinfo.getVtableOf(Sub, ISetName));
        try t.expect(hinfo.getVtableOf(Custom, ISetName) == sinfo.getVtableOf(SubSub, ISetName));

        // test interface -> interface
        const iage = ihuman.cast(IAge); //zoop.cast(ihuman, IAge);
        try t.expect(iage.getAge() == 88);
        try t.expect(iage.cast(IAge).getAge() == 88);

        // test call
        try t.expectEqualStrings(zoop.vcall(iage, IHuman.getName, .{}), "custom");
        try t.expect(zoop.vcall(&custom, IAge.getAge, .{}) == 88);
        try t.expect(zoop.vcall(custom.ptr(), IAge.getAge, .{}) == 88);
        try t.expect(zoop.vcall(ihuman, IAge.getAge, .{}) == 88);
        try t.expect(zoop.upcall(&custom, .getAge, .{}) == 99);
        try t.expect(zoop.upcall(custom.ptr(), .getAge, .{}) == 99);
        try t.expect(zoop.icall(iage, .getAge, .{}) == 88);

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
        psubsub = try zoop.new(t.allocator, SubSub, null);
        ihuman = zoop.cast(psubsub, IHuman);
        const cpsubsub: *const SubSub = psubsub;
        const cpsub = zoop.cast(cpsubsub, Sub);
        try t.expect(@typeInfo(@TypeOf(cpsub)).Pointer.is_const);
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
        std.debug.print("ihuman: {}\n", .{zoop.cast(ihuman, zoop.IObject)});
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
