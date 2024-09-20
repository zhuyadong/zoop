# [中文](README.CN.md) | [English](README.md)
# Zoop 是一种 Zig 的 OOP 方案

## 安装
在项目根目录下：
```shell
zig fetch "git+https://github.com/zhuyadong/zoop.git" --save=zoop
```
如果要安装特定版本：
```shell
zig fetch "git+https://github.com/zhuyadong/zoop.git#<ref id>" --save=zoop
```

## 定义类
```zig
// 定义一个类 Human
pub const Human = struct {
    // zoop 类的第一个字段必须对齐为 `zoop.alignment`
    name: []const u8 align(zoop.alignment),
    age: u8 = 30,

    // 如果没有清理工作，可以不定义`deinit`
    pub fn deinit(self: *Human) void {
        self.name = "";
    }

    pub fn getName(self: *const Human) []const u8 {
        return self.name;
    }

    pub fn setName(self: *Human, name: []const u8) void {
        self.name = name;
    }
};
```

## 创建和销毁类对象
```zig
const t = std.testing;

// 在堆上创建一个 `Human`
var phuman = try zoop.new(t.allocator, Human, null);
// 类字段如果有缺省值，则对象字段就会初始化为缺省值
try t.expect(phuman.age == 30);
// 销毁对象，释放内存。如果类定义了`deinit`，则会先调用`deinit`，然后释放内存。
zoop.destroy(phuman);

// 在栈上创建一个 `Human`
var human = zoop.make(Human, null);
// 通过`ptr()`访问对象字段
try t.expect(human.ptr().age == 30);
// 清理对象(会调用`deinit`如果有)，如果没有需要清理的工作，也可以不调用 `zoop.destroy`
zoop.destroy(human.ptr());

// `zoop.new`和`zoop.make`都支持创建时初始化
phuman = try zoop.new(t.allocator, Human, .{.name = "HeapObj", .age = 1});
human = zoop.make(Human, .{.name = "StackObj", .age = 2});
try t.expect(phuman.age == 1);
try t.expect(human.ptr().age == 2);
```
关于调用 `deinit`:
`zoop.destroy`会依次调用类及其所有父类的 `deinit` 方法（如果有）

## 继承
```zig
// 定义 `SuperMan`，继承自 `Human`，父类必须是第一个字段且对齐为 `zoop.alignment`，
// 字段名是随意的，不是必须为`super`，但建议用 `super`
pub const SuperMan = struct {
    super: Human align(zoop.alignment),
    // SuperMan 能活很久，u8 无法满足它，我们用 u16
    age: u16 = 9999,

    pub fn getAge(self: *SuperMan) u16 {
        return self.age;
    }

    pub fn setAge(self: *SuperMan, age: u16) void {
        self.age = age;
    }
};

// 先创建一个 `SuperMan` 对象
var psuperman = try zoop.new(t.allocator, SuperMan, null);
// 调用父类方法
psuperman.super.setName("super");
// 或者这样调用父类方法，这种方法适合继承层次太深，已经不知道哪个父亲实现了`setName`方法的情况。
// 另外，既然叫 `upcall`，就表示哪怕`SuperMan`自己实现了`setName`，
// 下面的调用仍然会调用最近的父类的`setName`方法
zoop.upcall(psuperman, .setName, .{"super"});
// 也可以灵活的访问类继承树上所有字段，比如想访问 `Human.age` 字段，可以这样：
var phuman_age = zoop.getField(psuperman, "age", u8);
try t.expect(phuman_age.* == 30);
// 访问 `SuperMan.age`，可以这样：
var psuper_age = zoop.getField(psuperman, "age", u16);
try t.expect(psuper_age.* == 9999);
// 要注意的是，如果两个`age`都是同样类型，又都叫 "age"，
// 那上面的 `zoop.getField`调用会导致编译错误，以避免 bug
```

## 类的类型转换
```zig
// 先创建一个 Human，一个 SuperMan
var phuman = try zoop.new(t.allocator, Human, null);
var psuper = try zoop.new(t.allocator, SuperMan, null);

// 子类可以转成父类
try t.expect(zoop.as(psuper, Human) != null);
try t.expect(zoop.cast(psuper, Human).age == 30);
// 父类不能转成子类 (如果用 `zoop.cast` 就会编译错)
try t.expect(zoop.as(phuman, SuperMan) == null);
// 指向子类的父类指针可以转换成子类
phuman = zoop.cast(psuper, Human);
try t.expect(zoop.as(phuman, SuperMan) != null);
```

## 定义接口
```zig
// 定义个用来访问名字的接口 `IName`
pub const IName = struct {
    // 接口只能定义`ptr`和`vptr`两个字段，且名字和类型都必须和下面一样
    ptr: *anyopaque,
    vptr: *anyopaque,

    // 定义 `getName` 接口方法
    pub fn getName(self: IHuman) []const u8 {
        return zoop.icall(self, .getName, .{});
    }
    // 定义 `setName` 接口方法
    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.icall(self, .setName, .{name});
    }
    // 别管 `zoop.icall` 是什么东西，照着写就行了
};

// 再定义个用来访问年龄的接口 `IAge`
pub const IAge = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getAge(self: IHuman) u16 {
        return zoop.icall(self, .getAge, .{});
    }
    pub fn setAge(self: IHuman, age: u16) void {
        zoop.icall(self, .setAge, .{age});
    }
}

// 接口也可以继承
pub const INameAndAge struct {
    pub const extends = .{IName, IAge};

    ptr: *anyopaque,
    vptr: *anyopaque,
}

// 可以指定接口中哪些方法不包含到接口 API 中。
// 只能指定本接口内定义的方法，不会影响继承来的方法
pub const INameAndAge struct {
    pub const extends = .{IName, IAge};
    // 不要包含 “eql" 方法
    pub const excludes = .{"eql"};

    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn eql(self: INameAndAge, other: INameAndAge) bool {
        return self.ptr == other.ptr;
    }
}

// 接口还可以提供`api`的缺省实现, 这样申明实现接口的类可以不实现这些接口,
// 仍然能正确编译和工作 (接口就变成了抽象类)
pub const IName = struct {
    ...// 和上面一样

    pub fn Default(comptime Class: type) type {
        return struct {
            pub fn getName(_: *Class) []const u8 {
                return "default name";
            }
        }
    }
}

```

## 实现接口
```zig
// 我们让 `Human` 实现 `IName` 接口
pub const Human = struct {
    pub const extends = .{IName};
    ...//和上面的代码一样
};

// 让 `SuperMan` 实现 `IAge` 接口
pub const SuperMan = struct {
    pub const extends = .{IAge};
    ...//和上面的代码一样
}
// 父类实现的接口，了类自动实现，因此 `SuperMan` 也实现了 `IName`，虽然它只申明了实现 `IAge`。
// 子类可以重复申明实现父类已经实现的接口，这样做不会有什么问题，也不会对结果有什么影响，
// 比如下面的代码和上面的等价：
pub const SuperMan = struct {
    pub const extends = .{IAge, IName};
    ...
}
```

## 类和接口之间转换
```zig
// 先创建一个 Human，一个 SuperMan
var phuman = try zoop.new(t.allocator, Human, .{.name = "human"});
var psuper = try zoop.new(t.allocator, SuperMan, .{.super = .{.name = "super"}});

// Human 实现了 IName，所以可以转
var iname = zoop.cast(phuman, IName);
// SuperMan 实现了 IAge，所以可以转
var iage = zoop.cast(psuper, IAge);
try t.expect(iage.getAge() == psuper.age);
try t.expectEqualStrings(iname.getName(), phuman.name);
// Human 没有实现 IAge，所以转换会失败。(注意现在`iname`指向的是`phuman`，`iage`指向的是`psuper`)
try t.expect(zoop.as(phuman, IAge) == null);
try t.expect(zoop.as(iname, IAge) == null);
// 现在让 iname 指向 psuper
iname = zoop.cast(psuper, IName);
// 或者这样写也行，就是性能有点影响 (`cast`是O(1)，而`as`最差情况下是O(n) n=SuperMan实现的接口数目)
iname = zoop.as(psuper, IName).?;
// 现在 iname 就可以转 IAge了
try t.expect(zoop.as(iname, IAge) != null);
try t.expectEqualStrings(iname.getName(), "super");
// 万物都可转成 zoop.IObject
try t.expect(zoop.as(phuman, zoop.IObject) != null);
try t.expect(zoop.as(psuper, zoop.IObject) != null);
// 也可从 IObject 转回来
var iobj = zoop.cast(psuper, zoop.IObject);
try t.expect(zoop.as(iobj, SuperMan).? == psuper);
```
总结一下 `cast` 和 `as`：
- `cast`适用情况
  - 子类 -> 父类
  - 子接口 -> 父接口
  - 类 -> 类及其父类实现的接口
- `as`适用的情况
  - 所有`cast`适用和不适用的情况 (万物都可`as`)

## 方法重写和虚方法调用
```zig
// 假如 SuperMan 重写了 getName 方法
pub const SuperMan = struct {
    ...//和上面一样

    pub fn getName(_: *SuperMan) []const u8 {
        return "override";
    }
}

// 现在 IName.getName 将会调用 SuperMan.getName 而不是 Human.getName
var psuper = try zoop.new(t.allocator, SuperMan, .{.super = .{.name = "human"}});
var iname = zoop.cast(psuper, IName);
try t.expectEqualStrings(iname.getName(), "override");
// 另一种调用接口方法的风格
try t.expectEqualStrings(zoop.vcall(psuper, IName.getName, .{}), "override");
// 虚方法调用对转换后的类同样有用
var phuman = zoop.cast(psuper, Human);
iname = zoop.cast(phuman, IName);
try t.expectEqualStrings(iname.getName(), "override");
try t.expectEqualStrings(zoop.vcall(phuman, IName.getName, .{}), "override");
```
`vcall`的性能说明：
`vcall`会在能使用`cast`的情况下使用`cast`，否则使用`as`

## 调试用接口 `zoop.IFormat`
`zoop.IFormat`能方便通过 `std.fmt` 的 `format(...)`机制来输出对象的字符串形式内容。
```zig
// 定义一个实现 `zoop.IFormat`的类
pub const SomeClass = struct {
    pub const extends = .{zoop.IFormat};
    name:[]const u8 align(zoop.alignment) = "some";

    pub fn formatAny(self: *SomeClass, writer: std.io.AnyWriter) anyerror!void {
        try writer.print("SomeClass.name = {s}", .{self.name});
    }
}

// 下面代码就能输出 `SomeClass.formatAny` 的内容
const psome = try zoop.new(t.allocator, SomeClass, null);
std.debug.print("{}\n", .{zoop.cast(psome, zoop.IFormat)});
// output: SomeClass.name = some
```