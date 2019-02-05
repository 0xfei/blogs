---
title: "thrift 编码解码详解"
date: 2018-05-09T09:13:30+08:00
draft: true
categories: ["后端架构"]
tags: ["后端架构", "源码分析"]
topics: ["后端架构", "源码分析"]
---

thrift结构体按照一定格式编码成字符串，供网络传输；通常上层会对解码和编码的调用做封装，把object编码为string，从string解码object：

```
boost::shared_ptr<TMemoryBuffer> mem_buffer(new TMemoryBuffer());
mem_buffer->resetBuffer(
    reinterpret_cast<uint8_t*>(const_cast<char*>(buffer.c_str())),
    buffer.size());
TBinaryProtocol protocol(mem_buffer);
data.read(&protocol);

boost::shared_ptr<TMemoryBuffer> mem_buffer(new TMemoryBuffer());
TBinaryProtocol protocol(mem_buffer);
data.write(&protocol);
buffer = mem_buffer->getBufferAsString();
```

<!--more-->

以下面的这个结构体为例：

```
struct Object
{
 1:i64 a;
 2:double b;
 3:optional binary c;
 4:bool d;
 5:optional list<i32> e;
}
```

thrift编译器生成的class有read和write方法，分别用来解码和编码对象：

```
uint32_t read(::apache::thrift::protocol::TProtocol* iprot);
uint32_t write(::apache::thrift::protocol::TProtocol* oprot) const;
```

先看如何把一个object编码成TProtocol：

```
uint32_t Object::write(::apache::thrift::protocol::TProtocol* oprot) const {
  uint32_t xfer = 0;
  oprot->incrementRecursionDepth(); // 防止栈溢出
  xfer += oprot->writeStructBegin("Object"); // 什么都不做
  xfer += oprot->writeFieldBegin("a", ::apache::thrift::protocol::T_I64, 1); // "a"这个参数没用，看下面的writeFieldBegin
  xfer += oprot->writeI64(this->a); // 二进制打包到oprot
  xfer += oprot->writeFieldEnd(); // 什么都不做
  if (this->__isset.c) {
    ...
  }
  if (this->__isset.e) {
    ... 遍历列表，写入
  }
  xfer += oprot->writeFieldStop(); // 写入T_STOP = 0
  xfer += oprot->writeStructEnd(); // 什么都不做
  oprot->decrementRecursionDepth(); // 栈计数减少
  return xfer;
}
```

```
template <class Transport_, class ByteOrder_>
uint32_t TBinaryProtocolT<Transport_, ByteOrder_>::writeFieldBegin(const char* name,
                                                                   const TType fieldType,
                                                                   const int16_t fieldId) {
  (void)name;
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)fieldType);
  wsize += writeI16(fieldId);
  return wsize;
}
```

`Object(100, 34.1, "0123456789", false,vector<int>(3,4))` 编码后的16进制如下。每个字段，需要1byte的类型，2byte的field_id，value本身（下面的-为了方便阅读才加的）。

```
0A00010000000000000064-04000240410CFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCD-0B00030000000A30313233343536373839-02000400-0F00050800000003000000040000000400000004-00
```

* T_I64类型是10(0x0A),field_id是1(0x0001),值是100（0x0000000000000064）
* T_DOUBLE类型是4(0x04),field_id是2(0x0002),值是34.1,二进制编码（一大堆，double双精度似乎占23个字节）
* T_STRING类型是11(0x0B),field_id是3(0x0003),值是"0123456789",10(0x0000000A)位长的字符串.
* T_BOOL类型是2（0x02）,field_id是4（0x0004）,值是false(0x00)
* T_LIST类型是15(0x0F),field_id是5(0x0005),有3个元素，都是(00000004)，
* 最后以0x00结尾。
 

再看read方法，会好理解很多：

```
uint32_t Object::read(::apache::thrift::protocol::TProtocol* iprot) {
 xfer += iprot->readStructBegin(fname); // do nothing
 while (true)
 {
   xfer += iprot->readFieldBegin(fname, ftype, fid);
   if (ftype == ::apache::thrift::protocol::T_STOP) {
    break;
   }
   switch (fid)
   {
     case 1:
       if (ftype == ::apache::thrift::protocol::T_I64) {
         xfer += iprot->readI64(this->a);
         this->__isset.a = true;
       } else {
         xfer += iprot->skip(ftype);
       }
       break;
     ... 省略几项
     case 5:
       if (ftype == ::apache::thrift::protocol::T_LIST) {
       {
         this->e.clear();
         uint32_t _size0;
         ::apache::thrift::protocol::TType _etype3;
         xfer += iprot->readListBegin(_etype3, _size0);
         this->e.resize(_size0);
         uint32_t _i4;
         for (_i4 = 0; _i4 < _size0; ++_i4)
         {
           xfer += iprot->readI32(this->e[_i4]);
         }
         xfer += iprot->readListEnd();
       }
       this->__isset.e = true;
     }
   xfer += iprot->readFieldEnd();
 }
 xfer += iprot->readStructEnd();
 return xfer;
}
```

编码解码不复杂，最关键的点就是field_id，这是一定不能改的，就算缺了某些字段名，或者增删字段，只要field_id兼容，也不会影响解析。

再说optional字段，增加**必要**字段，只需保证field_id不冲突，尽量不要用 optional。 对解析（read方法）来说，optional存不存在都无所谓，而在编码（write）的时候，如果用a.b=c而不是set方法，反而会丢掉这个字段，得不偿失。


修改thrift文件不是洪水猛兽，没必要每次新增字段都加optional。
