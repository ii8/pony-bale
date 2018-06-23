use "buffered"
use "collections"

interface val BaleItem[A: Any #send]
  fun apply(br: BaleReader): A^ ?

class BaleReader
  let r: Reader

  new create(data: ByteSeq) =>
    r = Reader
    r.append(data)

  new from_reader(r': Reader) =>
    r = r'

  fun ref u8(): U8 ? =>
    r.u8()?

  fun ref u16(): U16 ? =>
    r.u16_be()?

  fun ref u32(): U32 ? =>
    r.u32_be()?

  fun ref u64(): U64 ? =>
    r.u64_be()?

  fun ref i8(): I8 ? =>
    r.i8()?

  fun ref i16(): I16 ? =>
    r.i16_be()?

  fun ref i32(): I32 ? =>
    r.i32_be()?

  fun ref i64(): I64 ? =>
    r.i64_be()?

  fun ref f32(): F32 ? =>
    r.f32_be()?

  fun ref f64(): F64 ? =>
    r.f64_be()?

  fun ref uv(): U64 ? =>
    let b0 = u8()?.u64()
    if b0 <= 240 then
      b0
    elseif b0 <= 248 then
      let b1 = u8()?
      ((b0 - 241) * 256) + b1.u64() + 240
    else
      match b0
      | 249 =>
        let b1 = u8()?.u64()
        let b2 = u8()?.u64()
        2288 + (256 * b1) + b2
      | 250 =>
        let b12 = u16()?.u64()
        let b3 = u8()?.u64()
        (b12 << 8) + b3
      | 251 => u32()?.u64()
      | 252 =>
        let b1234 = u32()?.u64()
        let b5 = u8()?.u64()
        (b1234 << 8) + b5
      | 253 =>
        let b1234 = u32()?.u64()
        let b56 = u16()?.u64()
        (b1234 << 16) + b56
      | 254 =>
        let b1234 = u32()?.u64()
        let b56 = u16()?.u64()
        let b7 = u8()?.u64()
        (b1234 << 24) + (b56 << 8) + b7
      | 255 => u64()?
      else
        error
      end
    end

  fun ref union(): U64 ? =>
    uv()?

  fun ref array[A: Any #send](c: BaleItem[A]): Array[A] iso^ ? =>
    var len = uv()?
    let a = recover Array[A](len.usize()) end
    while len > 0 do
      a.push(c(this)?)
      len = len - 1
    end
    a

  fun ref iterate(): Iterator[BaleReader] ? =>
    let br = this
    let len = uv()?
    object
      fun ref has_next(): Bool =>
        len > 0
      fun ref next(): BaleReader ? =>
        if len > 0 then
          len = len - 1
          br
        else
          error
        end
    end

  fun tag none() ? =>
    error

  fun tag void() =>
    None

  fun ref bool(): Bool ? =>
    match r.u8()?
    | 0 => false
    | 1 => true
    else
      error
    end

  fun ref maybe(): Bool ? =>
    bool()?

  fun ref block(): Array[U8] iso^ ? =>
    let len = uv()?
    r.block(len.usize())?

  fun ref string(): String iso^ ? =>
    String.from_iso_array(block()?)

  fun ref map[A: Any #send](c: BaleItem[A]): Map[String, A] iso^ ? =>
    var len = uv()?
    let m = recover Map[String, A] end
    while len > 0 do
       let s: String = string()?
       m.insert(s, c(this)?)?
       len = len - 1
    end
    m

  fun done() ? =>
    if r.size() != 0 then
      error
    end

class BaleWriter
  let w: Writer

  new create() =>
    w = Writer

  new from_writer(w': Writer) =>
    w = w'

  fun ref u8(u: U8) =>
    w.u8(u)

  fun ref u16(u: U16) =>
    w.u16_be(u)

  fun ref u32(u: U32) =>
    w.u32_be(u)

  fun ref u64(u: U64) =>
    w.u64_be(u)

  fun ref i8(i: I8) =>
    w.u8(i.u8())

  fun ref i16(i: I16) =>
    w.i16_be(i)

  fun ref i32(i: I32) =>
    w.i32_be(i)

  fun ref i64(i: I64) =>
    w.i64_be(i)

  fun ref f32(f: F32) =>
    w.f32_be(f)

  fun ref f64(f: F64) =>
    w.f64_be(f)

  fun ref uv(x: U64) =>
    let y = x.u32()
    let z = (x >> 32).u32()
    if x <= 240 then
      u8(x.u8())
    elseif x <= 2287 then
      let y' = x - 240
      u8(((y' >> 8) + 241).u8())
      u8(y'.u8())
    elseif x <= 67823 then
      let y' = x - 2288
      u8(249)
      u8((y' >> 8).u8())
      u8(y'.u8())
    elseif z == 0 then
      if x <= 16777215 then
        u8(250)
        u8((y >> 16).u8())
        u16(y.u16())
      else
        u8(251)
        u32(y)
      end
    elseif z <= 255 then
      u8(252)
      u8(z.u8())
      u32(y)
    elseif z <= 65535 then
      u8(253)
      u16(z.u16())
      u32(y)
    elseif z <= 16777215 then
      u8(254)
      u8((z >> 16).u8())
      u16(z.u16())
      u32(y)
    else
      u8(255)
      u64(x)
    end

  fun ref union(index: U64) =>
    uv(index)

  fun ref array(len: USize) =>
    uv(len.u64())

  fun tag none() ? =>
    error

  fun tag void() =>
    None

  fun ref bool(b: Bool) =>
    if b then
      w.u8(1)
    else
      w.u8(0)
    end

  fun ref nothing() =>
    w.u8(0)

  fun ref just() =>
    w.u8(1)

  fun ref string(data: ByteSeq) =>
    uv(data.size().u64())
    w.write(data)

