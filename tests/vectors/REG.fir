circuit REG :
  module REG :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip input : UInt<16>, flip addr : UInt<16>, flip wr : UInt<1>, output : UInt<16>}

    wire _buffer_WIRE : UInt<16>[16] @[Reg.scala 14:31]
    _buffer_WIRE[0] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[1] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[2] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[3] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[4] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[5] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[6] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[7] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[8] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[9] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[10] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[11] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[12] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[13] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[14] <= UInt<16>("h0") @[Reg.scala 14:31]
    _buffer_WIRE[15] <= UInt<16>("h0") @[Reg.scala 14:31]
    reg buffer : UInt<16>[16], clock with :
      reset => (reset, _buffer_WIRE) @[Reg.scala 14:23]
    when io.wr : @[Reg.scala 16:15]
      node _T = bits(io.addr, 3, 0)
      buffer[_T] <= io.input @[Reg.scala 17:21]
      io.output <= UInt<16>("h0") @[Reg.scala 18:15]
    else :
      node _io_output_T = bits(io.addr, 3, 0)
      io.output <= buffer[_io_output_T] @[Reg.scala 20:15]

