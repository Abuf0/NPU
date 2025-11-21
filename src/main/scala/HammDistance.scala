import spinal.core._
import spinal.lib._

// i=0~7, j=14~27, 64bit, qmode=1, 1278 cycle

case class HammDistance(cfg:HammConfig, lantency: Int = 0) extends Component {
  val io = new Bundle {
    val pa = slave Flow(SInt(cfg.DW bits))
    val pb = slave Flow(SInt(cfg.DW bits))
    val dim = in UInt(2 bits)
    val score = master Flow(UInt(7 bits)) // todo cfg
    val flush = in Bool()
    val clear = in Bool()
    val pos_loop_cnt_in = in UInt(2 bits)
    val i_cnt_in = in UInt(cfg.U8W bits)
    val j_cnt_in = in UInt(cfg.U8W bits)
    val pos_loop_cnt_out = out (Reg(UInt(2 bits)) init(0))
    val i_cnt_out = out (Reg(UInt(cfg.U8W bits)) init(0))
    val j_cnt_out = out (Reg(UInt(cfg.U8W bits)) init(0))
  }

  def popCount(bits: Bits): UInt = {
    val w = bits.getWidth
    // 结果范围是 0 ~ w，所以位宽是 log2Up(w+1)
    var acc = U(0, log2Up(w + 1) bits)

    // 这个 for 是 Scala 层面的循环，用来构造表达式，不是时序逻辑
    for(i <- 0 until w) {
      acc = acc + bits(i).asUInt
    }

    acc
  }

  io.score.setAsReg() init(io.score.getZero)
  val step = UInt(2 bits)
  when(io.dim <=1 ){
    step := 0 + lantency
  } .otherwise {
    step := io.dim - 1 + lantency
  }

  val cnt_vld = Bool()
  val cnt = Reg(UInt(2 bits)) init(0)

  when(io.flush){
    cnt_vld := False
  } .otherwise {
    if (lantency == 0) {
      cnt_vld := io.pa.valid || io.pb.valid
    } else {
      cnt_vld := io.pa.valid || io.pb.valid || (cnt === step)
    }
  }

  val ham_done = (cnt === step) && cnt_vld
  when(ham_done || io.flush) {
    cnt := 0
  } .elsewhen(cnt_vld) {
    cnt := cnt + 1
  }

  val pop_cnt = popCount(io.pa.payload.asBits ^ io.pb.payload.asBits)
  if(lantency == 1){
    pop_cnt.setAsReg() init(0)
  }

  io.score.valid := ham_done
  when(cnt === lantency && cnt_vld){
    io.score.payload := pop_cnt.resized
  } .elsewhen(cnt_vld) {
    io.score.payload := io.score.payload + pop_cnt
  }

  val i_cnt_ham = Reg(UInt(cfg.U8W bits)) init (0)
  val j_cnt_ham = Reg(UInt(cfg.U8W bits)) init (0)
  val pos_loop_cnt_ham = Reg(UInt(2 bits)) init(0)
  when(io.clear) {
    pos_loop_cnt_ham := 0
    i_cnt_ham := 0
    j_cnt_ham := 0
  }.otherwise{
    when(cnt === 0 && (io.pa.valid || io.pb.valid)) {
      pos_loop_cnt_ham := io.pos_loop_cnt_in
      when(io.pos_loop_cnt_in === 0) {
        i_cnt_ham := io.i_cnt_in
        j_cnt_ham := io.j_cnt_in
      }
    }
  }
  when(io.clear) {
    io.pos_loop_cnt_out := 0
    io.i_cnt_out := 0
    io.j_cnt_out := 0
  } .otherwise {
    when(ham_done) {
      when(step === 0) {
        io.pos_loop_cnt_out := io.pos_loop_cnt_in
        io.i_cnt_out := io.i_cnt_in
        io.j_cnt_out := io.j_cnt_in
      }.otherwise {
        io.pos_loop_cnt_out := pos_loop_cnt_ham
        io.i_cnt_out := i_cnt_ham
        io.j_cnt_out := j_cnt_ham
      }
    }
  }

}
