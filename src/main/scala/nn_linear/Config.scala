package nn_linear
import FSMState.newElement
import spinal.core._
import spinal.lib._
import scala.io.Source


case class LinearConfig(
                         AW       : Int     = 17, // todo
                         DW       : Int     = 64, // or 32
                         U8MAX    : Int    = 255,
                         CFG_BANK : Int    = 6,
                         ACTMIN   : Int    = 0,
                         ACTMAX   : Int    = 255,
                         M2DLY    : Int    = 1, // support 1,2,...  // todo support 0
                         SC_CYC   : Int    = 1, // 64bit , 64DW
                         BZP_CYC  : Int    = 1, // 64bit, 64DW
                         SHIF_CYC : Int    = 1, // 32bit, 64DW
                         debug_en        : Boolean = true
                       ) {
  // —— 派生量（自动计算，不建议外部直接覆写）——
  def U8W: Int            = log2Up(U8MAX+1)
  def DS: Int             = DW/8
  def DOS:Int             = log2Up(DS)
  def DB :Int             = DW/8

}

case class bram_if(cfg: LinearConfig) extends Bundle with IMasterSlave {
  val mem_rd = Bool()
  val mem_wr = Bool()
  val mem_addr = UInt(cfg.AW bits)
  val mem_wdata = UInt(cfg.DW bits)
  val mem_wmask = UInt(cfg.DS bits)
  val mem_rdata = UInt(cfg.DW bits)

  override def asMaster(): Unit = {
    out(mem_rd,mem_wr,mem_addr,mem_wdata,mem_wmask)
    in(mem_rdata)
  }
}


case class commond_if(cfg: LinearConfig) extends Bundle with IMasterSlave {
  val cmd_cs = Bool()
  val cmd_en = UInt(15 bits)
  val cmd_data = UInt(cfg.DW bits)
  val cmd_ext_ready = Bool()
  val cmd_done = Bool()
  val cfg_err = Bool()

  override def asMaster(): Unit = {
    out(cmd_done,cfg_err)
    in(cmd_cs, cmd_en, cmd_data, cmd_ext_ready)
  }
}

case class LinearCfgBundle(cfg: LinearConfig) extends Bundle with IMasterSlave {
  val in_mem_base      = UInt (cfg.AW+cfg.DOS bits)
  val weight_mem_base  = UInt (cfg.AW+cfg.DOS bits)
  val out_mem_base  = UInt (cfg.AW+cfg.DOS bits)
  val multsc_base  = UInt (cfg.AW+cfg.DOS bits)
  val multbzp_base  = UInt (cfg.AW+cfg.DOS bits)
  val multshift_base  = UInt (cfg.AW+cfg.DOS bits)
  val actvalue = Bool()
  val outzp = UInt(cfg.U8W bits)
  val batch_num = UInt(32 bits)
  val infeat_num = UInt(32 bits)
  val outfeat_num = UInt(32 bits)
  override def asMaster(): Unit = {
    in(in_mem_base, weight_mem_base, out_mem_base,multsc_base)
    in(multbzp_base, multshift_base, actvalue, outzp)
    in(batch_num, infeat_num, outfeat_num)
  }
}

object FSMState extends SpinalEnum {
  val IDLE, WORK, BWAIT, EWAIT = newElement()
}

class Pipeline(depth: Int, width: Int) extends Component {
  val io = new Bundle {
    val i = in (UInt(width bits))         // 输入
    val iv = in Bool()
    val o = out (UInt(width bits))       // 输出
    val ov = out Bool()
  }

  // 创建一个 Vec 来存储多个寄存器，深度为 DEPTH，位宽为 DW
  val regs = Vec(Reg(UInt(width bits)) init(0), depth)
  val regs_v = Vec(Reg(Bool( )) init(False), depth)

  // 更新流水线寄存器
  for (i <- 0 until depth) {
    if (i == 0) {
      regs(i) := io.i  // 第一个寄存器直接接收输入
      regs_v(i) := io.iv
    } else {
      regs(i) := regs(i - 1)  // 后续寄存器传递前一个寄存器的值
      regs_v(i) := regs_v(i - 1)
    }
  }

  // 输出最终的寄存器值
  io.o := regs(depth - 1)
  io.ov := regs_v(depth - 1)
}


object HexFileLoader {
  // 每行 32bit 十六进制：如 00000001 / DEADBEEF / 0x12345678
  def loadHex64UInt(path: String): Seq[UInt] = {
    val lines = Source.fromFile(path)
      .getLines()
      .toSeq
      // 去掉行尾注释，可以写 12345678 // comment
      .map(_.takeWhile(_ != '/'))
      .map(_.trim)
      .filter(_.nonEmpty)

    lines.map { str =>
      val clean = str
        .stripPrefix("0x")
        .stripPrefix("0X")
        .replace("_", "")

      val value = BigInt(clean, 16)
      U(value, 64 bits)    // 这里直接生成 UInt(32 bits)
    }
  }
}
