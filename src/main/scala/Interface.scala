import spinal.core._
import spinal.lib._

// 定义状态枚举
object FSMState extends SpinalEnum {
  val IDLE, HAMMDIS, POS1, POS2, POS3, POS4 , WAIT = newElement()
}

case class bram_if(cfg: HammConfig) extends Bundle with IMasterSlave {
//  val mem_rd = Reg(Bool()) init(False)
//  val mem_wr = Reg(Bool()) init(False)
//  val mem_addr = Reg(UInt(cfg.AW bits)) init(0)
//  val mem_wdata = Reg(UInt(cfg.DW bits))
//  val mem_wmask = Reg(UInt(cfg.DS bits))
//  val mem_rdata = UInt(cfg.DW bits)
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


case class commond_if(cfg: HammConfig) extends Bundle with IMasterSlave {
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