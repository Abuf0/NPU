package nn_linear
import FSMState.newElement
import spinal.core._
import spinal.lib._

case class LinearConfig(
                         AW       : Int     = 17,
                         DW       : Int     = 64, // or 32
                         U8MAX    : Int    = 255,
                         CFG_BANK : Int    = 6,
                         ACTMIN   : Int    = 0,
                         ACTMAX   : Int    = 255,
                         debug_en        : Boolean = true
                       ) {
  // —— 派生量（自动计算，不建议外部直接覆写）——
  def U8W: Int            = log2Up(U8MAX+1)
  def DS: Int             = DW/8
  def DOS:Int             = log2Up(DS)

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
  val in_mem_base      = UInt (cfg.AW+2 bits)
  val weight_mem_base  = UInt (cfg.AW+2 bits)
  val out_mem_base  = UInt (cfg.AW+2 bits)
  val multsc_base  = UInt (cfg.AW+2 bits)
  val multbzp_base  = UInt (cfg.AW+2 bits)
  val multshift_base  = UInt (cfg.AW+2 bits)
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
  val IDLE, WORK, WAIT = newElement()
}