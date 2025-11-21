import spinal.core._
import spinal.lib._

case class GdCfgBundle(cfg: HammConfig) extends Bundle {
  val temp_mem_base      = UInt (cfg.AW+2 bits)
  val samp_mem_base      = UInt (cfg.AW+2 bits)
  val dist_res_mem_base  = UInt (cfg.AW+2 bits)
  val min_res_mem_base   = UInt (cfg.AW+2 bits)
  val i_start            = UInt (cfg.U8W bits)
  val i_end              = UInt (cfg.U8W bits)
  val j_start            = UInt (cfg.U8W bits)
  val j_end              = UInt (cfg.U8W bits)
  val nth                = UInt (cfg.U8W bits)
  val bitlen             = UInt (cfg.U8W bits)
  val bitq_mode          = UInt (2 bits)
  val param              = Vec.fill(4)(UInt (10 bits))
  val father_mode        = Bool()
  val dim                = UInt (2 bits)
}

case class GdCalculateHamdis_wrap(cfg:HammConfig, lantency: Int = 0) extends Component {
  val io = new Bundle {
    // bram memory interface
    val temp_mem_if = master(bram_if(cfg))
    val samp_mem_if = master(bram_if(cfg))
    val res_mem_if = master(bram_if(cfg))
    // control interface
    val cmd_if = master(commond_if(cfg))
    val rg_nScore1 = out UInt(7 bits)
  }

  def printCfgMapping(cfg: GdCfgBundle, hcfg: HammConfig, bankBits: Bits): Unit = {
    require(cfg.getBitsWidth <= bankBits.getWidth,
      s"cfg width=${cfg.getBitsWidth}, bankBits width=${bankBits.getWidth}")

    var offset = 0
    def show(name: String, width: Int): Unit = {
      val hi = offset + width - 1
      println(f"$name%-20s -> bankBits($hi%4d downto $offset%-4d)")
      offset += width
    }

    show("temp_mem_base",     hcfg.AW+2)
    show("samp_mem_base",     hcfg.AW+2)
    show("dist_res_mem_base", hcfg.AW+2)
    show("min_res_mem_base",  hcfg.AW+2)

    show("i_start",           hcfg.U8W)
    show("i_end",             hcfg.U8W)
    show("j_start",           hcfg.U8W)
    show("j_end",             hcfg.U8W)
    show("nth",               hcfg.U8W)
    show("bitlen",            hcfg.U8W)

    show("bitq_mode",         2)
    show("param(0)",          10)
    show("param(1)",          10)
    show("param(2)",          10)
    show("param(3)",          10)

    show("father_mode",       1)
    show("dim",               2)

    println(s"total used bits = $offset, cfg.getBitsWidth = ${cfg.getBitsWidth}, bankBits width=${bankBits.getWidth}")
  }



  val gdhamdis = new GdCalculateHamdis(cfg, lantency)

  val cmd_ready = io.cmd_if.cmd_cs && io.cmd_if.cmd_ext_ready
  val cmd_ready_d0 = RegNext(cmd_ready) init(False)
  val cmd_ready_d1 = RegNext(cmd_ready_d0) init(False)
  val cmd_start = cmd_ready_d0 && !cmd_ready_d1

  io.cmd_if.cmd_done := gdhamdis.io.done
  io.rg_nScore1 := gdhamdis.io.score
  gdhamdis.io.start := cmd_start
  io.temp_mem_if <> gdhamdis.io.temp_mem_if
  io.samp_mem_if <> gdhamdis.io.samp_mem_if
  io.res_mem_if <> gdhamdis.io.res_mem_if

  val REG_BANK = Vec.fill(cfg.CFG_BANK)(Reg(UInt(cfg.DW bits)) init(0))

  for (idx <- 0 to cfg.CFG_BANK-1){
    when(io.cmd_if.cmd_cs && io.cmd_if.cmd_en(idx)){
      REG_BANK(idx) := io.cmd_if.cmd_data
    }
  }

  // 把它们拼成一根大 Bits
  val bankBits: Bits = REG_BANK.asBits

  // 做一个 cfg 寄存器/信号视图
  val reg_cfg = GdCfgBundle(cfg)
  val regcfgWidth  = reg_cfg.getBitsWidth

  // 用紧凑的 bit 布局“解包”
  printCfgMapping(reg_cfg,cfg,bankBits)       // 生成时在控制台打印 mapping
  reg_cfg.assignFromBits(bankBits,regcfgWidth - 1, 0)

  // 再把 cfg 的字段接到 gdhamdis.io 上
  gdhamdis.io.temp_mem_base     := (reg_cfg.temp_mem_base >> 2)
  gdhamdis.io.samp_mem_base     := (reg_cfg.samp_mem_base >> 2)
  gdhamdis.io.dist_res_mem_base := (reg_cfg.dist_res_mem_base >> 2)
  gdhamdis.io.min_res_mem_base  := (reg_cfg.min_res_mem_base >> 2)
  gdhamdis.io.i_start           := reg_cfg.i_start
  gdhamdis.io.i_end             := reg_cfg.i_end
  gdhamdis.io.j_start           := reg_cfg.j_start
  gdhamdis.io.j_end             := reg_cfg.j_end
  gdhamdis.io.nth               := reg_cfg.nth
  gdhamdis.io.bitlen            := reg_cfg.bitlen
  gdhamdis.io.bitq_mode         := reg_cfg.bitq_mode
  gdhamdis.io.param             := reg_cfg.param
  gdhamdis.io.father_mode       := reg_cfg.father_mode
  gdhamdis.io.dim               := reg_cfg.dim


  val config_fail = (reg_cfg.temp_mem_base(1 downto 0).asBits.orR) || (reg_cfg.samp_mem_base(1 downto 0).asBits.orR) || (reg_cfg.dist_res_mem_base(1 downto 0).asBits.orR) || (reg_cfg.min_res_mem_base(1 downto 0).asBits.orR)
  io.cmd_if.cfg_err := RegNext(cmd_start && config_fail && !io.cmd_if.cfg_err) init(False)

}
