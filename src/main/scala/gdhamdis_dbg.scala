import spinal.core._
import spinal.lib._
import spinal.core.sim._

case class gdhamdis_dbg(cfg:HammConfig, lantency: Int = 0) extends Component {
  val io = new Bundle {
    // in/out
    val start = in Bool()
    val done = out Bool()
    // config
    val temp_mem_base = in UInt (cfg.AW bits)
    val samp_mem_base = in UInt (cfg.AW bits)
    val dist_res_mem_base = in UInt (cfg.AW bits)
    val min_res_mem_base = in UInt (cfg.AW bits)
    val i_start = in UInt (cfg.U8W bits)
    val i_end = in UInt (cfg.U8W bits)
    val j_start = in UInt (cfg.U8W bits)
    val j_end = in UInt (cfg.U8W bits)
    val nth = in UInt (cfg.U8W bits)
    val bitlen = in UInt (cfg.U8W bits) // 64, 128, 192
    val bitq_mode = in UInt (2 bits)
    val param = Vec.fill(4)(in UInt (10 bits))
    val father_mode = in Bool()
    val dim = in UInt (2 bits)
    val score = out UInt (7 bits)
  }

  val gdhamdis = new GdCalculateHamdis(cfg, lantency)
  gdhamdis.io.start := io.start
  gdhamdis.io.temp_mem_base := io.temp_mem_base
  gdhamdis.io.samp_mem_base := io.samp_mem_base
  gdhamdis.io.dist_res_mem_base := io.dist_res_mem_base
  gdhamdis.io.min_res_mem_base := io.min_res_mem_base
  gdhamdis.io.i_start := io.i_start
  gdhamdis.io.i_end := io.i_end
  gdhamdis.io.j_start := io.j_start
  gdhamdis.io.j_end := io.j_end
  gdhamdis.io.nth := io.nth
  gdhamdis.io.bitlen := io.bitlen
  gdhamdis.io.bitq_mode := io.bitq_mode
  gdhamdis.io.param := io.param
  gdhamdis.io.father_mode := io.father_mode
  gdhamdis.io.dim := io.dim
  io.score := gdhamdis.io.score
  io.done := gdhamdis.io.done

  val temp_ram = Mem(UInt(cfg.DW bits),131072)
  val samp_ram = Mem(UInt(cfg.DW bits),131072)
  val res_ram = Mem(UInt(cfg.DW bits),131072)

  temp_ram.simPublic()
  samp_ram.simPublic()
  res_ram.simPublic()

  val temp_init: Seq[UInt] = HexFileLoader.loadHex32UInt("./src/main/scala/temp_data.txt")
  val samp_init: Seq[UInt] = HexFileLoader.loadHex32UInt("./src/main/scala/samp_data.txt")

  val zero = U(0, 32 bits)
  val temp_initData: Seq[UInt] =
    temp_init ++ Seq.fill(131072 - temp_init.length)(zero)
  val samp_initData: Seq[UInt] =
    samp_init ++ Seq.fill(131072 - samp_init.length)(zero)
  val res_initData: Seq[UInt] =
    Seq.fill(131072)(zero)

  temp_ram.init(temp_initData)
  samp_ram.init(samp_initData)
  res_ram.init(res_initData)

  gdhamdis.io.temp_mem_if.mem_rdata := temp_ram.readSync(gdhamdis.io.temp_mem_if.mem_addr,gdhamdis.io.temp_mem_if.mem_rd)//temp_ram(io.temp_mem_if.mem_addr)
  gdhamdis.io.samp_mem_if.mem_rdata := samp_ram.readSync(gdhamdis.io.samp_mem_if.mem_addr,gdhamdis.io.samp_mem_if.mem_rd)//samp_ram(io.samp_mem_if.mem_addr)
  res_ram.write(
    address = gdhamdis.io.res_mem_if.mem_addr,
    data = gdhamdis.io.res_mem_if.mem_wdata,
    enable = gdhamdis.io.res_mem_if.mem_wr,
    mask = gdhamdis.io.res_mem_if.mem_wmask.asBits
  )


}

