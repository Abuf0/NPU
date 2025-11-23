package nn_linear
import spinal.core._
import spinal.lib._
import spinal.core.sim._

case class linear_dbg(cfg:LinearConfig) extends Component {
  val io = new Bundle {
    val reg_cfg = master(LinearCfgBundle(cfg))
    val start = in Bool()
    val done = out Bool()
  }

  val LINEAR = linear(cfg)

  val infeat_ram = Mem(UInt(cfg.DW bits),131072)
  val weight_ram = Mem(UInt(cfg.DW bits),131072)
  val outfeat_ram = Mem(UInt(cfg.DW bits),131072)

  infeat_ram.simPublic()
  weight_ram.simPublic()
  outfeat_ram.simPublic()

  // todo modify file path
  val infeat_init: Seq[UInt] = HexFileLoader.loadHex64UInt("./src/main/scala/nn_linear/infeat_data.txt")
  val weigtht_init: Seq[UInt] = HexFileLoader.loadHex64UInt("./src/main/scala/nn_linear/weight_data.txt")

  val zero = U(0, 64 bits)
  val infeat_initData: Seq[UInt] =
    infeat_init ++ Seq.fill(131072 - infeat_init.length)(zero)
  val weight_initData: Seq[UInt] =
    weigtht_init ++ Seq.fill(131072 - weigtht_init.length)(zero)
  val outfeat_initData: Seq[UInt] =
    Seq.fill(131072)(zero)

  infeat_ram.init(infeat_initData)
  weight_ram.init(weight_initData)
  outfeat_ram.init(outfeat_initData)

  val infeat_rdata = Reg(UInt(cfg.DW bits)) init(0)
  val weight_rdata = Reg(UInt(cfg.DW bits)) init(0)
  infeat_rdata := infeat_ram.readSync(LINEAR.io.in_mem_if.mem_addr,LINEAR.io.in_mem_if.mem_rd)//temp_ram(io.temp_mem_if.mem_addr)
  weight_rdata := weight_ram.readSync(LINEAR.io.weignt_mem_if.mem_addr,LINEAR.io.weignt_mem_if.mem_rd)//samp_ram(io.samp_mem_if.mem_addr)
  outfeat_ram.write(
    address = LINEAR.io.out_mem_if.mem_addr,
    data = LINEAR.io.out_mem_if.mem_wdata,
    enable = LINEAR.io.out_mem_if.mem_wr,
    mask = LINEAR.io.out_mem_if.mem_wmask.asBits
  )

  LINEAR.io.in_mem_if.mem_rdata := infeat_rdata
  LINEAR.io.weignt_mem_if.mem_rdata := weight_rdata
  LINEAR.io.start := io.start
  io.done := LINEAR.io.done
  LINEAR.io.reg_cfg := io.reg_cfg

}

