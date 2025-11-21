import spinal.core._
import spinal.lib._

class layernorm extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val data_memory_interface = master(memory_interface(NPUConfig())) // to bus
    val coef_memory_interface = master(memory_interface(NPUConfig())) // to bus
    val read_id = out UInt (2 bits)
  }
}

  trait Interface_MS extends Global_parameter {
    case class memory_interface(config: NPUConfig) extends Bundle with IMasterSlave {
      val rd = Bool()
      val wr = Bool()
      val addr = UInt(DataAddrBus bits)
      val wmask = UInt(DataAddrBus/8 bits)
      val wdata = UInt(DataBus bits)
      val rdata = UInt(DataBus bits)

      override def asMaster(): Unit = {
        in(rdata)
        out(rd,wr,addr,wmask,wdata)
      }
    }
  }

  trait Global_parameter {
    // 全局参数
    val DataAddrBus = 32
    val DataBus = 64
  }

  case class NPUConfig(){
    // TODO as Global Parameters //
  }

  //Generate the MyTopLevel's Verilog
  object layernorm {
    def main(args: Array[String]) {
      SpinalVerilog(new layernorm)
    }
  }
  object NPUConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))
  //Generate the MyTopLevel's Verilog using the above custom configuration.
  object cuteriscvVerilog {
    def main(args: Array[String]) {
      NPUConfig
        .generateVerilog(new layernorm)  // module name
      val top = SpinalVerilog(new layernorm)
      //top.mergeRTLSource("mergeRTL") // Merge all rtl sources into mergeRTL.vhd and mergeRTL.v files
    }
  }