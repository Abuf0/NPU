package nn_linear
import spinal.core._
import spinal.lib._

case class linear_wrap(cfg:LinearConfig) extends Component {
  val io = new Bundle {
    val in_mem_if = master(bram_if(cfg))
    val weignt_mem_if = master(bram_if(cfg))
    val out_mem_if = master(bram_if(cfg))
    val cmd_if = master(commond_if(cfg))
  }
  

}
