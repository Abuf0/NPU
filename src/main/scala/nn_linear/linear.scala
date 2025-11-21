package nn_linear
import spinal.core._
import spinal.lib._
import FSMState._


case class linear(cfg:LinearConfig) extends Component {
  val io = new Bundle {
    val in_mem_if = master(bram_if(cfg))
    val weignt_mem_if = master(bram_if(cfg))
    val out_mem_if = master(bram_if(cfg))
    val reg_cfg = master(LinearCfgBundle(cfg))
    val start = in Bool()
    val done = out Bool()
  }
  val state = Reg(FSMState()) init (IDLE)

  val infeat_step = Reg(UInt(io.reg_cfg.infeat_num.getWidth bits)) init(0)
  when(io.start){
    infeat_step := io.reg_cfg.infeat_num + (io.reg_cfg.infeat_num(cfg.DOS-1 downto 0).asBits.orR.asUInt << cfg.DOS)
  }
  val infeat_os = io.reg_cfg.infeat_num(cfg.DOS-1 downto 0)


  val state_work = (state === WORK)
  val dot_cnt = Reg(UInt(infeat_step.getWidth bits)) init(0)  // need + cfg.DOS
  val inf_cnt = Reg(UInt(io.reg_cfg.outfeat_num.getWidth bits)) init(0)
  val bat_cnt = Reg(UInt(io.reg_cfg.batch_num.getWidth bits)) init(0)
  val dot_end = (dot_cnt === infeat_step) && state_work
  val inf_end = dot_end && (inf_cnt === io.reg_cfg.outfeat_num-1)
  val batch_end = inf_end && (bat_cnt === io.reg_cfg.batch_num-1)
  val wait_done = True  // todo
  val is_last_loop = (state =/= IDLE) && (bat_cnt === io.reg_cfg.batch_num-1)

  when(state_work){
    when(inf_end){
      inf_cnt := 0
      bat_cnt := bat_cnt + 1
    } .elsewhen(dot_end){
      dot_cnt := 0
      inf_cnt := inf_cnt + 1
    } .otherwise{
      dot_cnt := dot_cnt + 1
    }
  }.elsewhen(io.done){
    bat_cnt := 0
  }

  val inf_data = RegNext(io.in_mem_if.mem_rdata) init(0)
  val wgt_data = RegNext(io.weignt_mem_if.mem_rdata) init(0)
  val inf_vld = RegNext(io.in_mem_if.mem_rd) init(False)
  val wgt_vld = RegNext(io.weignt_mem_if.mem_rd) init(False)




  switch(state) {
    is(IDLE) {
      when(io.start) {
        state := WORK
        }
      }
    is(WORK){
      when(dot_end){
        state := WAIT
      }
    }
    is(WAIT) {
      when(wait_done){
        when(is_last_loop){
          state := IDLE
        } .otherwise{
          state := WORK
        }
      }
    }
    default {
      state := IDLE
    }

    }





}
