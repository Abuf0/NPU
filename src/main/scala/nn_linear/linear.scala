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
  val dot_end = (dot_cnt === infeat_step - 1) && state_work
  val inf_end = dot_end && (inf_cnt === io.reg_cfg.outfeat_num-1)
  val batch_end = inf_end && (bat_cnt === io.reg_cfg.batch_num-1)
  val bwait_num = cfg.SC_CYC+cfg.BZP_CYC+cfg.SHIF_CYC
  val bwait_cnt = Reg(UInt(log2Up(bwait_num) bits)) init(0)
  val bwait_done = (state === BWAIT) && (bwait_cnt === bwait_num-1)
  val read_done_flag = Reg(Bool()) init(False)
  val last_flag = Reg(Bool()) init(False)
  val wr_vld = Reg(Bool()) init(False)
  val write_done = last_flag & wr_vld
  val ewait_done = (state === EWAIT) && write_done
  io.done := ewait_done
  io.done.setAsReg() init(False)

  when(state_work){
    when(batch_end){
      bat_cnt := 0
    } .elsewhen(inf_end){
      inf_cnt := 0
      bat_cnt := bat_cnt + 1
    } .elsewhen(dot_end){
      dot_cnt := 0
      inf_cnt := inf_cnt + 1
    } .otherwise{
      dot_cnt := dot_cnt + 1
    }
  }.elsewhen(state === BWAIT){
    when(bwait_done){
      bwait_cnt := 0
    } .otherwise {
      bwait_cnt := bwait_cnt + 1
    }
  }.elsewhen(io.done){
    dot_cnt := 0
    inf_cnt := 0
    bat_cnt := 0
    bwait_cnt := 0
  }

//  val infeat_data_r = Vec.fill(cfg.M2DLY)(Reg(UInt(cfg.DW bits)) init(0))
//  val weight_data_r = Vec.fill(cfg.M2DLY)(Reg(UInt(cfg.DW bits)) init(0))
//  val infeat_vld_r = Vec.fill(cfg.M2DLY)(Reg(Bool()) init(False))
//  val weight_vld_r = Vec.fill(cfg.M2DLY)(Reg(Bool()) init(False))
  val infeat_tail_r = Vec.fill(cfg.M2DLY)(Reg(Bool()) init(False))
  val infeat_head_r = Vec.fill(cfg.M2DLY)(Reg(Bool()) init(False))
  val read_done_flag_r = Vec.fill(cfg.M2DLY)(Reg(Bool()) init(False))

//  val inf_vld_r = RegNext(io.in_mem_if.mem_rd) init(False)
//  val wgt_vld_r = RegNext(io.weignt_mem_if.mem_rd) init(False)
  val inf_tail_r = RegNext(dot_end) init(False)
  val inf_head_r = RegNext(state_work && dot_cnt === 0) init(False)

  val infeat_tail_vld = infeat_tail_r(cfg.M2DLY-1)
  val infeat_head_vld = infeat_head_r(cfg.M2DLY-1)
  val read_done_flag_pp = read_done_flag_r(cfg.M2DLY-1)

  for (i <- 0 to cfg.M2DLY-1) {
    if(i == 0){
//      infeat_data_r(i) := io.in_mem_if.mem_rdata
//      weight_data_r(i) := io.weignt_mem_if.mem_rdata
//      infeat_vld_r(i) := inf_vld_r
//      weight_vld_r(i) := wgt_vld_r
      infeat_tail_r(i) := inf_tail_r
      infeat_head_r(i) := inf_head_r
      read_done_flag_r(i) := read_done_flag

    } else{
//      infeat_data_r(i) := infeat_data_r(i-1)
//      weight_data_r(i) := weight_data_r(i-1)
//      infeat_vld_r(i) := infeat_vld_r(i-1)
//      weight_vld_r(i) := weight_vld_r(i-1)
      infeat_tail_r(i) := infeat_tail_r(i-1)
      infeat_head_r(i) := infeat_head_r(i-1)
      read_done_flag_r(i) := read_done_flag_r(i-1)
    }
  }

  val infeat_data_pp = new Pipeline(cfg.M2DLY, cfg.DW)
  infeat_data_pp.io.i := io.in_mem_if.mem_rdata
  infeat_data_pp.io.iv := RegNext(io.in_mem_if.mem_rd) init(False)

  val weight_data_pp = new Pipeline(cfg.M2DLY, cfg.DW)
  weight_data_pp.io.i := io.weignt_mem_if.mem_rdata
  weight_data_pp.io.iv := RegNext(io.weignt_mem_if.mem_rd) init(False)

  val bwait_cnt_pp = new Pipeline(cfg.M2DLY, log2Up(bwait_num))
  bwait_cnt_pp.io.i := bwait_cnt
  bwait_cnt_pp.io.iv := (state === BWAIT)

  val infeat_data = infeat_data_pp.io.o
  val infeat_vld = infeat_data_pp.io.ov
  val weight_data = weight_data_pp.io.o
  val weight_vld = weight_data_pp.io.ov
  val param_cnt = bwait_cnt_pp.io.o
  val param_vld = bwait_cnt_pp.io.ov


  val mult_a = Vec.fill(cfg.DB)(UInt(cfg.U8W bits))
  val mult_b = Vec.fill(cfg.DB)(SInt(cfg.U8W bits))
  val mult_res = Vec.fill(cfg.DB)(Reg(SInt(2*cfg.U8W bits)) init(0))
  val psum = Reg(SInt(32 bits)) init(0)
  val acc = Reg(SInt(32 bits)) init(0)
  val acc_mult = Reg(SInt(32+64 bits)) init(0)
  val acc_mult_shift = SInt(32 bits)  // wire
  //val puchout = Reg(SInt(32 bits)) init(0)
  val multres_vld = RegNext(infeat_vld && weight_vld) init(False)
  val psum_vld = RegNext(multres_vld) init(False)
  val acc_vld = RegNext(psum_vld) init(False)
  val multsc_vld = RegNext(acc_vld) init(False)
  wr_vld := multsc_vld

  val multsc_addr = Reg(UInt(cfg.AW bits)) init(0)  // 64bit
  val multbzp_addr = Reg(UInt(cfg.AW bits)) init(0) // 64bit
  val multshift_addr = Reg(UInt(cfg.AW+1 bits)) init(0) // 32bit

  when(read_done_flag_pp){
    last_flag := True
  } .elsewhen(wr_vld){
    last_flag := False
  }

  // todo 很多可以考虑要不要复位
  val infeat_acc_head = RegNextWhen(infeat_head_vld, multres_vld) init(False)
  val param_multsc = RegNextWhen(weight_data.asSInt,param_vld && (param_cnt === cfg.SC_CYC-1)) init(0)
  val param_multbzp = RegNextWhen(weight_data.asSInt,param_vld && (param_cnt === cfg.SC_CYC+cfg.BZP_CYC-1)) init(0)
  val param_multshift = Reg(UInt(32 bits)) init(0)//RegNextWhen(weight_data,param_vld && (param_cnt === bwait_num-1))

  when(param_vld && (param_cnt === bwait_num-1)){
    when(multshift_addr(0)){
      param_multshift := weight_data(63 downto 32)
    } .otherwise{
      param_multshift := weight_data(31 downto 0)
    }
  }


  mult_a := infeat_data.subdivideIn(8 bits)
  mult_b := weight_data.asSInt.subdivideIn(8 bits)

  for (i <- 0 to cfg.DB - 1) {
    when(infeat_vld && weight_vld) {
      when(infeat_tail_vld && ((infeat_os =/= 0) || (i >= infeat_os))) {
        mult_res(i) := 0
      } .otherwise {
        mult_res(i) := (mult_a(i).asSInt * mult_b(i))
      }
    }
  }

  when(multres_vld){
    psum := mult_res.reduceBalancedTree(_+_).resized
  }

  when(psum_vld) {
    when(infeat_head_vld) {
      acc := psum
    } .otherwise{
      acc := acc + psum
    }
  }

  acc_mult := acc * param_multsc
  val multbzp_res = acc_mult + param_multbzp
  acc_mult_shift := (multbzp_res >> param_multshift).resized
  val actmin = UInt(cfg.U8W bit)
  when(io.reg_cfg.actvalue){
    actmin := io.reg_cfg.outzp
  } .otherwise{
    actmin := cfg.ACTMIN
  }
  val puchout = UInt(cfg.U8W bits)
  when(acc_mult_shift > actmin.asSInt){
    when(acc_mult_shift.asUInt.resize(cfg.U8W bits) > cfg.ACTMAX){
      puchout := cfg.ACTMAX
    } .otherwise {
      puchout := acc_mult_shift.asUInt.resize(cfg.U8W bits)
    }
  } .otherwise{
    puchout := actmin
  }


  io.in_mem_if.mem_wr := False
  io.in_mem_if.mem_wmask := 0
  io.in_mem_if.mem_wdata := 0
  io.weignt_mem_if.mem_wr := False
  io.weignt_mem_if.mem_wmask := 0
  io.weignt_mem_if.mem_wdata := 0
  io.out_mem_if.mem_rd := False

  io.in_mem_if.mem_addr.setAsReg() init(0)
  io.in_mem_if.mem_rd.setAsReg() init(False)
  io.weignt_mem_if.mem_addr.setAsReg() init(0)
  io.weignt_mem_if.mem_rd.setAsReg() init(False)
  io.out_mem_if.mem_addr.setAsReg() init(0)
  io.out_mem_if.mem_wr.setAsReg() init(False)
  io.out_mem_if.mem_wdata.setAsReg() init(0)
  io.out_mem_if.mem_wmask.setAsReg() init(0)

  val in_mem_addr_head = Reg(UInt(cfg.AW bits)) init(0)

  when(io.start){
    io.in_mem_if.mem_addr := (io.reg_cfg.in_mem_base >> cfg.DOS)
    in_mem_addr_head := (io.reg_cfg.in_mem_base >> cfg.DOS)
    io.in_mem_if.mem_rd := False
  } .elsewhen(state_work){
    io.in_mem_if.mem_rd := True
    when(inf_end){
      io.in_mem_if.mem_addr := io.in_mem_if.mem_addr + 1
      in_mem_addr_head := io.in_mem_if.mem_addr + 1
    }.elsewhen(dot_end){
      io.in_mem_if.mem_addr := in_mem_addr_head
    } .otherwise{
      io.in_mem_if.mem_addr := io.in_mem_if.mem_addr + 1
    }
  }.otherwise{
    io.in_mem_if.mem_rd := False
  }


  when(io.start){
    io.weignt_mem_if.mem_addr := (io.reg_cfg.weight_mem_base >> cfg.DOS)
    io.weignt_mem_if.mem_rd := False
    multsc_addr := (io.reg_cfg.multsc_base >> cfg.DOS)
    multbzp_addr := (io.reg_cfg.multbzp_base >> cfg.DOS)
    multshift_addr := ((io.reg_cfg.multshift_base >> cfg.DOS) << 1)
  } .elsewhen(state_work){
    io.weignt_mem_if.mem_rd := True
    io.weignt_mem_if.mem_addr := io.weignt_mem_if.mem_addr + 1
    } .elsewhen(state === BWAIT){
    io.weignt_mem_if.mem_rd := True
    when(bwait_cnt === cfg.SC_CYC-1){
      io.weignt_mem_if.mem_addr := multsc_addr + 1
      multsc_addr := multsc_addr + 1
    } .elsewhen(bwait_cnt === (cfg.SC_CYC + cfg.BZP_CYC-1)){
      io.weignt_mem_if.mem_addr := multbzp_addr + 1
      multbzp_addr := multbzp_addr + 1
    } .elsewhen(bwait_cnt === (cfg.SC_CYC + cfg.BZP_CYC + cfg.SHIF_CYC-1)){
      io.weignt_mem_if.mem_addr := ((multshift_addr + 1) >> 1)
      multshift_addr := multshift_addr + 1
    }
  } .otherwise{
    io.weignt_mem_if.mem_rd := False
  }

  val byte_cnt = Reg(UInt(log2Up(cfg.DB+1) bits)) init(0)
  val wdata = Vec.fill(cfg.DB)(puchout)
  when(io.start){
    io.out_mem_if.mem_addr := (io.reg_cfg.out_mem_base >> cfg.DOS)
    io.out_mem_if.mem_wr := False
    io.out_mem_if.mem_wmask := U(1).resize(cfg.DS).rotateLeft(cfg.DS-1)
  } .elsewhen(multsc_vld){  // 时序紧的话可以多delay 1拍
    io.out_mem_if.mem_wr := True
    io.out_mem_if.mem_addr := io.out_mem_if.mem_addr + (byte_cnt === cfg.DB - 1).asUInt
    io.out_mem_if.mem_wdata := wdata.asBits.asUInt
    io.out_mem_if.mem_wmask := io.out_mem_if.mem_wmask.rotateLeft(1)
  }.otherwise{
    io.out_mem_if.mem_wr := False
  }

  when(io.start){
    byte_cnt := 0
  } .elsewhen(io.out_mem_if.mem_wr){
    when(byte_cnt === cfg.DB - 1){
      byte_cnt := 0
    } .otherwise {
      byte_cnt := byte_cnt + 1
    }
  }

  when(batch_end){
    read_done_flag := True
  }.elsewhen(state === EWAIT){
    read_done_flag := False
  }

  switch(state) {
    is(IDLE) {
      when(io.start) {
        state := WORK
        }
      }
    is(WORK){
      when(dot_end){
        state := BWAIT
      }
    }
    is(BWAIT) { // 留给读参数和做量化
      when(bwait_done){
        when(read_done_flag){
          state := EWAIT
        } otherwise {
          state := WORK
        }
        }
      }
    is(EWAIT) {
      when(ewait_done){
        state := IDLE
      }
    }

  }




}
