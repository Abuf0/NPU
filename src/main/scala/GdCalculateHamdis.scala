import spinal.core._
import spinal.lib._
import FSMState._
import spinal.core.sim._

case class GdCalculateHamdis(cfg:HammConfig, lantency: Int = 0) extends Component {
  val io = new Bundle {
    // bram memory interface
    val temp_mem_if = master(bram_if(cfg))
    val samp_mem_if = master(bram_if(cfg))
    val res_mem_if = master(bram_if(cfg))
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

  /*** release ***/
  val temp_rdata = Reg(UInt(cfg.DW bits)) init(0)
  val samp_rdata = Reg(UInt(cfg.DW bits)) init(0)
  //temp_rdata := io.temp_mem_if.mem_rdata
  //samp_rdata := io.samp_mem_if.mem_rdata

  /*** debug ***/
  //if(cfg.debug_en){

    val temp_ram = Mem(UInt(cfg.DW bits),131072)
    val samp_ram = Mem(UInt(cfg.DW bits),131072)
    val res_ram = Mem(UInt(cfg.DW bits),131072)

    temp_ram.simPublic()
    samp_ram.simPublic()
    res_ram.simPublic()

    val temp_init: Seq[UInt] = HexFileLoader.loadHex32UInt("D:/Learn/IC/project/Spinalhdl/NPU/src/main/scala/temp_data.txt")
    val samp_init: Seq[UInt] = HexFileLoader.loadHex32UInt("D:/Learn/IC/project/Spinalhdl/NPU/src/main/scala/samp_data.txt")

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

    temp_rdata := temp_ram.readSync(io.temp_mem_if.mem_addr,io.temp_mem_if.mem_rd)//temp_ram(io.temp_mem_if.mem_addr)
    samp_rdata := samp_ram.readSync(io.samp_mem_if.mem_addr,io.samp_mem_if.mem_rd)//samp_ram(io.samp_mem_if.mem_addr)
    res_ram.write(
      address = io.res_mem_if.mem_addr,
      data = io.res_mem_if.mem_wdata,
      enable = io.res_mem_if.mem_wr,
      mask = io.res_mem_if.mem_wmask.asBits
    )



//  } else {
//    temp_rdata := io.temp_mem_if.mem_rdata
//    samp_rdata := io.samp_mem_if.mem_rdata
//  }




  val HAMM = new HammDistance(cfg, lantency)

  // 状态寄存器
  val state = Reg(FSMState()) init (IDLE)
  val flush = Bool()
  val pos_loop_done = Bool()
  pos_loop_done := False
  flush := False

  val i_num = io.i_end - io.i_start  // todo start reg
  val j_num = io.j_end - io.j_start
  val i_cnt = Reg(UInt(cfg.U8W bits)) init (0)
  val j_cnt = Reg(UInt(cfg.U8W bits)) init (0)
  val j_done = (j_cnt === j_num - 1) && pos_loop_done
  val i_done = (i_cnt === i_num - 1) && j_done
  val i_cnt_m = Reg(UInt(cfg.U8W bits)) init (0)  // todo
  val j_cnt_m = Reg(UInt(cfg.U8W bits)) init (0)
  val i_cnt_m1 = Reg(UInt(cfg.U8W bits)) init (0)  // todo
  val j_cnt_m1 = Reg(UInt(cfg.U8W bits)) init (0)
  val i_cnt_ih = Reg(UInt(cfg.U8W bits)) init (0)  // todo
  val j_cnt_ih = Reg(UInt(cfg.U8W bits)) init (0)
  //val i_cnt_oh = Reg(UInt(cfg.U8W bits)) init (0)  // todo
  //val j_cnt_oh = Reg(UInt(cfg.U8W bits)) init (0)
  val i_cnt_temp = Reg(UInt(cfg.U8W bits)) init (0)  // todo
  val j_cnt_temp = Reg(UInt(cfg.U8W bits)) init (0)
  val j_temp_done = (j_cnt_temp === j_num - 1) && pos_loop_done
  val i_temp_done = (i_cnt_temp === i_num - 1) && j_temp_done
  val pos_loop_cnt_m = Reg(UInt(2 bits)) init(0)
  val pos_loop_cnt_m1 = Reg(UInt(2 bits)) init(0)

  val pos_loop_cnt_ih = Reg(UInt(2 bits)) init(0)
  //val pos_loop_cnt_oh = Reg(UInt(2 bits)) init(0)

  val tempdata = UInt(cfg.DW bits)
  val tempdata_vld = Reg(Bool()) init(False)
  val tempdata_vld_d1 = RegNext(tempdata_vld) init(False)
  //val tempdata_vld_d2 = RegNext(tempdata_vld_d1) init(False)

  //val j_cnt_restore = j_cnt_oh
  //val i_cnt_restore = i_cnt_oh

  val calc_num = UInt(2 bits)
  when(io.father_mode){
    calc_num := io.bitlen >> 6
  } .otherwise {
    calc_num := io.dim
  }

  val state_pos = (state === POS1 || state === POS2 || state === POS3 || state === POS4)

  val is_last_loop = (j_cnt === j_num-1) && (i_cnt === i_num-1)
  val is_j_last_loop = (j_cnt === j_num-1)
  val pos_cnt = Reg(UInt(3 bits)) init(0)
  val pos_num = (io.bitlen >> 6).resize(3 bits)
  val pos_done = (pos_cnt === pos_num-1) && state_pos
  val wait_done = (state === WAIT) && (pos_cnt === 1)// todo
  val hamm_pos_done = (pos_cnt === (Max(io.dim,U(1)))) && (state === HAMMDIS)
  val hamm_done = (state === HAMMDIS) && HAMM.io.score.valid
  val is_j_wb_last_loop = (j_cnt_temp === j_num-1)
  val is_j_last_temp_vld = is_j_wb_last_loop && tempdata_vld
  val is_j_last_temp_vld_d1 = RegNext(is_j_last_temp_vld) init(False)
  val is_j_last_temp_vld_d2 = RegNext(is_j_last_temp_vld_d1) init(False)
  val is_j_last_temp_vld_d3 = RegNext(is_j_last_temp_vld_d2) init(False)
  val is_last_temp_vld = is_j_last_temp_vld && (i_cnt_temp === i_num-1)
  val is_last_temp_vld_d1 = RegNext(is_last_temp_vld) init(False)
  val is_last_temp_vld_d2 = RegNext(is_last_temp_vld_d1) init(False)
  val is_last_temp_vld_d3 = RegNext(is_last_temp_vld_d2) init(False)
  val wait_no_vld = Reg(Bool()) init(False)
  val wait_no_vld_d1 = RegNext(wait_no_vld) init(False)
  val wait_no_vld_d2 = RegNext(wait_no_vld_d1) init(False)
  val is_wb_last_loop = Reg(Bool()) init(False)
  //val is_wb_last_loop = is_j_last_loop && (i_cnt_temp === i_num - 1)
  val done_pre = Bool()
  done_pre := False


  when(is_last_loop && pos_loop_done){
    is_wb_last_loop := True
  } .elsewhen(io.done){
    is_wb_last_loop := False
  }

  when(is_j_wb_last_loop && flush){
    wait_no_vld := True
  } .otherwise {
    wait_no_vld := False
  }

  // sparsity = true
  when(done_pre){
    j_cnt := 0
  } .elsewhen(flush){
    when(is_j_wb_last_loop){
      j_cnt := 0
    } .otherwise{
      j_cnt := j_cnt_temp + 1
    }
  } .otherwise{//.elsewhen(state === POS1 || state === POS2 || state === POS3 || state === POS4) {
    when(j_done) {
      j_cnt := 0
    }.elsewhen(pos_loop_done) {
      j_cnt := j_cnt + 1
    }
  }

  when(done_pre){
    i_cnt := 0
  } .elsewhen(flush){
    when(i_temp_done){
      i_cnt := 0
    } .elsewhen(is_j_wb_last_loop){
      i_cnt := i_cnt_temp + 1
    }
  }.otherwise{//elsewhen(state === POS1 || state === POS2 || state === POS3 || state === POS4) {
    when(i_done) {
      i_cnt := 0
    }.elsewhen(j_done) {
      i_cnt := i_cnt + 1
    }
  }



  when(flush){
    pos_cnt := 0
  } .elsewhen(pos_done) {
    pos_cnt := 0
  } .elsewhen(wait_done || io.done) {
    pos_cnt := 0
  } .elsewhen(hamm_done) {
    pos_cnt := 0
  } .elsewhen(state =/= IDLE && !hamm_pos_done){
    pos_cnt := pos_cnt + 1
  }

  val pos_loop_cnt = Reg(UInt(2 bits)) init(0)
  when(flush) {
    pos_loop_cnt := 0
  } .elsewhen(pos_loop_done){
    pos_loop_cnt := 0
  } .elsewhen(pos_done){
    pos_loop_cnt := pos_loop_cnt + 1
  }


  when(done_pre){
    pos_loop_cnt_m := 0
    pos_loop_cnt_m1 := 0
    pos_loop_cnt_ih := 0
    i_cnt_m := 0
    j_cnt_m := 0
    i_cnt_m1 := 0
    j_cnt_m1 := 0
    i_cnt_ih := 0
    j_cnt_ih := 0
    i_cnt_temp := 0
    j_cnt_temp := 0
  } .elsewhen(state === POS1 || state === POS2 || state === POS3 || state === POS4 || state === WAIT) {
    pos_loop_cnt_m := pos_loop_cnt
    pos_loop_cnt_m1 := pos_loop_cnt_m
    pos_loop_cnt_ih := pos_loop_cnt_m1
    i_cnt_m := i_cnt
    j_cnt_m := j_cnt
    i_cnt_m1 := i_cnt_m
    j_cnt_m1 := j_cnt_m
    i_cnt_ih := i_cnt_m1
    j_cnt_ih := j_cnt_m1
    i_cnt_temp := HAMM.io.i_cnt_out // maybe可以直接用oh作为temp
    j_cnt_temp := HAMM.io.j_cnt_out
  }


  val dist_0 = RegNextWhen(HAMM.io.score.payload, HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 0) init(0)
  //val dist_1 = RegNextWhen(HAMM.io.score.payload, HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 1)
  val dist_2 = RegNextWhen(HAMM.io.score.payload, HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 2) init(0)
  //val dist_3 = RegNextWhen(HAMM.io.score.payload, HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 3)
  val hammdist1 = UInt(cfg.DW bits)
  val hammdist2 = UInt(cfg.DW bits)
  hammdist1 := dist_0.resize(cfg.DW) + HAMM.io.score.payload.resize(cfg.DW)
  hammdist2 := dist_0.resize(cfg.DW) + (io.bitlen >> 1).resize(cfg.DW) - HAMM.io.score.payload.resize(cfg.DW)
  val hammdist1_lat = Reg(UInt(cfg.DW bits)) init(0)
  val hammdist2_lat = Reg(UInt(cfg.DW bits)) init(0)


  val ccnt = Reg(UInt(cfg.U8W bits)) init(1)
  val mindata_buff = Vec.fill(cfg.VALPOS_DIM)(Reg(UInt(cfg.U8W bits)) init(cfg.U8MAX))
  val minpos_buff = Vec.fill(cfg.VALPOS_DIM)(Reg(UInt(cfg.U8W bits)) init(cfg.U8MAX))

  val update_dist = (tempdata <= io.param(3)) && (ccnt < io.nth)
  val update_min = (tempdata < mindata_buff(0))
  val update_submin = !update_min && (tempdata < mindata_buff(1))

  when(tempdata_vld){
    when(update_min){
      mindata_buff(0) := tempdata.resized
      minpos_buff(0) := j_cnt_temp + io.j_start
      mindata_buff(1) := mindata_buff(0)
      minpos_buff(1) := minpos_buff(0)
    } .elsewhen(update_submin){
      mindata_buff(1) := tempdata.resized
      minpos_buff(1) := j_cnt_temp + io.j_start
    }
  } .elsewhen(is_j_last_temp_vld_d2 || wait_no_vld_d1){
    mindata_buff(0) := cfg.U8MAX
    minpos_buff(0) := cfg.U8MAX
    mindata_buff(1) := cfg.U8MAX
    minpos_buff(1) := cfg.U8MAX
  }


  when(HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 1){
    hammdist1_lat := hammdist1
    hammdist2_lat := hammdist2
    tempdata_vld := (io.bitq_mode =/= 2)
  } .elsewhen(HAMM.io.score.valid && HAMM.io.pos_loop_cnt_out === 3){
    hammdist1_lat := (hammdist1_lat << 1).resize(cfg.DW) + dist_2.resize(cfg.DW) + HAMM.io.score.payload.resize(cfg.DW)
    hammdist2_lat := (hammdist2_lat << 1).resize(cfg.DW) + dist_2.resize(cfg.DW) + (io.bitlen >> 1).resize(cfg.DW) - HAMM.io.score.payload.resize(cfg.DW)
    tempdata_vld := (io.bitq_mode === 2)
  } .otherwise{
    tempdata_vld := False
  }

  when(io.bitq_mode =/= 2){
    tempdata := Min(hammdist1_lat, hammdist2_lat)
  } .otherwise{
    tempdata := Min(Max(U(0), Min(hammdist1_lat, hammdist2_lat)), U(255))
  }

  val pusdist = ((j_cnt_temp + io.j_start) << 8) + tempdata


  when(is_j_last_temp_vld_d2 || wait_no_vld_d1){  // cnt
    val pp_flag = ((i_cnt_temp + io.i_start)*io.nth)(0 downto 0)
    io.res_mem_if.mem_wr := True
    io.res_mem_if.mem_addr := (io.dist_res_mem_base + (((i_cnt_temp + io.i_start)*io.nth) >> 1)).resized
    io.res_mem_if.mem_wdata := ccnt.resize(16 bits) @@ ccnt.resize(16 bits)
    when(pp_flag === 1){
      io.res_mem_if.mem_wmask := U"1100"
    }.otherwise{
      io.res_mem_if.mem_wmask := U"0011"
    }
  } .elsewhen(is_j_last_temp_vld_d3 || wait_no_vld_d2){
    ccnt := 1
    io.res_mem_if.mem_wr := False
  }.elsewhen(update_dist && tempdata_vld){  // dist
    ccnt := ccnt + 1
    val pp_flag = ((i_cnt_temp + io.i_start)*io.nth + ccnt)(0 downto 0)
    io.res_mem_if.mem_wr := True
    io.res_mem_if.mem_addr := (io.dist_res_mem_base + (((i_cnt_temp + io.i_start)*io.nth + ccnt) >> 1)).resized
    io.res_mem_if.mem_wdata := pusdist.resize(16 bits) @@ pusdist.resize(16 bits)
      when(pp_flag === 1){
        io.res_mem_if.mem_wmask := U"1100"
      }.otherwise{
        io.res_mem_if.mem_wmask := U"0011"
      }
  } .elsewhen(is_j_last_temp_vld_d1 || wait_no_vld){ // min
    io.res_mem_if.mem_wr := True
    io.res_mem_if.mem_addr := (io.min_res_mem_base + i_cnt_temp + io.i_start ).resized
    io.res_mem_if.mem_wdata := mindata_buff(0) @@ minpos_buff(0) @@ mindata_buff(1) @@ minpos_buff(1)
    io.res_mem_if.mem_wmask := U"1111"
  } .otherwise{
    io.res_mem_if.mem_wr := False
  }




  val flush_req_0 = HAMM.io.score.payload > io.param(0)
  val flush_req_1 = ((hammdist1 << 1) > io.param(1)) && ((hammdist2 << 1) > io.param(1))
  val flush_req_2 = (((hammdist1_lat << 1) + HAMM.io.score.payload) > io.param(2)) && (((hammdist2_lat << 1) + HAMM.io.score.payload) > io.param(2))

    when(HAMM.io.pos_loop_cnt_out === 0 && HAMM.io.score.valid && flush_req_0){
    flush := True
    //restore ?
  } .elsewhen(HAMM.io.pos_loop_cnt_out === 1 && HAMM.io.score.valid && flush_req_1 && io.bitq_mode === 2){
    flush := True
  } .elsewhen(HAMM.io.pos_loop_cnt_out === 2 && HAMM.io.score.valid && flush_req_2  && io.bitq_mode === 2){
    flush := True
  }


  io.temp_mem_if.mem_wr.setAsReg() init(False)
  io.temp_mem_if.mem_rd.setAsReg() init(False)
  io.temp_mem_if.mem_addr.setAsReg() init(0)
  io.temp_mem_if.mem_wdata.setAsReg() init(0)
  io.temp_mem_if.mem_wmask.setAsReg() init(0)

  io.samp_mem_if.mem_wr.setAsReg() init(False)
  io.samp_mem_if.mem_rd.setAsReg() init(False)
  io.samp_mem_if.mem_addr.setAsReg() init(0)
  io.samp_mem_if.mem_wdata.setAsReg() init(0)
  io.samp_mem_if.mem_wmask.setAsReg() init(0)

  io.res_mem_if.mem_wr.setAsReg() init(False)
  io.res_mem_if.mem_rd.setAsReg() init(False)
  io.res_mem_if.mem_addr.setAsReg() init(0)
  io.res_mem_if.mem_wdata.setAsReg() init(0)
  io.res_mem_if.mem_wmask.setAsReg() init(0)

  io.temp_mem_if.mem_wr := False
  io.temp_mem_if.mem_wdata := 0
  io.temp_mem_if.mem_wmask := 0
  io.samp_mem_if.mem_wr := False
  io.samp_mem_if.mem_wdata := 0
  io.samp_mem_if.mem_wmask := 0
  io.res_mem_if.mem_rd := False

  when(flush){
    io.temp_mem_if.mem_rd := False
    io.samp_mem_if.mem_rd := False
  }
  .elsewhen(state === POS1 || state === POS2 || state === POS3 || state === POS4){
    io.temp_mem_if.mem_rd := True
    io.temp_mem_if.mem_addr := io.temp_mem_base + (io.i_start * cfg.DES_LEN) + (i_cnt * cfg.DES_LEN) + (pos_loop_cnt * calc_num) + pos_cnt
    io.samp_mem_if.mem_rd := True
    io.samp_mem_if.mem_addr := io.samp_mem_base + (io.j_start * cfg.DES_LEN) + (j_cnt  * cfg.DES_LEN) + (pos_loop_cnt * calc_num) + pos_cnt
  } .elsewhen(state === HAMMDIS && !hamm_pos_done) {
    io.temp_mem_if.mem_rd := True
    io.temp_mem_if.mem_addr := io.temp_mem_base + pos_cnt
    io.samp_mem_if.mem_rd := True
    io.samp_mem_if.mem_addr := io.samp_mem_base + pos_cnt
  }.otherwise{
    io.temp_mem_if.mem_rd := False
    io.samp_mem_if.mem_rd := False
  }

  val temp_mem_rvld_d1 = RegNext(io.temp_mem_if.mem_rd && !flush) init(False)
  val samp_mem_rvld_d1 = RegNext(io.samp_mem_if.mem_rd && !flush) init(False)
  val pa_vld = RegNext(temp_mem_rvld_d1 && !flush) init(False)
  val pb_vld = RegNext(samp_mem_rvld_d1 && !flush) init(False)
  HAMM.io.pa.valid := pa_vld
  HAMM.io.pb.valid := pb_vld
  HAMM.io.pa.payload := temp_rdata.asSInt
  HAMM.io.pb.payload := samp_rdata.asSInt
  HAMM.io.clear := done_pre || hamm_done
  io.done.setAsReg() init(False)
  io.score := RegNextWhen(HAMM.io.score.payload, HAMM.io.score.valid && !io.father_mode) init(0)
  when(!io.father_mode){
    HAMM.io.dim := io.dim
    io.done := HAMM.io.score.valid
    HAMM.io.flush := False
  } .otherwise {
    HAMM.io.dim := calc_num
    io.done := is_wb_last_loop && done_pre
    HAMM.io.flush := flush
  }
  HAMM.io.pos_loop_cnt_in := pos_loop_cnt_ih
  HAMM.io.i_cnt_in := i_cnt_ih
  HAMM.io.j_cnt_in := j_cnt_ih



  switch(state) {
    is(IDLE) {
      when(io.start) {
        when(!io.father_mode) {
          state := HAMMDIS
        } .otherwise{
          state := POS1
        }
      }
    }

    is(POS1) {
      when(flush) {
        pos_loop_done := True
        when(is_j_wb_last_loop) { state := WAIT }
          .otherwise {
            state := POS1
          }
      } .elsewhen(pos_done){
        state := POS2
      }
    }

    is(POS2) {
      when(flush) {
        pos_loop_done := True
        when(is_j_wb_last_loop) { state := WAIT }
          .otherwise {
            state := POS1
          }
      } .elsewhen(pos_done){
        when(io.bitq_mode === 2) {
          state := POS3
        } .elsewhen(is_j_last_loop){
          pos_loop_done := True
          state := WAIT
        }.otherwise{
          pos_loop_done := True
          state := POS1
          }
      }
    }

    is(POS3) {
      when(flush) {
        pos_loop_done := True
        when(is_j_wb_last_loop) {
          state := WAIT
        }
          .otherwise {
            state := POS1
          }
      } .elsewhen(pos_done){
        state := POS4
      }
    }

    is(POS4) {
      when(flush) {
        pos_loop_done := True
        when(is_j_wb_last_loop) { state := WAIT }
          .otherwise {
            state := POS1
          }
      } .elsewhen(pos_done){
        when(is_j_last_loop){
          pos_loop_done := True
          state := WAIT
        }.otherwise{
          pos_loop_done := True
          state := POS1
        }
        }
    }

    is(HAMMDIS) {
      when(hamm_done) {
        state := IDLE
      }
    }

    is(WAIT) {
      when( is_wb_last_loop){
        when(is_last_temp_vld_d3 || wait_no_vld_d2){
          //pos_loop_done := True
          state := IDLE
          done_pre := True
        }
      } .elsewhen(wait_done){
        //pos_loop_done := True
        state := POS1
      }
    }

  }



}