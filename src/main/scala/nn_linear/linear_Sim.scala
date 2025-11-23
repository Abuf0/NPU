package nn_linear
import spinal.core._
import spinal.core.sim._
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

object scfg extends SpinalConfig(
  targetDirectory        = "build/GM388/s2rtl",        // 输出目录
  oneFilePerComponent    = true,         // 每个组件单独一个 .v
  globalPrefix           = "",
  anonymSignalPrefix     = "tmp",
  noAssert               = true,

  defaultClockDomainFrequency = FixedFrequency(480 MHz),
  defaultConfigForClockDomains = ClockDomainConfig(
    resetKind         = ASYNC,
    resetActiveLevel  = LOW
  )
)

object tcfg extends SpinalConfig(
  targetDirectory        = "build/GM388/s2rtl/liear_release",        // 输出目录
  oneFilePerComponent    = true,         // 每个组件单独一个 .v
  globalPrefix           = "",
  anonymSignalPrefix     = "tmp",
  noAssert               = true,
  enumPrefixEnable       = false,



  defaultClockDomainFrequency = FixedFrequency(480 MHz),
  defaultConfigForClockDomains = ClockDomainConfig(
    resetKind         = ASYNC,
    resetActiveLevel  = LOW
  )
)


object top {
  def main(args: Array[String]) {
    val ccfg = LinearConfig()
    scfg.generateSystemVerilog(new linear_dbg(ccfg))
    //scfg.generateSystemVerilog(new HammDistance(ccfg))
    //tcfg.generateSystemVerilog(new GdCalculateHamdis_wrap(ccfg))
  }
}

import spinal.core.sim._



object linear_Sim {
  def main(args: Array[String]) {
    val ccfg = LinearConfig()
    SimConfig.withWave.doSim(new linear_dbg(ccfg)) { dut => // 实例化top level dut
      dut.clockDomain.forkStimulus(period = 10) // just simulation
      // INIT

      def IN_BASE = 0
      def OUT_BASE = 0
      def WGT_BASE = 0
      def SC_BASE = 100
      def BZP_BASE = 100
      def SHIFT_BASE = 100
      def BATCH_NUM = 2
      def IN_NUM = 16
      def OUT_NUM = 32
      def OUTZP = 0
      def ACTVALUE = false


      dut.io.start #= false
      dut.io.reg_cfg.in_mem_base #= IN_BASE
      dut.io.reg_cfg.out_mem_base #= OUT_BASE
      dut.io.reg_cfg.weight_mem_base #= WGT_BASE
      dut.io.reg_cfg.multsc_base #= SC_BASE
      dut.io.reg_cfg.multbzp_base #= BZP_BASE
      dut.io.reg_cfg.multshift_base #= SHIFT_BASE
      dut.io.reg_cfg.batch_num #= BATCH_NUM
      dut.io.reg_cfg.infeat_num #= IN_NUM
      dut.io.reg_cfg.outfeat_num #= OUT_NUM
      dut.io.reg_cfg.outzp #= OUTZP
      dut.io.reg_cfg.actvalue #= ACTVALUE



      dut.clockDomain.waitSampling(5)
      dut.clockDomain.waitRisingEdge()
      // DIM = 0
      for (idx <- 0 to 0) {
        sleep(1)
        dut.io.start #= true

        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()
        sleep(1)

        dut.io.start #= false
        dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      dut.clockDomain.waitSampling(10)



      /*** debug ***/


      dut.clockDomain.waitSampling(2000)



    }
  }

}
