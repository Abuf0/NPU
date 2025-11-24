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
  targetDirectory        = "build/GM388/s2rtl/gdhamdis_release",        // 输出目录
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
    val ccfg = HammConfig()
    //scfg.generateSystemVerilog(new GdCalculateHamdis(ccfg))
    //scfg.generateSystemVerilog(new HammDistance(ccfg))
    tcfg.generateSystemVerilog(new GdCalculateHamdis_wrap(ccfg))
  }
}

import spinal.core.sim._



object hamdis_sim {
  def main(args: Array[String]) {
    val ccfg = HammConfig()
    SimConfig.withWave.doSim(new gdhamdis_dbg(ccfg, 0)) { dut => // 实例化top level dut
      dut.clockDomain.forkStimulus(period = 10) // just simulation
      // INIT
      def BITQ_MODE = 2
      def BITLEN = 192
      def INUM = 37
      def JNUM = 128
      def NTH = 20

      def TEMP_BASE = 0
      def SAMP_BASE = 0
      def DST_BASE = 0
      def MIN_BASE = 2000  // > INUM * NTH/2 * 4

      def IS = 0
      def IE = IS+INUM
      def JS = 0
      def JE = JS+JNUM
      def DIM = 0


      def PARM0 = 58//96
      def PARM1 = 164//384
      def PARM2 = 230//480
      def PARM3 = 376//576

      dut.io.start #= false
      dut.io.father_mode #= false
      dut.io.dim #= DIM
      dut.io.bitq_mode #= BITQ_MODE
      dut.io.bitlen #= BITLEN
      dut.io.nth #= NTH
      dut.io.i_start #= IS
      dut.io.i_end #= IE
      dut.io.j_start #= JS
      dut.io.j_end #= JE
      dut.io.temp_mem_base #= (TEMP_BASE >> 2)
      dut.io.samp_mem_base #= (SAMP_BASE  >> 2)
      dut.io.dist_res_mem_base #= (DST_BASE  >> 2)
      dut.io.min_res_mem_base #= (MIN_BASE  >> 2) // todo
      dut.io.param(0) #= PARM0
      dut.io.param(1) #= PARM1
      dut.io.param(2) #= PARM2
      dut.io.param(3) #= PARM3

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

      dut.io.father_mode #= true
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

      dut.clockDomain.waitSampling(100000)

      /*** debug ***/

        val dis_writer = new PrintWriter("./src/main/scala/dist_res_ram_dump.txt")
        for (i <- 0 until INUM) {
          for (j <- 0 until NTH / 2) {
            // 读取一个 word（返回 BigInt）
            val addr = (DST_BASE >> 2) + (i * NTH / 2 + j)
            val data = dut.res_ram.getBigInt(addr)
            val data_l = data & 0x0000FFFF
            val data_h = (data >> 16) & 0x0000FFFF

            // 转成 8 位十六进制（32bit），补零
            val hex = data.toString(16).toUpperCase
            val padded = "0" * (8 - hex.length) + hex

            // 一行一个 32bit hex
            //dis_writer.println(padded)
            print(f"${data_l}%-8d ")
            print(f"${data_h}%-8d ")
            dis_writer.println(data_l)
            dis_writer.println(data_h)

          }
          println()

        }
        println()
        println()


        val mindata_writer = new PrintWriter("./src/main/scala/mindata_res_ram_dump.txt")
        val mindpos_writer = new PrintWriter("./src/main/scala/minpos_res_ram_dump.txt")

        for (i <- 0 until INUM) {
          val addr = i + (MIN_BASE >> 2)
          val data = dut.res_ram.getBigInt(addr)
          val min_data = (data >> 24) & 0x000000FF
          val min_pos = (data >> 16) & 0x000000FF
          val submin_data = (data >> 8) & 0x000000FF
          val submin_pos = (data) & 0x000000FF

          val hex = data.toString(16).toUpperCase
          val padded = "0" * (8 - hex.length) + hex

          // 一行一个 32bit hex
          //min_writer.println(padded)
          print(f"${min_data}%-8d ")
          print(f"${submin_data}%-8d ")
          println()
          print(f"${min_pos}%-8d ")
          print(f"${submin_pos}%-8d ")
          println()
          mindata_writer.println(min_data)
          mindata_writer.println(submin_data)
          mindpos_writer.println(min_pos)
          mindpos_writer.println(submin_pos)

        }
        println()

        dut.clockDomain.waitSampling(1000)

        dis_writer.close()
        mindata_writer.close()
        mindpos_writer.close()


        val allErrors = ArrayBuffer[String]()

        // 多个用例
        def runCase(name: String, dump: String, golden: String): Unit = {
          val errors = FileCompare.compareDecFiles(dump, golden)
          if (errors.isEmpty) {
            println(s"[PASS] $name: $dump == $golden")
          } else {
            println(s"[ERROR] $name: $dump vs $golden 失败，${errors.length} 处差异")
            // 把每条错误都打上用例前缀，汇总到 allErrors
            allErrors ++= errors.map(e => s"[$name] $e")
          }
        }

        // 连续跑多个比对
        runCase("DIST", "./src/main/scala/dist_res_ram_dump.txt", "./src/main/scala/dist_res_ref.txt")
        runCase("MIN-DATA", "./src/main/scala/mindata_res_ram_dump.txt", "./src/main/scala/mindata_res_ref.txt")
        runCase("MIN-POS", "./src/main/scala/minpos_res_ram_dump.txt", "./src/main/scala/minpos_res_ref.txt")

        // 全部用例跑完之后，再统一判断
        if (allErrors.isEmpty) {
          println("[ALL PASS]")
          simSuccess()
        } else {
          println(s"[ALL ERROR] 共 ${allErrors.length} 条错误：")
          allErrors.foreach(e => println("  - " + e))
          simFailure(allErrors.mkString("\n"))
        }


      dut.clockDomain.waitSampling(1000)



    }
  }

}