import spinal.core._
import spinal.lib._

/** 统一配置中心：基础字段可覆写；派生字段自动计算 */
case class HammConfig(
                       AW       : Int     = 17,
                       DW       : Int     = 32,
                       U8MAX    : Int    = 255,
                       DES_LEN  : Int    = 12,
                       VALPOS_DIM : Int  = 2,
                       CFG_BANK : Int    = 6,
                       sparsity : Boolean = true,
                       debug_en        : Boolean = true
                     ) {
  // —— 派生量（自动计算，不建议外部直接覆写）——
  def U8W: Int            = log2Up(U8MAX+1)
  def DS: Int             = DW/8

}




