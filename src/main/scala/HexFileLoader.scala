import spinal.core._
import scala.io.Source

object HexFileLoader {
  // 每行 32bit 十六进制：如 00000001 / DEADBEEF / 0x12345678
  def loadHex32UInt(path: String): Seq[UInt] = {
    val lines = Source.fromFile(path)
      .getLines()
      .toSeq
      // 去掉行尾注释，可以写 12345678 // comment
      .map(_.takeWhile(_ != '/'))
      .map(_.trim)
      .filter(_.nonEmpty)

    lines.map { str =>
      val clean = str
        .stripPrefix("0x")
        .stripPrefix("0X")
        .replace("_", "")

      val value = BigInt(clean, 16)
      U(value, 32 bits)    // 这里直接生成 UInt(32 bits)
    }
  }
}
