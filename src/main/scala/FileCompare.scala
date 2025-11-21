import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object FileCompare {
  /** 比对两个十进制 txt 文件（每行一个整数）
   *
   * 返回值：
   *   - Seq 为空：两个文件完全一致
   *   - Seq 非空：每个元素是一条具体错误描述
   */
  def compareDecFiles(path1: String, path2: String): Seq[String] = {
    val errors = ArrayBuffer[String]()

    val s1 = Source.fromFile(path1)
    val s2 = Source.fromFile(path2)

    try {
      val it1 = s1.getLines()
      val it2 = s2.getLines()

      var lineNum = 1
      while (it1.hasNext && it2.hasNext) {
        val l1 = it1.next().trim
        val l2 = it2.next().trim

        // 允许空行，但如果你保证没有空行，可以去掉这段判断
        if (l1.nonEmpty || l2.nonEmpty) {
          var parseOk1 = true
          var parseOk2 = true

          var v1: BigInt = 0
          var v2: BigInt = 0

          try {
            v1 = BigInt(l1)
          } catch {
            case _: NumberFormatException =>
              errors += s"[DUMP_FILE] 第 $lineNum 行解析失败: '$l1'"
              parseOk1 = false
          }

          try {
            v2 = BigInt(l2)
          } catch {
            case _: NumberFormatException =>
              errors += s"[REF_FILE] 第 $lineNum 行解析失败: '$l2'"
              parseOk2 = false
          }

          // 两边都解析成功才进行数值比较
          if (parseOk1 && parseOk2 && v1 != v2) {
            errors += s"Fail@ [$lineNum] : DUMP = $v1, REF = $v2"
          }
        }

        lineNum += 1
      }

      // 行数不一致的情况
      if (it1.hasNext || it2.hasNext) {
        val extra =
          if (it1.hasNext && !it2.hasNext) path1
          else if (!it1.hasNext && it2.hasNext) path2
          else s"$path1 和 $path2" // 理论上不会同时有剩余

        errors += s"文件行数不一致，从第 $lineNum 行开始，$extra 还有多余行"
      }

      errors.toSeq
    } finally {
      s1.close()
      s2.close()
    }
  }
}
