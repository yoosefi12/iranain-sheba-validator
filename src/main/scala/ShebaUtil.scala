object ShebaUtil {
  private def mod(num: String, a: Int) = {
    def c(num: String, a: Int, i: Int): Long = if (i < 0) 0 else (c(num, a, i - 1) * 10 + num.charAt(i).toInt - '0') % a

    c(num, a, num.length - 1)
  }

  private def iso7064Mod97_10(iban: String) = mod(iban, 97)

  def validateIranianSheba(str: String): Boolean = {
    val pattern = "IR[0-9]{24}"
    str match {
      case x if x.length != 26 => false
      case x if !x.matches(pattern) => false
      case x =>
        val n = x.substring(0, 2)
        val m = x.substring(2, 4)
        val o = x.substring(4)
        val d1 = n.charAt(0) - 65 + 10
        val d2 = n.charAt(1) - 65 + 10
        val newStr = s"$o$d1$d2$m"
        iso7064Mod97_10(newStr) == 1
    }
  }
}
