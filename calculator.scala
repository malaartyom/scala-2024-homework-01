/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  def isNumber(s: String): Boolean = {
    s.matches("-?\\d+")
  }


  /** Representation of `acc` register. */
  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false


  for (c <- commands) {
      c match {
        case "+" => acc = A + B; blink = false
        case "-" => acc = A - B; blink = false
        case "*" => acc = A * B; blink = false
        case "/" => if B == 0 then acc = 0; A = 0; B = 0 else acc = A / B; blink = false
        case "swap" => var temp = A; A = B; B = temp
        case "blink" => blink = !(blink)
        case "acc" => if blink then B = acc else A = acc; blink = !(blink)
        case i if isNumber(i) => if blink then B = parseInt(i) else A = parseInt(i)
        case x => println(x)

      }
  }
  println(acc)
}
