package calc

import scala.util.boundary, boundary.break

def calculator(commands: String*): Int = {
  def parseInt(s: String): Int = s.toInt
  def isNumber(s: String): Boolean = {
    s.matches("-?\\d+")
  }


  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false

  boundary:
    for (c <- commands) {
      c match {
        case "+" => acc = A + B; blink = false
        case "-" => acc = A - B; blink = false
        case "*" => acc = A * B; blink = false
        case "/" => if B == 0 then {
          acc = 0; A = 0
        } else acc = A / B; blink = false
        case "swap" => var temp = A; A = B; B = temp
        case "blink" => blink = !(blink)
        case "acc" => if blink then B = acc else A = acc; blink = !(blink)
        case i if isNumber(i) => if blink then B = parseInt(i) else A = parseInt(i); blink = !(blink)
        case "break" => break()
        case x => ???

      }
    }
  // println(acc)
  return acc
}

@main def main(commands: String*): Unit = {
  assert(calculator("22", "20", "+") == 42)
  assert(calculator("22", "20", "-") == 2)
  assert(calculator("22", "20", "swap", "-") == -2)
  assert(calculator("22", "20", "+") == 42)
  assert(calculator("20", "20", "+", "acc", "2", "+") == 42)
  assert(calculator("20", "20", "+", "acc", "2", "/") == 20)
  assert(calculator("20", "2", "*") == 40)
  assert(calculator("20", "0", "/") == 0)
  assert(calculator("blink", "1", "-") == -1)
  assert(calculator("break", "1", "2", "+", "3", "4", "+") == 0)
  assert(calculator("1", "2", "+", "break", "acc", "0", "/") == 3)
  println("All tests have passed!")

}
