import scala.util.boundary, boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to the reference described in
  * `README.md` to complete the assignment.
  */
@main def calculator(commands: String*): Unit = {

  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer, but
    * you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0
  // define additional registers here
  var A: Int = 0
  var B: Int = 0
  var Blink: Boolean = false;

  boundary:
    for (c <- commands) {
      c match {
        case "+" => {
          acc = A + B;
          Blink = false;
        }
        case "-" => {
          acc = A - B;
          Blink = false;
        }
        case "*" => {
          acc = A * B;
          Blink = false;
        }
        case "/" => {
          if (B == 0) {
            A = 0;
            B = 0;
            acc = 0;
            Blink = false;
          } else {
            acc = A / B;
          }
          Blink = false;
        }
        case "swap" => {
          var tmp = B
          B = A
          A = tmp
        }
        case "blink" => Blink = !Blink; // why this doesnt work - Blink ^= 1? =)
        case "acc"   => {
          if (Blink) B = acc else A = acc
          Blink = !Blink;
        }
        case "break" => break();
        case _       => {
          if (Blink) B = parseInt(c) else A = parseInt(c)
          Blink = !Blink;
        }
      }
    }

  println(acc)
}
