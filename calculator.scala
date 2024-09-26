/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */


import scala.util.boundary, boundary.break

/** Representations of registers
 * acc - accumulator
 * a - left operand
 * b - right operand
 * blink
 * break
 */
class Registers {
  var acc: Int = 0
  var a: Int = 0
  var b: Int = 0
  var blink: Boolean = false
  var break: Boolean = false
}


/**
 * Basic interface for all calculator commands
 */
trait Command {
  def exec(registers: Registers): Unit

  def unblink(registers: Registers): Unit = {
    registers.blink = false
  }

  def setByBlink(num: Int, registers: Registers): Unit = {
    if (registers.blink) {
      registers.b = num
    } else {
      registers.a = num
    }
  }
}


/**
 * Calculator commands:
 * Plus   (+)
 * Minus  (-)
 * Mult   (*)
 * Div    (/)
 * Swap   (swap A and B)
 * Blink  (reverse blink register)
 * Acc    (cp acc to A or B)
 * Number (write number to A or B)
 * Break  (stop exec)
 */

object Plus extends Command {
  override def exec(registers: Registers): Unit = {
    registers.acc = registers.a + registers.b
    unblink(registers)
  }
}


object Minus extends Command {
  override def exec(registers: Registers): Unit = {
    registers.acc = registers.a - registers.b
    unblink(registers)
  }
}


object Mult extends Command {
  override def exec(registers: Registers): Unit = {
    registers.acc = registers.a * registers.b
    unblink(registers)
  }
}


object Div extends Command {
  override def exec(registers: Registers): Unit = {
    if (registers.b == 0) {
      registers.a = 0
      registers.acc = 0
    } else {
      registers.acc = registers.a / registers.b
      unblink(registers)
    }
  }
}


object Swap extends Command {
  override def exec(registers: Registers): Unit = {
    val temp: Int = registers.a
    registers.a = registers.b
    registers.b = temp
  }
}


object Blink extends Command {
  override def exec(registers: Registers): Unit = {
    registers.blink = !registers.blink
  }
}


object Break extends Command {
  override def exec(registers: Registers): Unit = {
    registers.break = true
  }
}


object Acc extends Command {
  override def exec(registers: Registers): Unit = {
    setByBlink(registers.acc, registers)
    Blink.exec(registers)
  }
}


class Number(num: Int) extends Command {
  override def exec(registers: Registers): Unit = {
    setByBlink(num, registers)
    Blink.exec(registers)
  }
}


/** Converts given string `s` to integer.
 *
 * Throws [[NumberFormatException]] if `s` can't be converted to integer,
 * but you shouldn't worry about it at this moment.
 */
def parseInt(s: String): Int = s.toInt



/**
 * Matching character to a command
 */
def getCommand(command: String): Command =
  command match {
    case "+"     => Plus
    case "-"     => Minus
    case "*"     => Mult
    case "/"     => Div
    case "swap"  => Swap
    case "blink" => Blink
    case "acc"   => Acc
    case "break" => Break
    case x       => Number(parseInt(x))
  }



@main def calculator(commands: String*): Unit = {
  val registers = new Registers

  boundary:
    for (c <- commands) {
      getCommand(c).exec(registers)
      if (registers.break) {
        break()
      }
    }

  println(registers.acc)
}
