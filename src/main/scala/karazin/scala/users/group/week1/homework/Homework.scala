package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean = b match {
      case true => false
      case _ => true
    }
      
    def and(left: Boolean, right: Boolean): Boolean = left match {
        case false => false
        case _ => right match {
          case false => false
          case _ => true
        }
      }
    

    def or(left: Boolean, right: Boolean): Boolean = left match {
      case true => true
      case _ => right match {
        case true => true
        case _ => false
      }
    }

  end `Boolean Operators`

  object `Fermat Numbers`:

    val multiplication: (BigInt, BigInt) => BigInt = (x, y) => {
      def mult(x: BigInt, y: BigInt, acum: BigInt): BigInt = y match {
        case 0 => acum
        case _ => mult(x, y - 1, acum + x)
      }

      mult(x, y, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (x, y) => {
      def toPower(x:BigInt, y:BigInt, acum:BigInt): BigInt = y match {
        case 0 => acum
        case _ => toPower(x, y - 1, multiplication(acum, x))
      }

      toPower(x, y, 1)
    }

    val fermatNumber: Int => BigInt = (n) => power(2, power(2, n)) + 1
    

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    val lookAndSaySequenceElement: Int => BigInt = (n) => {
    	n match {
        case 1 => 1
        case 2 => 11
      }
      
    }

  end `Look-and-say Sequence`

end Homework
