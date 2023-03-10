import scala.collection.mutable

object Regex extends App{
  val eps = '\u0000'
  // the boolean indices whether the character had initially quotes or not
  var alphabet = Map[Char, Boolean]()
  def getOpenPos(str:String, pos: Int): Int = {
    var openings = 0
    var closures = 0
    var i = pos
    while(i >= 0) {
      i -= 1
      if(str.charAt(i) == '(' && closures == openings) return i
      if(str.charAt(i) == ')') closures += 1
      if(str.charAt(i) == '(') openings += 1
    }
    -1
  }

  def preprocess(s: List[Char]): List[(Char, Boolean)] = {
    alphabet = Map.empty
    var str: String = s.mkString("")

    var allDigits: String = "("
    for (i <- 0 to 9) {
      allDigits += i.toString
      if (i != 9) allDigits += '|'
      else allDigits += ')'
    }

    var allLowLetters: String = "("
    for (i <- 'a' to 'z') {
      allLowLetters += i
      if (i != 'z') allLowLetters += '|'
      else allLowLetters += ')'
    }

    var allUpLetters: String = "("
    for (i <- 'A' to 'Z') {
      allUpLetters += i.toString
      if (i != 'Z') allUpLetters += '|'
      else allUpLetters += ')'
    }

    str = str.replace("eps", eps.toString)
    str = str.replace("[0-9]", allDigits)
    str = str.replace("[a-z]", allLowLetters)
    str = str.replace("[A-Z]", allUpLetters)

    var i = 0
    while (i < str.length) {
      if (str.charAt(i) == '+' && str.charAt(i - 1) != '\'') {
        if (str.charAt(i - 1) == ')' && str.charAt(i - 2) == '9') {
          str = str.patch(i, allDigits + "*", 1)
        } else if (str.charAt(i - 1) == ')' && str.charAt(i - 2) == 'z') {
          str = str.patch(i, allLowLetters + "*", 1)
        } else if (str.charAt(i - 1) == ')' && str.charAt(i - 2) == 'Z') {
          str = str.patch(i, allUpLetters + "*", 1)
        } else if (i - 1 >= 0 && str.charAt(i - 1) == ')') {
          val openPos = getOpenPos(str, i - 1)
          str = str.patch(openPos, "(" + str.substring(openPos, i) + str.substring(openPos, i) + "*" + ")",
            str.substring(openPos, i).length + 1)
        } else {
          str = str.patch(i - 1, "(" + str(i - 1).toString + str(i - 1).toString + "*" + ")", 2)
        }
      }
      if (str.charAt(i) == '?') {
        if (i - 1 >= 0 && str.charAt(i - 1) == ')') {
          val openPos = getOpenPos(str, i - 1)
          str = str.patch(openPos, "(" + str.substring(openPos, i) + "|" + eps + ")", str.substring(openPos, i).length + 1)
        } else {
          str = str.patch(i - 1, "(" + str.charAt(i - 1) + "|" + eps + ")", 2)
        }
      }
      i += 1
    }

    var auxStr: List[(Char, Boolean)] = List.empty
    i = 0
    while (i < str.length) {
      if (str.charAt(i) == '\'' && i + 2 < str.length && str.charAt(i + 2) == '\'') {
        alphabet = alphabet.++(Map(str.charAt(i + 1) -> true))
        str = str.patch(i, str.charAt(i + 1).toString, 3)
        auxStr = auxStr.++(List(str.charAt(i) -> true))
      } else if (str.charAt(i) != '+' && str.charAt(i) != '*' && str.charAt(i) != '(' && str.charAt(i) != ')'
        && str.charAt(i) != '|' && str.charAt(i) != '?' && !alphabet.contains(str.charAt(i))) {
        auxStr = auxStr.++(List(str.charAt(i) -> false))
        alphabet = alphabet + (str.charAt(i) -> false)
      } else {
        auxStr = auxStr.++(List(str.charAt(i) -> false))
      }
      i += 1
    }
    //    println(str)
    //    println(auxStr)
    //    str
    auxStr
  }

  def isConcat(ch1: Char, ch2: Char): Boolean = {
    if(ch1 == ')' && ch2 == '(') return true
    if(alphabet.contains(ch1) && alphabet.contains(ch2)) return true
    if(alphabet.contains(ch1) && ch2 == '(') return true
    if(ch1 == ')' && alphabet.contains(ch2)) return true
    if(ch1 == '*' && ch2 == '(') return true
    if(ch1 == '*' && alphabet.contains(ch2)) return true

    false
  }

  def getPriority(operator: String): Int = {
    if(operator == "UNION") {
      return 1
    } else if(operator == "CONCAT") {
      return 2
    } else if(operator == "STAR") {
      return 3
    }
    0
  }

  def combineOperands(operators: mutable.Stack[String], operands: mutable.Stack[String]): Unit = {
    if (operators.top == "STAR") {
      val op1 = operands.pop
      val op = operators.pop

      val tmp = op + " " + op1
      operands.push(tmp)
    } else {
      val op1 = operands.pop
      val op2 = operands.pop
      val op = operators.pop

      val tmp = op + " " + op2 + " " + op1
      operands.push(tmp)
    }
  }

  def toPrenex(str: String): String = {
    val operands = mutable.Stack[String]()
    val operators = mutable.Stack[String]()

    val processedStr = preprocess(str.toList)
    for(i <- processedStr.indices) {
      var currOperator = ""
      if (processedStr(i)._1 == '|') currOperator = "UNION"
      if (i != 0 && isConcat(processedStr(i - 1)._1, processedStr(i)._1)) currOperator = "CONCAT"
      if (processedStr(i)._1 == '*' && !processedStr(i)._2) currOperator = "STAR"

      if (currOperator != "") {
        while (operators.nonEmpty && getPriority(currOperator) <= getPriority(operators.top)) {
          combineOperands(operators, operands)
        }
        operators.push(currOperator)
      }

      if(processedStr(i)._1 == '(' && !processedStr(i)._2) {
        operators.push("(")
      } else if(processedStr(i)._1 == ')' && !processedStr(i)._2) {
        while (operators.nonEmpty &&
          operators.top != "(") {
          combineOperands(operators, operands)
        }
        operators.pop()
      } else if(alphabet.contains(processedStr(i)._1)) {
        if(!alphabet(processedStr(i)._1)) {
          operands.push(processedStr(i)._1.toString)
        } else {
          operands.push("\'" + processedStr(i)._1.toString + "\'")
        }
      }
    }

    while (operators.nonEmpty) {
      combineOperands(operators, operands)
    }
    operands.top
  }

  val str = "'*'' '"
  println(Regex.toPrenex(str))
}
