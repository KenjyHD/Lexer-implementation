class AST(var left:AST, var right:AST, var elem:String) {
}

object AST extends App {
  def mySplit(str: String): List[String] = {
    var prevIndex = 0
    var quotePair = false
    var list: List[String] = List.empty
    var i = 0
    while(i < str.length) {
      if(str.charAt(i) == '\'' && i + 2 < str.length && str.charAt(i + 2) == '\'') {
        list = list.++(List(str.charAt(i + 1).toString))
        i += 2
        quotePair = true
      }

      if(str.charAt(i) == ' ' && !quotePair) {
        list = list.++(List(str.substring(prevIndex, i)))
        if(i + 1 < str.length) prevIndex = i + 1
      } else if(i + 1 == str.length && !quotePair) {
        list = list.++(List(str.substring(prevIndex, i + 1)))
      } else if(str.charAt(i) == ' ' && quotePair) {
        quotePair = false
        if(i + 1 < str.length) prevIndex = i + 1
      }
      i += 1
    }
    list
  }

  def parseToAST(str: String): AST = {
    var auxList:List[String] = List.empty
    auxList = mySplit(str)
    def aux(list:List[String]): AST = {
      if (list.isEmpty) return null
      auxList = auxList.tail
      if (list.head == "CONCAT") {
        new AST(aux(list.tail), aux(auxList), list.head)
      } else if (list.head == "UNION") {
        new AST(aux(list.tail), aux(auxList), list.head)
      } else if (list.head == "PLUS") {
        new AST(aux(list.tail), null, list.head)
      } else if (list.head == "MAYBE") {
        new AST(aux(list.tail), null, list.head)
      } else if (list.head == "STAR") {
        new AST(aux(list.tail), null, list.head)
      } else {
        new AST(null, null, list.head)
      }
    }

    aux(auxList)
  }

  def printAST(ast: AST): Int = {
    print(ast.elem + " ")
    if (ast.left != null) printAST(ast.left)
    if (ast.right != null) printAST(ast.right)
    0
  }
}
