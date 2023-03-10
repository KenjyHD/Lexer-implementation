class Nfa[A](var initState:A, var finStates:Set[A], var map:Map[A, List[(A, Char)]]) {

  def map[B](f: A => B) : Nfa[B] = {
    val newMap: Map[B, List[(B, Char)]] = map.map(x =>
      f(x._1) -> x._2.map(y => (f(y._1), y._2))
    )
    val newFinStates = finStates.map(f)
    new Nfa[B](f(initState), newFinStates, newMap)
  }

  def next(state:A, c: Char): Set[A] = {
    var set: Set[A] = Set.empty
    if (!isFinal(state)) {
      map(state).foreach(
        x => if (c == x._2) set += x._1
      )
    }
    set
  }

  def accepts(str: String): Boolean = {
    findCharTransition(initState, str)
  }

  def findCharTransition(currState: A, str: String): Boolean = {
    if(isFinal(currState) && str.isEmpty) return true

    if (!isFinal(currState)) {
      map(currState).foreach(
        x =>
          if (str.nonEmpty && (x._2 == str.head)) {
            if (findCharTransition(x._1, str.tail)) return true
          } else if (x._2 == '\u0000') {
            if (findCharTransition(x._1, str)) return true
          }
      )
    }
    false
  }

  def getStates : Set[A] = {
    var set: Set[A] = Set.empty
      map.foreach(
        x => set += x._1
      )
    set.++(finStates)
  }

  def isFinal(state: A): Boolean = {
    finStates.contains(state)
  }

  def getAlphabet: Set[Char] = {
    var set: Set[Char] = Set.empty
    map.foreach(x =>
      x._2.foreach(y =>
        set += y._2
      )
    )
    set
  }
}

object Nfa {
  def fromPrenex(str: String): Nfa[Int] = {
    var adjacentTransition:List[(Int, Char)] = List.empty
    val epsChar = new AST(null, null, "\\u0000")
    def aux(ast:AST, initState: Int, finStates: Set[Int]): Nfa[Int] = {
      var map: Map[Int, List[(Int, Char)]] = Map.empty
      if(ast.elem == "CONCAT") {

        val leftNFA = aux(ast.left, initState, Set(initState + 1))
        val rightNFA = aux(ast.right, leftNFA.finStates.head, finStates)

        new Nfa[Int](leftNFA.initState, rightNFA.finStates, leftNFA.map.++(rightNFA.map))
      } else if (ast.elem == "UNION") {
        val upNFA = aux(ast.left, initState + 1, finStates)
        val downNFA = aux(ast.right, upNFA.finStates.head + 1, finStates)

        unionAddFinTransitions(upNFA, downNFA, initState)
      } else if (ast.elem == "STAR") {
        val auxNFA = aux(ast.left, initState + 1, finStates)
        val newTransitions1 =
          Map(initState -> List(auxNFA.initState -> '\u0000', auxNFA.finStates.head + 1 -> '\u0000'))
        val newTransitions2 =
          Map(auxNFA.finStates.head -> List(auxNFA.finStates.head + 1 -> '\u0000', auxNFA.initState -> '\u0000'))

        new Nfa[Int](auxNFA.initState - 1, Set(auxNFA.finStates.head + 1), newTransitions1.++(newTransitions2).++(auxNFA.map))
      } else if (ast.elem == "PLUS") {
        val auxNFA1 = aux(ast.left, initState, finStates)
        val auxNFA2 = aux(ast.left, auxNFA1.finStates.head + 1, finStates)
        val newTransitions1 =
          Map(auxNFA1.finStates.head -> List(auxNFA2.initState -> '\u0000', auxNFA2.finStates.head + 1 -> '\u0000'))
        val newTransitions2 =
          Map(auxNFA2.finStates.head -> List(auxNFA2.finStates.head + 1 -> '\u0000', auxNFA2.initState -> '\u0000'))
        val finalMap = auxNFA1.map.++(auxNFA2.map).++(newTransitions1).++(newTransitions2)

        new Nfa[Int](auxNFA1.initState, Set(auxNFA2.finStates.head + 1), finalMap)
      } else if (ast.elem == "MAYBE") {
        val upNFA = aux(ast.left, initState + 1, finStates)
        val downNFA = aux(epsChar, upNFA.finStates.head + 1, finStates)

        unionAddFinTransitions(upNFA, downNFA, initState)
      } else {
        var atom: Char = '\u0000'
        if (ast.elem != "eps") {
          atom = ast.elem(0)
        }

        adjacentTransition = List((initState + 1) -> atom)
        map += (initState -> adjacentTransition)
        new Nfa[Int](initState, Set(initState + 1), map)
      }

    }

    def unionAddFinTransitions(upNFA: Nfa[Int], downNFA: Nfa[Int], initState: Int): Nfa[Int] = {
      val upEpsTransitions = List(upNFA.initState -> '\u0000')
      val downEpsTransitions = List(downNFA.initState -> '\u0000')

      var leftState: Map[Int, List[(Int, Char)]] = Map.empty
      leftState += (initState -> upEpsTransitions.++(downEpsTransitions))

      val upFinNFA = upNFA.map.++(Map(upNFA.finStates.head -> List(downNFA.finStates.head + 1 -> '\u0000')))
      val downFinNFA = downNFA.map.++(Map(downNFA.finStates.head -> List(downNFA.finStates.head + 1 -> '\u0000')))

      new Nfa[Int](upNFA.initState - 1, Set(downNFA.finStates.head + 1), leftState.++(upFinNFA).++(downFinNFA))
    }
    aux(AST.parseToAST(str), 0, Set(0))
  }

}

