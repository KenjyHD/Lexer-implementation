import scala.math.Numeric.IntIsIntegral

class Dfa[A] (var transitionMap: Map[Char, Map[A, Set[A]]]){
  var initState: A = null.asInstanceOf[A]
  var sinkState: A = null.asInstanceOf[A]
  var finStates: Set[A] = Set.empty

  val eps = '\u0000'

  def map[B](f: A => B): Dfa[B] = {
    val newTransitionMap: Map[Char, Map[B, Set[B]]] = transitionMap.map(x =>
      x._1 -> x._2.map(y => (f(y._1), y._2.map(f))
      )
    )
    val dfa: Dfa[B] = new Dfa[B](newTransitionMap)
    dfa.initState = f(initState)
    dfa.finStates = finStates.map(f)
    dfa
  }

  def next(state:A, c: Char): A = {
    for (st <- transitionMap(eps)) {
      if (st._2 == transitionMap(c)(state)) {
        return st._1
      }
    }
    null.asInstanceOf[A]
  }

  def accepts(str: String): Boolean = {
    tryAccept(str, initState)
  }

  def tryAccept(str: String, currState: A): Boolean = {
    if (isFinal(currState) && str.isEmpty) {
      return true
    } else if (currState != null && transitionMap(eps)(currState).isEmpty) {
      return false
    }

    if (str.nonEmpty && transitionMap.contains(str.head)) {
      if (tryAccept(str.tail, next(currState, str.head))) return true
    }
    false
  }

  def getStates : Set[A] = {
    var set: Set[A] = Set.empty
    transitionMap(eps).foreach(x =>
      set += x._1
    )
    set
  }

  def isFinal(state: A): Boolean = {
    finStates.contains(state)
  }

  def isSink(state: A): Boolean = {
    state == sinkState
  }
}

object Dfa extends App {
  val eps = '\u0000'
  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    fromNFA(nfa)
  }

  def fromNFA(nfa: Nfa[Int]): Dfa[Int] = {
    val alphabet = nfa.getAlphabet
    var firstStatesSet: Set[Int] = Set.empty

    def createFirstGroup(currState: Int): Set[Int] = {
      firstStatesSet += currState
      if (!nfa.isFinal(currState)) {
        nfa.map(currState).foreach(x =>
          if (x._2 == eps && !firstStatesSet.contains(x._1)) {
            createFirstGroup(x._1)
          }
        )
      }
      firstStatesSet
    }

    val firstStateList = createFirstGroup(0)

    var accesibleStates: Set[Int] = Set.empty
    var statePath: List[Int] = List.empty

    def listAccesibleStates(currState: Int, c: Char, charConsumed: Boolean): Set[Int] = {
      if (!nfa.isFinal(currState)) {
        var cycle = false
        if (statePath.contains(currState)) cycle = true
        statePath = statePath.++(List(currState))
        nfa.map(currState).foreach(x =>
          if (x._2 == eps && charConsumed) {
            accesibleStates += x._1
            if(!cycle) {
              listAccesibleStates(x._1, c, charConsumed)
            }
          } else if (x._2 == c && !charConsumed) {
            accesibleStates += x._1
            listAccesibleStates(x._1, c, charConsumed = true)
          }
        )
      } else if (charConsumed && nfa.isFinal(currState)) {
        accesibleStates += currState
      }
      accesibleStates
    }

    var transitionMap: Map[Char, Map[Int, Set[Int]]] = Map.empty
    transitionMap = transitionMap.++(Map('\u0000' -> Map(0 -> firstStateList)))

    def createTransitionList(currDFAStateSet: Set[Int], currDFAState: Int): Map[Int, Map[Char, Set[Set[Int]]]] = {
      var i = 0
      for (ch <- alphabet) {
        i += 1
        if (ch != '\u0000') {
          var set: Set[Int] = Set.empty
          for (nfaState <- currDFAStateSet) {
            accesibleStates = Set.empty
            statePath = List.empty
            set = set.++(listAccesibleStates(nfaState, ch, charConsumed = false))
          }
          if (!transitionMap.contains(ch)) {
            transitionMap = transitionMap.++(Map(ch -> Map(currDFAState -> set)))
          } else {
            transitionMap = transitionMap.++(Map(ch -> Map(currDFAState -> set).++(transitionMap(ch))))
          }
          val maxState = transitionMap(eps).keysIterator.max
          if (!containsSet(transitionMap(eps), set)) {
            transitionMap =
              transitionMap.++(Map(eps -> Map((maxState + 1) -> set).++(transitionMap(eps))))
            createTransitionList(set, maxState + 1)
          }
        }
      }
      null
    }

    def containsSet(map: Map[Int, Set[Int]], set: Set[Int]): Boolean = {
      map.foreach(x =>
        if (x._2 == set) return true
      )
      false
    }

    createTransitionList(transitionMap('\u0000')(0), 0)
    //println(transitionMap)
    val dfa: Dfa[Int] = new Dfa[Int](transitionMap)
    dfa.initState = 0

    def detectFinalStates(transitionMap: Map[Char, Map[Int, Set[Int]]]): Set[Int] = {
      var set: Set[Int] = Set.empty
      transitionMap(eps).foreach(x =>
        x._2.foreach(y =>
          if (nfa.isFinal(y)) set += x._1 /////////////////////////////////////////////////////
        )
      )
      set
    }

    def detectSinkState(transitionMap: Map[Char, Map[Int, Set[Int]]]): Int = {
      transitionMap(eps).foreach(
        x => if(x._2 == Set.empty) return x._1
      )
      0
    }
    dfa.sinkState = detectSinkState(dfa.transitionMap)
    dfa.finStates = detectFinalStates(dfa.transitionMap)
    dfa
  }

  println(fromPrenex("STAR STAR f").transitionMap)
//  println(fromPrenex("STAR UNION a b").transitionMap)

}