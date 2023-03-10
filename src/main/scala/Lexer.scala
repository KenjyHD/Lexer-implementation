case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    val eps = '\u0000'
    val splitSpec = spec.split(";\n")
    var tokenRegex: Map[String, String] = Map.empty
    val finNFA = new Nfa(0, Set.empty, Map(0 -> List.empty))
    var tokenFinState: Map[Int, String] = Map.empty
    var stateNr = 1
    for(line <- splitSpec) {
      val token = line.split(": ")(0)
      var regex = line.split(": ")(1)
      regex = regex.replace("\\n", "\n")
      regex = regex.replace("\\t", "\t")
      tokenRegex = tokenRegex.++(Map(token -> regex))
      val prenex = Regex.toPrenex(regex)
      val nfa = Nfa.fromPrenex(prenex).map(_ + stateNr)
      stateNr += nfa.map.size + 1
      finNFA.map = finNFA.map.++(nfa.map)
      finNFA.map = finNFA.map + (0 -> finNFA.map(0).++(List(nfa.initState -> eps)))
      finNFA.finStates += (stateNr - 1)
      tokenFinState += ((stateNr - 1) -> token)
    }
    val dfa = Dfa.fromNFA(finNFA)

    var res: Either[String,List[(String,String)]] = null

    def getToken(state: Int): String = {
      if(tokenFinState.contains(state)) {
        return tokenFinState(state)
      }
      null
    }

    var resList: List[(String, String)] = List.empty
    var posibleTokens: Map[Int, String] = Map.empty
    var lastKnownPath = ""
    var isError = false
    var errorMsg = ""
    var lines = 0
    def buildLexer(str: String, currPath: String, currState: Int): Unit = {
      if (str.nonEmpty && !dfa.transitionMap.contains(str.head)) {
        isError = true
        errorMsg = "No viable alternative at character " + (word.length - str.length) + ", line " + lines
        return
      }

      if(str.isEmpty && currPath.isEmpty) return

      if(str.isEmpty && !dfa.isSink(currState) && dfa.isFinal(currState)) {
        posibleTokens = Map.empty
        dfa.transitionMap(eps)(currState).foreach(
          x => if(getToken(x) != null) {
            posibleTokens = posibleTokens.++(Map(x -> getToken(x)))
          }
        )
        resList = resList.++(List(currPath.take(currPath.length) -> posibleTokens.minBy(_._1)._2))
        return
      }

      if(str.isEmpty && !dfa.isSink(currState) && !dfa.isFinal(currState)) {
        if(posibleTokens.isEmpty) {
          isError = true
          errorMsg = "No viable alternative at character EOF, line " + lines
          return
        }
        var maxLenRegex = ""
        var maxLenRegexToken = ""
        for(token <- posibleTokens) {
          if(maxLenRegex.length < tokenRegex(token._2).length) {
            maxLenRegex = tokenRegex(token._2)
            maxLenRegexToken = token._2
          }
        }
        resList = resList.++(List(maxLenRegex -> maxLenRegexToken))
        buildLexer(str, currPath.substring(maxLenRegex.length), currState)
        return
      }

      if (dfa.isFinal(currState)) {
        if (str.nonEmpty && str.head == '\n') lines += 1
        lastKnownPath = currPath
        posibleTokens = Map.empty
        dfa.transitionMap(eps)(currState).foreach(
          x => if(getToken(x) != null) {
            posibleTokens = posibleTokens.++(Map(x -> getToken(x)))
          }
        )
        buildLexer(str.tail, currPath + str.head, dfa.next(currState, str.head))
      } else if(dfa.isSink(currState)) {
        if (posibleTokens.isEmpty) {
          isError = true
          errorMsg = "No viable alternative at character " + (word.length - str.length - 1) + ", line " + lines
          return
        }
        resList = resList.++(List(lastKnownPath -> posibleTokens.minBy(_._1)._2))
        posibleTokens = Map.empty
        var reg = lastKnownPath
        if(reg.charAt(0) == '(') reg = "[(]" + reg.tail
        if(reg.charAt(0) == ')') reg = "[)]" + reg.tail
        if(reg.charAt(0) == '+') reg = "[+]" + reg.tail
        if(reg.charAt(0) == '*') reg = "[*]" + reg.tail
//        println(currPath.replaceFirst(reg, "") + str)
        buildLexer(currPath.replaceFirst(reg, "") + str, "", 0)
      } else {
        buildLexer(str.tail, currPath + str.head, dfa.next(currState, str.head))
      }
    }
    buildLexer(word, "", dfa.initState)
    if(isError) {
      res = Left(errorMsg)
    } else {
      res = Right(resList)
    }

    res
  }

//  def consumeSubRegex(regex: String, sub): String = {
//
//  }
}


object Test extends App {
  val source = io.Source.fromFile("src/main/scala/configuration")
  val spec = try source.mkString.stripMargin('#').replaceAll("\r\n", "\n") finally source.close()
  val testsNames = (1 to 8).toList.map(num => "src/test/prog_tests/" + num + ".in")
  val words = testsNames.map(file => {
    val _source = io.Source.fromFile(file)
    try _source.mkString finally _source.close()
  })
  def transform(lexRes: Either[String, List[(String, String)]]): List[String] = {
    lexRes match {
      case Left(_) => throw new Exception("Parsing cannot fail here, revise you configuration")
      case Right(l) => l.map(elem => elem._2)
    }
  }
//  val results = words.map(word => transform(Lexer(spec).lex(word)))
//  println("+ a".replaceFirst("[+] ", ""))
  val results = transform(Lexer(spec).lex(words(7)))
  println(words(7))
  println(results)

//  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "IF", "OPEN_PARANTHESIS", "VARIABLE", "EQUAL", "NUMBER", "CLOSE_PARANTHESIS", "THEN", "VARIABLE", "ASSIGN", "VARIABLE", "PLUS", "NUMBER", "ELSE", "VARIABLE", "ASSIGN", "VARIABLE", "FI", "END"))
//  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "IF", "OPEN_PARANTHESIS", "VARIABLE", "PLUS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "THEN", "VARIABLE", "ASSIGN", "VARIABLE", "MINUS", "VARIABLE", "ELSE", "VARIABLE", "ASSIGN", "VARIABLE", "FI", "END"))
//  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "WHILE", "OPEN_PARANTHESIS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "DO", "VARIABLE", "ASSIGN", "VARIABLE", "MINUS", "NUMBER", "OD", "END"))
//  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "WHILE", "OPEN_PARANTHESIS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "DO", "BEGIN", "VARIABLE", "ASSIGN", "VARIABLE", "MULTIPLY", "VARIABLE", "VARIABLE", "ASSIGN", "VARIABLE", "MINUS", "NUMBER", "END", "OD", "END"))
//  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "WHILE", "OPEN_PARANTHESIS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "DO", "BEGIN", "VARIABLE", "ASSIGN", "VARIABLE", "MULTIPLY", "VARIABLE", "VARIABLE", "ASSIGN", "VARIABLE", "MINUS", "NUMBER", "IF", "OPEN_PARANTHESIS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "THEN", "VARIABLE", "ASSIGN", "NUMBER", "ELSE", "VARIABLE", "ASSIGN", "VARIABLE", "PLUS", "NUMBER", "FI", "END", "OD", "END"))
  assert(results == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "VARIABLE", "ASSIGN", "NUMBER", "IF", "OPEN_PARANTHESIS", "VARIABLE", "MINUS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "THEN", "VARIABLE", "ASSIGN", "NUMBER", "ELSE", "BEGIN", "WHILE", "OPEN_PARANTHESIS", "VARIABLE", "MINUS", "VARIABLE", "GREATER", "NUMBER", "CLOSE_PARANTHESIS", "DO", "VARIABLE", "ASSIGN", "VARIABLE", "PLUS", "NUMBER", "OD", "VARIABLE", "ASSIGN", "MINUS", "NUMBER", "END", "FI", "END"))

  //  println(Dfa.fromPrenex(Regex.toPrenex("1' '0\n")).transitionMap)
}