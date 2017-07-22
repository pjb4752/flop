package flop

object Analyze {

  case class State(isModuleLevel: Boolean)

  case class CompileError(val message: String) extends Exception(message)

  def analyze(forms: List[Form]): List[Node] =
    forms.map(tryAnalyze(State(isModuleLevel = true)))

  private def tryAnalyze(state: State)(form: Form): Node = form match {
    case Form.NumF(v) => Node.NumLit(v)
    case Form.StrF(v) => Node.StrLit(v)
    case Form.SymF(v) => Node.SymLit(v)
    case Form.ListF(l) => analyzeList(state, l)
  }

  private def analyzeList(state: State, list: List[Form]): Node = list match {
    case op :: args => analyzeOp(state, op, args)
    case _ => Node.ListLit(List[Node]())
  }

  private def analyzeOp(state: State, op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => analyzeDef(state, args)
      case "let" => analyzeLet(state, args)
      case "if" => analyzeIf(state, args)
      case _ => analyzeApply(state, s, args)
    }
    case u => throw CompileError(s"cannot apply ${u}")
  }

  private def analyzeDef(state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid def form, expected (def SYM EXPR)")
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError("def expects first arg to be a name")
    } else if (!state.isModuleLevel) {
      throw CompileError("def must occur at module (top) level")
    } else {
      val sym = tryAnalyze(state)(args(0))
      val expr = tryAnalyze(state.copy(isModuleLevel = false))(args(1))
      Node.DefN(sym.asInstanceOf[Node.SymLit], expr)
    }
  }

  private def analyzeLet(state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid let form, expected (let BIND EXPR)")
    } else {
      val bindings = analyzeBindings(state, args(0))
      val exprs = tryAnalyze(state.copy(isModuleLevel = false))(args(1))
      Node.LetN(bindings, exprs)
    }
  }

  private def analyzeIf(state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError("invalid if form, expected (if TEST IF-EXPR ELSE-EXPR)")
    } else {
      val analyzed = args.map(tryAnalyze(state.copy(isModuleLevel = false)))
      Node.IfN(analyzed(0), analyzed(1), analyzed(2))
    }
  }

  private def analyzeApply(state: State, op: String, args: List[Form]): Node = {
    if (!Core.exported.contains(op)) {
      throw CompileError(s"unknown operation: ${op}")
    }
    val fn = Core.exported(op)
    if (args.length != fn.arity) {
      throw CompileError(s"arity mismatch in ${op}, expected ${fn.arity}, got ${args.length}")
    }
    val params = args.map(tryAnalyze(state.copy(isModuleLevel = false)))
    Node.ApplyN(fn, params)
  }

  private def analyzeBindings(state: State, form: Form): List[(Node.SymLit, Node)] = {
    val rawBindings = form match {
      case Form.ListF(raw) => raw
      case _ => throw CompileError("BIND must be a list of SYM EXPR pairs")
    }
    if (rawBindings.length % 2 != 0) {
      throw CompileError("BIND must have even number of terms")
    }
    rawBindings.grouped(2).map(analyzeBinding(state)).toList
  }

  private def analyzeBinding(state: State)(forms: List[Form]): (Node.SymLit, Node) = {
    if (!forms.head.isInstanceOf[Form.SymF]) {
      throw CompileError("BIND expects first value to be a name")
    } else {
      val sym = tryAnalyze(state.copy(isModuleLevel = false))(forms(0))
      val expr = tryAnalyze(state.copy(isModuleLevel = false))(forms(1))
      (sym.asInstanceOf[Node.SymLit], expr)
    }
  }
}
