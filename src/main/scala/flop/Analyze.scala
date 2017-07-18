package flop

object Analyze {

  case class CompileError(val message: String) extends Exception(message)

  def analyze(forms: List[Form]): List[Node] = {
    @scala.annotation.tailrec
    def analyze0(forms: List[Form], nodes: List[Node]): List[Node] = {
      if (forms.isEmpty) {
        nodes
      } else {
        analyze0(forms.tail, tryAnalyze(forms.head) :: nodes)
      }
    }
    analyze0(forms, List[Node]())
  }

  private def tryAnalyze(form: Form): Node = form match {
    case Form.NumF(v) => Node.NumLit(v)
    case Form.StrF(v) => Node.StrLit(v)
    case Form.SymF(v) => Node.SymLit(v)
    case Form.ListF(l) => analyzeList(l)
  }

  private def analyzeList(list: List[Form]): Node = list match {
    case op :: args => analyzeOp(op, args)
    case _ => Node.ListLit(List[Node]())
  }

  private def analyzeOp(op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => analyzeDef(args)
      case "let" => analyzeLet(args)
      case "if" => analyzeIf(args)
      case _ => analyzeApply(s, args)
    }
    case u => throw new CompileError(s"cannot apply ${u}")
  }

  private def analyzeDef(args: List[Form]): Node = {
    if (args.length != 2) {
      throw new CompileError("invalid def form, expected (def SYM EXPR)")
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw new CompileError("def expects first arg to be a name")
    } else {
      val sym = tryAnalyze(args(0))
      val expr = tryAnalyze(args(1))
      Node.DefN(sym.asInstanceOf[Node.SymLit], expr)
    }
  }

  private def analyzeLet(args: List[Form]): Node = {
    if (args.length != 2) {
      throw new CompileError("invalid let form, expected (let BIND EXPR)")
    } else {
      val bindings = analyzeBindings(args(0))
      val exprs = tryAnalyze(args(1))
      Node.LetN(bindings, exprs)
    }
  }

  private def analyzeIf(args: List[Form]): Node = {
    if (args.length != 3) {
      throw new CompileError("invalid if form, expected (if TEST IF-EXPR ELSE-EXPR)")
    } else {
      val analyzed = args.map(tryAnalyze)
      Node.IfN(analyzed(0), analyzed(1), analyzed(2))
    }
  }

  private def analyzeApply(op: String, args: List[Form]): Node = {
    if (!Core.exported.contains(op)) {
      throw new CompileError(s"unknown operation: ${op}")
    }
    val fn = Core.exported(op)
    if (args.length != fn.arity) {
      throw new CompileError(s"arity mismatch in ${op}, expected ${fn.arity}, got ${args.length}")
    }
    val params = args.map(tryAnalyze)
    Node.ApplyN(fn, params)
  }

  private def analyzeBindings(form: Form): List[(Node.SymLit, Node)] = {
    val rawBindings = form match {
      case Form.ListF(raw) => raw
      case _ => throw new CompileError("BIND must be a list of SYM EXPR pairs")
    }
    if (rawBindings.length % 2 != 0) {
      throw new CompileError("BIND must have even number of terms")
    }
    rawBindings.grouped(2).map(analyzeBinding).toList
  }

  private def analyzeBinding(forms: List[Form]): (Node.SymLit, Node) = {
    if (!forms.head.isInstanceOf[Form.SymF]) {
      throw new CompileError("BIND expects first value to be a name")
    } else {
      val sym = tryAnalyze(forms(0))
      val expr = tryAnalyze(forms(1))
      (sym.asInstanceOf[Node.SymLit], expr)
    }
  }
}
