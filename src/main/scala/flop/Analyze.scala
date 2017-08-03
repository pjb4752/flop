package flop

object Analyze {

  case class State(isModuleLevel: Boolean, moduleVars: Map[String, Type],
      localVars: List[Map[String, Type]]) {

    def insertModuleVar(name: String, vType: Type): State = {
      this.copy(moduleVars = moduleVars + (name -> vType))
    }

    def insertLocalScope(newVars: Map[String, Type]): State = {
      this.copy(localVars = newVars :: localVars)
    }
  }

  case class CompileError(val message: String) extends Exception(message)

  def analyze(state: State, forms: List[Form]): (State, List[Node]) = {
    val (newState, nodes) = forms.foldLeft((state, List[Node]()))({
        case ((state, nodes), f) =>
      val newNode = tryAnalyze(state)(f)
      val newState = newNode match {
        case Node.DefN(n, _, r) => state.insertModuleVar(n.value, r)
        case _ => state
      }
      (newState, newNode :: nodes)
    })

    (newState, nodes.reverse)
  }

  private def tryAnalyze(state: State)(form: Form): Node = form match {
    case Form.NumF(v) => Node.NumLit(v)
    case Form.StrF(v) => Node.StrLit(v)
    case Form.SymF(v) => analyzeSymbol(state, v)
    case Form.ListF(l) => analyzeList(state, l)
    case Form.MapF(m) => analyzeMap(state, m)
  }

  private def analyzeSymbol(state: State, name: String): Node = {
    if (Core.specialForms.contains(name)) {
      throw CompileError(s"cannot take value of special form ${name}")
    }
    val eType = lookupSymbolType(state, name)
    Node.SymLit(name, eType)
  }

  private def analyzeList(state: State, list: List[Form]): Node = list match {
    case op :: args => analyzeOp(state, op, args)
    case _ => Node.ListLit(List[Node]())
  }

  private def analyzeMap(state: State, map: Map[Form, Form]): Node = {
    val analyzeFn = tryAnalyze(state.copy(isModuleLevel = false)) _
    Node.MapLit(map.map({ case (k ,v) => (analyzeFn(k), analyzeFn(v)) }))
  }

  private def analyzeOp(state: State, op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => analyzeDef(state, args)
      case "let" => analyzeLet(state, args)
      case "if" => analyzeIf(state, args)
      case "fn" => analyzeFunction(state, args)
      case "list" => analyzeListForm(state, args)
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
      val symbolText = args(0).asInstanceOf[Form.SymF].value
      val expr = tryAnalyze(state.copy(isModuleLevel = false))(args(1))

      if (Core.reserved.contains(symbolText)) {
        throw CompileError(s"cannot redefine ${symbolText}")
      }
      val symbol = Node.SymLit(symbolText, expr.eType)

      Node.DefN(symbol, expr, expr.eType)
    }
  }

  private def analyzeLet(state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid let form, expected (let BIND EXPR)")
    } else {
      val bindings = analyzeBindings(state, args(0))
      val symbols = bindings.map({ case (s, e) => (s.value -> e.eType) }).toMap
      val newState = state.insertLocalScope(symbols)
      val expr = tryAnalyze(newState.copy(isModuleLevel = false))(args(1))

      Node.LetN(bindings, expr, expr.eType)
    }
  }

  private def analyzeIf(state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError("invalid if form, expected (if TEST IF-EXPR ELSE-EXPR)")
    } else {
      val analyzed = args.map(tryAnalyze(state.copy(isModuleLevel = false)))
      val testType = analyzed(0).eType
      val ifType = analyzed(1).eType
      val elseType = analyzed(2).eType

      if (testType != Type.Boolean) {
        throw CompileError(s"if test type ${testType} is not a Boolean")
      } else if (ifType != elseType) {
        throw CompileError(s"if expr type ${ifType} does not match else expr type ${elseType}")
      }

      Node.IfN(analyzed(0), analyzed(1), analyzed(2), ifType)
    }
  }

  private def analyzeFunction(state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError("invalid fn form, expected (fn RETURN PARAM EXPR)")
    }

    val rType = analyzeTypeForm(args(0))
    val params = analyzeParams(args(1))
    val symbols = params.map({ case (s, t) => (s.value -> t) }).toMap
    val newState = state.insertLocalScope(symbols)
    val body = tryAnalyze(newState.copy(isModuleLevel = false))(args(2))

    if (body.eType != rType) {
      throw CompileError(s"actual return type ${body.eType} does not match expected ${rType}")
    }

    Node.FlopFn(params, rType, body)
  }

  private def analyzeListForm(state: State, args: List[Form]): Node = {
    val analyzeFn = tryAnalyze(state.copy(isModuleLevel = false)) _
    Node.ListLit(args.map(analyzeFn))
  }

  private def analyzeApply(state: State, op: String, args: List[Form]): Node = {
    val symType = lookupSymbolType(state, op)
    val fnType = symType match {
      case a: Type.Fn => a
      case t => throw CompileError(s"attempt to apply ${op} which is type ${t}")
    }

    val arity = fnType.pTypes.length

    if (args.length != arity) {
      throw CompileError(s"arity mismatch in ${op}, expected ${arity}, got ${args.length}")
    }

    val arguments = args.map(tryAnalyze(state.copy(isModuleLevel = false)))
    for ((arg, i) <- arguments.zipWithIndex) {
      val aType = arg match {
        case Node.SymLit(v, _) => lookupSymbolType(state, v)
        case _ => arg.eType
      }
      val pType = fnType.pTypes(i)
      if (pType != aType) {
        throw CompileError(s"param ${i} expected type ${pType}, got ${aType}")
      }
    }

    if (Core.builtins.contains(op)) {
      val node = Core.builtins(op)
      node.eType match {
        case _:Type.Fn => {
          val builtinFn = node.asInstanceOf[Node.LuaFn]
          Node.LuaApply(builtinFn, arguments, fnType.rType)
        }
        case t => throw CompileError(s"expected fn type, but ${op} is type ${t}")
      }
    } else {
      Node.FlopApply(op, arguments, fnType.rType)
    }
  }

  private def analyzeTypeForm(form: Form): Type = form match {
    case Form.SymF(s) => analyzeTypeLiteral(s)
    case t => throw CompileError(s"unknown type: ${t}")
  }

  private def analyzeTypeLiteral(lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError(s"unknown type: ${t}")
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
      val symbolText = forms(0).asInstanceOf[Form.SymF].value
      val expr = tryAnalyze(state.copy(isModuleLevel = false))(forms(1))
      if (Core.reserved.contains(symbolText)) {
        throw CompileError(s"cannot redefine ${symbolText}")
      }
      val symbol = Node.SymLit(symbolText, expr.eType)

      (symbol, expr)
    }
  }

  private def analyzeParams(form: Form): Node.Params = {
    val rawParams = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError("fn PARAM must be a map of (NAME TYPE)")
    }

    rawParams.map({ case (n, t) => analyzeParam(n, t) }).toList
  }

  private def analyzeParam(pName: Form, pType: Form): Node.Param = {
    val rawName = pName match {
      case Form.SymF(value) => value
      case _ => throw CompileError("fn PARAM must be a name")
    }
    if (Core.reserved.contains(rawName)) {
      throw CompileError(s"cannot redefine ${rawName}")
    }
    val symType = analyzeTypeForm(pType)

    (Node.SymLit(rawName, symType), symType)
  }

  private def lookupSymbolType(state: State, name: String): Type = {
    val localBind = state.localVars.find(_.contains(name))

    if (localBind.nonEmpty) {
      localBind.get(name)
    } else if (state.moduleVars.contains(name)) {
      state.moduleVars(name)
    } else if (Core.builtins.contains(name)) {
      Core.builtins(name).eType
    } else {
      throw CompileError(s"symbol ${name} not found")
    }
  }
}
