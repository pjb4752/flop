package flop

object Analyze {

  case class State(isModuleLevel: Boolean, moduleTraits: Map[String, List[Node.FnDef]],
      moduleVars: Map[String, Type], localVars: List[Map[String, Type]]) {

    def insertModuleTrait(name: String, fnDefs: List[Node.FnDef]) = {
      this.copy(moduleTraits = moduleTraits + (name -> fnDefs))
    }

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
        case Node.TraitN(n, f) => state.insertModuleTrait(n.value, f)
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

    name match {
      case "true" => Node.TrueLit
      case "false" => Node.FalseLit
      case _ => {
        val eType = lookupSymbolType(state, name)
        Node.SymLit(name, eType)
      }
    }
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
      case "trait" => analyzeTrait(state, args)
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

    val fnType = Type.FreeFn(params.map(_._2), rType)
    Node.FlopFn(fnType, params, body)
  }

  private def analyzeListForm(state: State, args: List[Form]): Node = {
    val analyzeFn = tryAnalyze(state.copy(isModuleLevel = false)) _
    Node.ListLit(args.map(analyzeFn))
  }

  private def analyzeTrait(state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid trait form, expected (trait NAME FNDEFS")
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError("trait expects first arg to be a name")
    } else if (!state.isModuleLevel) {
      throw CompileError("trait must occur at module (top) level")
    } else {
      val symbolText = args.head.asInstanceOf[Form.SymF].value
      if (Core.builtinTraits.keys.toSet.contains(symbolText)) {
        throw CompileError(s"cannot redefine trait ${symbolText}")
      } else if (state.moduleTraits.contains(symbolText)) {
        throw CompileError(s"cannot redefine trait ${symbolText}")
      }
      val symbol = Node.SymLit(symbolText, Type.Trait)
      val fnDefs = analyzeFnDefs(symbol, args(1))

      Node.TraitN(symbol, fnDefs)
    }
  }

  private def analyzeApply(state: State, op: String, args: List[Form]): Node = {
    lookupSymbolType(state, op) match {
      case ff: Type.FreeFn => analyzeApplyFreeFn(state, op, ff, args)
      case lf: Type.LuaFn => analyzeApplyLuaFn(state, op, lf, args)
      case tf: Type.TraitFn => analyzeApplyTraitFn(state, op, tf, args)
      case t => throw CompileError(s"attempt to apply ${op} which is type ${t}")
    }
  }

  private def analyzeApplyFreeFn(state: State, op: String, fnType: Type.FreeFn,
       args: List[Form]): Node = {
    val arguments = analyzeArguments(state, op, fnType, args)

    Node.FlopApply(op, arguments, fnType.rType)
  }

  private def analyzeApplyLuaFn(state: State, op: String, fnType: Type.LuaFn,
      args: List[Form]): Node = {
    val arguments = analyzeArguments(state, op, fnType, args)
    val luaFn = Core.builtins(op).asInstanceOf[Node.LuaFn]

    Node.LuaApply(luaFn, arguments, fnType.rType)
  }

  private def analyzeApplyTraitFn(state: State, op: String,
      fnType: Type.TraitFn, args: List[Form]): Node = {

    // TODO For now we assume that any trait takes "self" as first param
    // but in the future we'll need to type annotate somehow for traitFns
    // that might not take a self parameter, or come up with another solution
    val arguments = analyzeArguments(state, op, fnType, args, Some(0))
    val selfType = arguments.head
    val traitImpl = findTraitImpl(state, op, selfType.eType)

    if (traitImpl.isEmpty) {
      throw CompileError(s"No implementation of trait FN '${op}' for type ${selfType.eType}")
    }

    // at this point none of this should blow up
    val fnImpl = traitImpl.get.fnImpls.find(_._1.value == op).get._2
    val rType = fnImpl.rType

    fnImpl match {
      case lf: Node.LuaFn => Node.LuaApply(lf, arguments, rType)
      case ff: Node.FlopFn => Node.FlopApply(op, arguments, rType)
    }
  }

  private def analyzeArguments(state: State, op: String, fnType: Type.Fn,
      args: List[Form], selfPos: Option[Int] = None): List[Node] = {
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
      if (selfPos.nonEmpty && selfPos.get == i) {
        // don't check type of selfType
      } else if (pType != aType) {
        throw CompileError(s"param ${i} expected type ${pType}, got ${aType}")
      }
    }

    arguments
  }

  private def analyzeTypeForm(form: Form): Type = form match {
    case Form.SymF(s) => analyzeTypeLiteral(s)
    case t => throw CompileError(s"unknown type: ${t}")
  }

  private def analyzeTypeLiteral(lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
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

  private def analyzeFnDefs(traitName: Node.SymLit, form: Form): List[Node.FnDef] = {
    val rawDefs = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError("FNDEFS must be a map of (NAME FNDEF)")
    }

    rawDefs.map({ case (n, f) => analyzeFnDef(traitName, n, f) }).toList
  }

  private def analyzeFnDef(traitName: Node.SymLit, fName: Form, fnDef: Form): Node.FnDef = {
    val rawName = fName match {
      case Form.SymF(value) => value
      case _ => throw CompileError("FNDEF must be a (NAME DEF) pair")
    }
    val rawFnDef = fnDef match {
      case Form.ListF(values) => values
      case _ => throw CompileError("FNDEF must be a (NAME DEF) pair")
    }
    if (rawFnDef.length < 1) {
      throw CompileError("invalid FNDEF form, expected (RETURN [PARAMS])")
    }

    val types = rawFnDef.map(analyzeTypeForm)
    val fnType = Type.TraitFn(types.tail, types.head)
    val fnName = Node.SymLit(rawName, fnType)

    Node.FnDef(traitName, fnName, fnType)
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
      val maybeFn = findTraitFnType(state, name)

      if (maybeFn.nonEmpty) {
        maybeFn.get
      } else {
        throw CompileError(s"symbol ${name} not found")
      }
    }
  }

  private def findTraitFnDef(state: State, name: String): Option[Node.FnDef] = {
    Core.builtinTraits.flatMap(_._2).
        find({ case Node.FnDef(_, n, _) => n.value == name })
  }

  private def findTraitFnType(state: State, name: String): Option[Type] = {
    findTraitFnDef(state, name).map({ case Node.FnDef(_, _, t) => t })
  }

  private def findTraitImpl(state: State, name: String, selfType: Type):
      Option[Node.TraitImpl] = {
    Core.traitImpls.get(name).flatMap(_.get(selfType))
  }
}
