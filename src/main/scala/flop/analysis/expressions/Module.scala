package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

import flop.analysis.ModuleTree.{Module => FlopModule}

object Module {

  def analyze(table: SymbolTable, form: Form): FlopModule = {
    val forms = form match {
      case Form.ListF(v) => v
      case _ => throw syntaxError(defaultMessage(form.toString))
    }

    val (nameParts, imports) = forms match {
      case Form.SymF(m) :: Form.SymF(n) :: Nil => analyzeModuleDef(m, n)
      case Form.SymF(m) :: Form.SymF(n) :: Form.ListF(i) :: Nil => analyzeModuleDef(m, n, i)
      case _ => throw syntaxError(defaultMessage(forms.toString))
    }

    val analyzedImports = imports match {
      case Form.SymF(v) :: tail if v == "import" => analyzeImports(tail)
      case Nil => Map[String, Name.ModuleName]()
      case _ => {
        val message = s"""Expected module import statements
                         |  got: ${imports}""".stripMargin
        throw syntaxError(message)
      }
    }

    val root :: rest = nameParts
    val name :: paths = rest.reverse
    val moduleName = Name.ModuleName(root, paths, name)
    val initialModule = FlopModule.initial(moduleName, Core.stdLibImports)
    initialModule.copy(imports = initialModule.imports ++ analyzedImports)
  }

  private def defaultMessage(target: String): String = {
    s"""Expected valid module header
       |  got: ${target}""".stripMargin
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """module definitions must be of the form:
        |  (module NAME [IMPORTS])
        |  where:
        |    NAME is a SYMBOL of the form:
        |      root.path2.pathn.name
        |    IMPORTS is of the form:
        |      (import IMPORT-EXPR)
        |    where:
        |      IMPORT-EXPR is of the form:
        |        fully.qualified.import OR
        |        (fully.qualified.import abbr)""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }

  private def analyzeModuleDef(m: String, n: String): (List[String], List[Form]) = {
    if (m != "module") {
      throw syntaxError(defaultMessage(m))
    } else if (!isValidName(n)) {
      throw syntaxError(s"Module NAME '${n}' is not of the form path.name")
    } else {
      (analyzeName(n), List[Form]())
    }
  }

  private def analyzeModuleDef(m: String, n: String, rest: List[Form]): (List[String], List[Form]) = {
    val (name, _) = analyzeModuleDef(m, n)
    (name, rest)
  }

  private def isValidName(name: String): Boolean = {
    val nameParts = analyzeName(name)
    nameParts.length >= 3 && !nameParts.exists(_.isEmpty)
  }

  private def analyzeName(name: String): List[String] = {
    name.split('.').toList
  }

  private def analyzeImports(forms: List[Form]): Map[String, Name.ModuleName] = {
    forms.foldLeft(Map[String, Name.ModuleName]())((m, f) => {
      val newImport = f match {
        case Form.SymF(v) => analyzeSimpleImport(v)
        case Form.ListF(l) => analyzeAliasedImport(l)
        case _ => {
          val message = s"Expected import statement to contain " +
            "a valid module path, got ${forms}"
          throw syntaxError(message)
        }
      }

      val newAlias = newImport._1
      if (m.contains(newAlias)) {
        val message = s"Duplicated module alias ${newAlias}"
        throw CompileError.moduleError(message)
      }

      m + newImport
    }).toMap
  }

  private def analyzeSimpleImport(module: String): (String, Name.ModuleName) = {
    val moduleName = analyzeModuleName(module)
    (moduleName.name, moduleName)
  }

  private def analyzeAliasedImport(form: List[Form]): (String, Name.ModuleName) = {
    if (form.length != 2) {
      val message = s"Expected import statement to contain " +
        s"a fully qualified module path and abbreviation, got ${form}"
      throw syntaxError(message)
    }

    val moduleForm :: abbrForm :: Nil = form

    val module = moduleForm match {
      case Form.SymF(m) => m
      case _ => {
        val message = s"Expected import statement to contain " +
          s"a fully qualified module path, got ${moduleForm}"
        throw syntaxError(message)
      }
    }

    val abbr = abbrForm match {
      case Form.SymF(a) => a
      case _ => {
        val message = s"Expected import statement to contain " +
          s"a module abbreviation, got ${moduleForm}"
        throw syntaxError(message)
      }
    }

    val moduleName = analyzeModuleName(module)
    (abbr, moduleName)
  }

  private def analyzeModuleName(module: String): Name.ModuleName = {
    val moduleParts = module.split('.').toList

    if (moduleParts.length < 3) {
      val message = s"Expected import statement to contain " +
        s" a fully qualified module path, got ${module}"
      throw syntaxError(message)
    }

    Name.ModuleName.fromList(moduleParts)
  }
}
