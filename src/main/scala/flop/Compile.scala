package flop

import java.nio.file.{Path, Paths, Files}
import java.util.stream.Collectors

import scala.collection.JavaConverters._

import flop.analysis._
import flop.backend._
import flop.io.ModuleIO
import flop.reading._

object Compile {

  def compileAll(rawPaths: List[String]): Unit = {
    val paths = rawPaths.map(rp => Paths.get(rp))
    val badPaths = paths.filter(p => !Files.exists(p))

    if (badPaths.isEmpty) {
      compileAll(SymbolTable.withRoot(""), paths)
    } else {
      println(s"Bad file paths: ${badPaths}")
    }
  }

  def compileAll(table: SymbolTable, paths: List[Path]): SymbolTable = {
    paths.foldLeft(table)({ case (t, p) =>
      val source = Files.lines(p).collect(Collectors.joining())
      // TODO handle empty file
      val forms = Reading.read(source)

      compileModule(t, forms)
    })
  }

  def compileModule(table: SymbolTable, forms: List[Form]): SymbolTable = {
    val module = Analysis.analyzeModuleDef(table, forms.head)
    val Name.ModuleName(tree, paths, name) = module.name

    if (SymbolTable.isLoaded(table, tree, paths :+ name)) {
      table
    } else {
      val toLoad = module.imports.filter({ case (_, Name.ModuleName(t, p, n)) =>
        !SymbolTable.isLoaded(table, t, p :+ n)
      })
      val paths = toLoad.map(i => ModuleIO.pathFromName(i._2))

      // first we compile dependencies
      val tableWithDeps = compileAll(table, paths.toList)
      // then we add the module to the table
      val modTable = SymbolTable.addModule(tableWithDeps, module)
      // then we compile the module in question
      val (finalTable, ast) = Analysis.analyze(modTable, module, forms.tail)
      val mPaths = module.name.paths :+ module.name.name
      val finalModule = SymbolTable.findModule(
          finalTable, module.name.tree, mPaths).get

      emitSource(finalModule, ast)

      finalTable
    }
  }

  def emitSource(module: ModuleTree.Module, ast: List[Node]): Unit = {
    val source = Backend.emit(ast)
    val dirPath = ModuleIO.outputDirPath(module.name)
    Files.createDirectories(dirPath)

    val outputPath = ModuleIO.outputPath(module.name)
    val emittedSource = Backend.emitModule(module, ast)

    Files.write(outputPath, emittedSource.asJava)
  }
}
