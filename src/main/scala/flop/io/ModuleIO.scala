package flop.io

import java.nio.file.{Path, Paths}

import flop.analysis.Name

object ModuleIO {

  def pathFromName(name: Name.ModuleName): Path = {
    val pathParts = (name.tree :: name.paths) :+ addExtension(name.name)
    Paths.get(pathParts.mkString(pathSeparator))
  }

  def pathSeparator = "/"

  def addExtension(name: String) = s"${name}.flp"
}
