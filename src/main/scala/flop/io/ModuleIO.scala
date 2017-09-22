package flop.io

import java.nio.file.{Path, Paths}

import flop.analysis.Name

object ModuleIO {

  def pathFromName(name: Name.ModuleName): Path = {
    val pathParts = (name.tree :: name.paths) :+ addExtension(name.name)
    Paths.get(pathParts.mkString(pathSeparator))
  }

  def outputDir(name: Name.ModuleName): List[String] = {
    name.tree :: buildDir :: name.paths
  }

  def outputDirPath(name: Name.ModuleName): Path = {
    Paths.get(outputDir(name).mkString(pathSeparator))
  }

  def outputPath(name: Name.ModuleName): Path = {
    val pathParts = outputDir(name) :+ addLuaExtension(name.name)
    Paths.get(pathParts.mkString(pathSeparator))
  }

  def buildDir = "build"

  def pathSeparator = "/"

  def addExtension(name: String) = s"${name}.flp"

  def addLuaExtension(name: String) = s"${name}.lua"
}
