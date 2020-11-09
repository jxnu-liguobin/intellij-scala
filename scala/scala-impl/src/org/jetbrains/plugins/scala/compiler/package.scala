package org.jetbrains.plugins.scala

import java.io.File

import com.intellij.openapi.projectRoots.{JavaSdk, JavaSdkVersion, Sdk}
import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.plugins.scala.compiler.CompileServerLauncher.CompileServerProblem

/**
 * @author Pavel Fatin
 */
package object compiler {
  case class JDK(executable: File, tools: Option[File], name: String, version: Option[JavaSdkVersion])

  def toJdk(sdk: Sdk): Either[CompileServerProblem, JDK] = sdk.getSdkType match {
    case jdkType: JavaSdk =>
      val vmExecutable = new File(jdkType.getVMExecutablePath(sdk))
      val tools = Option(jdkType.getToolsPath(sdk)).map(new File(_)) // TODO properly handle JDK 6 on Mac OS
      val version = Option(jdkType.getVersion(sdk))
      Right(JDK(vmExecutable, tools, sdk.getName, version))
    case unexpected =>
      Left(CompileServerProblem.Error(ScalaBundle.message("unexpected.sdk.type.for.sdk", unexpected, sdk)))
  }

  implicit class RichFile(private val file: File) extends AnyVal {
    def canonicalPath: String = FileUtil.toCanonicalPath(file.getPath)
  }
}
