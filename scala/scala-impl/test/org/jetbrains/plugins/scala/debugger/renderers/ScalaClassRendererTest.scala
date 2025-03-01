package org.jetbrains.plugins.scala.debugger.renderers

import com.intellij.debugger.settings.NodeRendererSettings
import org.jetbrains.plugins.scala.DebuggerTests
import org.jetbrains.plugins.scala.util.runners.{MultipleScalaVersionsRunner, RunWithScalaVersions, TestScalaVersion}
import org.junit.experimental.categories.Category
import org.junit.runner.RunWith

import scala.concurrent.duration._

@RunWith(classOf[MultipleScalaVersionsRunner])
@RunWithScalaVersions(Array(
  TestScalaVersion.Scala_2_11,
  TestScalaVersion.Scala_2_12,
  TestScalaVersion.Scala_2_13,
  TestScalaVersion.Scala_3_0,
  TestScalaVersion.Scala_3_1
))
@Category(Array(classOf[DebuggerTests]))
class ScalaClassRendererTest extends RendererTestBase {

  addFileWithBreakpoints("test/ScalaObject.scala",
    s"""package test
       |
       |object ScalaObject {
       |  private[this] val privateThisVal: Double = 1.0
       |  private val privateVal: Int = 2
       |  private[test] val packagePrivateVal: String = "3"
       |  val publicVal: Array[Int] = Array.empty
       |
       |  lazy val lazyVal: String = "lazy"
       |
       |  private[this] var privateThisVar: Double = 4.0
       |  private var privateVar: Int = 5
       |  private[test] var packagePrivateVar: String = "6"
       |  var publicVar: Array[Int] = Array.empty
       |
       |  override def hashCode: Int = 1
       |
       |  def main(args: Array[String]): Unit = {
       |    // Need to use all private variables to avoid compiler optimizations
       |    val myThis = ScalaObject.this
       |    println(privateThisVal)$bp
       |    println(privateVal)
       |    println(privateThisVar)
       |    println(privateVar)
       |  }
       |}""".stripMargin)
  def testScalaObject(): Unit = {
    testClassRenderer("test/ScalaObject")("myThis", "test.ScalaObject$", "@1",
      Set(
        "privateThisVal = 1.0",
        "privateVal = 2",
        "packagePrivateVal = 3",
        "publicVal = {int[0]@uniqueID}[]",
        "lazyVal = lazy",
        "privateThisVar = 4.0",
        "privateVar = 5",
        "packagePrivateVar = 6",
        "publicVar = {int[0]@uniqueID}[]"
      ))(10.seconds)
  }

  addFileWithBreakpoints("test/ScalaClass.scala",
    s"""package test
       |
       |class ScalaClass(unusedConstructorParam: Int, usedConstructorParam: Int) {
       |  private[this] val privateThisVal: Double = 1.0
       |  private val privateVal: Int = 2
       |  private[test] val packagePrivateVal: String = "3"
       |  val publicVal: Array[Int] = Array.empty
       |
       |  lazy val lazyVal: String = "lazy"
       |
       |  private[this] var privateThisVar: Double = 4.0
       |  private var privateVar: Int = 5
       |  private[test] var packagePrivateVar: String = "6"
       |  var publicVar: Array[Int] = Array.empty
       |
       |  override def hashCode: Int = 1
       |
       |  def foo(): Unit = {
       |    // Need to use all private variables to avoid compiler optimizations
       |    val myThis = ScalaClass.this
       |    println(privateThisVal)$bp
       |    println(privateVal)
       |    println(privateThisVar)
       |    println(privateVar)
       |    println(usedConstructorParam)
       |  }
       |}
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    new ScalaClass(10, 20).foo()
       |  }
       |}""".stripMargin)
  def testScalaClass(): Unit = {
    testClassRenderer("test/Main")("myThis", "test.ScalaClass", "@1",
      Set(
        "privateThisVal = 1.0",
        "privateVal = 2",
        "packagePrivateVal = 3",
        "publicVal = {int[0]@uniqueID}[]",
        "lazyVal = lazy",
        "privateThisVar = 4.0",
        "privateVar = 5",
        "packagePrivateVar = 6",
        "publicVar = {int[0]@uniqueID}[]",
        "usedConstructorParam = 20"
      ))(10.seconds)
  }

  private val UNIQUE_ID = "uniqueID"

  private def testClassRenderer(mainClassName: String)(
      varName: String,
      className: String,
      afterTypeLabel: String,
      expectedChildrenLabels: Set[String])(implicit timeout: Duration): Unit = {
    import org.junit.Assert._
    runDebugger(mainClassName) {
      waitForBreakpoint()
      val (label, childrenLabels) =
        renderLabelAndChildren(varName, _.getLabel, renderChildren = expectedChildrenLabels.nonEmpty, expectedChildrenLabels.size)

      val classRenderer = NodeRendererSettings.getInstance().getClassRenderer
      val typeName = classRenderer.renderTypeName(className)
      val expectedLabel = s"$varName = {$typeName@$UNIQUE_ID}$className$afterTypeLabel"

      assertEquals(expectedLabel, label)
      assertEquals(expectedChildrenLabels, childrenLabels.toSet)
    }
  }
}
