package org.jetbrains.plugins.scala
package refactoring.extractMethod

import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.util.TypeAnnotationSettings

/**
 * Nikolay.Tropin
 * 2014-05-20
 */
class ScalaExtractMethodCaseClass extends ScalaExtractMethodTestBase {
  override def folderPath: String = super.folderPath + "caseClass/"

  def testNoReturnSeveralOutput(): Unit = {
    val settings = TypeAnnotationSettings.alwaysAddType(ScalaCodeStyleSettings.getInstance(getProject))
    doTest(settings = TypeAnnotationSettings.noTypeAnnotationForPublic(settings))
  }

  def testReturnSeveralOutput1(): Unit = doTest()

  def testReturnSeveralOutput2(): Unit = doTest()

  def testUnitReturnSeveralOutput1(): Unit = doTest()

  def testUnitReturnSeveralOutput2(): Unit = doTest()
}
