package org.jetbrains.plugins.scala.lang.optimize
package generated

import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings

class OptimizeImportsImplicitsTest extends OptimizeImportsTestBase {
  //This class was generated by build script, please don't change this
  override def folderPath: String = super.folderPath + "implicits/"
  
  def testAssignment(): Unit = doTest()

  def testImplicitApply(): Unit = doTest()

  def testImplicitReference(): Unit = doTest()

  def testImplicitReference2(): Unit = doTest()

  def testImplicitReturnAndValVarAssignment(): Unit = doTest()

  def testImplicitlyConvertedAndProvidedArguments(): Unit = doTest()

  def testImplicitParamatersUsed(): Unit = doTest()

  def testImplicitParametersDeeperLevel(): Unit = doTest()

  def testImplicitsNewClass(): Unit = doTest()

  def testJConversions(): Unit = doTest()

  def testOverloadedImplicits(): Unit = doTest()

  def testSCL6003(): Unit = doTest()

  def testSCL6514(): Unit = doTest()

  def testSCL6650(): Unit = doTest()

  def testSCL6783(): Unit = doTest()

  def testSCL7158(): Unit = {
    val settings = ScalaCodeStyleSettings.getInstance(getProject)
    val old = settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR
    settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = true
    try {
      doTest()
    } finally {
      settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = old
    }
  }

  def testSCL7269(): Unit = doTest()

  def testSCL12332(): Unit = doTest()

  def testSCL12609(): Unit = doTest()

  //one of the examples from SCL-9326
  def testFromUnderscore(): Unit = doTest()
}