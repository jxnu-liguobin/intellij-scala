package org.jetbrains.plugins.scala.lang.resolve2


/**
 * Pavel.Fatin, 02.02.2010
 */

class QualifierSourceImmediateTest extends ResolveTestBase {
  override def folderPath: String = {
    super.folderPath + "qualifier/source/immediate/"
  }

  def testCaseClass() = doTest()
  //TODO
//  def testCaseClassObject = doTest
  //TODO
//  def testCaseClassObjectSyntetic = doTest
  def testCaseObject() = doTest()
  //TODO
//  def testCaseObjectSyntetic = doTest
  def testClass() = doTest()
  def testObject() = doTest()
  def testTrait() = doTest()
}