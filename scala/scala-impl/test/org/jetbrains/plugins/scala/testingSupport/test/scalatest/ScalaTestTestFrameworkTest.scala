package org.jetbrains.plugins.scala.testingSupport.test.scalatest

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.settings.ScalaProjectSettings
import org.junit.Assert

class ScalaTestTestFrameworkTest extends ScalaLightCodeInsightFixtureTestAdapter {

  private val scalaTestFramework = new ScalaTestTestFramework

  def testDefaultSuperClass(): Unit = {
    val scalaProjectSettings = ScalaProjectSettings.getInstance(getProject)

    scalaProjectSettings.setScalaTestDefaultSuperClass("org.scalatest.FlatSpec")
    Assert.assertEquals("org.scalatest.FlatSpec", scalaTestFramework.getDefaultSuperClass)

    scalaProjectSettings.setScalaTestDefaultSuperClass("org.scalatest.WordSPec")
    Assert.assertEquals("org.scalatest.WordSPec", scalaTestFramework.getDefaultSuperClass)
  }
}
