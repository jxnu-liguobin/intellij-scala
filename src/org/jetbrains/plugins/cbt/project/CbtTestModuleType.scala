package org.jetbrains.plugins.cbt.project

import com.intellij.openapi.module.ModuleType
import org.jetbrains.plugins.cbt.project.template.CbtModuleBuilder
import org.jetbrains.plugins.scala.icons.Icons

class CbtTestModuleType extends ModuleType[CbtModuleBuilder](CbtExtraModuleType.ID) {
  override def createModuleBuilder = new CbtModuleBuilder

  override def getName = CbtTestModuleType.NAME

  override def getDescription = CbtTestModuleType.NAME

  override def getNodeIcon(isOpened: Boolean) = Icons.CBT_TEST_MODULE
}

object CbtTestModuleType {
  val ID: String = "CBT_TEST_MODULE"
  val NAME: String = "CBT Extra Module"
}

