package org.jetbrains.plugins.scala.codeInsight.hints.rangeHints

import com.intellij.codeInsight.hints.ImmediateConfigurable
import com.intellij.codeInsight.hints.settings.InlayProviderSettingsModel
import com.intellij.lang.Language
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.codeInsight.hints.ScalaHintsSettings
import org.jetbrains.plugins.scala.codeInsight.implicits.ImplicitHints
import org.jetbrains.plugins.scala.codeInsight.{ScalaCodeInsightBundle, ScalaCodeInsightSettings}
import org.jetbrains.plugins.scala.extensions.StringExt

import java.util
import javax.swing.JComponent

class RangeHintsForToAndUntilSettingsModel(project: Project) extends InlayProviderSettingsModel(
  true,
  "Scala.RangeHintsForToAndUntilSettingsModel",
  ScalaLanguage.INSTANCE
) {
  // have a temporary version of the settings, so apply/cancel mechanism works
  object settings {
    private val global = ScalaCodeInsightSettings.getInstance()

    reset()

    def reset(): Unit = {
      setEnabled(global.showRangeHintsForToAndUntil)
    }

    def apply(): Unit = {
      global.showRangeHintsForToAndUntil = isEnabled
    }

    def isModified: Boolean =
      global.showRangeHintsForToAndUntil != isEnabled
  }

  override def getCases: util.List[ImmediateConfigurable.Case] = util.Collections.emptyList()

  override def getMainCheckBoxLabel: String = ScalaCodeInsightBundle.message("show.range.hints.for.to.and.until")

  override def getName: String = ScalaCodeInsightBundle.message("range.hints.for.to.and.until")

  override def getComponent: JComponent = null

  override def getPreviewText: String = {
    if (project.isDefault)
      return null

    """
      |val r1 = 1 to 10
      |val r2 = 1.to(10)
      |val r3 = 1 until 10
      |val r4 = 1 until (10, -1)
      |""".stripMargin.withNormalizedSeparator
  }

  override def apply(): Unit = {
    settings.apply()
    ImplicitHints.updateInAllEditors()
  }

  // create a dedicated pass for the preview
  private lazy val previewPass: RangeInlayHintsPass = new RangeInlayHintsPass {
    override def isPreview: Boolean = true
    override val settings: ScalaHintsSettings = new ScalaHintsSettings.Defaults {
      override def showRangeHintsForToAndUntil: Boolean = true
    }
  }

  override def collectAndApply(editor: Editor, psiFile: PsiFile): Unit = {
    previewPass.collectRangeHints(editor, psiFile)
    previewPass.regenerateRangeInlayHints(editor, editor.getInlayModel, psiFile)
  }

  override def isModified: Boolean = settings.isModified

  override def reset(): Unit =
    settings.reset()

  override def getDescription: String = null

  override def getCaseDescription(aCase: ImmediateConfigurable.Case): String = null

  override def getCasePreview(aCase: ImmediateConfigurable.Case): String = null

  override def getCasePreviewLanguage(aCase: ImmediateConfigurable.Case): Language = ScalaLanguage.INSTANCE
}
