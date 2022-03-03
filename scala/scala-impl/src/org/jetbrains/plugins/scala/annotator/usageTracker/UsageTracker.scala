package org.jetbrains.plugins.scala.annotator.usageTracker

import com.intellij.psi.{PsiElement, PsiNamedElement}
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.editor.importOptimizer.ImportInfoProvider
import org.jetbrains.plugins.scala.extensions.{IteratorExt, PsiElementExt, PsiFileExt}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScAssignment, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScEnumCase
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportExpr
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.usages._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.lang.psi.{ScImportsHolder, ScalaPsiUtil}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object UsageTracker {

  def registerUsedElementsAndImports(element: PsiElement, results: Iterable[ScalaResolveResult], checkWrite: Boolean): Unit = {
    for (resolveResult <- results if resolveResult != null) {
      registerUsedImports(element, resolveResult)
      registerUsedElement(element, resolveResult, checkWrite)
    }
  }

  def registerUsedImports(elem: PsiElement, imports: Set[ImportUsed]): Unit = {
    if (!elem.isValid) return
    if (imports.isEmpty)
      return

    elem.getContainingFile match {
      case scalaFile: ScalaFile =>
        val refHolder = ScalaRefCountHolder.getInstance(scalaFile)
        imports.foreach(refHolder.registerImportUsed)
      case _ =>
    }
  }

  def registerUsedImports(element: PsiElement, resolveResult: ScalaResolveResult): Unit = {
    registerUsedImports(element, resolveResult.importsUsed)
  }

  def getUnusedImports(file: ScalaFile): Seq[ImportUsed] = {
    val redundantBuilder = ArraySeq.newBuilder[ImportUsed]

    val potentiallyRedundantImports = RedundantImportUtils.collectPotentiallyRedundantImports(file)

    val importExprToUsedImports: mutable.Buffer[(ScImportExpr, Iterable[ImportUsed])] =
      mutable.ArrayBuffer.empty

    val importHolders: Iterator[ScImportsHolder] =
      file.depthFirst().filterByType[ScImportsHolder]

    for {
      importHolder <- importHolders
      importStmt   <- importHolder.getImportStatements
      importExprs  = importStmt.importExprs
      importExpr   <- importExprs
    } {
      val importsUsed = ImportUsed.buildAllFor(importExpr)
      importExprToUsedImports += ((importExpr, importsUsed))
    }

    val refHolder = ScalaRefCountHolder.getInstance(file)

    refHolder.runIfUnusedReferencesInfoIsAlreadyRetrievedOrSkip { () =>
      def isRedundant(importUsed: ImportUsed): Boolean =
        potentiallyRedundantImports.contains(importUsed) &&
          RedundantImportUtils.isActuallyRedundant(importUsed, file.getProject, file.isScala3File)

      def treatAsUnused(importUsed: ImportUsed): Boolean =
        refHolder.usageFound(importUsed) && !isRedundant(importUsed) ||
          importUsed.isAlwaysUsed

      importExprToUsedImports.foreach { case (expr, importsUsed) =>
        val toHighlight: Iterable[ImportUsed] = importsUsed.filterNot(treatAsUnused)

        val wholeImportExprIsUnused = toHighlight.size == importsUsed.size
        if (wholeImportExprIsUnused)
          redundantBuilder += new ImportExprUsed(expr)
        else
          redundantBuilder ++= toHighlight
      }
    }

    val result0 = redundantBuilder.result()
    val result1 = ImportInfoProvider.filterOutUsedImports(file, result0)
    result1
  }

  private def collectAllNamedElementTargets(resolveResult: ScalaResolveResult): Seq[PsiNamedElement] = {

    @tailrec
    def getLeafSyntheticNavigationElement(e: PsiElement): Option[PsiElement] =
      e match {
        case m: ScMember =>
          if (m.syntheticNavigationElement == null) Some(e)
          else getLeafSyntheticNavigationElement(m.syntheticNavigationElement)
        case _ => None
      }

    val res0 = resolveResult.element match {
      case ScEnumCase.Original(c) => Seq(c)
      case m: ScMember =>
        (
          getLeafSyntheticNavigationElement(m)
//            ++ Option(m.syntheticContainingClass)
//            ++ Option(m.originalGivenElement)
          )
          .toSeq.collect { case n: ScNamedElement => n }
      case _ => Seq.empty
    }

    res0 ++ resolveResult.parentElement.toSeq :+ resolveResult.element
  }

  private def registerTargetElement(sourceElement: PsiElement, targetElement: PsiNamedElement, checkWrite: Boolean): Unit =
    if (targetElement.isValid && targetElement.getContainingFile == sourceElement.getContainingFile &&
      !PsiTreeUtil.isAncestor(targetElement, sourceElement, true)) { //to filter recursive usages

      val valueUseds = sourceElement match {
        case ref: ScReferenceExpression if checkWrite && ScalaPsiUtil.isPossiblyAssignment(ref) =>

          val additionalWrite = ref.getContext match {
            case ScAssignment.resolvesTo(assignmentTarget) if assignmentTarget != targetElement =>
              Seq(WriteValueUsed(assignmentTarget))
            case _ => Seq.empty
          }

          WriteValueUsed(targetElement) +: additionalWrite
        case _ => Seq(ReadValueUsed(targetElement))
      }

      val holder = ScalaRefCountHolder.getInstance(sourceElement.getContainingFile)
      valueUseds.foreach(holder.registerValueUsed)
    }


  private def registerUsedElement(sourceElement: PsiElement,
                                  resolveResult: ScalaResolveResult,
                                  checkWrite: Boolean): Unit =
    collectAllNamedElementTargets(resolveResult)
      .foreach(registerTargetElement(sourceElement, _, checkWrite))
}
