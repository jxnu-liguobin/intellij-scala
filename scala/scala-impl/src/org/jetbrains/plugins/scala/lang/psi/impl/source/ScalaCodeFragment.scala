package org.jetbrains.plugins.scala
package lang
package psi
package impl
package source

import com.intellij.openapi.command.undo.{BasicUndoableAction, UndoManager}
import com.intellij.openapi.project.Project
import com.intellij.psi.IntentionFilterOwner.IntentionActionsFilter
import com.intellij.psi.JavaCodeFragment.{ExceptionHandler, VisibilityChecker}
import com.intellij.psi._
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.impl.source.tree.FileElement
import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.testFramework.LightVirtualFile
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder.ImportPath
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScReference, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression

import scala.collection.mutable

/**
  * @author Alexander Podkhalyuzin
  */
final class ScalaCodeFragment private(private var viewProvider: SingleRootFileViewProvider)
  extends ScalaFileImpl(viewProvider)
    with JavaCodeFragment
    with IntentionFilterOwner
    with ScDeclarationSequenceHolder {

  import ScalaPsiElementFactory._

  getViewProvider.forceCachedPsi(this)

  private var thisType: PsiType = _
  private var superType: PsiType = _
  private var exceptionHandler: ExceptionHandler = _
  private var resolveScope: GlobalSearchScope = _
  private var filter: IntentionActionsFilter = _
  private var imports = mutable.HashSet.empty[String]

  override def getThisType: PsiType = thisType

  override def setThisType(thisType: PsiType): Unit = {
    this.thisType = thisType
  }

  override def getSuperType: PsiType = superType

  override def setSuperType(superType: PsiType): Unit = {
    this.superType = superType
  }

  override def importsToString(): String = {
    imports.mkString(",")
  }

  override def addImportsFromString(imports: String): Unit = {
    this.imports ++= imports.split(',').filter(_.nonEmpty)
  }

  override def setVisibilityChecker(checker: VisibilityChecker): Unit = {}

  override def getVisibilityChecker: VisibilityChecker = VisibilityChecker.EVERYTHING_VISIBLE

  override def setExceptionHandler(exceptionHandler: ExceptionHandler): Unit = {
    this.exceptionHandler = exceptionHandler
  }

  override def getExceptionHandler: ExceptionHandler = exceptionHandler

  override def forceResolveScope(resolveScope: GlobalSearchScope): Unit = {
    this.resolveScope = resolveScope
  }

  override def getForcedResolveScope: GlobalSearchScope = resolveScope

  override def setIntentionActionsFilter(filter: IntentionActionsFilter): Unit = {
    this.filter = filter
  }

  override def getIntentionActionsFilter: IntentionActionsFilter = filter

  override def getViewProvider: SingleRootFileViewProvider = viewProvider

  private def myAddImportForPath(path: ImportPath, ref: ScReference): Unit = {
    //NOTE path.aliasName is not handled, I don't know at what conditions it might be actual here
    val pathFqn = path.qualifiedName

    imports += pathFqn
    myManager.beforeChange(false)

    UndoManager.getInstance(myManager.getProject)
      .undoableActionPerformed(new ImportClassUndoableAction(pathFqn))

    val newRef = ref match {
      case st: ScStableCodeReference if st.resolve() == null =>
        Some(createReferenceFromText(st.getText, st.getParent, st))
      case expr: ScReferenceExpression if expr.resolve() == null =>
        Some(createExpressionFromText(expr.getText, expr).asInstanceOf[ScReferenceExpression])
      case _ => None
    }

    newRef match {
      case Some(r) if r.resolve() != null =>
        ref.replace(r)
      case _ =>
    }
  }

  override protected[psi] def addImportsForPathsImpl(paths: Seq[ImportPath], refsContainer: PsiElement): Unit = {
    refsContainer match {
      case ref: ScReference =>
        paths.foreach(myAddImportForPath(_, ref))
      case _ =>
    }
  }

  @Deprecated
  override def importClass(aClass: PsiClass): Boolean = {
    addImportForClass(aClass, ref = null)
    true
  }

  override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState,
                                   lastParent: PsiElement, place: PsiElement): Boolean = {
    for (qName <- imports) {
      val imp = ScalaPsiElementFactory.createImportFromText(s"import _root_.$qName", this, this)
      if (!imp.processDeclarations(processor, state, lastParent, place)) return false
    }

    super[ScDeclarationSequenceHolder].processDeclarations(processor, state, lastParent, place)
  }

  override def clone(): PsiFileImpl = {
    val fileElement = calcTreeElement.clone.asInstanceOf[FileElement]
    val clone = cloneImpl(fileElement).asInstanceOf[ScalaCodeFragment]
    clone.myOriginalFile = this
    clone.imports = imports

    val fileManager = getManager.asInstanceOf[PsiManagerEx].getFileManager
    val virtualFile = new LightVirtualFile(getName, getLanguage, getText)
    val cloneViewProvider = fileManager.createFileViewProvider(virtualFile, false).asInstanceOf[SingleRootFileViewProvider]

    cloneViewProvider.forceCachedPsi(clone)
    clone.viewProvider = cloneViewProvider

    clone
  }

  private class ImportClassUndoableAction(path: String) extends BasicUndoableAction {

    override def undo(): Unit = {
      imports -= path
    }

    override def redo(): Unit = {
      imports += path
    }
  }

}

object ScalaCodeFragment {

  def apply(text: String,
            context: PsiElement = null,
            child: PsiElement = null)
           (implicit project: Project): ScalaCodeFragment = {
    val language = Option(context)
      .map(_.getContainingFile.getLanguage)
      .filter(_.isKindOf(ScalaLanguage.INSTANCE))
      .getOrElse(ScalaLanguage.INSTANCE)

    val viewProvider = new SingleRootFileViewProvider(
      PsiManager.getInstance(project),
      new LightVirtualFile("Dummy.scala", language, text),
      true
    )
    val fragment = new ScalaCodeFragment(viewProvider)
    fragment.context = context
    fragment.child = child
    fragment
  }

}