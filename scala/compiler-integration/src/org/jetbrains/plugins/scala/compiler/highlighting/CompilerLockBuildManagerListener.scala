package org.jetbrains.plugins.scala.compiler.highlighting

import com.intellij.compiler.server.BuildManagerListener
import com.intellij.openapi.project.Project
import com.intellij.serviceContainer.AlreadyDisposedException
import org.jetbrains.plugins.scala.compiler.JpsSessionErrorTrackerService
import org.jetbrains.plugins.scala.compiler.highlighting.CompilerLockBuildManagerListener.suppressAlreadyDisposed
import org.jetbrains.plugins.scala.settings.ScalaHighlightingMode

import java.util.UUID

class CompilerLockBuildManagerListener
  extends BuildManagerListener {

  override def buildStarted(project: Project, sessionId: UUID, isAutomake: Boolean): Unit = suppressAlreadyDisposed {
    CompilerHighlightingService.get(project).cancel()
    CompilerLock.get(project).lock(sessionId.toString)
  }

  override def buildFinished(project: Project, sessionId: UUID, isAutomake: Boolean): Unit = suppressAlreadyDisposed {
    CompilerLock.get(project).unlock(sessionId.toString, exceptionIfNotLocked = false)
    if (ScalaHighlightingMode.showCompilerErrorsScala3(project) && !JpsSessionErrorTrackerService.instance(project).hasError(sessionId)) {
      CompilerHighlightingService.get(project).triggerDocumentCompilationInAllOpenEditors()
    }
  }
}

object CompilerLockBuildManagerListener {

  private def suppressAlreadyDisposed(body: => Unit): Unit =
    try {
      body
    } catch {
      case _: AlreadyDisposedException => // ignore
    }
}
