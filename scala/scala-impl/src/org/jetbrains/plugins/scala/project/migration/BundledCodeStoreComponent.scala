package org.jetbrains.plugins.scala.project.migration

import com.intellij.openapi.externalSystem.model.project.LibraryData
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.libraries.Library
import com.intellij.openapi.startup.StartupActivity
import org.jetbrains.plugins.scala.codeInspection.bundled.BundledInspectionBase
import org.jetbrains.plugins.scala.components.libextensions.{LibraryExtensionsListener, LibraryExtensionsManager}
import org.jetbrains.plugins.scala.project.ProjectExt
import org.jetbrains.plugins.scala.project.migration.BundledCodeStoreComponent._
import org.jetbrains.plugins.scala.project.migration.api.{MigrationApiService, MigrationReport, SettingsDescriptor}
import org.jetbrains.plugins.scala.project.migration.apiimpl.MigrationApiImpl
import org.jetbrains.plugins.scala.project.migration.handlers.{ArtifactHandlerService, ScalaLibraryMigrationHandler}
import org.jetbrains.plugins.scala.settings.ScalaProjectSettings

import scala.collection.mutable

/**
  * User: Dmitry.Naydanov
  * Date: 14.11.16.
  */
final class BundledCodeStoreComponent(project: Project) {

  private var oldLibrariesCopy: Option[Seq[Library]] = None
  private var newLibrariesCopy: Option[Seq[LibraryData]] = None
  private var isImportFinished = false
  private var areInjectorsLoaded = false

  private val extensionManager = LibraryExtensionsManager.getInstance(project)
  private def getMigrationApi: MigrationApiService = MigrationApiImpl.getApiInstance(project)
  
  private def copyOldLibraries(): Unit = {
    oldLibrariesCopy match {
      case None =>
        oldLibrariesCopy = Option(project.libraries
          .map(_.getModifiableModel)
          .flatMap {
            case l: Library => Seq(l)
            case _ => Seq.empty
          })
      case _ =>
    }
  }
  
  private def disposeOldLibraries(): Unit = {
    oldLibrariesCopy match {
      case Some(copies) =>
        copies.foreach(_.dispose())
        oldLibrariesCopy = None
      case _ => 
    }
  }
  
  private def processFoundToHandlers(toHandlers: Iterable[ScalaLibraryMigrationHandler], 
                                     toLib: LibraryData): Seq[(ScalaLibraryMigrator, Library, LibraryData)] = {
    if (toHandlers.isEmpty) Seq.empty else oldLibrariesCopy match {
      case Some(libs) => libs.flatMap {
        case fromLib if fromLib.getName != toLib.getInternalName =>
          val foundHandlers = toHandlers.filter(_.acceptsFrom(fromLib))

          val filteredHandlers = foundHandlers.foldLeft(mutable.HashSet[ScalaLibraryMigrationHandler]()) {
            case (set, handler) => if (set.exists(_.precede(handler))) set else {set.add(handler); set}
          }

          filteredHandlers.flatMap(_.getMigrators(fromLib, toLib)).map(h => (h, fromLib, toLib))
        case _ => Seq.empty
      }
      case _ => Seq.empty
    }
  }
  
  
  def getLoadedInspections: Iterable[BundledInspectionBase] = extensionManager.getExtensions(classOf[BundledInspectionBase])

  def getFilteredInspections: Iterable[BundledInspectionBase] = {
    if (!isInspectionsEnabled(project)) return Seq.empty

    val disabled = ScalaProjectSettings.getInstance(project).getBundledInspectionIdsDisabled
    getLoadedInspections.filter(b => !disabled.contains(b.getId))
  }
  
  def setLibraryData(data: Iterable[LibraryData]) {
    synchronized {
      newLibrariesCopy = Option(data.toSeq)
      if (isImportFinished && areInjectorsLoaded) onImportFinished()
    }
  }
  
  def notifyImportFinished() {
    synchronized {
      isImportFinished = true
      if (newLibrariesCopy.isDefined && areInjectorsLoaded) onImportFinished()
    }
  }

  def onExtensionsAdded(): Unit = {
    synchronized {
      areInjectorsLoaded = true
      if (isImportFinished && newLibrariesCopy.isDefined) onImportFinished()
    }
  }

  def onImportAboutToStart() {
    if (!isEnabled(project)) return 
    copyOldLibraries()
    newLibrariesCopy = None
    isImportFinished = false
    areInjectorsLoaded = false
  }
  
  def onImportFinished() {
    if (!isEnabled(project)) return 
    
    val bundledHandlers = extensionManager.getExtensions(classOf[ScalaLibraryMigrationHandler])
    val handlerComponent = ArtifactHandlerService.getInstance(project)
    
    
    newLibrariesCopy.map { c => c flatMap {
        target => 
          val predefinedHandlersForTarget = handlerComponent.getAllForTo(target)
          
          if (predefinedHandlersForTarget.nonEmpty) 
            processFoundToHandlers(predefinedHandlersForTarget, target) 
          else 
            processFoundToHandlers(bundledHandlers.filter(h => h.acceptsTo(target)), target)
          
    }}.foreach {
      case foundMigrators if foundMigrators.nonEmpty =>
        val service = getMigrationApi

        service.showPopup(MIGRATORS_FOUND_MESSAGE,
          "Dependency migrators found", {
            case "show" =>
              service.showDialog("Scala plugin", "Following migrators has been found",
                SettingsDescriptor(foundMigrators.map(m => (s"${m._1.getName} from ${m._2.getName} to ${m._3.getInternalName}", true)), Iterable.empty, Iterable.empty),
                (descriptor: SettingsDescriptor) => {
                  val chosen = descriptor.checkBoxes.zip(foundMigrators).collect {
                    case ((_, isSelected), migrator) if isSelected => migrator
                  }

                  new ScalaMigrationRunner()(project).runMigrators(chosen.map(_._1).toSeq, service)
                }, ())
            case "close" =>
            case _ =>
          })
      case _ => 
    }
    
    disposeOldLibraries()
    newLibrariesCopy = None
    isImportFinished = false
    areInjectorsLoaded = false
  }
  

  project.getMessageBus.connect(project).subscribe(LibraryExtensionsManager.EXTENSIONS_TOPIC, new LibraryExtensionsListener {
    override def newExtensionsAdded(): Unit = onExtensionsAdded()
  })

}

object BundledCodeStoreComponent {
  def getInstance(project: Project): BundledCodeStoreComponent = project.getService(classOf[BundledCodeStoreComponent])
  
  def isEnabled(project: Project): Boolean = isMigratorsEnabled(project) || isInspectionsEnabled(project)
  
  def isMigratorsEnabled(project: Project): Boolean = ScalaProjectSettings.getInstance(project).isBundledMigratorsSearchEnabled
  
  def isInspectionsEnabled(project: Project): Boolean = ScalaProjectSettings.getInstance(project).isBundledInspectionsSearchEnabled 

  val STACKTRACE_FROM_REPORT_CUT_SIZE = 8

  val MIGRATORS_FOUND_MESSAGE: String =
    """
      |<html>
      |<body>
      |<p>Some migrators has been found.</p>
      |<br>
      |<a href="ftp://show">Show</a>  <a href="ftp://close">Close</a>
      |</body>
      |</html>
    """.stripMargin

  def showWarning(service: MigrationApiService, txt: String): Unit =
    service.showReport(MigrationReport.createSingleMessageReport(MigrationReport.Warning, txt))

  def showExceptionWarning(service: MigrationApiService, ex: Throwable): Unit = showWarning(service, ex.getMessage +
    "\n" + ex.getStackTrace.take(STACKTRACE_FROM_REPORT_CUT_SIZE).mkString("\n"))

  private final class Startup extends StartupActivity {
    override def runActivity(project: Project): Unit =
      getInstance(project)
  }
}
