package org.jetbrains.jps.incremental.scala

import com.intellij.openapi.application.PathManager
import com.intellij.openapi.diagnostic.{Logger => JpsLogger}
import com.intellij.openapi.util.Key
import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.incremental.ModuleLevelBuilder.ExitCode
import org.jetbrains.jps.incremental.messages.ProgressMessage
import org.jetbrains.jps.incremental.scala.Server.ServerError
import org.jetbrains.jps.incremental.scala.local.LocalServer
import org.jetbrains.jps.incremental.{CompileContext, ModuleLevelBuilder}
import org.jetbrains.jps.model.module.JpsModule
import org.jetbrains.plugins.scala.compiler.data.{CompilationData, SbtData}
import org.jetbrains.plugins.scala.server.CompileServerProperties

import _root_.java.io._
import _root_.java.net.InetAddress
import _root_.java.util.ServiceLoader
import _root_.scala.jdk.CollectionConverters._
import scala.concurrent.duration.DurationInt

// TODO: use a proper naming. Scala builder of what? Strings? Code? Psi trees?
object ScalaBuilder {

  /**
   * If there is some error during connection to the Compile Server,
   * remember it and do not try using CS when building subsequent module chunks
   */
  private val DisableScalaCompileServerForContextKey = Key.create[Boolean]("DisableScalaCompileServerForContextKey")

  import data._

  def compile(
    context: CompileContext,
    chunk: ModuleChunk,
    sources: Seq[File],
    allSources: Seq[File],
    modules: Set[JpsModule],
    client: Client
  ): Either[String, ModuleLevelBuilder.ExitCode] = {
    context.processMessage(new ProgressMessage(JpsBundle.message("reading.compilation.settings.0", chunk.getPresentableShortName)))

    for {
      sbtData         <-  sbtData
      dataFactory     = dataFactoryOf(context)
      compilerData    <- dataFactory.getCompilerDataFactory.from(context, chunk)
      compilationData <- dataFactory.getCompilationDataFactory.from(sources, allSources, context,  chunk)
    } yield {
      Log.info(s"Compiling ${compilationData.sources.size} files; module: ${chunk.getPresentableShortName}; compiler: ${compilerData.compilerJars.map(_.compilerJar).orNull}")
      scalaLibraryWarning(modules, compilationData, client)

      // TODO: ensure Scala Compile server is stopped in order it doesn't eventually
      //  do any compilation in parallel with local compilation
      def fallbackToLocalServer(reasonMessage: String, e: Exception): ExitCode = {
        context.putUserData(DisableScalaCompileServerForContextKey, true)
        //noinspection ScalaExtractStringToBundle,ReferencePassedToNls
        val message = reasonMessage + s" [${chunk.getPresentableShortName}]\n" + JpsBundle.message("trying.to.compile.without.it")
        client.warning(message)
        Log.warn(message, e)

        localServer.doCompile(sbtData, compilerData, compilationData, client)
      }

      val server = getServer(context, compilerData.compilerJars.exists(_.hasScala3))
      server.compile(sbtData, compilerData, compilationData, client) match {
        case Left(error) =>
          import ScalaCompileServerJpsMessages._
          import ServerError._
          error match {
            case SocketConnectTimeout(address, port, timeout, cause) =>
              fallbackToLocalServer(cantConnectToCompileServerErrorMessage(address, port, reason = Some(socketConnectTimeout(timeout))), cause)
            case ConnectionError(address, port, cause)               =>
              fallbackToLocalServer(cantConnectToCompileServerErrorMessage(address, port, reason = Option(cause.getMessage).filter(_.trim.nonEmpty)), cause)
            case UnknownHost(address, cause)               =>
              val message = unknownHostErrorMessage(address)
              client.error(message)
              Log.error(message, cause)
              ExitCode.ABORT
          }
        case Right(exitCode) =>
          exitCode
      }
    }
  }

  private def dataFactoryOf(context: CompileContext): DataFactoryService = {
    val df = ServiceLoader.load(classOf[DataFactoryService])
    val registeredDataFactories = df.iterator().asScala.toList
    Log.info(s"Registered factories of ${classOf[DataFactoryService].getName}: $registeredDataFactories")
    val firstEnabledDataFactory = registeredDataFactories.find(_.isEnabled(context.getProjectDescriptor.getProject))
    Log.info(s"First enabled factory (if any): $firstEnabledDataFactory")
    firstEnabledDataFactory.getOrElse(DefaultDataFactoryService)
  }

  def hasBuildModules(chunk: ModuleChunk): Boolean = {
    chunk.getModules.asScala.exists(_.getName.endsWith("-build")) // gen-idea doesn't use the sbt module type
  }

  def projectSettings(context: CompileContext): model.ProjectSettings =
    SettingsManager.getProjectSettings(context.getProjectDescriptor.getProject)

  val Log: JpsLogger = JpsLogger.getInstance(ScalaBuilder.getClass.getName)

  // Cached local localServer
  private var cachedLocalServer: Option[LocalServer] = None

  private val lock = new Object()

  private def localServer: LocalServer = {
    lock.synchronized {
      val server = cachedLocalServer.getOrElse(new local.LocalServer())
      cachedLocalServer = Some(server)
      server
    }
  }

  private def cleanLocalServerCache(): Unit =
    lock.synchronized {
      cachedLocalServer = None
    }

  private lazy val sbtData = {
    val pluginJpsRoot = new File(PathManager.getJarPathForClass(getClass)).getParentFile
    val javaClassVersion = System.getProperty("java.class.version")
    SbtData.from(pluginJpsRoot, javaClassVersion)
  }

  private def scalaLibraryWarning(modules: Set[JpsModule], compilationData: CompilationData, client: Client): Unit = {
    val hasScalaFacet = modules.exists(SettingsManager.getScalaSdk(_).isDefined)
    val hasScalaLibrary = compilationData.classpath.exists(_.getName.startsWith("scala-library"))

    val hasScalaSources = compilationData.sources.exists(_.getName.endsWith(".scala"))

    if (hasScalaFacet && !hasScalaLibrary && hasScalaSources) {
      val names = modules.map(_.getName).mkString(", ")
      client.warning(JpsBundle.message("no.scala.library.jar.in.module.dependencies", names))
    }
  }

  private def getServer(implicit context: CompileContext, moduleHasScala3: Boolean): Server = {
    val useRemoteServer = isCompileServerEnabled &&
      !CompileServerProperties.isMyselfScalaCompileServer &&
      !context.getUserData(DisableScalaCompileServerForContextKey)

    if (useRemoteServer) {
      cleanLocalServerCache()
      val port = globalSettings.getCompileServerPort
      Log.info(s"Using remote server with port: $port")
      val socketConnectTimeout = Option(System.getProperty("scala.compile.server.socket.connect.timeout.milliseconds")).map(_.toInt.milliseconds).getOrElse(10.milliseconds)
      new remote.RemoteServer(InetAddress.getByName(null), port, socketConnectTimeout)
    } else {
      Log.info("Using local server")
      localServer
    }
  }

  def isCompileServerEnabled(implicit context: CompileContext): Boolean =
    globalSettings.isCompileServerEnabled

  private def globalSettings(implicit context: CompileContext) =
    SettingsManager.getGlobalSettings(context.getProjectDescriptor.getModel.getGlobal)
}
