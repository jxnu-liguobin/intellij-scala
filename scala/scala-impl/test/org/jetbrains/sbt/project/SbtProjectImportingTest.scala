package org.jetbrains.sbt.project

import com.intellij.compiler.CompilerConfiguration
import com.intellij.compiler.impl.javaCompiler.javac.JavacConfiguration
import com.intellij.openapi.externalSystem.util.{DisposeAwareProjectChange, ExternalSystemApiUtil}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl
import com.intellij.openapi.roots.{LanguageLevelModuleExtension, LanguageLevelProjectExtension, ModuleRootModificationUtil}
import com.intellij.pom.java.LanguageLevel
import com.intellij.testFramework.IdeaTestUtil
import org.jetbrains.annotations.Nullable
import org.jetbrains.jps.model.java.compiler.JpsJavaCompilerOptions
import org.jetbrains.plugins.scala.extensions.inWriteAction
import org.jetbrains.plugins.scala.project.ProjectExt
import org.jetbrains.plugins.scala.project.external.JdkByName
import org.jetbrains.plugins.scala.{ScalaVersion, SlowTests}
import org.jetbrains.sbt.settings.SbtSettings
import org.jetbrains.sbt.RichFile
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.experimental.categories.Category

import java.io.File
import java.net.URI
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.SeqHasAsJava

@Category(Array(classOf[SlowTests]))
class SbtProjectImportingTest extends ImportingTestCase
  with ProjectStructureExpectedLibrariesOps
  with ExactMatch {

  import ProjectStructureDsl._
  import ProjectStructureMatcher.ProjectComparisonOptions.Implicit.default

  def testSimple(): Unit = {
    val scalaLibrary = expectedScalaLibrary("2.13.5")
    runSimpleTest("simple", scalaLibrary :: Nil)

    // Adding the assertion here not to create a separate heavy test for such a tiny check
    // org.jetbrains.plugins.scala.project.ProjectExt#modulesWithScala
    Assert.assertEquals(
      "modulesWithScala should return list of non *-build modules",
      Seq("simple"),
      myProject.modulesWithScala.map(_.getName),
    )
  }

  //noinspection RedundantDefaultArgument
  def testSimple_Scala3(): Unit = {
    val scalaLibraries = Seq(
      expectedScalaLibrary("3.0.2", isSdk = true),
      expectedScalaLibrary("2.13.6", isSdk = false),
    )
    runSimpleTest("simple-scala3", scalaLibraries)
  }

  def testSimpleDoNotUseCoursier(): Unit = {
    val scalaLibrary = expectedScalaLibraryFromIvy(ScalaVersion.fromString("2.12.10").get)
    runSimpleTest("simpleDoNotUseCoursier", scalaLibrary :: Nil)
  }

  private def runSimpleTest(projectName: String, expectedScalaLibraries: Seq[library]): Unit =
    runTest(
      new project(projectName) {
        libraries := expectedScalaLibraries

        modules := Seq(
          new module(projectName) {
            contentRoots += getProjectPath
            ProjectStructureDsl.sources := Seq("src/main/scala", "src/main/java")
            testSources := Seq("src/test/scala", "src/test/java")
            resources := Seq("src/main/resources")
            testResources := Seq("src/test/resources")
            excluded := Seq("target")
            libraryDependencies := expectedScalaLibraries
          },
          new module(s"$projectName-build") {
            ProjectStructureDsl.sources := Seq("")
            excluded := Seq("project/target", "target")
          }
        )
      }
    )

  def testProjectWithUppercaseName(): Unit = runTest {
    new project("MyProjectWithUppercaseName") {
      lazy val scalaLibrary: library = expectedScalaLibrary("2.13.6")
      libraries += scalaLibrary

      modules := Seq(
        new module("MyProjectWithUppercaseName") {
          libraryDependencies += scalaLibrary
        },
        new module("MyProjectWithUppercaseName-build") {
        }
      )
    }
  }

  def testMultiModule(): Unit = runTest(
    new project("multiModule") {
      lazy val foo: module = new module("foo") {
        moduleDependencies += new dependency(bar) {
          isExported := true
        }
      }

      lazy val bar  = new module("bar")
      lazy val root = new module("multiModule")

      modules := Seq(root, foo, bar)
    })

  def testUnmanagedDependency(): Unit = runTest(
    new project("unmanagedDependency") {
      val scalaLibrary: library = expectedScalaLibrary("2.13.6")
      libraries := Seq(scalaLibrary)

      modules += new module("unmanagedDependency") {
        lazy val unmanagedLibrary: library = new library("sbt: unmanaged-jars") {
          libClasses += (testProjectDir / "lib" / "unmanaged.jar").getAbsolutePath
        }

        libraries := Seq(unmanagedLibrary)
        libraryDependencies := Seq(unmanagedLibrary, scalaLibrary)
      }
    }
  )

  def testSharedSources(): Unit = runTest(
    new project("sharedSourcesProject") {
      lazy val root: module = new module("sharedSourcesProject") {
        contentRoots := Seq(getProjectPath)
        sources := Seq("src/main/scala")
        moduleDependencies := Nil
      }

      lazy val sharedSourcesModule: module = new module("sharedSources-sources") {
        contentRoots := Seq(getProjectPath + "/shared")
        sources := Seq("src/main/scala")
      }

      lazy val foo: module = new module("foo") {
        moduleDependencies := Seq(sharedSourcesModule)
      }

      lazy val bar: module = new module("bar") {
        moduleDependencies := Seq(sharedSourcesModule)
      }

      modules := Seq(root, foo, bar, sharedSourcesModule)
    }
  )

  def testExcludedDirectories(): Unit = runTest(
    new project("root") {
      modules += new module("root") {
        excluded := Seq(
          "directory-to-exclude-1",
          "directory/to/exclude/2"
        )
      }
    }
  )

  /**
   * SCL-12520: Generate shared sources module when it is only used form a single other module
   */
  def testSCL12520(): Unit = runTest(
    new project("scl12520") {
      val sharedModule: module = new module("p1-sources") {
        contentRoots += getProjectPath + "/p1/shared"
      }

      val jvmModule: module = new module("p1") {
        moduleDependencies += sharedModule
        contentRoots += getProjectPath + "/p1/jvm"
      }

      val rootModule: module = new module("scl12520") {}
      val rootBuildModule: module = new module("scl12520-build") {}

      modules := Seq(sharedModule, rootModule, rootBuildModule, jvmModule)
    }
  )

  /**
   * SCL-13600: generate all modules when there is a duplicate project id in the sbt build
   * due to references to different builds, or multiple sbt projects being imported independently from IDEA
   */
  def testSCL13600(): Unit = runTest(
    new project("scl13600") {
      val buildURI: URI = testProjectDir.getCanonicalFile.toURI
      lazy val root: module = new module("root") {
        sbtBuildURI := buildURI
        sbtProjectId := "root"

        moduleDependencies := Seq(
          new dependency(c1) { isExported := true },
          new dependency(c2) { isExported := true },
        )
      }

      lazy val c1: module = new module("c1") {
        sbtBuildURI := buildURI.resolve("c1/")
        sbtProjectId := "c1"
        moduleDependencies := Seq()
      }
      lazy val c1Root: module = new module("c1.root", Array("root")) {
        sbtBuildURI := buildURI.resolve("c1/")
        sbtProjectId := "root"
        moduleDependencies := Seq()
      }

      lazy val c2: module = new module("c2") {
        sbtBuildURI := buildURI.resolve("c2/")
        sbtProjectId := "c2"
        moduleDependencies := Seq()
      }
      lazy val c2Root: module = new module("c2.root", Array("root")) {
        sbtBuildURI := buildURI.resolve("c2/")
        sbtProjectId := "root"
        moduleDependencies := Seq()
      }

      modules := Seq(root, c1, c1Root, c2, c2Root)
    }
  )

  def testSCL14635(): Unit = runTest(
    new project("SCL-14635") {
      private val buildURI: URI = testProjectDir.getCanonicalFile.toURI
      private val buildModulesGroup = Array("sbt-build-modules")

      lazy val base: module = new module("SCL-14635") {
        sbtBuildURI := buildURI
        sbtProjectId := "root"
      }

      // NOTE: sbtIdeaPlugin also has inner module named `sbt-idea-plugin` (with dashes), but it's separate, non-root module
      lazy val ideaPluginRoot: module = new module("sbtIdeaPlugin") {
        sbtBuildURI := new URI("git://github.com/JetBrains/sbt-idea-plugin")
        sbtProjectId := "sbtIdeaPlugin"
      }

      lazy val ideaPluginInnerModule = new module("sbt-idea-plugin")

      lazy val ideaShell: module = new module("sbt-idea-shell") {
        sbtBuildURI := new URI("git://github.com/JetBrains/sbt-idea-shell#master")
        sbtProjectId := "sbt-idea-shell"
      }

      lazy val ideSettings: module = new module("sbt-ide-settings") {
        sbtBuildURI := new URI("https://github.com/JetBrains/sbt-ide-settings.git")
        sbtProjectId := "sbt-ide-settings"
      }

      modules := Seq(
        base,
        ideaPluginRoot,
        ideaPluginInnerModule,
        ideaShell,
        ideSettings,

        new module("sbt-declarative-core"),
        new module("sbt-declarative-packaging"),
        new module("sbt-declarative-visualizer"),

        //build-modules
        new module("SCL-14635-build", buildModulesGroup),
        new module("sbtIdeaPlugin-build", buildModulesGroup),
        new module("sbt-idea-shell-build", buildModulesGroup),
        new module("sbt-ide-settings-build", buildModulesGroup)
      )
    }
  )

  def testCrossplatform(): Unit = runTest(
    new project("crossplatform") {
      lazy val root = new module("crossplatform")
      lazy val crossJS = new module("crossJS", Array("cross"))
      lazy val crossJVM = new module("crossJVM", Array("cross"))
      lazy val crossNative = new module("crossNative", Array("cross"))
      lazy val crossSources = new module("cross-sources", Array("cross"))

      modules := Seq(root, crossJS, crossJVM, crossNative, crossSources)
    }
  )

  //noinspection TypeAnnotation
  // SCL-16204, SCL-17597
  def testJavaLanguageLevelAndTargetByteCodeLevel(): Unit = {
    val projectSdk = IdeaTestUtil.getMockJdk9
    inWriteAction {
      ProjectJdkTable.getInstance.addJdk(projectSdk)
      // sbt can't be run with mock project JDK, so use internal JDK as in other tests
      val internalJdk = JavaAwareProjectJdkTableImpl.getInstanceEx.getInternalJdk: @nowarn
      val settings = SbtSettings.getInstance(myProject)
      settings.setCustomVMPath(internalJdk.getHomePath)
      settings.setCustomVMEnabled(true)
    }
    try runTestWithSdk(projectSdk,
      new project("java-language-level-and-target-byte-code-level") {
        // we expect no other options except -source -target --release or --enable-preview in this test
        // these options are specially handled and saved in the dedicated settings, so we don't expect any extra javacOptions
        javacOptions := Nil
        sdk := JdkByName(projectSdk.getName)

        def moduleX(name: String, source: LanguageLevel, @Nullable target: String): module = new module(name) {
          javaLanguageLevel := source
          javaTargetBytecodeLevel := target
          javacOptions := Nil
          sdk := JdkByName(projectSdk.getName)
        }

        val sdkLanguageLevel: LanguageLevel = LanguageLevel.JDK_1_9

        val root = moduleX("java-language-level-and-target-byte-code-level", sdkLanguageLevel, null)

        // Module naming: `source_target_release`
        // `x` means option is missing
        val module_x_x_x = moduleX("module_x_x_x", sdkLanguageLevel, null)

        val module_8_8_x   = moduleX("module_8_8_x", LanguageLevel.JDK_1_8, "8")
        val module_8_11_x  = moduleX("module_8_11_x", LanguageLevel.JDK_1_8, "11")
        val module_11_8_x  = moduleX("module_11_8_x", LanguageLevel.JDK_11, "8")
        val module_11_11_x = moduleX("module_11_11_x", LanguageLevel.JDK_11, "11")

        // no explicit target: javac will use source level by default
        val module_8_x_x  = moduleX("module_8_x_x", LanguageLevel.JDK_1_8, null)
        val module_11_x_x = moduleX("module_11_x_x", LanguageLevel.JDK_11, null)
        val module_14_x_x = moduleX("module_14_x_x", LanguageLevel.JDK_14, null)
        val module_15_x_x = moduleX("module_15_x_x", LanguageLevel.JDK_15, null)

        val module_x_8_x  = moduleX("module_x_8_x", sdkLanguageLevel, "8")
        val module_x_11_x = moduleX("module_x_11_x", sdkLanguageLevel, "11")

        val module_x_x_8  = moduleX("module_x_x_8", LanguageLevel.JDK_1_8, "8")
        val module_x_x_11 = moduleX("module_x_x_11", LanguageLevel.JDK_11, "11")

        // Java preview features
        // NOTE: IntelliJ API supports only 2 last preview versions of java language level (in com.intellij.pom.java.LanguageLevel)
        // When a new version of Java releases and IDEA supports it, we should update this test
        //
        // no explicit target: javac will use source level by default
        val module_8_x_x_preview  = moduleX("module_8_x_x_preview", LanguageLevel.JDK_1_8, null) // no preview for Java 8
        val module_11_x_x_preview = moduleX("module_11_x_x_preview", LanguageLevel.JDK_11, null) // no preview for Java 11
        val module_14_x_x_preview = moduleX("module_14_x_x_preview", LanguageLevel.JDK_14, null) // no preview for Java 11
        val module_16_x_x_preview = moduleX("module_16_x_x_preview", LanguageLevel.JDK_16_PREVIEW, null)

        val module_x_x_8_preview  = moduleX("module_x_x_8_preview", LanguageLevel.JDK_1_8, "8")
        val module_x_x_11_preview = moduleX("module_x_x_11_preview", LanguageLevel.JDK_11, "11")
        val module_x_x_14_preview = moduleX("module_x_x_14_preview", LanguageLevel.JDK_14, "14")
        val module_x_x_16_preview = moduleX("module_x_x_16_preview", LanguageLevel.JDK_16_PREVIEW, "16")

        modules := Seq(
          root,
          module_x_x_x,
          module_8_8_x, module_8_11_x, module_11_8_x, module_11_11_x,
          module_8_x_x, module_11_x_x, module_14_x_x, module_15_x_x,
          module_x_8_x, module_x_11_x,
          module_x_x_8, module_x_x_11,
          module_8_x_x_preview, module_11_x_x_preview, module_14_x_x_preview, module_16_x_x_preview,
          module_x_x_8_preview, module_x_x_11_preview, module_x_x_14_preview, module_x_x_16_preview,
        )
      }
    ) finally {
      inWriteAction {
        ProjectJdkTable.getInstance.removeJdk(projectSdk)
      }
    }
  }

  //noinspection TypeAnnotation
  // SCL-16204, SCL-17597
  def testJavaLanguageLevelAndTargetByteCodeLevel_NoOptions(): Unit = {
    def doRunTest(): Unit = runTest(
      new project("java-language-level-and-target-byte-code-level-no-options") {
        javacOptions := Nil
        javaLanguageLevel := LanguageLevel.JDK_11
        javaTargetBytecodeLevel := null

        def moduleX(name: String, source: LanguageLevel, @Nullable target: String): module = new module(name) {
          javaLanguageLevel := source
          javaTargetBytecodeLevel := target
          javacOptions := Nil
        }

        val root = moduleX("java-language-level-and-target-byte-code-level-no-options", LanguageLevel.JDK_11, null)
        val module1 = moduleX("module1", LanguageLevel.JDK_11, null)

        modules := Seq(root, module1)
      }
    )

    doRunTest()

    // Emulate User changing the settings manually
    ExternalSystemApiUtil.executeProjectChangeAction(true, new DisposeAwareProjectChange(myProject) {
      override def execute(): Unit = {
        val ManuallySetTarget = "9"
        val ManuallySetSource = LanguageLevel.JDK_1_9

        setOptions(myProject, ManuallySetSource, ManuallySetTarget, Seq("-some-root-option"))

        val projectModules = myProject.modules
        projectModules.foreach(setOptions(_, ManuallySetSource, ManuallySetTarget, Seq("-some-module-option")))
      }
    })

    // Manually set settings should be rewritten if no explicit javac options provided
    doRunTest()
  }

  private def setOptions(project: Project, source: LanguageLevel, target: String, other: Seq[String]): Unit = {
    val compilerSettings = CompilerConfiguration.getInstance(project)
    compilerSettings.setProjectBytecodeTarget(target)

    val options = JavacConfiguration.getOptions(project, classOf[JavacConfiguration])
    options.ADDITIONAL_OPTIONS_STRING = other.mkString(" ")

    val ext = LanguageLevelProjectExtension.getInstance(project)
    ext.setLanguageLevel(source)
  }

  private def setOptions(module: Module, source: LanguageLevel, target: String, other: Seq[String]): Unit = {
    val compilerSettings = CompilerConfiguration.getInstance(module.getProject)
    compilerSettings.setBytecodeTargetLevel(module, target)
    compilerSettings.setAdditionalOptions(module, other.asJava)

    ModuleRootModificationUtil.updateModel(module,
      _.getModuleExtension(classOf[LanguageLevelModuleExtension]).setLanguageLevel(source)
    )
  }

  // TODO: also create test for scalacOptions

  //noinspection TypeAnnotation
  def testJavacOptionsPerModule(): Unit = runTest(
    new project("javac-options-per-module") {
      javacOptions := Nil // no storing project level options

      def moduleX(name: String, expectedJavacOptions: Seq[String]): module = new module(name) {
        javacOptions := expectedJavacOptions
      }

      // TODO: currently IDEA doesn't support more finely-grained scopes,like `in (Compile, compile)
      //  so option root_option_in_compile_compile is not included
      //  IDEA-232043, SCL-11883, SCL-17020
      val root = moduleX("javac-options-per-module", Seq("root_option", "root_option_in_compile"))

      val module1 = moduleX("module1", Seq("module_1_option"))
      val module2 = moduleX("module2", Seq("module_2_option_in_compile"))
      val module3 = moduleX("module3", Seq())

      modules := Seq(
        root, module1, module2, module3
      )
    }
  )

  def testJavacSpecialOptionsForRootProject(): Unit = {
    runTest(
      new project("javac-special-options-for-root-project") {
        // no storing project level options
        javacOptions := Nil
        javaTargetBytecodeLevel := null
        javaLanguageLevel := LanguageLevel.JDK_11 // from internal sdk

        val root: module = new module("javac-special-options-for-root-project") {
          javaLanguageLevel := LanguageLevel.JDK_1_9
          javaTargetBytecodeLevel := "1.7"
          javacOptions := Seq(
            "-g:none",
            "-nowarn",
            "-deprecation",
            "-Werror"
          )
        }
        modules:= Seq(root)
      }
    )

    val compilerOptions = JavacConfiguration.getOptions(myProject, classOf[JavacConfiguration])
    val defaultCompilerOptions = new JpsJavaCompilerOptions

    assertEquals(defaultCompilerOptions.DEBUGGING_INFO, compilerOptions.DEBUGGING_INFO)
    assertEquals(defaultCompilerOptions.GENERATE_NO_WARNINGS, compilerOptions.GENERATE_NO_WARNINGS)
    assertEquals(defaultCompilerOptions.DEPRECATION, compilerOptions.DEPRECATION)
    assertEquals(defaultCompilerOptions.ADDITIONAL_OPTIONS_STRING, compilerOptions.ADDITIONAL_OPTIONS_STRING)
    assertEquals(defaultCompilerOptions.MAXIMUM_HEAP_SIZE, compilerOptions.MAXIMUM_HEAP_SIZE)
    assertEquals(defaultCompilerOptions.PREFER_TARGET_JDK_COMPILER, compilerOptions.PREFER_TARGET_JDK_COMPILER)
  }
}

object SbtProjectImportingTest {
  implicit class StringOps(str: String) {
    def toURI: URI = new File(str).getCanonicalFile.toURI
  }
}
