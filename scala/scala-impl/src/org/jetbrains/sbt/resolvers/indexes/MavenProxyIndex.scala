package org.jetbrains.sbt.resolvers.indexes

import com.intellij.openapi.progress.ProgressIndicator
import org.jetbrains.idea.maven.indices.{MavenIndex, MavenIndexUtils, MavenIndicesManager}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.sbt.resolvers.ArtifactInfo

import scala.jdk.CollectionConverters._

/**
  * @author Mikhail Mutcianko
  * @since 26.07.16
  */
class MavenProxyIndex(val root: String, val name: String, implicit val project: ProjectContext) extends ResolverIndex {

  override def doUpdate(progressIndicator: Option[ProgressIndicator] = None): Unit = {
    findPlatformMavenResolver
      .foreach(i =>
        MavenIndicesManager.getInstance(project).scheduleUpdateContent(List(i).asJava)
      )
  }

  override def getUpdateTimeStamp: Long = {
    findPlatformMavenResolver.map(_.getUpdateTimestamp).getOrElse(ResolverIndex.NO_TIMESTAMP)
  }

  override def close(): Unit = ()

  override def searchGroup(artifactId: String): Set[String] = {
    findPlatformMavenResolver.map { r =>
      if (artifactId != "")
        r.getGroupIds.asScala.filter(r.hasArtifactId(_, artifactId)).toSet
      else
        r.getGroupIds.asScala.toSet
    }.getOrElse(Set.empty)
  }

  override def searchArtifact(groupId: String): Set[String] = {
    findPlatformMavenResolver.map {
      _.getArtifactIds(groupId).asScala.toSet
    }.getOrElse(Set.empty)
  }

  override def searchVersion(groupId: String, artifactId: String): Set[String] = {
    findPlatformMavenResolver.map {
      _.getVersions(groupId, artifactId).asScala.toSet
    }.getOrElse(Set.empty)
  }

  private def findPlatformMavenResolver: Option[MavenIndex] = {
    val index = MavenIndicesManager.getInstance(project).getIndex
    val indices = index.getIndices.asScala
    indices.find(_.getRepositoryPathOrUrl == MavenIndexUtils.normalizePathOrUrl(root))
  }

  override def searchArtifactInfo(fqName: String)(implicit project: ProjectContext): Set[ArtifactInfo] = Set.empty
}

