package org.jetbrains.plugins.scala.debugger
package ui

import com.intellij.debugger.engine.evaluation.EvaluationContext
import com.intellij.debugger.impl.DebuggerUtilsAsync
import com.intellij.debugger.settings.NodeRendererSettings
import com.intellij.debugger.ui.impl.watch.FieldDescriptorImpl
import com.intellij.debugger.ui.tree.ValueDescriptor
import com.intellij.debugger.ui.tree.render.{ChildrenBuilder, ClassRenderer, DescriptorLabelListener}
import com.sun.jdi._
import org.jetbrains.plugins.scala.ScalaBundle

import scala.jdk.CollectionConverters._
import scala.util.Try

class ScalaClassRenderer extends ClassRenderer {

  import ScalaClassRenderer._

  override val getUniqueId: String = getClass.getSimpleName

  override def getName: String = ScalaBundle.message("scala.class.renderer")

  def isApplicableFor(tpe: Type): Boolean = tpe match {
    case ct: ClassType => isScalaSource(ct)
    case _ => false
  }

  override def isEnabled: Boolean = true

  override def calcLabel(descriptor: ValueDescriptor, context: EvaluationContext, labelListener: DescriptorLabelListener): String = {
    val renderer = NodeRendererSettings.getInstance().getToStringRenderer
    renderer.calcLabel(descriptor, context, labelListener)
  }

  override def buildChildren(value: Value, builder: ChildrenBuilder, context: EvaluationContext): Unit = {
    val ref = value.asInstanceOf[ObjectReference]
    val project = context.getProject
    DebuggerUtilsAsync.allFields(ref.referenceType())
      .thenAccept { fields =>
        val toShow = fields.asScala.filter(shouldDisplayField(ref, _))
        if (toShow.isEmpty) {
          setClassHasNoFieldsToDisplayMessage(builder, builder.getNodeManager)
        } else {
          val manager = builder.getNodeManager
          val nodes = toShow.map { field =>
            val desc =
              if (isLazyVal(ref, field)) LazyValDescriptor.create(project, ref, field)
              else new FieldDescriptorImpl(project, ref, field)
            manager.createNode(desc, context)
          }.asJava
          builder.setChildren(nodes)
        }
      }
  }
}

private object ScalaClassRenderer {
  private val Module: String = "MODULE$"

  private val Bitmap: String = "bitmap$"

  val Offset: String = "OFFSET$"

  def isModule(f: Field): Boolean = f.name() == Module

  def isBitmap(f: Field): Boolean = f.name().contains(Bitmap)

  def isOffset(f: Field): Boolean = f.name().startsWith(Offset)

  def isScalaSource(ct: ClassType): Boolean =
    Try(ct.sourceName().endsWith(".scala")).getOrElse(false)

  def isLazyVal(ref: ObjectReference, f: Field): Boolean = {
    val allFields = ref.referenceType().allFields().asScala
    val isScala3 = allFields.exists(_.name().startsWith(Offset))
    val fieldName = f.name()
    if (isScala3) fieldName.contains("$lzy")
    else {
      val allMethods = ref.referenceType().allMethods().asScala
      allMethods.exists { m =>
        val methodName = m.name()
        methodName.contains(fieldName) && methodName.contains("lzycompute")
      }
    }
  }

  private def shouldDisplayField(ref: ObjectReference, f: Field): Boolean =
    !ScalaSyntheticProvider.hasSpecialization(f, Some(ref.referenceType())) &&
      !isModule(f) && !isBitmap(f) && !isOffset(f)
}
