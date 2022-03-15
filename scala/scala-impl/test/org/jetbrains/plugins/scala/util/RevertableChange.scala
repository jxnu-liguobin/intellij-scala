package org.jetbrains.plugins.scala.util

import com.intellij.openapi.util.registry.{Registry, RegistryValue}
import org.jetbrains.plugins.scala.util.RevertableChange.CompositeRevertableChange

trait RevertableChange {
  def applyChange(): Unit
  def revertChange(): Unit

  final def apply(body: => Any): Unit =
    run(body)

  final def run(body: => Any): Unit = {
    this.applyChange()
    try
      body
    finally
      this.revertChange()
  }

  final def |+| (change: RevertableChange): RevertableChange = {
    val changes = this match {
      case composite: CompositeRevertableChange => composite.changes :+ change
      case _                                    => Seq(this, change)
    }
    new CompositeRevertableChange(changes)
  }
}

object RevertableChange {

  object NoOpRevertableChange extends RevertableChange {
    override def applyChange(): Unit = ()
    override def revertChange(): Unit = ()
  }

  class CompositeRevertableChange(val changes: Seq[RevertableChange]) extends RevertableChange {
    override def applyChange(): Unit = changes.foreach(_.applyChange())
    override def revertChange(): Unit = changes.reverse.foreach(_.revertChange())
  }


  def withModifiedSetting2[Settings, T](instance: => Settings)
                                       (value: T)
                                       (get: Settings => T, set: (Settings, T) => Unit): RevertableChange = new RevertableChange {
    private var before: Option[T] = None

    override def applyChange(): Unit = {
      before = Some(get(instance))
      set(instance, value)
    }

    override def revertChange(): Unit =
      before.foreach(set(instance, _))
  }

  def withModifiedRegistryValue(key: String, newValue: Boolean): RevertableChange =
    withModifiedRegistryValueInternal[Boolean](key, newValue, _.asBoolean, _ setValue _)

  def withModifiedRegistryValue(key: String, newValue: Int): RevertableChange =
    withModifiedRegistryValueInternal[Int](key, newValue, _.asInteger(), _ setValue _)

  private def withModifiedRegistryValueInternal[A](key: String,
                                                   newValue: A,
                                                   getter: RegistryValue => A,
                                                   setter: (RegistryValue, A) => Unit): RevertableChange =
    new RevertableChange {
      private var before: Option[A] = None

      override def applyChange(): Unit = {
        val registryValue = Registry.get(key)
        before = Some(getter(registryValue))
        setter(registryValue, newValue)
      }

      override def revertChange(): Unit =
        before.foreach { oldValue =>
          setter(Registry.get(key), oldValue)
        }
    }

}