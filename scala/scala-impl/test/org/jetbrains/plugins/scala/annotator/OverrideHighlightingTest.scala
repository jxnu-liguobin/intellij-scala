package org.jetbrains.plugins.scala.annotator

class OverrideHighlightingTest extends ScalaHighlightingTestBase {
  def testScl13051(): Unit = {
    val code =
      s"""
         |trait Base {
         |  def foo: Int = 42
         |}
         |
         |class AClass extends Base {
         |  override def foo: String = "42"
         |}
       """.stripMargin
    assertMatches(errorsFromScalaCode(code)) {
      case Error(_, "Overriding type String does not conform to base type Int") :: Nil =>
    }
  }

  def testScl13051_1(): Unit = {
    val code =
      s"""
         |trait T1 {
         |  val foo: T1
         |}
         |trait T2 extends T1 {
         |  override val foo: T2
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testScl13051_2(): Unit = {
    val code =
      s"""
         |trait Base {
         |  def foo(x: Int): Int = 42
         |  def foo(x: String): String = "42"
         |}
         |class AClass extends Base {
         |  override def foo(x: Int): Int = 42
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051_3(): Unit = {
    val code =
      s"""
         |trait Base[c] {
         |  def foo: c
         |}
         |
         |class AClass[a] extends Base[a]{
         |  override val foo: a = ???
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051Setter(): Unit = {
    val code =
      s"""
         |abstract class A() {
         |  var x: Int
         |}
         |
         |abstract class B() extends A() {
         |  var xx: Int = 0;
         |  def x: Int = xx
         |  def x_=(y: Int) = xx = y;
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051replaceDesignators(): Unit = {
    val code =
      """
        |trait Eater {
        |  type Food[T]
        |}
        |
        |trait Fruit {
        |  type Seed
        |}
        |
        |trait PipExtractor {
        |  def extract(a: Fruit): a.Seed
        |}
        |
        |trait LaserGuidedPipExtractor extends PipExtractor {
        |  def extract(f: Fruit): f.Seed
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  private def addIntType(over: String): String = {
    addType(over, "Int")
  }

  //noinspection SameParameterValue
  private def addType(definitionWithoutType: String, typ: String): String = {
    val Array(lhs, rhs) = definitionWithoutType.split("=")
    s"$lhs: $typ =$rhs"
  }

  private def inheritWithStringToIntConversion(baseDefinition: String, overrideDefinition: String) =
    s"""
        |class Test {
        |  implicit def s2i(s: String): Int = s.length
        |  trait Base { $baseDefinition }
        |  trait Sub extends Base { $overrideDefinition }
        |  trait Sub2 extends Base { ${addIntType(overrideDefinition)} }
        |}
      """.stripMargin

  def testValInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "val foo = \"\"")))
  }

  def testValInheritReturnTypeParens(): Unit = {
    //TODO: this, for some reason, does not work in compiler
    assertMatches(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "val foo = \"\""))) {
      case Error(_, "Overriding type String does not conform to base type () => Int") :: Nil =>
    }
  }

  def testFunInheritReturnTypeParens(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "def foo = \"\"")))
  }

  def testFunInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "def foo = \"\"")))
  }

  def testParensFunInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "def foo() = \"\"")))
  }

  def testParensFunInheritReturnTypeParens(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "def foo() = \"\"")))
  }

  def testVarInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "var foo = \"\"")))
  }

  def testVarInheritReturnTypeParens(): Unit = {
    //TODO: this, for some reason, does not work in compiler
    assertMatches(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "var foo = \"\""))) {
      case Error(_, "Overriding type String does not conform to base type () => Int") :: Nil =>
    }
  }

  def testSCL14152(): Unit = {
    val code =
      """
        |sealed trait TagExpr
        |
        |sealed trait Composite extends TagExpr {
        |  def head: TagExpr
        |  def tail: Seq[TagExpr]
        |  def all: Seq[TagExpr] = head +: tail
        |}
        |
        |final case class And(head: TagExpr, tail: TagExpr*) extends Composite
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL14922(): Unit = {
    val code =
      """
        |trait A {
        |  trait Internal
        |  val i = new Internal {}
        |}
        |trait B extends A {
        |  trait Internal extends super.Internal
        |  override val i = new Internal {}
        |}
        |trait C extends A {
        |  trait Internal extends super.Internal
        |  override val i = new Internal {}
        |}
        |trait D extends B with C {
        |  trait Internal extends super[B].Internal with super[C].Internal
        |  override val i = new Internal {}
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL14707(): Unit = {
    val code =
      """
        |trait BaseComponent {
        |  trait BaseComponent {
        |    val abstractName: String
        |  }
        |}
        |
        |trait AbstractChildComponent extends BaseComponent {
        |  trait AbstractChildComponent extends AbstractChildComponent.super[BaseComponent].BaseComponent {
        |    def abstractMethod() : scala.Unit
        |  }
        |}
        |
        |trait ConcreteComponent extends AbstractChildComponent {
        |
        |  object ConcreteComponent extends AbstractChildComponent {
        |    override def abstractMethod(): Unit = ()
        |
        |    override val abstractName = "hello world"
        |  }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  protected val SetterAndGetterTraitsCode =
    """trait Setter {
      |  def setValue(foo: String): Unit
      |}
      |
      |trait Getter {
      |  def getValue: String
      |}
      |
      |trait GetterWithSetter extends Getter with Setter
      |""".stripMargin

  // SCL-14462
  def testDontShowErrorForBeanPropertiesOverridingMethods(): Unit = {
    val code =
      s"""$SetterAndGetterTraitsCode
         |
         |import scala.beans.BeanProperty
         |
         |class A1 extends Getter { @BeanProperty var value = "foo" }
         |class B1 extends Setter { @BeanProperty var value = "foo" }
         |class C1 extends GetterWithSetter { @BeanProperty var value = "foo" }
         |
         |class A2(@BeanProperty val value: String) extends Getter
         |class B2(@BeanProperty var value: String) extends Setter
         |class C2(@BeanProperty var value: String) extends GetterWithSetter
         |""".stripMargin
    assertNoErrors(code)
  }

  def testShowErrorForBeanPropertiesOverridingMethodsWithTypeMissmatch(): Unit = {
    val code =
      s"""$SetterAndGetterTraitsCode
         |
         |import scala.beans.BeanProperty
         |
         |class A3 extends Getter { @BeanProperty var value: Int = 42 }
         |class B3 extends Setter { @BeanProperty var value: Int = 42 }
         |class C3 extends GetterWithSetter { @BeanProperty var value: Int = 42 }
         |
         |class A4(@BeanProperty val value: Int) extends Getter
         |class B4(@BeanProperty var value: Int) extends Setter
         |class C4(@BeanProperty var value: Int) extends GetterWithSetter
         |""".stripMargin
    assertErrors(code, Seq(
      Error("value", "Overriding type Int does not conform to base type String"),
      Error("B3", "Class 'B3' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("C3", "Class 'C3' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("value", "Overriding type Int does not conform to base type String"),
      //
      Error("value", "Overriding type Int does not conform to base type String"),
      Error("B4", "Class 'B4' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("C4", "Class 'C4' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("value", "Overriding type Int does not conform to base type String"),
    ): _*)
  }

  def testOverridingTypeDoesNotConformToBaseType_ExtendingScalaClass(): Unit = {
    val code =
      s"""class BaseScala {
         |  protected def fooProtected: String = null
         |  def foo1: String = ???
         |  def foo2(): String = ???
         |  def foo3(x: Int): String = ???
         |  def foo4(x: Int, y: String): String = ???
         |}
         |
         |abstract class B extends BaseScala {
         |  override protected def fooProtected: Int = ???
         |  override def foo1: Int = ???
         |  override def foo2(): Int = ???
         |  override def foo3(x: Int): Int = ???
         |  override def foo4(x: Int, y: String): Int = ???
         |}
         |""".stripMargin

    assertErrorsText(code,
      """Error(fooProtected,Overriding type Int does not conform to base type String)
        |Error(foo1,Overriding type Int does not conform to base type String)
        |Error(foo2,Overriding type () => Int does not conform to base type () => String)
        |Error(foo3,Overriding type Int => Int does not conform to base type Int => String)
        |Error(foo4,Overriding type (Int, String) => Int does not conform to base type (Int, String) => String)
        |""".stripMargin)
  }

  //SCL-20010
  def testOverridingTypeDoesNotConformToBaseType_ExtendingJavaClass(): Unit = {
    getFixture.addFileToProject("BaseJava.java",
      """public class BaseJava {
        |    protected String fooProtected() { return null; }
        |    public String fooPublic1() { return null; }
        |    public String fooPublic2(int x) { return null; }
        |    public String fooPublic3(int x, String y) { return null; }
        |}
        |""".stripMargin
    )

    val code =
      s"""class A extends BaseJava {
         |  override protected def fooProtected: Int = ???
         |  override def fooPublic1(): Int = ???
         |  override def fooPublic2(x: Int): Int = ???
         |  override def fooPublic3(x: Int, y: String): Int = ???
         |}
         |""".stripMargin

    assertErrorsText(code,
      """Error(fooProtected,Overriding type Int does not conform to base type () => String)
        |Error(fooPublic1,Overriding type () => Int does not conform to base type () => String)
        |Error(fooPublic2,Overriding type Int => Int does not conform to base type Int => String)
        |Error(fooPublic3,Overriding type (Int, String) => Int does not conform to base type (Int, String) => String)
        |""".stripMargin)
  }

  def testOverridingTypeDoesNotConformToBaseType_ExtendingJavaClass_FromJdk(): Unit = {
    val code =
      s"""class MyComparator extends java.util.Comparator[String] {
         |  override def compare(o1: String, o2: String): AnyRef = ???
         |}
         |""".stripMargin

    assertErrorsText(code,
      """Error(compare,Overriding type (String, String) => AnyRef does not conform to base type (T, T) => Int)
        |""".stripMargin)
  }


  def testOverridingTypeConformsToBaseType_ExtendingJavaClass(): Unit = {
    getFixture.addFileToProject("BaseJava.java",
      """public class BaseJava {
        |    protected String fooProtected() { return null; }
        |    public String fooPublic1() { return null; }
        |    public String fooPublic2(int x) { return null; }
        |    public String fooPublic3(int x, String y) { return null; }
        |}
        |""".stripMargin
    )

    val code =
      s"""class A extends BaseJava {
         |  override protected def fooProtected: String = ???
         |  override def fooPublic1: String = ???
         |  override def fooPublic2(x: Int): String = ???
         |  override def fooPublic3(x: Int, y: String): String = ???
         |}
         |""".stripMargin

    assertNoErrors(code)
  }


  def testOverridingTypeConformsToBaseType_ExtendingJavaClass_FromJdk(): Unit = {
    val code =
      s"""class MyComparator extends java.util.Comparator[String] {
         |  override def compare(o1: String, o2: String): Int = ???
         |}
         |""".stripMargin
    assertNoErrors(code)
  }
}
