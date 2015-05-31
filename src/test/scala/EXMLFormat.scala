/**
 * Created by romastyi on 11.04.2015.
 */

import org.specs2.mutable._
import play.api.libs.functional.syntax._
import play.tools.xml._

class EXMLFormat extends Specification {

  case class Foo(id: Long, name: String, age: Int, amount: Float, isX: Boolean, opt: Option[Double], numbers: List[Int], map: Map[String, Short])
  case class Bar(foo: Option[Foo])
  case class Yup(bar: Option[List[Bar]])

  implicit def or[T](implicit r: XMLReader[T]) = new XMLReader[Option[T]] {
    def read(x: xml.NodeSeq): XMLResult[Option[T]] = {
      x.collectFirst { case e: xml.Elem => e.child }.map { e =>
        r.read(e) match {
          case XMLSuccess(v, p) => XMLSuccess(Some(v), p)
          case e: XMLError => e
        }
      }.getOrElse(XMLSuccess(None))
    }
  }

  implicit def ow[T](implicit w: XMLWriter[T]) = new XMLWriter[Option[T]] {
    def write(t: Option[T], base: xml.NodeSeq) = {
      t.fold(xml.NodeSeq.Empty) { v =>
        base.collectFirst{ case e: xml.Elem => e.copy(child = w.write(v, xml.NodeSeq.Empty)) }.getOrElse(xml.NodeSeq.Empty)
      }
    }
  }

  "EXMLFormat" should {

    val foo = Foo(1234L, "albert", 23, 123.456F, isX = true, None, List(123, 57), Map("alpha" -> 23.toShort, "beta" -> 87.toShort))
    val xmlFoo = <Foo>
      <id>1234</id>
      <name>albert</name>
      <age>23</age>
      <amount>123.456</amount>
      <isX>true</isX>
      <numbers>
        <nb>123</nb>
        <nb>57</nb>
      </numbers>
      <map>
        <item><key>alpha</key><value>23</value></item>
        <item><key>beta</key><value>87</value></item>
      </map>
    </Foo>

    val bar = Bar(Some(foo))
    val xmlBar = <Bar><foo>{ xmlFoo }</foo></Bar>

    val yup = Yup(Some(List(bar, bar)))
    val xmlYup = <Yup><bar>{ xmlBar }{ xmlBar }</bar></Yup>

    val yupEmpty = Yup(Some(List()))
    val xmlYupEmpty = <Yup><bar/></Yup>

    val yupNone = Yup(None)
    val xmlYupNone = <Yup/>

    "Writer" in {

      implicit val w = {
        implicit val xmlL = XMLWriter[List[Int]]("nb")
        XMLWriter("Foo")(((EXMLPath \ "id").write[Long] and
          (EXMLPath \ "name").write[String] and
          (EXMLPath \ "age").write[Int] and
          (EXMLPath \ "amount").write[Float] and
          (EXMLPath \ "isX").write[Boolean] and
          (EXMLPath \ "opt").write[Option[Double]] and
          (EXMLPath \ "numbers").writeList[List[Int]] and
          (EXMLPath \ "map").writeMap[Map[String, Short]]
          ).apply(unlift(Foo.unapply)))
      }

      EXML.toXML(foo) must ==/(xmlFoo.toString())
    }

    "Writer (macro)" in {

      implicit val w = {
        implicit val xmlL = XMLWriter[List[Int]]("nb")
        EXML.writer[Foo]
      }

      EXML.toXML(foo) must ==/(xmlFoo.toString())
    }

    "Reader" in {

      implicit val r = {
        implicit val xmlL = XMLReader[List[Int]]("nb")
        XMLReader("Foo")(((EXMLPath \ "id").read[Long] and
          (EXMLPath \ "name").read[String] and
          (EXMLPath \ "age").read[Int] and
          (EXMLPath \ "amount").read[Float] and
          (EXMLPath \ "isX").read[Boolean] and
          (EXMLPath \ "opt").read[Option[Double]] and
          (EXMLPath \ "numbers").readList[List[Int]] and
          (EXMLPath \ "map").readMap[Map[String, Short]]
          ).apply(Foo.apply _))
      }

      EXML.fromXML[Foo](xmlFoo).asOpt must equalTo(Some(foo))
    }

    "Reader (macro)" in {

      implicit val r = {
        implicit val xmlL = XMLReader[List[Int]]("nb")
        EXML.reader[Foo]
      }

      EXML.fromXML[Foo](xmlFoo).asOpt must equalTo(Some(foo))
    }

    "Formatter" in {

      implicit val f = {
        implicit val xmlL = XMLFormatter[List[Int]]("nb")
        XMLFormatter("Foo")(((EXMLPath \ "id").format[Long] and
          (EXMLPath \ "name").format[String] and
          (EXMLPath \ "age").format[Int] and
          (EXMLPath \ "amount").format[Float] and
          (EXMLPath \ "isX").format[Boolean] and
          (EXMLPath \ "opt").format[Option[Double]] and
          (EXMLPath \ "numbers").formatList[List[Int]] and
          (EXMLPath \ "map").formatMap[Map[String, Short]]
          ).apply(Foo.apply, unlift(Foo.unapply)))
      }

      EXML.toXML(foo) must ==/(xmlFoo.toString())

      EXML.fromXML[Foo](xmlFoo).asOpt must equalTo(Some(foo))

      EXML.fromXML[Foo](EXML.toXML(foo)).asOpt must equalTo(Some(foo))
    }

    "Formatter (macro)" in {

      implicit val f = {
        implicit val xmlL = XMLFormatter[List[Int]]("nb")
        EXML.format[Foo]
      }

      EXML.toXML(foo) must ==/(xmlFoo.toString())

      EXML.fromXML[Foo](xmlFoo).asOpt must equalTo(Some(foo))

      EXML.fromXML[Foo](EXML.toXML(foo)).asOpt must equalTo(Some(foo))
    }

    "List (macro)" in {

      implicit val f = {
        implicit val xmlL = XMLFormatter[List[Int]]("nb")
        EXML.format[Foo]
      }

      <list>{ EXML.toXML(List(foo, foo)) }</list> must ==/(<list>{ xmlFoo }{ xmlFoo }</list>.toString())

      EXML.fromXML[List[Foo]](<list>{ xmlFoo }{ xmlFoo }</list>.child).asOpt must equalTo(Some(List(foo, foo)))

      EXML.fromXML[List[Foo]](<list>{ EXML.toXML(List(foo, foo)) }</list>.child).asOpt must equalTo(Some(List(foo, foo)))
    }

    "Nested classes (macro)" in {

      implicit val f = {
        implicit val xmlL = XMLFormatter[List[Int]]("nb")
        EXML.format[Foo]
      }

      implicit val b = EXML.format[Bar]

      implicit val y = EXML.format[Yup]

      EXML.toXML(Bar(None)) must ==/(<Bar/>.toString())

      EXML.fromXML[Bar](<Bar/>).asOpt must equalTo(Some(Bar(None)))

      EXML.fromXML[Bar](EXML.toXML(Bar(None))).asOpt must equalTo(Some(Bar(None)))

      EXML.toXML(bar) must ==/(xmlBar.toString())

      EXML.fromXML[Bar](xmlBar).asOpt must equalTo(Some(bar))

      EXML.fromXML[Bar](EXML.toXML(bar)).asOpt must equalTo(Some(bar))

      EXML.toXML(yup) must ==/(xmlYup.toString())

      EXML.fromXML[Yup](xmlYup).asOpt must equalTo(Some(yup))

      EXML.fromXML[Yup](EXML.toXML(yup)).asOpt must equalTo(Some(yup))

      EXML.toXML(yupEmpty) must ==/(xmlYupEmpty.toString())

      EXML.fromXML[Yup](xmlYupEmpty).asOpt must equalTo(Some(yupEmpty))

      EXML.fromXML[Yup](EXML.toXML(yupEmpty)).asOpt must equalTo(Some(yupEmpty))

      EXML.toXML(yupNone) must ==/(xmlYupNone.toString())

      EXML.fromXML[Yup](xmlYupNone).asOpt must equalTo(Some(yupNone))

      EXML.fromXML[Yup](EXML.toXML(yupNone)).asOpt must equalTo(Some(yupNone))
    }
  }
}
