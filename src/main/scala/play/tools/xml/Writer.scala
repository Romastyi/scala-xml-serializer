package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

import scala.collection.{Map, Traversable}

trait XMLWriter[-T] {
  def write(t: T, base: xml.NodeSeq): xml.NodeSeq
}

object XMLWriter extends BasicWriters with SpecialWriters {

  def apply[T](f: (T, xml.NodeSeq) => xml.NodeSeq): XMLWriter[T] = new XMLWriter[T] {
    def write(t: T, base: xml.NodeSeq): xml.NodeSeq = f(t, base)
  }

  def apply[T](name: String)(implicit w: XMLWriter[T]): XMLWriter[T] = new XMLWriter[T] {
    def write(t: T, base: xml.NodeSeq): xml.NodeSeq = EXMLPath.addChild(base, EXML.toXML(t, EXMLPath.emptyElem(name)))
  }
}

trait OXMLWriter[-A] extends XMLWriter[A] {

  def write(o: A): xml.NodeSeq

  def at[A](path: EXMLPath)(implicit wrs: XMLWriter[A]): OXMLWriter[A] =
    OXMLWriter[A] { a => EXMLPath.addChild(path.base, wrs.write(a, EXMLPath.emptyElem(path.path))) }
}

object OXMLWriter extends BasicWriters with SpecialWriters {

  import play.api.libs.functional._

  implicit val functionalCanBuildOWriter: FunctionalCanBuild[OXMLWriter] = new FunctionalCanBuild[OXMLWriter] {

    def apply[A, B](wa: OXMLWriter[A], wb: OXMLWriter[B]): OXMLWriter[A ~ B] = OXMLWriter[A ~ B] { case a ~ b => wa.write(a) ++ wb.write(b) }
  }

  implicit val contravariantfunctorOWriter: ContravariantFunctor[OXMLWriter] = new ContravariantFunctor[OXMLWriter] {

    def contramap[A, B](wa: OXMLWriter[A], f: B => A): OXMLWriter[B] = OXMLWriter[B](b => wa.write(f(b)))
  }

  def apply[A](f: A => xml.NodeSeq): OXMLWriter[A] = new OXMLWriter[A] {
    def write(o: A): xml.NodeSeq = f(o)
    def write(t: A, base: xml.NodeSeq): xml.NodeSeq = EXMLPath.addChild(base, f(t))
  }

  def atMap[A](path: EXMLPath)(implicit wrs: XMLWriter[A]): OXMLWriter[A] = {
    OXMLWriter[A] { a => EXMLPath.createElem(path, wrs.write(a, <item/>)) }
  }

  def atList[A](path: EXMLPath)(implicit wrs: XMLWriter[A]): OXMLWriter[A] = {
    OXMLWriter[A] { a => EXMLPath.createElem(path, wrs.write(a, xml.NodeSeq.Empty)) }
  }

  def at[A](path: EXMLPath)(implicit wrs: XMLWriter[A]): OXMLWriter[A] = {
    OXMLWriter[A] { a => EXMLPath.addChild(path.base, wrs.write(a, EXMLPath.emptyElem(path.path))) }
  }
}

object BasicWriters extends BasicWriters

trait BasicWriters {
  implicit object StringWriter extends XMLWriter[String] {
    def write(s: String, base: xml.NodeSeq): xml.NodeSeq = base.collectFirst{ case e: xml.Elem => e.copy(child = xml.Text(s)) }.getOrElse(xml.Text(s))
  }

  implicit object IntWriter extends XMLWriter[Int] {
    def write(s: Int, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }

  implicit object LongWriter extends XMLWriter[Long] {
    def write(s: Long, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }

  implicit object FloatWriter extends XMLWriter[Float] {
    def write(s: Float, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }

  implicit object ShortWriter extends XMLWriter[Short] {
    def write(s: Short, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }

  implicit object DoubleWriter extends XMLWriter[Double] {
    def write(s: Double, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }

  implicit object BooleanWriter extends XMLWriter[Boolean] {
    def write(s: Boolean, base: xml.NodeSeq): xml.NodeSeq = StringWriter.write(s.toString, base)
  }
}

object SpecialWriters extends SpecialWriters

trait SpecialWriters {

  val xsiNS = xml.NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", xml.TopScope)

  implicit def optionWriter[T](implicit w: XMLWriter[T]) = new XMLWriter[Option[T]] {
    def write(t: Option[T], base: xml.NodeSeq) = {
      t match {
        case None => base.collectFirst{ case e: xml.Elem => e.copy(scope = xsiNS) % xml.Attribute("xsi", "nil", "true", xml.Null) }.getOrElse(xml.NodeSeq.Empty)
        case Some(t) => w.write(t, base)
      }
    }
  }

  implicit def traversableWriter[T](implicit w: XMLWriter[T]) = new XMLWriter[Traversable[T]] {
    def write(t: Traversable[T], base: xml.NodeSeq) = {
      t.foldLeft(xml.NodeSeq.Empty)( (acc, n) => acc ++ w.write(n, base) )
    }
  }

  implicit def mapWriter[K, V](implicit kw: XMLWriter[K], vw: XMLWriter[V]) = new XMLWriter[Map[K, V]] {
    def write(m: Map[K, V], base: xml.NodeSeq) = {
      m.foldLeft(xml.NodeSeq.Empty){ (acc, n) =>
        base.collectFirst{ case e:xml.Elem =>
          e.copy( child = kw.write(n._1, <key/>) ++ vw.write(n._2, <value/>) )
        }.map( acc ++ _ ).getOrElse(acc)
      }
    }
  }
}
