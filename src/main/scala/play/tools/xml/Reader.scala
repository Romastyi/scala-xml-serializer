package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

import scala.language.{implicitConversions, higherKinds}
import scala.collection.generic

trait XMLReader[T] { self =>

  def read(x: xml.NodeSeq): XMLResult[T]

  def map[B](f: T => B): XMLReader[B] =
    XMLReader[B] { (x: xml.NodeSeq) => self.read(x).map(f) }
}

object XMLReader extends BasicReaders with SpecialReaders {

  import play.api.libs.functional._

  protected def child(x: xml.NodeSeq) = x flatMap {
    case e: xml.Elem => e.child.collect{ case e1: xml.Elem => e1 }
    case _ => xml.NodeSeq.Empty
  }

  protected def descendant_or_self(x: xml.NodeSeq, name: String): xml.NodeSeq = {
    (x match {
      case e: xml.Elem if e.label == name => e
      case _ => x flatMap {
        case e: xml.Elem if e.label == name => e
        case _ => xml.NodeSeq.Empty
      }
    }) ++ (x \ name)
  }

  implicit def applicative(implicit applicativeXMLResult: Applicative[XMLResult]): Applicative[XMLReader] = new Applicative[XMLReader] {

    def pure[A](a: A): XMLReader[A] = XMLReader[A] { (_: xml.NodeSeq) => XMLSuccess(a) }

    def map[A, B](m: XMLReader[A], f: A => B): XMLReader[B] = m.map(f)

    def apply[A, B](mf: XMLReader[A => B], ma: XMLReader[A]): XMLReader[B] = new XMLReader[B] {
      def read(x: xml.NodeSeq) = applicativeXMLResult(mf.read(x), ma.read(x))
    }
  }

  implicit def alternative(implicit a: Applicative[XMLReader]): Alternative[XMLReader] = new Alternative[XMLReader] {
    val app = a
    def |[A, B >: A](alt1: XMLReader[A], alt2: XMLReader[B]): XMLReader[B] = new XMLReader[B] {
      def read(x: xml.NodeSeq) = alt1.read(x) match {
        case r @ XMLSuccess(_, _) => r
        case r @ XMLError(e1) => alt2.read(x) match {
          case r2 @ XMLSuccess(_, _) => r2
          case r2 @ XMLError(e2) => XMLError(XMLError.merge(e1, e2))
        }
      }
    }

    def empty: XMLReader[Nothing] = new XMLReader[Nothing] {
      def read(x: xml.NodeSeq) = XMLError(Seq())
    }
  }

  implicit def functorReads(implicit a: Applicative[XMLReader]): Functor[XMLReader] = new Functor[XMLReader] {
    def fmap[A, B](read: XMLReader[A], f: A => B): XMLReader[B] = a.map(read, f)
  }

  def apply[T](f: xml.NodeSeq => XMLResult[T]): XMLReader[T] = new XMLReader[T] {
    def read(x: xml.NodeSeq): XMLResult[T] = f(x)
  }

  def apply[T](name: String)(implicit r: XMLReader[T]): XMLReader[T] = new XMLReader[T] {
    def read(x: xml.NodeSeq): XMLResult[T] = r.read(descendant_or_self(x, name))
  }

  def atMap[T](path: EXMLPath)(implicit r: XMLReader[T]): XMLReader[T] = {
    XMLReader[T] { (x: xml.NodeSeq) => r.read(x \ path.path \ "item") }
  }

  def atList[T](path: EXMLPath)(implicit r: XMLReader[T]): XMLReader[T] = {
    XMLReader[T] { (x: xml.NodeSeq) => r.read(child(x \ path.path)) }
  }

  def at[T](path: EXMLPath)(implicit r: XMLReader[T]): XMLReader[T] = {
    XMLReader[T] { (x: xml.NodeSeq) => r.read(x \ path.path) }
  }
}

object BasicReaders extends BasicReaders

trait BasicReaders {
  import scala.util.control.Exception._

  lazy val exceptions = Seq(classOf[NumberFormatException], classOf[IllegalArgumentException])

  implicit object StringReader extends XMLReader[String] {
    def read(x: xml.NodeSeq): XMLResult[String] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.string"))))
    else
      XMLSuccess(x.text)
  }

  implicit object IntReader extends XMLReader[Int] {
    def read(x: xml.NodeSeq): XMLResult[Int] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.int"))))
    else
      catching(exceptions: _*) either x.text.toInt match {
        case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
        case Right(v) => XMLSuccess(v)
      }
  }

  implicit object LongReader extends XMLReader[Long] {
    def read(x: xml.NodeSeq): XMLResult[Long] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.long"))))
    else
      catching(exceptions: _*) either x.text.toLong match {
        case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
        case Right(v) => XMLSuccess(v)
      }
  }

  implicit object ShortReader extends XMLReader[Short] {
    def read(x: xml.NodeSeq): XMLResult[Short] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.short"))))
    else
      catching(exceptions: _*) either x.text.toShort match {
        case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
        case Right(v) => XMLSuccess(v)
      }
  }

  implicit object FloatReader extends XMLReader[Float] {
    def read(x: xml.NodeSeq): XMLResult[Float] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.float"))))
    else
      catching(exceptions: _*) either x.text.toFloat match {
        case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
        case Right(v) => XMLSuccess(v)
      }
  }

  implicit object DoubleReader extends XMLReader[Double] {
    def read(x: xml.NodeSeq): XMLResult[Double] = if (x.isEmpty)
      XMLError(Seq(("", Seq("error.expected.double"))))
    else
      catching(exceptions: _*) either x.text.toDouble match {
        case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
        case Right(v) => XMLSuccess(v)
      }
  }

  implicit object BooleanReader extends XMLReader[Boolean] {
    def read(x: xml.NodeSeq): XMLResult[Boolean] =
      if (x.isEmpty)
        XMLError(Seq(("", Seq("error.expected.boolean"))))
      else
        catching(exceptions: _*) either x.text.toBoolean match {
          case Left(e) => XMLError(Seq(("", Seq(e.getMessage))))
          case Right(v) => XMLSuccess(v)
        }
  }
}

object SpecialReaders extends SpecialReaders

trait SpecialReaders {
  implicit def optionReader[T](implicit r: XMLReader[T]) = new XMLReader[Option[T]] {
    def read(x: xml.NodeSeq): XMLResult[Option[T]] = {
      x.collectFirst{ case e: xml.Elem => e }.map{ e =>
        if (e.attributes.exists{ a => a.key == "nil" && a.value.text == "true" })
          XMLSuccess(None)
        else r.read(e) match {
          case XMLSuccess(v, p) => XMLSuccess(Some(v), p)
          case e: XMLError => e
        }
      }.getOrElse(XMLSuccess(None))
    }
  }

  implicit def traversableReader[F[_], A](implicit bf: generic.CanBuildFrom[F[_], A, F[A]], r: XMLReader[A]) = new XMLReader[F[A]] {
    def read(x: xml.NodeSeq): XMLResult[F[A]] = {

      type Errors = Seq[(String, Seq[String])]
      def locate(e: Errors, idx: Int) = e.map { case(p, valerr) => s"$p[$idx]" -> valerr }

      x.iterator.zipWithIndex.foldLeft(Right(Vector.empty): Either[Errors, Vector[A]]){
        case (acc, (node, idx)) => (acc, r.read(node)) match {
          case (Right(vs), XMLSuccess(v, _)) => Right(vs :+ v)
          case (Right(_), XMLError(e)) => Left(locate(e, idx))
          case (Left(e), _: XMLSuccess[_]) => Left(e)
          case (Left(e1), XMLError(e2)) => Left(XMLError.merge(e1, locate(e2, idx)))
        }
      }.fold(XMLError.apply, { res =>
        val builder = bf()
        builder.sizeHint(res)
        builder ++= res
        XMLSuccess(builder.result())
      })
    }
  }

  implicit def mapReader[K, V](implicit rk: XMLReader[K], rv: XMLReader[V]): XMLReader[collection.immutable.Map[K,V]] = new XMLReader[collection.immutable.Map[K,V]] {
    def read(x: xml.NodeSeq): XMLResult[collection.immutable.Map[K, V]] = {

      type Errors = Seq[(String, Seq[String])]
      def locate(e: Errors, key: K) = e.map { case(p, valerr) => s"$p[$key]" -> valerr }

      x.iterator.foldLeft(Right(Map.empty): Either[Errors, Map[K, V]]){
        case (acc, node) => (acc, EXML.fromXML[K](node \ "key")) match {
          case (Right(vs), XMLSuccess(k, _)) =>
            EXML.fromXML[V](node \ "value") match {
              case XMLSuccess(v, _) => Right(vs + (k -> v))
              case XMLError(e) => Left(locate(e, k))
            }
          case (Right(_), XMLError(e)) => Left(e)
          case (Left(e), _: XMLSuccess[_]) => Left(e)
          case (Left(e1), XMLError(e2)) => Left(XMLError.merge(e1, e2))
        }
      }.fold(XMLError.apply, m => XMLSuccess(m))
    }
  }
}

