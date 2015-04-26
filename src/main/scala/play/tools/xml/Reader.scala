package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

import scala.language.{implicitConversions, higherKinds}
import scala.collection.generic

trait XMLReader[T] { self =>

  def read(x: xml.NodeSeq): Option[T]

  def map[B](f: T => B): XMLReader[B] =
    XMLReader[B] { (x: xml.NodeSeq) => self.read(x).map(f) }
}

object XMLReader extends BasicReaders with SpecialReaders {

  import play.api.libs.functional._

  protected def child(x: xml.NodeSeq) = x flatMap {
    case e: xml.Elem => e.child.filter {
      case e1: xml.Elem => true
      case _ => false
    }
    case _ => xml.NodeSeq.Empty
  }

  protected def descendant_or_self(x: xml.NodeSeq, name: String): xml.NodeSeq = {
    (x match {
      case e: xml.Elem if e.label == name => e
      case _ => x flatMap {
        case e: xml.Elem if e.label == name => e
        case _ =>  xml.NodeSeq.Empty
      }
    }) ++ (x \ name)
  }

  implicit def applicative(implicit applicativeJsResult: Applicative[Option]): Applicative[XMLReader] = new Applicative[XMLReader] {

    def pure[A](a: A): XMLReader[A] = XMLReader[A] { (_: xml.NodeSeq) => Some(a) }

    def map[A, B](m: XMLReader[A], f: A => B): XMLReader[B] = m.map(f)

    def apply[A, B](mf: XMLReader[A => B], ma: XMLReader[A]): XMLReader[B] = new XMLReader[B] {
      def read(x: xml.NodeSeq) = applicativeJsResult(mf.read(x), ma.read(x))
    }
  }

  implicit def alternative(implicit a: Applicative[XMLReader]): Alternative[XMLReader] = new Alternative[XMLReader] {
    val app = a
    def |[A, B >: A](alt1: XMLReader[A], alt2: XMLReader[B]): XMLReader[B] = new XMLReader[B] {
      def read(x: xml.NodeSeq) = alt1.read(x) match {
        case Some(r) => Some(r)
        case None => alt2.read(x) match {
          case Some(r2) => Some(r2)
          case None => None
        }
      }
    }

    def empty: XMLReader[Nothing] = new XMLReader[Nothing] {
      def read(x: xml.NodeSeq) = None
    }
  }

  implicit def functorReads(implicit a: Applicative[XMLReader]): Functor[XMLReader] = new Functor[XMLReader] {
    def fmap[A, B](read: XMLReader[A], f: A => B): XMLReader[B] = a.map(read, f)
  }

  def apply[T](f: xml.NodeSeq => Option[T]): XMLReader[T] = new XMLReader[T] {
    def read(x: xml.NodeSeq): Option[T] = f(x)
  }

  def apply[T](name: String)(implicit r: XMLReader[T]): XMLReader[T] = new XMLReader[T] {
    def read(x: xml.NodeSeq): Option[T] = r.read(descendant_or_self(x, name))
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
    def read(x: xml.NodeSeq): Option[String] = if(x.isEmpty) None else Some(x.text)
  }

  implicit object IntReader extends XMLReader[Int] {
    def read(x: xml.NodeSeq): Option[Int] = if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toInt
  }

  implicit object LongReader extends XMLReader[Long] {
    def read(x: xml.NodeSeq): Option[Long] =  if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toLong
  }

  implicit object ShortReader extends XMLReader[Short] {
    def read(x: xml.NodeSeq): Option[Short] =  if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toShort
  }

  implicit object FloatReader extends XMLReader[Float] {
    def read(x: xml.NodeSeq): Option[Float] =  if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toFloat
  }

  implicit object DoubleReader extends XMLReader[Double] {
    def read(x: xml.NodeSeq): Option[Double] =  if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toDouble
  }

  implicit object BooleanReader extends XMLReader[Boolean] {
    def read(x: xml.NodeSeq): Option[Boolean] =  if(x.isEmpty) None else catching(exceptions: _*) opt x.text.toBoolean
  }
}

object SpecialReaders extends SpecialReaders

trait SpecialReaders {
  implicit def optionReader[T](implicit r: XMLReader[T]) = new XMLReader[Option[T]] {
    def read(x: xml.NodeSeq): Option[Option[T]] = {
      x.collectFirst{ case e: xml.Elem => e }.map{ e =>
        if(e.attributes.exists{ a => a.key == "nil" && a.value.text == "true" }) None
        else r.read(e)
      }.orElse(Some(None))
    }
  }

  implicit def traversableReader[F[_], A](implicit bf: generic.CanBuildFrom[F[_], A, F[A]], r: XMLReader[A]) = new XMLReader[F[A]] {
    def read(x: xml.NodeSeq): Option[F[A]] = {
      val builder = bf()
      x.foreach{ n => r.read(n).foreach{ builder += _ } }
      Some(builder.result)
    }
  }

  implicit def mapReader[K, V](implicit rk: XMLReader[K], rv: XMLReader[V]): XMLReader[collection.immutable.Map[K,V]] = new XMLReader[collection.immutable.Map[K,V]] {
    def read(x: xml.NodeSeq): Option[collection.immutable.Map[K, V]] = {
      Some(x.collect{ case e: xml.Elem =>
        for(k <- EXML.fromXML[K](e \ "key");
            v <- EXML.fromXML[V](e \ "value")
        ) yield( k -> v )
      }.filter(_.isDefined).map(_.get).toMap[K,V])
    }
  }
}

