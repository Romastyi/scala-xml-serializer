package play.tools.xml

/**
 * Created by romastyi on 31/05/15.
 */

case class XMLSuccess[T](value: T, path: String = "") extends XMLResult[T] {

  def get: T = value
}

case class XMLError(errors: Seq[(String, Seq[String])]) extends XMLResult[Nothing] {

  def get: Nothing = throw new NoSuchElementException("XMLError.get")

  def ++(other: XMLError): XMLError = XMLError(XMLError.merge(this, other))
}

object XMLError {

  def merge(e1: XMLError, e2: XMLError): Seq[(String, Seq[String])] =
    merge(e1.errors, e2.errors)

  def merge(e1: Seq[(String, Seq[String])], e2: Seq[(String, Seq[String])]): Seq[(String, Seq[String])] =
    (e1 ++ e2).groupBy(_._1).mapValues(_.flatMap(_._2)).toSeq
}

sealed trait XMLResult[+A] { self =>

  def isSuccess: Boolean = this.isInstanceOf[XMLSuccess[_]]
  def isError: Boolean = this.isInstanceOf[XMLError]

  def get: A

  def asOpt: Option[A] = this match {
    case XMLSuccess(v, path) => Some(v)
    case e: XMLError => println(e.errors); None
  }

  def foreach(f: A => Unit): Unit = this match {
    case XMLSuccess(a, _) => f(a)
    case XMLError(_) => ()
  }

  def map[X](f: A => X): XMLResult[X] = this match {
    case XMLSuccess(v, path) => XMLSuccess(f(v), path)
    case e: XMLError => e
  }
}

object XMLResult {

  import play.api.libs.functional._

  implicit def alternativeXMLResult(implicit a: Applicative[XMLResult]): Alternative[XMLResult] = new Alternative[XMLResult] {
    val app = a
    def |[A, B >: A](alt1: XMLResult[A], alt2: XMLResult[B]): XMLResult[B] = (alt1, alt2) match {
      case (XMLError(e), XMLSuccess(t, p)) => XMLSuccess(t, p)
      case (XMLSuccess(t, p), _) => XMLSuccess(t, p)
      case (XMLError(e1), XMLError(e2)) => XMLError(XMLError.merge(e1, e2))
    }
    def empty: XMLResult[Nothing] = XMLError(Seq())
  }

  implicit val applicativeXMLResult: Applicative[XMLResult] = new Applicative[XMLResult] {

    def pure[A](a: A): XMLResult[A] = XMLSuccess(a)

    def map[A, B](m: XMLResult[A], f: A => B): XMLResult[B] = m.map(f)

    def apply[A, B](mf: XMLResult[A => B], ma: XMLResult[A]): XMLResult[B] = (mf, ma) match {
      case (XMLSuccess(f, _), XMLSuccess(a, _)) => XMLSuccess(f(a))
      case (XMLError(e1), XMLError(e2)) => XMLError(XMLError.merge(e1, e2))
      case (XMLError(e), _) => XMLError(e)
      case (_, XMLError(e)) => XMLError(e)
    }
  }

  implicit val functorJsResult: Functor[XMLResult] = new Functor[XMLResult] {
    override def fmap[A, B](m: XMLResult[A], f: A => B) = m map f
  }
}