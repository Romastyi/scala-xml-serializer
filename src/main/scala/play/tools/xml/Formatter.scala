package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

trait XMLFormatter[T] extends XMLReader[T] with XMLWriter[T]

object XMLFormatter extends DefaultFormatters {

  def apply[T](reader: XMLReader[T], writer: XMLWriter[T]): XMLFormatter[T] = {
    new XMLFormatter[T] {
      def read(x: xml.NodeSeq): XMLResult[T] = reader.read(x)
      def write(t: T, base: xml.NodeSeq): xml.NodeSeq = writer.write(t, base)
    }
  }

  def apply[T](name: String)(implicit f: XMLFormatter[T]): XMLFormatter[T] = {
    new XMLFormatter[T] {
      def read(x: xml.NodeSeq): XMLResult[T] = XMLReader(name)(f).read(x)
      def write(t: T, base: xml.NodeSeq): xml.NodeSeq = XMLWriter(name)(f).write(t, base)
    }
  }
}

trait OXMLFormatter[T] extends XMLReader[T] with OXMLWriter[T] with XMLFormatter[T]

object OXMLFormatter extends DefaultFormatters {

  import play.api.libs.functional._
  import scala.reflect.runtime.universe._

  implicit def functionalCanBuildFormats(implicit rcb: FunctionalCanBuild[XMLReader], wcb: FunctionalCanBuild[OXMLWriter]): FunctionalCanBuild[OXMLFormatter] = new FunctionalCanBuild[OXMLFormatter] {

    def apply[A, B](fa: OXMLFormatter[A], fb: OXMLFormatter[B]): OXMLFormatter[A ~ B] =
      OXMLFormatter[A ~ B](
        rcb(fa, fb),
        wcb(fa, fb)
      )
  }

  implicit val invariantFunctorOFormat: InvariantFunctor[OXMLFormatter] = new InvariantFunctor[OXMLFormatter] {

    def inmap[A, B](fa: OXMLFormatter[A], f1: A => B, f2: B => A): OXMLFormatter[B] = OXMLFormatter[B]((x: xml.NodeSeq) => fa.read(x).map(f1), (b: B) => fa.write(f2(b)))
  }

  implicit def GenericOFormat[T](implicit f: XMLReader[T], t: OXMLWriter[T]): XMLFormatter[T] = apply(f, t)

  def apply[A](read: xml.NodeSeq => XMLResult[A], write: A => xml.NodeSeq): OXMLFormatter[A] = {
    OXMLFormatter[A](XMLReader[A](read), OXMLWriter[A](write))
  }

  def apply[A](r: XMLReader[A], w: OXMLWriter[A]): OXMLFormatter[A] = new OXMLFormatter[A] {
    def read(x: xml.NodeSeq): XMLResult[A] = r.read(x)

    def write(o: A): xml.NodeSeq = w.write(o)
    def write(t: A, base: xml.NodeSeq): xml.NodeSeq = w.write(t, base)
  }

  def atMap[T](path: EXMLPath)(implicit f: XMLFormatter[T]): OXMLFormatter[T] = {
    OXMLFormatter[T](XMLReader.atMap[T](path), OXMLWriter.atMap[T](path))
  }

  def atList[T](path: EXMLPath)(implicit f: XMLFormatter[T]): OXMLFormatter[T] = {
    OXMLFormatter[T](XMLReader.atList[T](path), OXMLWriter.atList[T](path))
  }

  def at[T](path: EXMLPath)(implicit f: XMLFormatter[T]): OXMLFormatter[T] = {
    OXMLFormatter[T](XMLReader.at[T](path), OXMLWriter.at[T](path))
  }
}

trait DefaultFormatters {

  implicit def GenericFormatter[T](implicit reader: XMLReader[T], writer: XMLWriter[T]): XMLFormatter[T] = {
    new XMLFormatter[T] {
      def read(x: xml.NodeSeq): XMLResult[T] = reader.read(x)
      def write(t: T, base: xml.NodeSeq): xml.NodeSeq = writer.write(t, base)
    }
  }
}

