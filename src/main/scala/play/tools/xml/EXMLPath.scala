package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

case class EXMLPath(base: xml.NodeSeq, path: String = "") {

  def \(name: String): EXMLPath = EXMLPath(base, name)

  override def toString = "/" + path

  def format[T](implicit f: XMLFormatter[T]): OXMLFormatter[T] = OXMLFormatter.at[T](this)
  def formatList[T <: Traversable[_]](implicit f: XMLFormatter[T]): OXMLFormatter[T] = OXMLFormatter.atList[T](this)
  def formatMap[T <: Map[_, _]](implicit f: XMLFormatter[T]): OXMLFormatter[T] = OXMLFormatter.atMap[T](this)

  def read[T](implicit r: XMLReader[T]): XMLReader[T] = XMLReader.at[T](this)
  def readList[T <: Traversable[_]](implicit r: XMLReader[T]): XMLReader[T] = XMLReader.atList[T](this)
  def readMap[T <: Map[_, _]](implicit r: XMLReader[T]): XMLReader[T] = XMLReader.atMap[T](this)

  def write[T](implicit w: XMLWriter[T]): OXMLWriter[T] = OXMLWriter.at[T](this)
  def writeList[T <: Traversable[_]](implicit w: XMLWriter[T]): OXMLWriter[T] = OXMLWriter.atList[T](this)
  def writeMap[T <: Map[_, _]](implicit w: XMLWriter[T]): OXMLWriter[T] = OXMLWriter.atMap[T](this)
}

object EXMLPath extends EXMLPath(xml.NodeSeq.Empty, "") {

  def addChild(base: xml.NodeSeq, nodes: xml.NodeSeq): xml.NodeSeq =
    base.collectFirst { case e: xml.Elem => e.copy(child = nodes) }.getOrElse(nodes)

  def createElem(path: EXMLPath, nodes: xml.NodeSeq): xml.NodeSeq =
    addChild(path.base, xml.Elem(null, path.path, xml.Null, xml.TopScope, true, nodes: _*))

  def emptyElem(name: String): xml.Elem =
    xml.Elem(null, name, xml.Null, xml.TopScope, true, xml.NodeSeq.Empty: _*)
}
