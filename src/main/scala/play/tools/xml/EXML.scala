package play.tools.xml

object EXML {

	def toXML[T](t: T, base: xml.NodeSeq = xml.NodeSeq.Empty)(implicit w: XMLWriter[T]): xml.NodeSeq = w.write(t, base)
	def fromXML[T](x: xml.NodeSeq)(implicit r: XMLReader[T]): XMLResult[T] = r.read(x)

	import language.experimental.macros

	def reader[T]: XMLReader[T] = macro EXMLMacroImpl.readsImpl[T]
	def writer[T]: XMLWriter[T] = macro EXMLMacroImpl.writesImpl[T]
	def format[T]: XMLFormatter[T] = macro EXMLMacroImpl.formatImpl[T]
}
