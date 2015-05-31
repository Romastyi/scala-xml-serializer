package play.tools.xml

object ESOAP extends ESOAP

trait ESOAP {
  def toSOAP[T](t: T, ns: xml.NamespaceBinding, base: xml.Elem)
               (implicit r: XMLWriter[T]): xml.NodeSeq = {
    DefaultImplicits.SoapEnvelopeWriter[T](r).write(SoapEnvelope(t)(ns), base)
  }
  def toSOAP[T](t: T)(implicit r: XMLWriter[T], ns: xml.NamespaceBinding): xml.NodeSeq = {
    toSOAP(t, ns, <envelope/>)
  }
  def toSOAP[T](t: T, ns: xml.NamespaceBinding)(implicit r: XMLWriter[T]): xml.NodeSeq = {
    toSOAP(t, ns, <envelope/>)
  }

  def fromSOAP[T](x: xml.NodeSeq)(implicit r: XMLReader[T]): XMLResult[T] = {
    DefaultImplicits.SoapEnvelopeReader[T](r).read(x) match {
      case XMLSuccess(SoapEnvelope(t), p) => XMLSuccess(t, p)
      case e: XMLError => e
    }
  }

  val SoapNS = xml.NamespaceBinding("soapenv", "http://schemas.xmlsoap.org/soap/envelope/", xml.TopScope)
}


case class SoapEnvelope[T](t: T)(implicit _namespace: xml.NamespaceBinding = ESOAP.SoapNS) {
  def namespace = _namespace
}

case class SoapFault[T](
                         faultcode: String,
                         faultstring: String,
                         faultactor: String,
                         detail: T
                         )

object SoapFault {
  // Found an invalid namespace for the SOAP Envelope element
  val FAULTCODE_VERSION_MISMATCH = "SOAP-ENV:VersionMismatch"
  // An immediate child element of the Header element, with the mustUnderstand attribute set to "1", was not understood
  val FAULTCODE_MUST_UNDERSTAND = "SOAP-ENV:MustUnderstand"
  // The message was incorrectly formed or contained incorrect information
  val FAULTCODE_CLIENT = "SOAP-ENV:Client"
  // There was a problem with the server so the message could not proceed
  val FAULTCODE_SERVER = "SOAP-ENV:Server"
}


object DefaultImplicits extends DefaultImplicits

trait DefaultImplicits extends DefaultSOAPFormatters// with DefaultFormatters with BasicReaders with SpecialReaders with BasicWriters with SpecialWriters

trait DefaultSOAPFormatters {

  implicit def SoapEnvelopeReader[T](implicit fmt: XMLReader[T]) = new XMLReader[SoapEnvelope[T]] {
    def read(x: xml.NodeSeq): XMLResult[SoapEnvelope[T]] = {
      x.collectFirst{ case x:xml.Elem if x.label == "Envelope" => x}.flatMap { env =>
        (env \ "Body").headOption.map{ body =>
          fmt.read(body).map{ t =>
            SoapEnvelope(t)(env.scope)
          }
        }
      }
    }.getOrElse(XMLError(Seq(("", Seq("error.expected.soap.envelope")))))
  }

  implicit def SoapEnvelopeWriter[T](implicit fmt: XMLWriter[T]) = new XMLWriter[SoapEnvelope[T]] {
    def write(st: SoapEnvelope[T], base: xml.NodeSeq) = {
      val env = <soapenv:Envelope>
        <soapenv:Header/>
        <soapenv:Body>
          {
          EXML.toXML(st.t)
          }
        </soapenv:Body>
      </soapenv:Envelope>
      env.copy(scope = st.namespace)
    }
  }

  implicit def SoapFaultReader[T](implicit fmt: XMLReader[T], strR:XMLReader[String]) = new XMLReader[SoapFault[T]] {
    def read(x: xml.NodeSeq): XMLResult[SoapFault[T]] = {
      val envelope = x \\ "Fault"
      envelope.headOption.map {elt =>
        (
          strR.read(elt \ "faultcode"),
          strR.read(elt \ "faultstring"),
          strR.read(elt \ "faultactor"),
          fmt.read(elt \ "detail")
          ) match {
          case (e: XMLError,_,_,_) => e ++ XMLError(Seq(("faultcode"  , Seq("Code part missing in SOAP Fault"))))
          case (_,e: XMLError,_,_) => e ++ XMLError(Seq(("faultstring", Seq("Message part missing in SOAP Fault"))))
          case (_,_,e: XMLError,_) => e ++ XMLError(Seq(("faultactor" , Seq("Actor part missing in SOAP Fault"))))
          case (_,_,_,e: XMLError) => e ++ XMLError(Seq(("detail"     , Seq("Detail part missing in SOAP Fault"))))
          case (XMLSuccess(code,_),XMLSuccess(msg,_),XMLSuccess(actor,_),XMLSuccess(detail,_)) =>
            XMLSuccess(SoapFault(code, msg, actor, detail))
          case _ => XMLError(Seq(("", Seq("error.expected.soap.fault"))))
        }
      }
    }.getOrElse(XMLError(Seq(("", Seq("error.expected.soap.fault")))))
  }

  implicit def SoapFaultWriter[T](implicit fmt: XMLWriter[T]) = new XMLWriter[SoapFault[T]] {
    def write(fault: SoapFault[T], base: xml.NodeSeq) = {
      <soapenv:Fault>
        <faultcode>{ fault.faultcode }</faultcode>
        <faultstring>{ fault.faultstring }</faultstring>
        <faultactor>{ fault.faultactor }</faultactor>
        <detail>
          {
          EXML.toXML(fault.detail)
          }
        </detail>
      </soapenv:Fault>
    }
  }
}
