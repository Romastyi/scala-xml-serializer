package play.tools.xml

/**
 * Created by romastyi on 11.04.2015.
 */

import scala.language.higherKinds
import scala.reflect.macros.Context
import language.experimental.macros

object EXMLMacroImpl {

  def formatImpl[A: c.WeakTypeTag](c: Context): c.Expr[XMLFormatter[A]] =
    macroImpl[A, XMLFormatter](c, "format", "inmap", reader = true, writer = true)

  def readsImpl[A: c.WeakTypeTag](c: Context): c.Expr[XMLReader[A]] =
    macroImpl[A, XMLReader](c, "read", "map", reader = true, writer = false)

  def writesImpl[A: c.WeakTypeTag](c: Context): c.Expr[XMLWriter[A]] =
    macroImpl[A, XMLWriter](c, "write", "contramap", reader = false, writer = true)

  def macroImpl[A, M[_]](c: Context, methodName: String, mapLikeMethod: String, reader: Boolean, writer: Boolean)(implicit atag: c.WeakTypeTag[A], matag: c.WeakTypeTag[M[A]]): c.Expr[M[A]] = {

//    val nullableMethodName = s"${methodName}Nullable"
//    val lazyMethodName = s"lazy${methodName.capitalize}"

    def conditionalList[T](ifReads: T, ifWrites: T): List[T] =
      (if (reader) List(ifReads) else Nil) :::
        (if (writer) List(ifWrites) else Nil)

    import c.universe._
    import c.universe.Flag._

    val companioned = weakTypeOf[A].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    val xmlPkg = Select(Select(Ident(newTermName("play")), newTermName("tools")), newTermName("xml"))
    val readerSelect = Select(xmlPkg, newTermName("XMLReader"))
    val writerSelect = Select(xmlPkg, newTermName("XMLWriter"))
    val formatterSelect = Select(xmlPkg, newTermName("XMLFormatter"))
    val xmlPath = Select(xmlPkg, newTermName("EXMLPath"))

    val libsPkg = Select(Select(Ident(newTermName("play")), newTermName("api")), newTermName("libs"))
    val functionalSyntaxPkg = Select(Select(libsPkg, newTermName("functional")), newTermName("syntax"))
    val unliftIdent = Select(functionalSyntaxPkg, newTermName("unlift"))

    val unapply = companionType.declaration(stringToTermName("unapply"))
    val unapplySeq = companionType.declaration(stringToTermName("unapplySeq"))
    val hasVarArgs = unapplySeq != NoSymbol

    val effectiveUnapply = Seq(unapply, unapplySeq).filter(_ != NoSymbol).headOption match {
      case None => c.abort(c.enclosingPosition, "No unapply or unapplySeq function found")
      case Some(s) => s.asMethod
    }

    val unapplyReturnTypes: Option[List[Type]] = effectiveUnapply.returnType match {
      case TypeRef(_, _, Nil) => {
        c.abort(c.enclosingPosition, s"Unapply of ${companionSymbol} has no parameters. Are you using an empty case class?")
        None
      }
      case TypeRef(_, _, args) =>
        args.head match {
          case t @ TypeRef(_, _, Nil) => Some(List(t))
          case t @ TypeRef(_, _, args) => {
            import c.universe.definitions.TupleClass
            if (!TupleClass.seq.exists(tupleSym => t.baseType(tupleSym) ne NoType)) Some(List(t))
            else if (t <:< typeOf[Product]) Some(args)
            else None
          }
          case _ => None
        }
      case _ => None
    }

//    println("Unapply return type:" + unapplyReturnTypes)

    val applies =
      companionType.declaration(stringToTermName("apply")) match {
        case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
        case s => s.asMethod.alternatives
      }

    // searches apply method corresponding to unapply
    val apply = applies.collectFirst {
      case (apply: MethodSymbol) if hasVarArgs && {
        val someApplyTypes = apply.paramss.headOption.map(_.map(_.asTerm.typeSignature))
        val someInitApply = someApplyTypes.map(_.init)
        val someApplyLast = someApplyTypes.map(_.last)
        val someInitUnapply = unapplyReturnTypes.map(_.init)
        val someUnapplyLast = unapplyReturnTypes.map(_.last)
        val initsMatch = someInitApply == someInitUnapply
        val lastMatch = (for {
          lastApply <- someApplyLast
          lastUnapply <- someUnapplyLast
        } yield lastApply <:< lastUnapply).getOrElse(false)
        initsMatch && lastMatch
      } => apply
      case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
    }

    val params = apply match {
      case Some(apply) => apply.paramss.head //verify there is a single parameter group
      case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
    }

//    println("apply found:" + params)

    final case class Implicit(paramName: Name, paramType: Type, neededImplicit: Tree, isRecursive: Boolean, tpe: Type)

    val createImplicit = { (name: Name, implType: c.universe.type#Type) =>
      val (isRecursive, tpe) = implType match {
//        case TypeRef(_, t, args) =>
//          val isRec = args.exists(_.typeSymbol == companioned)
//          // Option[_] needs special treatment because we need to use XXXOpt
//          val tp = if (implType.typeConstructor <:< typeOf[Option[_]].typeConstructor) args.head else implType
//          (isRec, tp)
        case TypeRef(_, t, _) =>
          (false, implType)
      }

      // builds M implicit from expected type
      val neededImplicitType = appliedType(matag.tpe.typeConstructor, tpe :: Nil)
      // infers implicit
//      println("need implicit: " + neededImplicitType)
      val neededImplicit = c.inferImplicitValue(neededImplicitType)
      Implicit(name, implType, neededImplicit, isRecursive, tpe)
    }

    val applyParamImplicits = params.map { param => createImplicit(param.name, param.typeSignature) }
    val effectiveInferredImplicits = if (hasVarArgs) {
      val varArgsImplicit = createImplicit(applyParamImplicits.last.paramName, unapplyReturnTypes.get.last)
      applyParamImplicits.init :+ varArgsImplicit
    } else applyParamImplicits

//    println("Effective implicits: " + effectiveInferredImplicits)

    // if any implicit is missing, abort
    val missingImplicits = effectiveInferredImplicits.collect { case Implicit(_, t, impl, rec, _) if (impl == EmptyTree && !rec) => t }
    if (missingImplicits.nonEmpty)
      c.abort(c.enclosingPosition, s"No implicit format for ${missingImplicits.mkString(", ")} available.")

    val helperMember = Select(This(tpnme.EMPTY), newTermName("lazyStuff"))
    def callHelper(target: Tree, methodName: String): Tree =
      Apply(Select(target, newTermName(methodName)), List(helperMember))
    def readerWriterHelper(methodName: String): List[Tree] =
      conditionalList(readerSelect, writerSelect).map(s => callHelper(s, methodName))

    var hasRec = false

    // combines all reads into CanBuild
    val canBuild = effectiveInferredImplicits.map {
      case Implicit(name, t, impl, rec, tpe) =>

        // inception of (EXMLPath \ name).read(impl)
        val pathTree = Apply(
          Select(xmlPath, newTermName(scala.reflect.NameTransformer.encode("\\"))),
          List(Literal(Constant(name.decoded)))
        )

//        if (!rec) {

          val callMethodName = if (tpe.typeConstructor <:< typeOf[Traversable[_]].typeConstructor)
            s"${methodName}List"
          else if (tpe.typeConstructor <:< typeOf[Map[_, _]].typeConstructor)
            s"${methodName}Map"
          else
            methodName

          Apply(
            Select(pathTree, newTermName(callMethodName)),
            List(impl)
          )
//        } else {
//          hasRec = true
//          if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
//            Apply(
//              Select(pathTree, newTermName(nullableMethodName)),
//              callHelper(Apply(xmlPath, Nil), lazyMethodName) :: Nil
//            )
//          else {
//            Apply(
//              Select(pathTree, newTermName(lazyMethodName)),
//              if (tpe.typeConstructor <:< typeOf[List[_]].typeConstructor)
//                readerWriterHelper("list")
//              else if (tpe.typeConstructor <:< typeOf[Set[_]].typeConstructor)
//                readerWriterHelper("set")
//              else if (tpe.typeConstructor <:< typeOf[Seq[_]].typeConstructor)
//                readerWriterHelper("seq")
//              else if (tpe.typeConstructor <:< typeOf[Map[_, _]].typeConstructor)
//                readerWriterHelper("map")
//              else List(helperMember)
//            )
//          }
//        }
    }.reduceLeft { (acc, r) =>
      Apply(
        Select(acc, newTermName("and")),
        List(r)
      )
    }

    // builds the final M[A] using apply method
    //val applyMethod = Ident( companionSymbol )

    val applyBody = {
      val body = params.foldLeft(List[Tree]())((l, e) =>
        l :+ Ident(newTermName(e.name.encoded))
      )
      if (hasVarArgs)
        body.init :+ Typed(body.last, Ident(tpnme.WILDCARD_STAR))
      else body
    }
    val applyMethod =
      Function(
        params.foldLeft(List[ValDef]())((l, e) =>
          l :+ ValDef(Modifiers(PARAM), newTermName(e.name.encoded), TypeTree(), EmptyTree)
        ),
        Apply(
          Select(Ident(companionSymbol), newTermName("apply")),
          applyBody
        )
      )

    val unapplyMethod = Apply(
      unliftIdent,
      List(
        Select(Ident(companionSymbol), effectiveUnapply.name)
      )
    )

    // if case class has one single field, needs to use inmap instead of canbuild.apply
    val method = if (params.length > 1) "apply" else mapLikeMethod
    val innerTree = Apply(
      Select(canBuild, newTermName(method)),
      conditionalList(applyMethod, unapplyMethod)
    )

    val className = Literal(Constant(companioned.name.decoded))
    val selector = methodName match {
      case "format" => Apply(formatterSelect, List(className))
      case "read"   => Apply(readerSelect, List(className))
      case "write"  => Apply(writerSelect, List(className))
    }

    val finalTree = Apply(selector, List(innerTree))

//    println("finalTree: " + finalTree)

    val importFunctionalSyntax = Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1)))
    val block = Block(
      List(importFunctionalSyntax),
      finalTree
    )

//    println("block: " + block)

    c.Expr[M[A]](block)
  }
}
