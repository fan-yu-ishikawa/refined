package eu.timepit.refined.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class RefinedCompanion[FTP] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RefinedCompanion.impl
}

object RefinedCompanion {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val ftpType =
      c.macroApplication match {
        case Apply(Select(Apply(Select(New(AppliedTypeTree(_, List(tpe))), _), _), _), _) =>
          tpe.duplicate
      }

    val companion = annottees.head
    val newCompanion = companion.tree match {
      case q"$mods object $name { ..$body }" =>
        q"""
          $mods object $name {
            $body

            def fromInt(x: Int): Either[String, $ftpType] =
              _root_.eu.timepit.refined.api.RefType.applyRef[$ftpType](x)
          }
        """
    }
    c.Expr(newCompanion)
  }
}
