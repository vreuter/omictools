package omictools
package generic


object CollectionTools {
  import scala.collection.GenTraversableOnce
  import cats.Monoid
  // TODO: implement; need Applicative?
  def cleanParse[A, L, R, F[_] <: TraversableOnce[_]](as: F[A])(f: A => Either[L, R])(implicit ev: Monoid[F[_]]): Either[F[L], F[R]] = ???
}
