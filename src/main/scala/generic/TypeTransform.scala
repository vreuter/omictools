package omictools
package generic


/**
 * Implicit type transformations.
 *
 * @author Vince Reuter
 */
object TypeTransform {
  
  import cats.{ Alternative, Monad }
  import cats.instances.either._
  
  /**
   * Separate a collection of {@code Either}s into a pair of collections of unwrapped elements.
   *
   * {@code Either} is an effective type for working in error-prone contexs, but the resulting values can be 
   * difficult to use. Often, the values that are of interest for downstream use are those that are wrapped by 
   * the {@code Either} subtype instances. This function aims to facilitate ease of use in that regard, providing 
   * implicit separation of the collection of {@code Either}s into a pair of colllections. The first collection 
   * in the resulting pair contains each value previously wrapped in a {@code Left}, and the second collection 
   * contains each value previously wrapped in a {@code Right}.
   *
   * @tparam L The type wrapped in a {@code Left}
   * @tparam R The type wrapped in a {@code Right}
   * @tparam F The collection type, in which each element is a {@code Either[L, R]}
   * @param fabs The collection of {@code Either}s to separate
   * @param AF The implicit {@code Alternative} instance for the collection type {@code F}
   * @param FM The implicit {@code Monad} instance for the collection type {@code F}
   * @return Pair of collections in which first component is the unwrapped {@code L}s and the second component is 
   *    the unwrapped {@code R}s
   */
  implicit def parseEithers[L, R, F[_]](fabs: F[Either[L, R]])(
    implicit AF: Alternative[F], FM: Monad[F]): (F[L], F[R]) = AF.separate(fabs)

}
