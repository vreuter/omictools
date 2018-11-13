package omictools


/**
 * Tools for working with organism designation, primarily as {@code Genus.species}.
 *
 * @author Vince Reuter
 */
object OrganismTools {

  /** Genus + species representation of an organism. */
  trait Organism {
    def genus: String
    def species: String
    override def toString: String = s"$genus $species"
  }

  /** "Mouse" */
  final case object MusMusculus extends Organism {
    def genus: String = "Mus"
    def species: String = "musculus"
  }

  /** "Human" */
  final case object HomoSapiens extends Organism {
    def genus: String = "Homo"
    def species: String = "sapiens"
  }

  /** "Fly" */
  final case object DrosophilaMelanogaster extends Organism {
    def genus: String = "Drosophila"
    def species: String = "melanogaster"
  }

  /**
   * Syntactic convenience and implicits for working with {@code Organism}.
   *
   * @author Vince Reuter
   */
  object Organism {
    import cats.{ Eq, Show, Order }
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.eq._
    
    /** Create new organism instance from genus and species name. */
    def apply(g: String, s: String): Organism = new Organism { def genus = g; def species = s }
    
    /**
     * From a provided delimiter, create a new {@code Show} instance form organisms.
     *
     * @param sep Delimiter to place between genus and species.
     * @return New instance with which to show organism text, by placing delimiter between genus and species name
     */
    def makeShow: String => Show[Organism] = sep => new Show[Organism] {
      def show(o: Organism): String = s"${o.genus}${sep}${o.species}"
    }
    
    /** The text format required by several {@code R} packages for organism specification is {@code <Genus>.<species>} */
    implicit val showOrganism: Show[Organism] = makeShow(".")

    /** Order organisms lexicographically, first by genus then within genus by species. */
    implicit val ordOrganism: Order[Organism] = new Order[Organism] {
      def compare(a: Organism, b: Organism): Int = {
        val genusCmp = a.genus compare b.genus
        if (genusCmp =!= 0) genusCmp else (a.species compare b.species)
      }
    }

    /** Derive {@code Eq} from {@code Order}. */
    implicit val eqOrganism: Eq[Organism] = 
      new Eq[Organism] { def eqv(a: Organism, b: Organism): Boolean = 0 === ordOrganism.compare(a, b) }
  
  }

}
