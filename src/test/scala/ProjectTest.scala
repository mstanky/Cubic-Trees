import org.scalatest.FunSpec
import org.scalatest.Matchers
import Root._
import TreeGenerator._

class ProjectTest extends FunSpec with Matchers{
    describe("The TreeReader object"){
      it("can tell whether there is another tree to be tested."){
          ???
      }
    }
    describe("The DatWriter boject"){
          ???
    }
    describe("The Root object"){
        describe("can created a rooted adjacency matrix from a regular one that"){
            val rootedAdjMat = makeRootedMatrix(treeOne)
            it("has one degree-3 node."){
                assert( rootedAdjMat.filter( _.count(_ == 1) == 3 ).size == 1 )
            }
        }
    }
}
