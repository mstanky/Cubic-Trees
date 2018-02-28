import org.scalatest.FunSpec
import org.scalatest.Matchers

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
        import Root._
        describe("can created a rooted adjacency matrix from a regular one that"){
            val rootedAdjMat = createdRootedMatrix(treeOne)
            it("has one degree-3 node."){
                assert( oneRooted.filter( _.count(_ == 1) == 3 ).size == 1 )
            }
        }
    }
}
