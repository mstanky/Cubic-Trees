import org.scalatest.FunSpec
import org.scalatest.Matchers

class ProjectTest extends FunSpec with Matchers{
    describe("The TreeReader object"){
        it("can tell whether there is another tree to be tested."){
            ???
        }
    }
    describe("The DatWriter boject"){
        it("can convert an adjacency matrix to an adjacency list."){
            val treeOne = Array(Array(0,1,0,0,0,0,0,0,0,0,0,0),
                          Array(1,0,1,0,0,0,1,0,0,0,0,0),
                          Array(0,1,0,1,0,0,0,1,0,0,0,0),
                          Array(0,0,1,0,1,0,0,0,1,0,0,0),
                          Array(0,0,0,1,0,1,0,0,0,1,0,0),
                          Array(0,0,0,0,1,0,0,0,0,0,0,0),
                          Array(0,1,0,0,0,0,0,0,0,0,0,0),
                          Array(0,0,1,0,0,0,0,0,0,0,1,1),
                          Array(0,0,0,1,0,0,0,0,0,0,0,0),
                          Array(0,0,0,0,1,0,0,0,0,0,0,0),
                          Array(0,0,0,0,0,0,0,1,0,0,0,0),
                          Array(0,0,0,0,0,0,0,1,0,0,0,0))
            val out = 
            assert(DatWriter.toAdjList(treeOne) == out)
        }
    }
}