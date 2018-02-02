import IsomorphismTester._
import scala.collection.mutable.ArrayBuffer
object TreeGenerator{
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

    val treeTwo = Array(Array(0,1,0,0,0,0,0,0,0,0,0,0),
                        Array(1,0,1,0,0,0,0,1,0,0,0,0),
                        Array(0,1,0,1,0,0,0,0,1,0,0,0),
                        Array(0,0,1,0,1,0,0,0,0,1,0,0),
                        Array(0,0,0,1,0,1,0,0,0,0,1,0),
                        Array(0,0,0,0,1,0,1,0,0,0,0,1),
                        Array(0,0,0,0,0,1,0,0,0,0,0,0),
                        Array(0,1,0,0,0,0,0,0,0,0,0,0),
                        Array(0,0,1,0,0,0,0,0,0,0,0,0),
                        Array(0,0,0,1,0,0,0,0,0,0,0,0),
                        Array(0,0,0,0,1,0,0,0,0,0,0,0),
                        Array(0,0,0,0,0,1,0,0,0,0,0,0))

    def growTree(adjMatrix: Array[Array[Int]]): List[Array[Array[Int]]] = {
        val blankArray = Array.fill(adjMatrix.size + 2)(0)
        var out = ArrayBuffer(blankArray,blankArray.clone)
        for(line <- adjMatrix.reverse){
            var newLine = line.toBuffer :+ 0 :+ 0
            out = newLine.toArray +: out
        }
        val newMat = out.toArray

        val leafNodes = newMat.zipWithIndex.filter(_._1.count(_ == 1) == 1).map(_._2)
        var treeSet = List.empty[Array[Array[Int]]]
        for(leaf <- leafNodes){
            val lastIndex = newMat.size - 1
            val secondLastIndex = lastIndex - 1
            val tree = newMat.map(_.clone).clone
            tree(leaf)(lastIndex) = 1
            tree(leaf)(secondLastIndex) = 1
            tree(lastIndex)(leaf) = 1
            tree(secondLastIndex)(leaf) = 1
            treeSet +:= tree
        }
        treeSet
    }
    private def testTrees(tree: Array[Array[Int]], rest: List[Array[Array[Int]]]): List[Array[Array[Int]]] = {
        if(rest.isEmpty) List.empty
        else if(areIsomorphic(tree,rest.head)) testTrees(tree,rest.tail)
        else rest.head :: testTrees(tree, rest.tail)
    }
    def parseTrees(treeList: List[Array[Array[Int]]]): List[Array[Array[Int]]] = {
        if(treeList.isEmpty) List.empty
        else {
             treeList.head :: parseTrees(testTrees(treeList.head, treeList.tail))
        }
    }
}