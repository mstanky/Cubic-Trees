import Root._
object IsomorphismTester {
    private def pruneTree(adjMatrix: Array[Array[Int]]): Array[Array[Int]] = {
        val out = adjMatrix.map(_.clone).clone
        val targetIndexes = adjMatrix.zipWithIndex.filter(_._1.count(_ == 1) == 1).map(_._2)
        val size = adjMatrix.size
        for(num <- 0 until size){
            if(targetIndexes.contains(num)) out(num) = Array.fill(out.size)(0)
            else{
               for(i <- targetIndexes)
                   out(num)(i) = 0
            }
        }
        out
    }
    private def generateCubicLabels(adjMatrix: Array[Array[Int]]): Array[Int] = {//Inefficient, quadratic
        val cubicLabels = Array.fill(adjMatrix.size)(0)
        for(line <- adjMatrix.zipWithIndex){ //O(n)
            val neighbors = line._1.zipWithIndex.filter(_._1 == 1).map(x=>x._2) //O(n)
            if(neighbors.size > 1) //O(1)
                for(i<-neighbors) //O(1)
                    if(adjMatrix(i).count(_ == 1) == 3) cubicLabels(line._2) += 1 //O(n)
        }
        cubicLabels
    }
    def longestItem(listSet: Set[List[Int]]): List[List[Int]] ={
        val out = listSet.toList.sortWith(_.length > _.length)
        val longest = out.head.size
        out.filter(_.length == longest)
    }
    def generateSequences(adjMatrix: Array[Array[Int]]): Set[List[Int]] = {
        var out = Set.empty[List[Int]]
        val cubicLabels = generateCubicLabels(adjMatrix)
        def findPaths(adjMatrix: Array[Array[Int]], path: List[Int], seq: List[Int]): Unit = {
            if(adjMatrix(path.head).count(x => x == 1) == 0) out += seq //path
            else{
                for(neigh <- adjMatrix(path.head).zipWithIndex.filter( x => x._1 == 1).map(x => x._2)){
                    val nextNode = neigh
                    val newPath = nextNode +: path
                    val newSeq = cubicLabels(nextNode) +: seq
                    val newAdjMat = adjMatrix.map(_.clone).clone
                    newAdjMat(nextNode)(path.head) = 0
                    findPaths(newAdjMat,newPath, newSeq)
                }
            }
        }
        val prunedTree = pruneTree(adjMatrix)
        val startNodes: Set[Int] = {
            var out = Set.empty[Int]
            for(line <- prunedTree.zipWithIndex){
                if(line._1.count(_ == 1) == 1) out += line._2
            }
            out
        }
        for(node <- startNodes)
            findPaths(prunedTree, List(node), List(cubicLabels(node)))
        out//.toList.sortWith(_.length > _.length).head
        //POSSIBLE BUG - ASSUMES THERE WILL BE ONLY ONE OF THIS LENGTH
    }

    def areIsomorphic(tree1: Array[Array[Int]], tree2: Array[Array[Int]]): Boolean = {
        ahuTreeIsomorphism(tree1,tree2)
    }
}