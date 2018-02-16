object DatWriter{
    def toAdjList(adjMat: Array[Array[Int]]): Array[List[Int]] = {
        val out = new Array[List[Int]](adjMat.size)
        for(num <- 0 until out.size){
            out(num) = adjMat(num).zipWithIndex.filter(_._1==1).map(_._2).toList
        }
        out
    }

    def bipartition(adjList: Array[List[Int]]): Set[Int] = {
		val seen = Array.fill(adjList.size)(0)
		var white = Set.empty[Int]
		def assignNode(node: Int, flag: Boolean): Unit = {
			if(seen.count(_==1) == seen.size) return
			else if(seen(node) == 1) return
			else{
				seen(node) = 1
				if(flag)
					white += node
				for(neigh <- adjList(node)){
					assignNode(neigh,!flag)
				}
			}
		}
		assignNode(0,true)
		white
    }

    def writeOut(adjList: Array[List[Int]], whiteNodes: Set[Int]): String = {
        val size = adjList.size
        val name = s"""name="$size node cubic tree";"""
        val n = s"n=$size;"
		var tree = "tree=["
        for(num <- whiteNodes; neigh <- adjList(num)){
			tree += s"[$num $neigh]"
        }
		tree += "];"
		List(name, n, tree).mkString("\n")
    }
}