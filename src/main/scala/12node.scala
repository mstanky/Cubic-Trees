object TwelveCaterpillar {
	val diamNums = (0 to 12).toList
    val combs = diamNums.combinations(7).toList.map(13 :: _.reverse).map(_.reverse.toArray).filter(_.count(_ > 6) == 4)
    def buildDiams(combination: Array[Int]): Set[Array[Int]] = {
    	//assume sorted from combs
    	val smallPerms = combination.slice(0,4).permutations
    	val largeSide = combination.slice(4,combination.size)
    	var out = Set.empty[Array[Int]]
    	while(smallPerms.hasNext){
    		val smallOrder = smallPerms.next
    		if(!smallOrder.contains(0) && largeSide.contains(13)){
	    		val largePerms = largeSide.permutations
	    		while(largePerms.hasNext){
	    			val largeOrder = largePerms.next
	    			if(largeOrder(2) == 13){
		    			var diam = List.empty[Int]
		    			for(num <- 3 to 0 by -1)
		    				diam = largeOrder(num) :: smallOrder(num) :: diam
		    			if(!out.contains(diam.reverse.toArray)) out += diam.toArray
	    			}
	    		}
    		}
    	}
    	out
    }
    def isAlphaDiam(diam: Array[Int]): Boolean = {
    	val diffs = Array.fill(13)(0)
    	for(num <- 0 until diam.size - 1)
    		diffs(Math.abs(diam(num) - diam(num+1))-1) += 1
    	diffs.count(_ > 1) == 0
    }
    def findRemainingNums(diam: Array[Int]): Set[Int] = {
    	(0 to 13).toSet -- diam.toSet
    }
    def buildTree(diam: Array[Int]): Array[Array[Int]] = {
    	val out = Array(diam, Array.fill(diam.size)(-1))
    	out
    }
    def generateTrees(diam:Array[Int]): Set[Array[Array[Int]]] = {
    	val remainder = findRemainingNums(diam).toArray.permutations
    	var out = Set.empty[Array[Array[Int]]]
    	while(remainder.hasNext){
    		val perm = remainder.next
    		val tree = buildTree(diam)
    		for(num <- 1 to 6)
    			tree(1)(num) = perm(num - 1)
    		if(!out.contains(Array(tree(0).reverse, tree(1).reverse)))
    			out += tree
    	}
    	out
    }
    def isAlphaTree(tree: Array[Array[Int]]): Boolean = {
    	val diffs = Array.fill(13)(0)
    	for(num <- 0 until tree(0).size -1)
    		diffs(Math.abs(tree(0)(num) - tree(0)(num+1))-1) += 1
    	for(num <- 1 to 6)
    		diffs(Math.abs(tree(0)(num) - tree(1)(num)) - 1) += 1
    	diffs.count(_ > 1) == 0 && tree(1)(2) < 6 && tree(1)(4) < 6 && tree(1)(6) < 6
    }
    def compliment(tree: Array[Array[Int]]): Array[Array[Int]] = {
      val out = tree.map(_.clone)
      for(num <- 0 until tree(0).size){
        out(0)(num) = Math.abs(13 - out(0)(num))
        out(1)(num) match {
          case -1 => out(1)(num) = -1
          case _ => out(1)(num) = Math.abs(13 - out(1)(num))
        }
      }
      out
    }
    val diams = combs.flatMap(buildDiams(_).toList).filter(isAlphaDiam(_)).flatMap(generateTrees(_).toList).filter(isAlphaTree(_))//.map(compliment(_))
}
