object Caterpillar {
    def permutations: Set[Array[Int]] = {
    	var out = Set.empty[Array[Int]]
    	for(i <- 1 to 10){
    		for(j <- 1 to 10 if j != i){
    			for(k <- 1 to 10 if k != j){
    				for(l <- 1 to 10 if l != k){
    					if(Set(i,j,k,l).size == 4 && (i < 5 && l < 5)){
    						if(j < 5 && k > 4)
	    						out += Array(i,11,j,k,l)
	    					if(k < 5 && j > 4)
    							out += Array(i,j,k,11,l)
    					}
    				}
    			}
    		}
    	}
    	out
    }
    def parse(total: Set[Array[Int]]): Set[Array[Int]] = {
    	var out = Set.empty[Array[Int]]
    	for(i <- total){
    		val diffs = Set(i(0)-i(1), i(1)-i(2), i(2)-i(3),i(3)-i(4)).map(x => Math.abs(x))
    		if(diffs.size == 4) out += i
    	}
    	out
    }
    def createLabel(diam: Array[Int]): Array[Array[Int]] = {
    	val label = Array.fill(7,2)(-1)
    	for(num <- 1 to 5){
    		label(0)(num) = diam(num-1)
    	}
    	label
    }
    def genLabels(diams: Set[Array[Int]]): Set[Array[Array[Int]]] = {
    	???
    }
    val diameters = parse(permutations)
    val labels = genLabels(diameters)
}
