import Caterpillar._

object Main extends App {
	for(lab <- labels)
		println(lab.deep.mkString(","))
}