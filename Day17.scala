package Day17
object Day17 extends App{
	def choose(l: List[Int],left: Int):Int = {
		if (left==0) 
			return 1
		if (l.size==0 && left != 0)
			return 0
		return choose(l.tail,left-l.head)+choose(l.tail,left)
	}
	val input = scala.io.Source.fromFile("1.txt").getLines.toList.map(_.toInt)
	val part1 = choose(input,150)
	println(part1)
	//Part2, copy from reddit
	val part2 = Range(0, input.length + 1)
				.map(input
				.zipWithIndex
				.combinations(_)
				.count(_.map(_._1).sum == 150))
				.find(_ > 0)
	println(part2.get)
}
