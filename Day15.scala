package Day15
import scala.io.Source
object Day15 extends App{
	def sumAll(list: List[(Int,List[Int])]) = 
		list.map( {case (quantity,innerList) => innerList.map(_*quantity)}).transpose.map(_.sum.max(0))
	val input = Source.fromFile("1.txt").getLines.map(_.split(",")).map(_.map(_.split(" ").last)).map(_.toList.map(_.toInt)).toList
	val input1 = input.map(_.slice(0,4))
	val input2 = input
	val combin = List.fill(input1.size)({0 to 100}).flatten.combinations(input1.size).filter(_.sum==100).toList
	
	val ingredient = combin.flatMap(_.permutations)
	val part1 = ingredient.map(_.zip(input1)).map(sumAll).map(_.product).max
	val part2 = ingredient.map(_.zip(input2)).map(sumAll).filter(_(4)==500).map(_.take(4).product).max
	println(part1)
	println(part2)
}
