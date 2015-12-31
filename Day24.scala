package Day24
object Day24 extends App{
	def existsGroup(packages: List[Long],weight: Long, combSize: Int, restGroupSize: Int):Boolean = 
		packages.combinations(combSize).filter(_.sum==weight).toList match{
		case _ if restGroupSize==1 => packages.sum==weight
		case List() if combSize < packages.size => existsGroup(packages,weight,combSize+1,restGroupSize)
		case List() => false
		case successGroup =>successGroup.exists(g => existsGroup(packages.diff(g),weight,1,restGroupSize-1))
		}
	def choose(packages: List[Long],weight: Long, combSize: Int, restGroupSize: Int):Long = 
		packages.combinations(combSize).filter(p => p.sum==weight && existsGroup(packages.diff(p),weight,combSize,restGroupSize)).toList match{
		case List() => choose(packages,weight,combSize+1,restGroupSize)
		case group => group.sortBy(_.product).head.product
	}
	val input = scala.io.Source.fromFile("1.txt").getLines.map(_.toLong).toList
	val weight1 = input.sum/3
	val weight2 = input.sum/4
	val part1 = choose(input,weight1,1,2)
	println(part1)
	val part2 = choose(input,weight2,1,3)
	println(part2)
}
