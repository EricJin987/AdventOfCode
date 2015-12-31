package Day25
object Day25 extends App{
	val (row,column) = (2978,3083)
	def getCode(row: Int, column: Int, value: Long): Long = (row,column) match{
		case (2978,3083) => value
		case (1,c) => getCode(c+1,1,value*252533%33554393)
		case (r,c) => getCode(r-1,c+1,value*252533%33554393)
	}
	val part1 = getCode(1,1,20151125)
	println(part1)
}
