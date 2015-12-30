package Day23
object Day23 extends App{
	val input = scala.io.Source.fromFile("1.txt").getLines.map(_.replace(",","").split(" ")).zipWithIndex.map(_.swap).toMap
	val registers1 = scala.collection.mutable.Map("a"->0L,"b"->0L)
	val registers2 = scala.collection.mutable.Map("a"->1L,"b"->0L)
	def execute(registers: scala.collection.mutable.Map[String,Long], instructions:Map[Int,Array[String]]){
		def run(index: Int): scala.collection.mutable.Map[String,Long] = instructions.getOrElse(index,Array("end")) match {
			case Array("hlf",r) => registers.update(r,registers(r)/2);run(index+1)
			case Array("tpl",r) => registers.update(r,registers(r)*3); run(index+1)
			case Array("inc",r) => registers.update(r,registers(r)+1); run(index+1)
			case Array("jmp",offset) => run(index + offset.toInt)
			case Array("jie",r,offset) =>  if (registers(r)%2==0) run(index+offset.toInt) else run(index+1)
			case Array("jio",r,offset) => if (registers(r)==1) run(index+offset.toInt) else run(index+1)
			case _ => registers
		}
		println(run(0)("b"))
	}
	val part1 = execute(registers1,input)
	val part2 = execute(registers2,input)
}
