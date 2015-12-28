package Day21
object Day21 extends App{
	case class Player(hitPoints: Int, damage: Int, ar: Int){
		def arm(weapon: (Int,Int,Int), armor: (Int,Int,Int), ring1: (Int,Int,Int), ring2: (Int,Int,Int)) = {
			Player(hitPoints,
				damage+weapon._2+armor._2+ring1._2+ring2._2,
				ar+weapon._3+armor._3+ring1._3+ring2._3)
		}
		def defeat(boss: Player) = {
			val ma = (damage-boss.ar).max(1)
			val ba = (boss.damage-ar).max(1)
			ma*(hitPoints/ba+1) >= boss.hitPoints
		}
	}
	val weapons = Set((8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0))
	val armors = Set((0,0,0),(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,5))
	val rings = Set((0,0,0),(0,0,0),(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3))
	val me = Player(100,0,0)
	val boss = Player(109,8,2)
	val part1 = (for {
		weapon <- weapons
		armor <- armors
		ring1 <- rings
		ring2 <- rings-ring1
		if me.arm(weapon,armor,ring1,ring2).defeat(boss)
	} yield (weapon._1+armor._1+ring1._1+ring2._1)).min
	val part2 = (for {
		weapon <- weapons
		armor <- armors
		ring1 <- rings
		ring2 <- rings-ring1
		if !(me.arm(weapon,armor,ring1,ring2).defeat(boss))
	} yield (weapon._1+armor._1+ring1._1+ring2._1)).max
	println(part1)
	println(part2)
}
