import scala.util.Random

println(0x3405 & 0x0f00 >> 8)
println(0x3405 & 0x00ff)
println(0xff.toByte)
print(Byte.MaxValue)

val b1 = 0xaa.toByte
val b2 = 0xbb

println(b1 & 0xff)
println(b2)
println((0xff - b1))


println(((b1 + b2)).toByte)

val i = -17
println((0x01.toByte & 0x1))
println(0xaa.toByte.toBinaryString)

println(0x123)

println(0xb123.toShort & 0x0fff)

println(0x123 + 0x5)
println((0x123 + 0x5).toShort)

val rnd = new scala.util.Random(System.currentTimeMillis())


0 + rnd.nextInt( (255 - 0) + 1 )
0 + rnd.nextInt( (255 - 0) + 1 )
0 + rnd.nextInt( (255 - 0) + 1 )
0 + rnd.nextInt( (255 - 0) + 1 )
