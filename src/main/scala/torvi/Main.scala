package torvi

import java.nio.file.{Paths, Files}

object Main extends App {
  if(args.length != 1) {
    println("Usage: /path/to/rom.ch8")
    sys.exit()
  }
  if(!Files.exists(Paths.get(args(0)))) {
    println("Invalid file specified")
    sys.exit()
  }

  println("TODO")
}
