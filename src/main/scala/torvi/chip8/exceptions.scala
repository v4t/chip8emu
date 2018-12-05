package torvi.chip8

class InvalidOpCodeException(s:String) extends Exception(s){}

class RomTooLargeException(s:String) extends Exception(s){}

class StackUnderflowException(s:String) extends Exception(s){}

class StackOverflowException(s:String) extends Exception(s){}
