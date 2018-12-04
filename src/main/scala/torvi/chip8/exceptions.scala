package torvi.chip8

class RomTooLargeException(s:String) extends Exception(s){}

class StackUnderflowException(s:String) extends Exception(s){}

class StackOverflowException(s:String) extends Exception(s){}
