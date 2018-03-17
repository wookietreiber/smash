package smash
package native

import scalanative.native._

@link("history")
@link("readline")
@extern
object readline {
  def readline(prompt: CString): CString = extern

  def add_history(line: CString): Unit = extern
  def read_history(file: CString): CInt = extern
  def write_history(file: CString): CInt = extern
}
