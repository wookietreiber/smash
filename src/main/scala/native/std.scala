package smash
package native

import scalanative.native._

/** Contains native functions that are not yet in a released scala-native.
  *
  * @groupname contribute functions I should contribute
  * @groupname unreleased functions that are not released yet
  */
@extern
object std {

  /** @group contribute */
  def execvp(file: CString, args: Ptr[CString]): CInt = extern

  /** @group contribute */
  def get_current_dir_name(): CString = extern

  /** @group contribute */
  def mkstemp(template: CString): CInt = extern

  /** @group contribute */
  def wait(wstatus: Ptr[CInt]): CInt = extern

  /** @group contribute */
  def waitpid(pid: CInt, wstatus: Ptr[CInt], options: CInt): CInt = extern

}
