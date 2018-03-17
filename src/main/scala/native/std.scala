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
  def get_current_dir_name(): CString = extern

}
