package smash

import java.io.File

final case class Config(file: Option[File] = None)

object smash extends App {
  val parser = new scopt.OptionParser[Config](BuildInfo.name) {
    head(BuildInfo.name, BuildInfo.version)

    arg[File]("file")
      .optional()
      .action((x, c) => c.copy(file = Some(x)))
      .text("read commands from file, defaults to STDIN")
      .validate(x =>
        if (x.exists) {
          success
        } else {
          failure(s"input file $x does not exist")
      })

    note("\nOptions:\n")

    help("help").text("prints this usage text")

    version("version")

    note("")
  }

  parser.parse(args, Config()) match {
    case Some(config) =>
      val status = config.file match {
        case None =>
          Interpreter.interactive.run()

        case Some(file) =>
          val source = scala.io.Source.fromFile(file)
          try {
            Interpreter.noninteractive.run(source)
          } finally {
            source.close()
          }
      }

      sys exit status

    case None =>
  }
}
