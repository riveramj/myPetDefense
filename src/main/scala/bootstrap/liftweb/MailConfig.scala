package bootstrap.liftweb

import javax.mail.internet.{MimeMessage, MimeMultipart}
import javax.mail.{Authenticator, PasswordAuthentication}
import net.liftweb.common._
import net.liftweb.util._

object MailConfig extends Loggable {

  def init = {
    if (Props.mode == Props.RunModes.Development || Props.mode == Props.RunModes.Test) {
      Mailer.devModeSend.default.set((m: MimeMessage) =>
        logger.info("Dev mode message:\n" + prettyPrintMime(m))
      )
    } else {
      Mailer.customProperties = Map(
        "mail.smtp.starttls.enable" -> "true",
        "mail.smtp.ssl.protocols"   -> "TLSv1.2"
      )

      (Props.get("mail.user"), Props.get("mail.password")) match {
        case (Full(username), Full(password)) =>
          Mailer.authenticator = Full(new Authenticator() {
            override def getPasswordAuthentication = new PasswordAuthentication(username, password)
          })
        case _ => throw new Exception("Username/password not supplied for Mailer.")
      }
    }
  }

  private def prettyPrintMime(mimeMessage: MimeMessage): String = {
    val headerBuilder = new StringBuilder
    val headers       = mimeMessage.getAllHeaderLines

    while (headers.hasMoreElements)
      headerBuilder ++= headers.nextElement.toString + "\n"

    def content(message: MimeMessage): String = {
      message.getContent match {
        case multipart: MimeMultipart => {
            for {
              index <- 0 until multipart.getCount
              bodyPart = multipart.getBodyPart(index)
            } yield {
              val partContentType = bodyPart.getDataHandler.getContentType
              val partContent     = bodyPart.getContent
              val expandedContent = {
                partContent match {
                  case partMimeMessage: MimeMessage =>
                    content(partMimeMessage)
                  case other =>
                    other.toString
                }
              }
              ("""
              |[%s]
              |%s
            """.stripMargin).format(partContentType, expandedContent)
            }
          } mkString "\n"
        case other =>
          other.toString
      }
    }

    ("""
       |%s
       |%s
     """.stripMargin).format(headerBuilder.toString, content(mimeMessage))
  }

}
