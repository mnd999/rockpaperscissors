import java.net.URLEncoder
import java.security.MessageDigest
import java.util
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, _}
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

object App {


  case class Wtf(choice: String)
  case class Wtf2(choice: String, firstPlayer: String)

  def link(choice: String) = {
    s"http://localhost:8080/second?firstChose=${URLEncoder.encode(encrypt(choice),"utf-8")}"
  }

  def key() = {
    val key = "donttell".getBytes("UTF-8");
    val sha = MessageDigest.getInstance("SHA-1");
    val key2 = sha.digest(key);
    util.Arrays.copyOf(key2, 16); // use only first 128 bit
  }

  def encrypt(choice: String) = {
    val secretKeySpec = new SecretKeySpec(key(), "AES")
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec)
    new String(Base64.getEncoder.encode(cipher.doFinal((choice + "_" + Math.random()).getBytes())), "utf-8")
  }


  def decrypt(cryptotext: String) = {
    val secretKeySpec = new SecretKeySpec(key(), "AES")
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, secretKeySpec)
    val unbased = Base64.getDecoder.decode(cryptotext.getBytes)
    new String(cipher.doFinal(unbased)).takeWhile(x => x != '_')
  }

  def pickWinner(p1: String, p2: String) = (p1, p2)  match {
    case (x,y) if x == y  => "Draw"
    case ("rock", "paper") => "P2 wins"
    case ("rock", "scissors") => "P1 wins"
    case ("paper", "rock") => "P1 wins"
    case ("paper", "scissors") => "P2 wins"
    case ("scissors", "rock") => "P2 wins"
    case ("scissors", "paper") => "P1 wins"
    case _ => "WTF?!"
  }

  def form(hidden: Option[String]) = {

    val h = hidden.map(x => s"""<input type="hidden" name="firstPlayer" value="$x"/>""").getOrElse("")
    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,

      s"""
                <h1>Rock Paper Scissors</h1>

                <form method=post> <input type="submit" name="choice" value="rock" />$h </form>
                <form method=post> <input type="submit" name="choice" value="paper" />$h </form>
                <form method=post> <input type="submit" name="choice" value="scissors" />$h </form>

    """"))
    }

  def main(args: Array[String]) {

      implicit val system = ActorSystem("my-system")
      implicit val materializer = ActorMaterializer()
      // needed for the future flatMap/onComplete in the end
      implicit val executionContext = system.dispatcher

      val route =
        path("first") {
          get {
            form(None)
          } ~
          post {
            decodeRequest {
              formFields(('choice)).as(Wtf) { c =>
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                  s"""
                Give the other player this link:

                <a href="${link(c.choice)}">
                     ${link(c.choice)}
                </a>

                </a>

                """))}
              }
          }
        } ~
      path("second") {
        get {
          parameters(('firstChose.as[String]))
            .as(Wtf) { wtf =>
              form(Some(wtf.choice))
            }
        } ~
        post {
          decodeRequest {
            formFields(('choice, 'firstPlayer)).as(Wtf2) { c =>
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                s"""
<pre>
                    game is complete because both players have gone

                    first player : ${decrypt(c.firstPlayer)}

                    second player : ${c.choice}

                    ${pickWinner(decrypt(c.firstPlayer), c.choice)}
</pre>
                 """.stripMargin))
            }
          }
        }
      }

      val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

      println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
      StdIn.readLine() // let it run until user presses return
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done
    }

}