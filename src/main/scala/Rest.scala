package org.shl.rest


object JsonMapping {

  import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
  import com.fasterxml.jackson.databind.ObjectMapper
  import com.fasterxml.jackson.module.scala.DefaultScalaModule

  val mapper = new ObjectMapper() with ScalaObjectMapper {
    registerModule(DefaultScalaModule)
  }
}

trait JsonMapping {
  val mapper = JsonMapping.mapper
}


object HTTP {
  import java.net.http.{HttpClient, HttpRequest, HttpResponse}
  import java.net.{URLEncoder, URI}
  import java.nio.charset.StandardCharsets

  def enc(v:String) = URLEncoder.encode(v, StandardCharsets.UTF_8.toString)

  def renderArgs(args:Map[String,Any]):String = args.map( { case (k,v) => s"${enc(k)}=${enc(v.toString)}" } ).mkString("&")

  def renderURL(url:String, args:Map[String,Any]):URI = URI.create(s"${url}?${renderArgs(args)}")

  def genRequest(url:String, args:Map[String,Any]):() => HttpRequest = () => HttpRequest.newBuilder().uri(renderURL(url, args)).build()

  val client = HttpClient.newHttpClient

  def apply(url:String, argSupplier:Map[String,Any]) = new AnyRef {
    private val request = genRequest(url, argSupplier)
    override def toString = request().toString
    def call() = client.send(request(), HttpResponse.BodyHandlers.ofString()).body
  }
}

object RestConsumer {
  implicit class FastCoverters(val a:Any) extends AnyVal {
    def asMap = a.asInstanceOf[Map[String,Any]]
    def asList = a.asInstanceOf[List[Map[String,Any]]]
  }

  implicit class FastAccessors(val a:Any) extends AnyVal {
    def get(x:Symbol) = a.asInstanceOf[Map[String,Any]].apply(x.name)
  }
}

abstract class RestConsumer extends JsonMapping with Function1[Map[String, Any],Map[String, Any]]{
  def url:String
  def defaultArgs: Map[String,Any]

  def apply(args: Map[String,Any] = defaultArgs):Map[String, Any] = {
    mapper.readValue[Map[String, Any]](HTTP(url, args).call)
  }
}
