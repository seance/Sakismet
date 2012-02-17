package object sakismet {
  
  import dispatch.{HttpExecutor, Http}
  
  val UserAgent = "Sakismet/1.0.0"
  val AkismetBaseHost = "rest.akismet.com"
  val AkismetKeyedHost = "%s.rest.akismet.com"
  val AkismetDebugHeader = "x-akismet-debug-help"
  val AkismetBasePath = "1.1"
  val AkismetPort = 80
  
  type N = None.type
  type S = Some.type
  
  implicit def http = new Http
  
  object Sakismet {
    def apply[H <: HttpExecutor](key: String, blog: String)(implicit http: H) =
      new Builder[H, {
	    type UserIp = N
	    type Referrer = N
	    type Permalink = N
	    type CommentType = N
	    type CommentAuthor = N
	    type CommentAuthorEmail = N
	    type CommentAuthorUrl = N
	    type CommentContent = N
	  }](key, new Invoker[H](http)) {
	    override val params = Map("blog" -> blog, "user_agent" -> UserAgent)
	  }
  }
  
  class Builder[H <: HttpExecutor, Xs] private[sakismet](key: String, val inv: Invoker[H]) {
    self: Builder[H,_] =>
    type selfXs = Xs
    val params = Map[String, String]()
    
    def user_ip(v: String)(implicit ev: Xs <:< { type UserIp <: N }) =
      new Builder[H, selfXs { type UserIp = S }](key, inv) {
      override val params = self.params + ("user_ip" -> v)
    }
    
    def referrer(v: String)(implicit ev: Xs <:< { type Referrer <: N }) =
      new Builder[H, selfXs { type Referrer = S }](key, inv) {
      override val params = self.params + ("referrer" -> v)
    }
    
    def permalink(v: String)(implicit ev: Xs <:< { type Permalink <: N }) =
      new Builder[H, selfXs { type Permalink = S }](key, inv) {
      override val params = self.params + ("permalink" -> v)
    }
    
    def comment_type(v: String)(implicit ev: Xs <:< { type CommentType <: N }) =
      new Builder[H, selfXs { type CommentType = S }](key, inv) {
      override val params = self.params + ("comment_type" -> v)
    }
    
    def comment_author(v: String)(implicit ev: Xs <:< { type CommentAuthor <: N }) =
      new Builder[H, selfXs { type CommentAuthor = S }](key, inv) {
      override val params = self.params + ("comment_author" -> v)
    }
    
    def comment_author_email(v: String)(implicit ev: Xs <:< { type CommentAuthorEmail <: N }) =
      new Builder[H, selfXs { type CommentAuthorEmail = S }](key, inv) {
      override val params = self.params + ("comment_author_email" -> v)
    }
    
    def comment_author_url(v: String)(implicit ev: Xs <:< { type CommentAuthorUrl <: N }) =
      new Builder[H, selfXs { type CommentAuthorUrl = S }](key, inv) {
      override val params = self.params + ("comment_author_url" -> v)
    }
    
    def comment_content(v: String)(implicit ev: Xs <:< { type CommentContent <: N }) =
      new Builder[H, selfXs { type CommentContent = S }](key, inv) {
      override val params = self.params + ("comment_content" -> v)
    }

    type AkismetOk = { type UserIp <: S; type Referrer <: S }
    
    def verify_key() = inv.invoke(None, "verify-key", params + ("key" -> key)) {
      case "valid" => true
      case "invalid" => false
    }
    
    def comment_check()(implicit ev: Xs <:< AkismetOk) = inv.invoke(key, "comment-check", params) {
      case "true" => true
      case "false" => false
    }
    
    def submit_spam()(implicit ev: Xs <:< AkismetOk) = inv.invoke(key, "submit-spam", params) {
      case "Thanks for making the web a better place." => ()
    }
    
    def submit_ham()(implicit ev: Xs <:< AkismetOk) = inv.invoke(key, "submit-ham", params) {
      case "Thanks for making the web a better place." => ()
    }
    
    def shutdown = inv.http.shutdown
  }
  
  class Invoker[H <: HttpExecutor] private[sakismet](val http: H) {
    import dispatch._
    type PF[R] = PartialFunction[String, R]
    
    private def baseUrl(key: Option[String]) = :/(key match {
      case Some(key) => AkismetKeyedHost format key
      case None => AkismetBaseHost
    }, AkismetPort) / AkismetBasePath
    
    def invoke[R](key: Option[String], op: String, params: Map[String, String])(f: PF[R]) =
      http(baseUrl(key) / op << params >:+ { (headers, req) => req.as_str ~> f.orElse {
        case "invalid" => throw new InvalidRequest(headers(AkismetDebugHeader).mkString)
        case _ => throw new UnknownResponse(headers(AkismetDebugHeader).mkString)
      }})
  }
  
  private implicit def str2opt(s: String): Option[String] = Some(s)
  
  class InvalidRequest(x: String) extends RuntimeException(x)
  class UnknownResponse(x: String) extends RuntimeException(x)
}