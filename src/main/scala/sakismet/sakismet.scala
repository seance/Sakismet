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
    self: Builder[H, _] =>
    type Ev[X] = Xs <:< X
    val params = Map[String, String]()
    
    private def builder[Xs](mapping: (String, String)) = new Builder[H, Xs](key, inv) {
      override val params = self.params + mapping
    }
    
    def user_ip[X <: { type UserIp <: N }: Ev](v: String) =
      builder[Xs { type UserIp = S }]("user_ip" -> v)
    
    def referrer[X <: { type Referrer <: N }: Ev](v: String) =
      builder[Xs { type Referrer = S }]("referrer" -> v)
        
    def permalink[X <: { type Permalink <: N }: Ev](v: String) =
      builder[Xs { type Permalink = S }]("permalink" -> v)
    
    def comment_type[X <: { type CommentType <: N }: Ev](v: String) =
      builder[Xs { type CommentType = S }]("comment_type" -> v)
    
    def comment_author[X <: { type CommentAuthor <: N }: Ev](v: String) =
      builder[Xs { type CommentAuthor = S }]("comment_author" -> v)
    
    def comment_author_email[X <: { type CommentAuthorEmail <: N }: Ev](v: String) =
      builder[Xs { type CommentAuthorEmail = S }]("comment_author_email" -> v)
    
    def comment_author_url[X <: { type CommentAuthorUrl <: N }: Ev](v: String) =
      builder[Xs { type CommentAuthorUrl = S }]("comment_author_url" -> v)
    
    def comment_content[X <: { type CommentContent <: N }: Ev](v: String) =
      builder[Xs { type CommentContent = S }]("comment_content" -> v)

    type AkismetOk = { type UserIp <: S; type Referrer <: S }
    
    /**
     * Verify that the given key and blog are recognized by Akismet.
     * 
     * @return {@code true} if key is valid, otherwise {@code false}
     */
    def verify_key() = inv.invoke(None, "verify-key", params + ("key" -> key)) {
      case "valid" => true
      case "invalid" => false
    }
    
    /**
     * Check whether Akismet thinks a given comment is spam.
     * 
     * @return {@code true} if comment is spam, otherwise {@code false}
     */
    def comment_check[X <: AkismetOk: Ev]() = inv.invoke(key, "comment-check", params) {
      case "true" => true
      case "false" => false
    }
    
    /**
     * Report false negative to Akismet.
     */
    def submit_spam[X <: AkismetOk: Ev]() = inv.invoke(key, "submit-spam", params) {
      case "Thanks for making the web a better place." => ()
    }
    
    /**
     * Report false positive to Akismet.
     */
    def submit_ham[X <: AkismetOk: Ev]() = inv.invoke(key, "submit-ham", params) {
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