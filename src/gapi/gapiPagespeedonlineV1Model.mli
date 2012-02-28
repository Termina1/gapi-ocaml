(* Warning! This file is generated. Modify at your own risk. *)

(** Data definition for Page Speed Online API (v1).
  
  For more information about this data model, see the
  {{:https://code.google.com/apis/pagespeedonline/v1/getting_started.html}API Documentation}.
  *)

module Result :
sig
  module VersionData :
  sig
    type t = {
      major : int;
      (** The major version number of the Page Speed SDK used to generate these results. *)
      minor : int;
      (** The minor version number of the Page Speed SDK used to generate these results. *)
      
    }
    
    val major : (t, int) GapiLens.t
    val minor : (t, int) GapiLens.t
    
    val empty : t
    
    val render : t -> GapiJson.json_data_model list
    
    val parse : t -> GapiJson.json_data_model -> t
    
  end
  
  module PageStatsData :
  sig
    type t = {
      cssResponseBytes : string;
      (** Number of uncompressed response bytes for CSS resources on the page. *)
      flashResponseBytes : string;
      (** Number of response bytes for flash resources on the page. *)
      htmlResponseBytes : string;
      (** Number of uncompressed response bytes for the main HTML document and all iframes on the page. *)
      imageResponseBytes : string;
      (** Number of response bytes for image resources on the page. *)
      javascriptResponseBytes : string;
      (** Number of uncompressed response bytes for JS resources on the page. *)
      numberCssResources : int;
      (** Number of CSS resources referenced by the page. *)
      numberHosts : int;
      (** Number of unique hosts referenced by the page. *)
      numberJsResources : int;
      (** Number of JavaScript resources referenced by the page. *)
      numberResources : int;
      (** Number of HTTP resources loaded by the page. *)
      numberStaticResources : int;
      (** Number of static (i.e. cacheable) resources on the page. *)
      otherResponseBytes : string;
      (** Number of response bytes for other resources on the page. *)
      textResponseBytes : string;
      (** Number of uncompressed response bytes for text resources not covered by other statistics (i.e non-HTML, non-script, non-CSS resources) on the page. *)
      totalRequestBytes : string;
      (** Total size of all request bytes sent by the page. *)
      
    }
    
    val cssResponseBytes : (t, string) GapiLens.t
    val flashResponseBytes : (t, string) GapiLens.t
    val htmlResponseBytes : (t, string) GapiLens.t
    val imageResponseBytes : (t, string) GapiLens.t
    val javascriptResponseBytes : (t, string) GapiLens.t
    val numberCssResources : (t, int) GapiLens.t
    val numberHosts : (t, int) GapiLens.t
    val numberJsResources : (t, int) GapiLens.t
    val numberResources : (t, int) GapiLens.t
    val numberStaticResources : (t, int) GapiLens.t
    val otherResponseBytes : (t, string) GapiLens.t
    val textResponseBytes : (t, string) GapiLens.t
    val totalRequestBytes : (t, string) GapiLens.t
    
    val empty : t
    
    val render : t -> GapiJson.json_data_model list
    
    val parse : t -> GapiJson.json_data_model -> t
    
  end
  
  module FormattedResultsData :
  sig
    module RuleResultsData :
    sig
      module UrlBlocksData :
      sig
        module UrlsData :
        sig
          module ResultData :
          sig
            module ArgsData :
            sig
              type t = {
                _type : string;
                (** Type of argument. One of URL, STRING_LITERAL, INT_LITERAL, BYTES, or DURATION. *)
                value : string;
                (** Argument value, as a localized string. *)
                
              }
              
              val _type : (t, string) GapiLens.t
              val value : (t, string) GapiLens.t
              
              val empty : t
              
              val render : t -> GapiJson.json_data_model list
              
              val parse : t -> GapiJson.json_data_model -> t
              
            end
            
            type t = {
              args : ArgsData.t list;
              (** List of arguments for the format string. *)
              format : string;
              (** A localized format string with $N placeholders, where N is the 1-indexed argument number, e.g. 'Minifying the resource at URL $1 can save $2 bytes'. *)
              
            }
            
            val args : (t, ArgsData.t list) GapiLens.t
            val format : (t, string) GapiLens.t
            
            val empty : t
            
            val render : t -> GapiJson.json_data_model list
            
            val parse : t -> GapiJson.json_data_model -> t
            
          end
          
          module DetailsData :
          sig
            module ArgsData :
            sig
              type t = {
                _type : string;
                (** Type of argument. One of URL, STRING_LITERAL, INT_LITERAL, BYTES, or DURATION. *)
                value : string;
                (** Argument value, as a localized string. *)
                
              }
              
              val _type : (t, string) GapiLens.t
              val value : (t, string) GapiLens.t
              
              val empty : t
              
              val render : t -> GapiJson.json_data_model list
              
              val parse : t -> GapiJson.json_data_model -> t
              
            end
            
            type t = {
              args : ArgsData.t list;
              (** List of arguments for the format string. *)
              format : string;
              (** A localized format string with $N placeholders, where N is the 1-indexed argument number, e.g. 'Unnecessary metadata for this resource adds an additional $1 bytes to its download size'. *)
              
            }
            
            val args : (t, ArgsData.t list) GapiLens.t
            val format : (t, string) GapiLens.t
            
            val empty : t
            
            val render : t -> GapiJson.json_data_model list
            
            val parse : t -> GapiJson.json_data_model -> t
            
          end
          
          type t = {
            details : DetailsData.t list;
            (** List of entries that provide additional details about a single URL. Optional. *)
            result : ResultData.t;
            (** A format string that gives information about the URL, and a list of arguments for that format string. *)
            
          }
          
          val details : (t, DetailsData.t list) GapiLens.t
          val result : (t, ResultData.t) GapiLens.t
          
          val empty : t
          
          val render : t -> GapiJson.json_data_model list
          
          val parse : t -> GapiJson.json_data_model -> t
          
        end
        
        module HeaderData :
        sig
          module ArgsData :
          sig
            type t = {
              _type : string;
              (** Type of argument. One of URL, STRING_LITERAL, INT_LITERAL, BYTES, or DURATION. *)
              value : string;
              (** Argument value, as a localized string. *)
              
            }
            
            val _type : (t, string) GapiLens.t
            val value : (t, string) GapiLens.t
            
            val empty : t
            
            val render : t -> GapiJson.json_data_model list
            
            val parse : t -> GapiJson.json_data_model -> t
            
          end
          
          type t = {
            args : ArgsData.t list;
            (** List of arguments for the format string. *)
            format : string;
            (** A localized format string with $N placeholders, where N is the 1-indexed argument number, e.g. 'Minifying the following $1 resources would save a total of $2 bytes'. *)
            
          }
          
          val args : (t, ArgsData.t list) GapiLens.t
          val format : (t, string) GapiLens.t
          
          val empty : t
          
          val render : t -> GapiJson.json_data_model list
          
          val parse : t -> GapiJson.json_data_model -> t
          
        end
        
        type t = {
          header : HeaderData.t;
          (** Heading to be displayed with the list of URLs. *)
          urls : UrlsData.t list;
          (** List of entries that provide information about URLs in the url block. Optional. *)
          
        }
        
        val header : (t, HeaderData.t) GapiLens.t
        val urls : (t, UrlsData.t list) GapiLens.t
        
        val empty : t
        
        val render : t -> GapiJson.json_data_model list
        
        val parse : t -> GapiJson.json_data_model -> t
        
      end
      
      type t = {
        localizedRuleName : string;
        (** Localized name of the rule, intended for presentation to a user. *)
        ruleImpact : float;
        (** The impact (unbounded floating point value) that implementing the suggestions for this rule would have on making the page faster. Impact is comparable between rules to determine which rule's suggestions would have a higher or lower impact on making a page faster. For instance, if enabling compression would save 1MB, while optimizing images would save 500kB, the enable compression rule would have 2x the impact of the image optimization rule, all other things being equal. *)
        ruleScore : int;
        (** The score (0-100) for this rule. The rule score indicates how well a page implements the recommendations for the given rule. For instance, if none of the compressible resources on a page are compressed, the rule score would be 0, while if all of the compressible resources on a page are compressed, the rule score would be 100. *)
        urlBlocks : UrlBlocksData.t list;
        (** List of blocks of URLs. Each block may contain a heading and a list of URLs. Each URL may optionally include additional details. *)
        
      }
      
      val localizedRuleName : (t, string) GapiLens.t
      val ruleImpact : (t, float) GapiLens.t
      val ruleScore : (t, int) GapiLens.t
      val urlBlocks : (t, UrlBlocksData.t list) GapiLens.t
      
      val empty : t
      
      val render : t -> GapiJson.json_data_model list
      
      val parse : t -> GapiJson.json_data_model -> t
      
    end
    
    type t = {
      locale : string;
      (** The locale of the formattedResults, e.g. "en_US". *)
      ruleResults : (string * RuleResultsData.t) list;
      (** Dictionary of formatted rule results, with one entry for each Page Speed rule instantiated and run by the server. *)
      
    }
    
    val locale : (t, string) GapiLens.t
    val ruleResults : (t, (string * RuleResultsData.t) list) GapiLens.t
    
    val empty : t
    
    val render : t -> GapiJson.json_data_model list
    
    val parse : t -> GapiJson.json_data_model -> t
    
  end
  
  type t = {
    formattedResults : FormattedResultsData.t;
    (** Localized Page Speed results. Contains a ruleResults entry for each Page Speed rule instantiated and run by the server. *)
    id : string;
    (** Canonicalized and final URL for the document, after following page redirects (if any). *)
    invalidRules : string list;
    (** List of rules that were specified in the request, but which the server did not know how to instantiate. *)
    kind : string;
    (** Kind of result. *)
    pageStats : PageStatsData.t;
    (** Summary statistics for the page, such as number of JavaScript bytes, number of HTML bytes, etc. *)
    responseCode : int;
    (** Response code for the document. 200 indicates a normal page load. 4xx/5xx indicates an error. *)
    score : int;
    (** The Page Speed Score (0-100), which indicates how much faster a page could be. A high score indicates little room for improvement, while a lower score indicates more room for improvement. *)
    title : string;
    (** Title of the page, as displayed in the browser's title bar. *)
    version : VersionData.t;
    (** The version of the Page Speed SDK used to generate these results. *)
    
  }
  
  val formattedResults : (t, FormattedResultsData.t) GapiLens.t
  val id : (t, string) GapiLens.t
  val invalidRules : (t, string list) GapiLens.t
  val kind : (t, string) GapiLens.t
  val pageStats : (t, PageStatsData.t) GapiLens.t
  val responseCode : (t, int) GapiLens.t
  val score : (t, int) GapiLens.t
  val title : (t, string) GapiLens.t
  val version : (t, VersionData.t) GapiLens.t
  
  val empty : t
  
  val render : t -> GapiJson.json_data_model list
  
  val parse : t -> GapiJson.json_data_model -> t
  
  val to_data_model : t -> GapiJson.json_data_model
  
  val of_data_model : GapiJson.json_data_model -> t
  
end

