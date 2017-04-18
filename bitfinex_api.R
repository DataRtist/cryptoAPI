ua <- user_agent("my_user_agent")
ua

bitfinex_api <- function(path) {
  url <- modify_url("https://api.bitfinex.com", path = path)
  
  resp <- GET(url, ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "BitFinex API request failed [%s]\n%s", 
        status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "bitfinex_api"
  )
}
bitfinex_api("/v1/pubticker/btcusb")
bitfinex_api("/v1/pubticker/btcusd")
bitfinex_api("/v1/stats/btcusd")

get_ticker <- function(symbol){
  path <- paste("v1","pubticker",symbol)
  bitfinex_api(path)
}

get_stats <- function(symbol){
  path <- paste0("/",paste("v1","stats",symbol,sep="/"))
  bitfinex_api(path)
}
get_stats("BTCUSD")
