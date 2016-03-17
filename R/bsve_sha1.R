# get harbinger-authentication token for BSVE

bsve_sha1 <- function(api_key , secret_key, email ){
   timestamp <- round(as.numeric(Sys.time())*1000)
   nonce <- sample(1:1e6, 1)
   hmac <- paste(api_key, secret_key, sep= ":") 
   message <- paste0( api_key, timestamp, nonce, email )
   hash <- openssl::sha1( message, key= hmac )
  paste0( "apikey=", api_key, ";timestamp=", timestamp, ";nonce=", nonce, ";signature=", hash)
}
