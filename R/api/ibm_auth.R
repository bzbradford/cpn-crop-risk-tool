# Handle IBM authentication ----

#' Get the authorization token from the authentication server
#' @param url IBM authentication endpoint
#' @param keys list with org_id, tenant_id, and api_key
refresh_auth <- function(url = OPTS$ibm_auth_endpoint, keys = OPTS$ibm_keys) {
  ibm_auth <<- tryCatch({
    req <- request(url) %>%
      req_url_query(orgId = keys$org_id) %>%
      req_headers_redacted(
        "x-ibm-client-id" = sprintf("saascore-%s", keys$tenant_id),
        "x-api-key" = keys$api_key
      ) %>%
      req_timeout(5) %>%
      req_error(is_error = \(resp) FALSE)
    resp <- req_perform(req)
    status <- resp_status(resp)

    if (status >= 400) {
      echo(resp)
      stop("HTTP error ", status)
    }

    message("Authorization token refreshed at ", now("UTC"))
    list(
      timestamp = now("UTC"),
      status = status,
      token = resp_body_string(resp)
    )
  }, error = function(e) {
    message("Failed to get authorization token: ", e$message)
    list(
      timestamp = 1,
      status = 500,
      token = NULL
    )
  })
  saveRDS(ibm_auth, "ibm_auth.rds")
}

# refresh_auth()


#' Get the current IBM token or refresh if needed
#' token is valid for 1 hour
#' @returns auth token
get_ibm_token <- function() {
  # look for a stored token if available
  if (!exists("ibm_auth")) {
    if (file.exists("ibm_auth.rds")) {
      ibm_auth <<- read_rds("ibm_auth.rds")
    } else {
      refresh_auth()
    }
  }

  # if token is stale get a new one
  if (ibm_auth$timestamp < now("UTC") - minutes(59)) refresh_auth()

  ibm_auth$token
}

# get_ibm_token()
