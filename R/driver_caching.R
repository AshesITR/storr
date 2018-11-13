##' TODO
##'
##' @title Storr with a caching driver
##' @param master Master driver containing all data and keys
##' @param cache Cache driver (should be faster than `master`) containing cached versions of the data and optionally keys
##' @param cache_keys Should the cache also be used for hash lookup? Only use this if data for a particular key is unlikely to change.
##' @param default_namespace Default namespace (see
##'   \code{\link{storr}}).
##' @export
##' @examples
##' TODO
##' 
##' path <- tempfile()
##' st <- storr::storr_cachingstorr(driver_rds(path),
##'                               driver_environment())
##' st$set("a", runif(10))
##' st$get("a")
##'
##' # The data can be also seen by connecting to the rds store
##' rds <- storr::storr_rds(path)
##' rds$list() # empty
##' rds$list_hashes() # here's the data
##' rds$get_value(rds$list_hashes())
##'
##' st$destroy()
storr_cachingstorr <- function(master, cache, default_namespace = "objects", cache_keys = FALSE) {
    storr(driver_cachingstorr(master, cache, cache_keys), default_namespace)
}


driver_cachingstorr <- function(master, cache, cache_keys) {
    R6_driver_cachingstorr$new(master, cache, cache_keys)
}


R6_driver_cachingstorr <- R6::R6Class(
    "driver_cachingstorr",
    cloneable = FALSE,
    public = list(
        master = NULL,
        cache = NULL,
        cache_keys = NULL,
        traits = NULL,
        hash_algorithm = NULL,
        
        get_hash = function(key, namespace) {
            if (self$cache_keys) {
                if (self$cache$exists_hash(key, namespace)) {
                    return(self$cache$get_hash(key, namespace))
                } else {
                    h <- self$master$get_hash(key, namespace)
                    self$cache$set_hash(key, namespace, hash)
                }
            } else {
                self$master$get_hash(key, namespace)
            }
        },
        set_hash = function(key, namespace, hash) {
            if (self$cache_keys) {
                self$cache$set_hash(key, namespace, hash)
            }
            self$master$set_hash(key, namespace, hash)
        },
        get_object = function(hash) {
            if (self$cache$exists_object(hash)) {
                return(self$cache$get_object(hash))
            } else {
                obj <- self$master$get_object(hash)
                self$cache$set_object(hash, obj)
                obj
            }
        },
        set_object = function(hash, value) {
            self$cache$set_object(hash, value)
            self$master$set_object(hash, value)
        },
        exists_hash = function(key, namespace) {
            # Only trust cache on existing keys
            if (self$cache_keys) {
                cache_res <- self$cache$exists_hash(key, namespace)
                if (all(cache_res) && length(cache_res)) return(cache_res)
            }
            
            self$master$exists_hash(key, namespace)
        },
        exists_object = function(hash) {
            # Only trust cache on existing objects
            if (self$cache_keys) {
                cache_res <- self$cache$exists_object(hash)
                if (all(cache_res) && length(cache_res)) return(cache_res)
            }
            
            self$master$exists_object(hash)
        },
        del_hash = function(key, namespace) {
            if (self$cache$exists_hash(key, namespace))
                self$cache$del_hash(key, namespace)
            self$master$del_hash(key, namespace)
        },
        del_object = function(hash) {
            if (self$cache$exists_object(hash))
                self$cache$del_object(hash)
            self$master$del_object(hash)
        },
        
        ## inherited from master
        list_hashes = NULL,
        list_keys = NULL,
        list_namespaces = NULL,
        
        initialize = function(master, cache, cache_keys) {
            self$master <- assert_probably_storr_driver(master)
            self$cache <- assert_probably_storr_driver(cache)
            assert_scalar_logical(cache_keys)
            self$cache_keys <- cache_keys
            
            self$traits <- storr_traits(self$master$traits)
            self$hash_algorithm <- master$hash_algorithm
            
            self$list_hashes <- master$list_hashes
            self$list_keys <- master$list_keys
            self$list_namespaces <- master$list_namespaces
        },
        
        type = function() {
            sprintf("cachingstorr (master: %s, cache: %s, %scaching keys)",
                    self$master$type(), self$cache$type(), {if (self$cache_keys) "" else "not "})
        },
        
        destroy = function() {
            self$master$destroy()
            self$cache$destroy()
        }))
