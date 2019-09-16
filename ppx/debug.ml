let debug_resolve = ref (try ignore (Sys.getenv "PPX_IMPLICITS_DEBUG_RESOLVE"); true with _ -> false)
let debug_unif = ref (try ignore (Sys.getenv "PPX_IMPLICITS_DEBUG_UNIF"); true with _ -> false)
