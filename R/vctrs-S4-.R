
#' S7 classes for vctrs and S4 Vectors
#' useful for S7 method dispatch


class_vctrs <- S7::new_union(S7::class_atomic, S7::class_list,
                             S7::class_data.frame, S7::class_factor,
                             S7::class_Date, S7::class_POSIXct)

class_s4_vctrs <- getClass("Vector")
class_DF <- getClass("DataFrame")