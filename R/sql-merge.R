# x <- structure(list(from = structure(list(from = structure("flights", class = c("ident",
#                                                                                 "sql", "character")), select = structure("*", class = c("sql",
#                                                                                                                                         "character")), where = structure(c("`year` = 2013.0", "`month` = 1.0",
#                                                                                                                                                                            "`day` < 5.0"), class = c("sql", "character")), group_by = character(0),
#                                           having = character(0), order_by = character(0), distinct = FALSE,
#                                           limit = NULL), .Names = c("from", "select", "where", "group_by",
#                                                                     "having", "order_by", "distinct", "limit"), class = c("select_query",
#                                                                                                                           "query")), select = structure(c("year", "month", "day", "carrier",
#                                                                                                                                                           "dep_delay", "air_time", "distance"), .Names = c("year", "month",
#                                                                                                                                                                                                            "day", "carrier", "dep_delay", "air_time", "distance"), class = c("ident",
#                                                                                                                                                                                                                                                                              "sql", "character")), where = character(0), group_by = character(0),
#                     having = character(0), order_by = character(0), distinct = FALSE,
#                     limit = NULL), .Names = c("from", "select", "where", "group_by",
#                                               "having", "order_by", "distinct", "limit"), class = c("select_query",
#                                                                                                     "query"))
#
#
# y <- structure(list(from = structure("flights", class = c("ident",
#                                                           "sql", "character")), select = structure("*", class = c("sql",
#                                                                                                                   "character")), where = structure(c("`year` = 2013.0", "`month` = 1.0",
#                                                                                                                                                      "`day` < 5.0"), class = c("sql", "character")), group_by = character(0),
#                     having = character(0), order_by = character(0), distinct = FALSE,
#                     limit = NULL), .Names = c("from", "select", "where", "group_by",
#                                               "having", "order_by", "distinct", "limit"), class = c("select_query",
#                                                                                                     "query"))
#
# z <- structure(list(from = x, select = structure("*", class = c("sql","character")),
#                     where = character(0),
#                     group_by = character(0),
#                     having = character(0),
#                     order_by = character(0),
#                     distinct = FALSE,
#                     limit = NULL), .Names = c("from", "select", "where", "group_by",
#                                               "having", "order_by", "distinct", "limit"), class = c("select_query",
#                                                                                                     "query"))

#' @export
#' @rdname sql_build
sql_merge <- function(x, y, ...) {
  UseMethod("sql_merge")
}

#' @export
sql_merge.select_query <- function(x, y, ...){

  if (!inherits(x$from, "sql") & !inherits(y$from, "sql")) y <- sql_merge(y, y$from)

  from <- if (inherits(x$from, "sql")) x$from else y$from
  select <- if (all(x$select == "*")) y$select else x$select
  where <- if (length(x$where) > 0) x$where else y$where
  group_by <- if (length(x$group_by) > 0) x$group_by else y$group_by
  having <- if (length(x$having) > 0) x$having else y$having
  order_by <- if (length(x$order_by) > 0) x$order_by else y$order_by
  distinct <- x$distinct | y$distinct
  limit <- if (is.null(x$limit)) y$limit else x$limit

  select_query(
    from = from,
    select = select,
    where = where,
    group_by = group_by,
    having = having,
    order_by = order_by,
    distinct = distinct,
    limit = limit
  )
}
