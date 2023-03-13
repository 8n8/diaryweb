module Indicator exposing
  ( create,
    created,
    userError,
    serverError,
    got,
    get,
    delete,
    deleted
  )

create : Int
create =
  0

created : Int
created =
  1

userError : Int
userError =
  2

serverError : Int
serverError =
  3

got : Int
got =
  4

get : Int
get =
  5

delete : Int
delete =
  6

deleted : Int
deleted =
  7