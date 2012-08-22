{-# LANGUAGE MultiParamTypeClasses #-}

module Domains where

import CodeRep

data Kind = Ring | Field

class CodeRep dr => DomainRep dr v where
  kind :: dr a -> Kind
  zero :: dr v
  one :: dr v
  plus :: dr v -> dr v -> dr v
  times :: dr v -> dr v -> dr v
  minus :: dr v -> dr v -> dr v
  uminus :: dr v -> dr v
  div :: dr v -> dr v -> dr v
  better_than :: Maybe (dr v -> dr v -> dr Bool)
  normalizer :: Maybe (dr v -> dr v)


class (CodeRep ctr, DomainRep dr v) => Container2D ctr dr v where
  get :: ctr a -> dr Int -> dr Int -> dr a
  dim1 :: ctr a -> dr Int
  dim2 :: ctr a -> dr Int
  mapper :: Maybe (dr a -> dr b) -> ctr a -> ctr b
  copy :: ctr a -> ctr a
  init :: dr Int -> dr Int -> ctr a
